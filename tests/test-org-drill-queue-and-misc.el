;;; test-org-drill-queue-and-misc.el --- Tests for queue popping, fontification, and miscellaneous helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for several disparate small functions:
;;
;; - `org-drill-pop-next-pending-entry': priority-aware queue popper
;;   (failed → overdue → young → new/old → again).  Drives which card
;;   is shown next during a session.
;; - `org-drill-card-tag-caller': dispatches per-card-tag hook functions
;;   from `org-drill-card-tags-alist'.
;; - `org-drill-id-get-create-with-warning': creates an :ID: on demand
;;   and warns the user once per session.
;; - `org-drill-add-cloze-fontification': installs buffer-local cloze
;;   regex/font-lock spec.
;; - `org-drill-strip-all-data': bulk version of strip-entry-data.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-drill)

;;;; Helpers

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

;;;; org-drill-pop-next-pending-entry

(ert-deftest test-org-drill-pop-next-pending-entry-empty-session-returns-nil ()
  (with-fixed-now
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (should (null (org-drill-pop-next-pending-entry session))))))

(ert-deftest test-org-drill-pop-next-pending-entry-prioritizes-failed-first ()
  "When failed-entries are present, they're popped before overdue/young/new."
  (with-fixed-now
    (with-temp-buffer
      (insert "* Failed :drill:\n\n* New :drill:\n\n* Old :drill:\n")
      (org-mode)
      (let* ((session (org-drill-session))
             (m-failed (save-excursion (goto-char (point-min))
                                       (point-marker)))
             (m-new (save-excursion (goto-char (point-min))
                                    (re-search-forward "^\\* New" nil t)
                                    (line-beginning-position)
                                    (point-marker)))
             (m-old (save-excursion (goto-char (point-min))
                                    (re-search-forward "^\\* Old" nil t)
                                    (line-beginning-position)
                                    (point-marker))))
        (oset session start-time (float-time (current-time)))
        (oset session failed-entries (list m-failed))
        (oset session new-entries (list m-new))
        (oset session old-mature-entries (list m-old))
        (let ((popped (org-drill-pop-next-pending-entry session)))
          (should (eq m-failed popped))
          ;; failed-entries is now empty.
          (should (null (oref session failed-entries))))))))

(ert-deftest test-org-drill-pop-next-pending-entry-falls-through-to-again ()
  "When all primary queues are empty but again-entries has items, pops from again."
  (with-fixed-now
    (with-temp-buffer
      (insert "* Again :drill:\n")
      (org-mode)
      (let* ((session (org-drill-session))
             (m (save-excursion (goto-char (point-min)) (point-marker))))
        (oset session start-time (float-time (current-time)))
        (oset session again-entries (list m))
        (let ((popped (org-drill-pop-next-pending-entry session)))
          (should (eq m popped))
          (should (null (oref session again-entries))))))))

(ert-deftest test-org-drill-pop-next-pending-entry-respects-max-item-limit ()
  "Once max-items is reached, primary queues are skipped — only again-entries is reachable."
  (with-fixed-now
    (with-temp-buffer
      (insert "* Done :drill:\n* Done :drill:\n* New :drill:\n* Again :drill:\n")
      (org-mode)
      (let* ((session (org-drill-session))
             (org-drill-maximum-items-per-session 2)
             (m-done1 (save-excursion (goto-char (point-min)) (point-marker)))
             (m-done2 (save-excursion (goto-char (point-min))
                                      (re-search-forward "^\\* Done" nil t 2)
                                      (line-beginning-position)
                                      (point-marker)))
             (m-new (save-excursion (goto-char (point-min))
                                    (re-search-forward "^\\* New" nil t)
                                    (line-beginning-position)
                                    (point-marker)))
             (m-again (save-excursion (goto-char (point-min))
                                      (re-search-forward "^\\* Again" nil t)
                                      (line-beginning-position)
                                      (point-marker))))
        (oset session start-time (float-time (current-time)))
        (oset session done-entries (list m-done1 m-done2))   ; at limit
        (oset session new-entries (list m-new))
        (oset session again-entries (list m-again))
        (let ((popped (org-drill-pop-next-pending-entry session)))
          ;; new-entries is gated by limit; again-entries isn't → pops m-again.
          (should (eq m-again popped))
          ;; new-entries unchanged.
          (should (member m-new (oref session new-entries))))))))

;;;; org-drill-card-tag-caller

(ert-deftest test-org-drill-card-tag-caller-runs-mapped-function ()
  "When TAG is in `org-drill-card-tags-alist', the ITEM'th function is called."
  (let* ((called nil)
         (org-drill-card-tags-alist
          `(("mytag" ,(lambda () (setq called 'first))
                     ,(lambda () (setq called 'second))))))
    (org-drill-card-tag-caller 1 "mytag")
    (should (eq 'first called))
    (org-drill-card-tag-caller 2 "mytag")
    (should (eq 'second called))))

(ert-deftest test-org-drill-card-tag-caller-unknown-tag-no-op ()
  "Calling with a tag not in the alist falls through to `ignore' silently."
  (let ((org-drill-card-tags-alist nil))
    ;; should not error
    (should (null (org-drill-card-tag-caller 1 "no-such-tag")))))

;;;; org-drill-id-get-create-with-warning

(defmacro with-tempfile-org-buffer (content &rest body)
  "Run BODY in a buffer visiting a temp file with CONTENT.
`org-id-get-create' refuses to operate on non-file-visiting buffers,
so the id-creation tests need a real (if temporary) backing file."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-org-drill-id-get-create-with-warning-creates-id-and-flags-session ()
  "On the first call against an entry without an ID, the session's
warned-about-id-creation slot flips to t and a fresh ID is returned."
  (with-tempfile-org-buffer "* Question :drill:\n"
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'sit-for) #'ignore))
        (let ((id (org-drill-id-get-create-with-warning session)))
          (should (stringp id))
          (should (oref session warned-about-id-creation)))))))

(ert-deftest test-org-drill-id-get-create-with-warning-doesnt-rewarn ()
  "Once warned, the session flag stays set and no extra warning fires."
  (with-tempfile-org-buffer "* Question :drill:\n"
    (let ((session (org-drill-session)))
      (oset session warned-about-id-creation t)
      (cl-letf (((symbol-function 'sit-for) #'ignore))
        (org-drill-id-get-create-with-warning session)
        (should (oref session warned-about-id-creation))))))

;;;; org-drill-add-cloze-fontification

(ert-deftest test-org-drill-add-cloze-fontification-sets-buffer-local-regex ()
  "Sets buffer-local `org-drill-cloze-regexp' built from the current delimiters."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (org-mode)
      (let ((org-drill-left-cloze-delimiter "{{")
            (org-drill-right-cloze-delimiter "}}"))
        (org-drill-add-cloze-fontification)
        (should (local-variable-p 'org-drill-cloze-regexp))
        ;; The buffer-local regex matches the new delimiters.
        (should (string-match-p org-drill-cloze-regexp "test {{x}} more"))
        (should-not (string-match-p org-drill-cloze-regexp "test [x] more"))))))

(ert-deftest test-org-drill-add-cloze-fontification-sets-buffer-local-keywords ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (org-mode)
      (org-drill-add-cloze-fontification)
      (should (local-variable-p 'org-drill-cloze-keywords))
      (should (listp org-drill-cloze-keywords)))))

;;;; org-drill-strip-all-data

(ert-deftest test-org-drill-strip-all-data-no-confirm-no-action ()
  "Without yes-or-no-p confirmation, nothing changes — destructive
actions require explicit consent."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-store-item-data 10 3 1 5 3.8 2.4)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (org-drill-strip-all-data)
        ;; Properties survive — user said no.
        (should (org-entry-get (point) "DRILL_LAST_INTERVAL"))))))

(ert-deftest test-org-drill-strip-all-data-with-confirm-strips ()
  "With yes-or-no-p approval, every drill scheduling property is removed."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-store-item-data 10 3 1 5 3.8 2.4)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-drill-strip-all-data)
        (dolist (prop org-drill-scheduling-properties)
          (should (null (org-entry-get (point) prop))))))))

(provide 'test-org-drill-queue-and-misc)

;;; test-org-drill-queue-and-misc.el ends here
