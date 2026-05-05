;;; test-org-drill-orchestration-helpers.el --- Tests for session orchestration helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the helpers extracted from `org-drill' and
;; `org-drill-merge-buffers' during the refactor pass:
;;
;; - `org-drill--prepare-fresh-session': zero out queues + counters
;; - `org-drill--queues-empty-p': has-pending predicate
;; - `org-drill--collect-entries': scan + sort overdue
;; - `org-drill--show-resume-hint': post-suspend message
;; - `org-drill--show-end-message': resume-vs-final-report dispatch
;; - `org-drill--build-dest-id-table': merge-buffers id→marker scan
;; - `org-drill--copy-scheduling-to-marker': merge-buffers per-entry copy
;; - `org-drill--strip-unmatched-dest-entries': merge-buffers cleanup

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defun make-marker-at (pos)
  (let ((m (make-marker))) (set-marker m pos) m))

(defmacro with-org-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

;;;; org-drill--prepare-fresh-session

(ert-deftest test-prepare-fresh-session-zeros-counters ()
  "Session counters and queues are reset to their initform-equivalents."
  (let ((session (org-drill-session)))
    (oset session done-entries (list (make-marker-at 1)))
    (oset session dormant-entry-count 99)
    (oset session new-entries (list (make-marker-at 1)))
    (oset session due-entry-count 50)
    (org-drill--prepare-fresh-session session nil)
    (should (null (oref session done-entries)))
    (should (= 0 (oref session dormant-entry-count)))
    (should (null (oref session new-entries)))
    (should (= 0 (oref session due-entry-count)))))

(ert-deftest test-prepare-fresh-session-sets-cram-mode ()
  "The cram arg flips the session's cram-mode slot."
  (let ((session (org-drill-session)))
    (org-drill--prepare-fresh-session session t)
    (should (oref session cram-mode))
    (org-drill--prepare-fresh-session session nil)
    (should-not (oref session cram-mode))))

(ert-deftest test-prepare-fresh-session-sets-start-time ()
  "start-time is set to the current float-time."
  (let ((session (org-drill-session)))
    (org-drill--prepare-fresh-session session nil)
    (should (numberp (oref session start-time)))
    (should (> (oref session start-time) 0.0))))

;;;; org-drill--queues-empty-p

(ert-deftest test-queues-empty-p-fresh-session-true ()
  (should (org-drill--queues-empty-p (org-drill-session))))

(ert-deftest test-queues-empty-p-with-current-item-false ()
  (let ((session (org-drill-session)))
    (oset session current-item (make-marker-at 1))
    (should-not (org-drill--queues-empty-p session))))

(ert-deftest test-queues-empty-p-with-any-queue-non-empty-false ()
  "Any of the five drill queues being non-empty makes the predicate false."
  (dolist (slot '(new-entries failed-entries overdue-entries
                  young-mature-entries old-mature-entries))
    (let ((session (org-drill-session)))
      (eieio-oset session slot (list (make-marker-at 1)))
      (should-not (org-drill--queues-empty-p session)))))

;;;; org-drill--collect-entries

(ert-deftest test-collect-entries-populates-from-tempfile ()
  "Scans a buffer and routes entries into session queues based on status."
  (with-org-tempfile "* First :drill:\nbody one\n* Second :drill:\nbody two\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill--collect-entries session 'file nil)
        ;; Both unscheduled drill entries are :new
        (should (= 2 (length (oref session new-entries))))))))

(ert-deftest test-collect-entries-sets-overdue-count ()
  "After collection, overdue-entry-count matches the overdue queue length."
  (with-org-tempfile "* First :drill:\nbody\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill--collect-entries session 'file nil)
        (should (= (length (oref session overdue-entries))
                   (oref session overdue-entry-count)))))))

;;;; org-drill--show-resume-hint and --show-end-message

(ert-deftest test-show-resume-hint-emits-message ()
  "Resume-hint includes the literal `org-drill-resume' command name."
  (let ((messages-seen nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages-seen))))
      (org-drill--show-resume-hint)
      (should (cl-some (lambda (m) (string-match-p "org-drill-resume" m))
                       messages-seen)))))

(ert-deftest test-show-end-message-suspended-takes-resume-branch ()
  "When end-pos is set, dispatcher shows the resume hint, not the final report."
  (let ((session (org-drill-session))
        (resume-hint-shown nil)
        (final-report-shown nil))
    (oset session end-pos :quit)
    (cl-letf (((symbol-function 'org-drill--show-resume-hint)
               (lambda () (setq resume-hint-shown t)))
              ((symbol-function 'org-drill-final-report)
               (lambda (_) (setq final-report-shown t))))
      (org-drill--show-end-message session)
      (should resume-hint-shown)
      (should-not final-report-shown))))

(ert-deftest test-show-end-message-clean-completion-runs-final-report ()
  "When end-pos is nil, dispatcher runs final-report and skips resume hint."
  (let ((session (org-drill-session))
        (resume-hint-shown nil)
        (final-report-shown nil)
        (org-drill-save-buffers-after-drill-sessions-p nil))
    (oset session end-pos nil)
    (cl-letf (((symbol-function 'org-drill--show-resume-hint)
               (lambda () (setq resume-hint-shown t)))
              ((symbol-function 'org-drill-final-report)
               (lambda (_) (setq final-report-shown t)))
              ((symbol-function 'persist-save) #'ignore)
              ((symbol-function 'sit-for) #'ignore))
      (org-drill--show-end-message session)
      (should final-report-shown)
      (should-not resume-hint-shown))))

(ert-deftest test-show-end-message-marker-end-pos-jumps-to-marker ()
  "When end-pos is a live marker, dispatcher navigates to it before showing
the resume hint."
  (let ((tmpfile (make-temp-file "org-drill-end-pos-" nil ".org")))
    (unwind-protect
        (let ((session (org-drill-session))
              (jumped-to nil))
          (with-current-buffer (find-file-noselect tmpfile)
            (insert "* First :drill:\n* Second :drill:\nbody\n")
            (org-mode)
            (goto-char (point-max))
            (let ((m (point-marker)))
              (oset session end-pos m)
              (cl-letf (((symbol-function 'org-drill-goto-entry)
                         (lambda (mk) (setq jumped-to mk)))
                        ((symbol-function 'org-reveal) #'ignore)
                        ((symbol-function 'org-fold-show-entry) #'ignore)
                        ((symbol-function 'org-drill--show-resume-hint) #'ignore))
                (org-drill--show-end-message session)
                (should (eq jumped-to m))))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

;;;; org-drill--build-dest-id-table

(ert-deftest test-build-dest-id-table-populates-id-marker-pairs ()
  "Each entry with an :ID: ends up keyed in `org-drill-dest-id-table'."
  (with-org-tempfile "* First :drill:\n:PROPERTIES:\n:ID: abc\n:END:\n* Second :drill:\n:PROPERTIES:\n:ID: def\n:END:\n"
    (clrhash org-drill-dest-id-table)
    (org-drill--build-dest-id-table (current-buffer))
    (should (gethash "abc" org-drill-dest-id-table))
    (should (gethash "def" org-drill-dest-id-table))))

(ert-deftest test-build-dest-id-table-skips-entries-without-id ()
  (with-org-tempfile "* First :drill:\nno-id\n* Second :drill:\n:PROPERTIES:\n:ID: abc\n:END:\n"
    (clrhash org-drill-dest-id-table)
    (org-drill--build-dest-id-table (current-buffer))
    (should (= 1 (hash-table-count org-drill-dest-id-table)))
    (should (gethash "abc" org-drill-dest-id-table))))

;;;; org-drill--copy-scheduling-to-marker

(ert-deftest test-copy-scheduling-to-marker-writes-properties ()
  "Source's scheduling state lands on the destination marker."
  (let ((src-file (make-temp-file "org-drill-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-dst-" nil ".org")))
    (unwind-protect
        (let (dst-marker)
          (with-current-buffer (find-file-noselect dst-file)
            (let ((org-startup-folded nil))
              (insert "* Question :drill:\n")
              (org-mode)
              (goto-char (point-min))
              (setq dst-marker (point-marker))))
          (with-current-buffer (find-file-noselect src-file)
            (let ((org-startup-folded nil))
              (insert "* Question :drill:\n")
              (org-mode)
              (goto-char (point-min))
              (org-drill-store-item-data 10 3 1 5 3.8 2.4)
              (org-drill--copy-scheduling-to-marker dst-marker)))
          (with-current-buffer (find-file-noselect dst-file)
            (goto-char (point-min))
            (should (equal "10.0" (org-entry-get (point) "DRILL_LAST_INTERVAL")))
            (should (equal "5" (org-entry-get (point) "DRILL_TOTAL_REPEATS")))))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

(ert-deftest test-copy-scheduling-to-marker-skips-virgin-source ()
  "Source with total-repeats=0 leaves the destination's data untouched."
  (let ((src-file (make-temp-file "org-drill-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-dst-" nil ".org")))
    (unwind-protect
        (let (dst-marker)
          (with-current-buffer (find-file-noselect dst-file)
            (let ((org-startup-folded nil))
              (insert "* Question :drill:\n")
              (org-mode)
              (goto-char (point-min))
              ;; Pre-existing data on dest:
              (org-set-property "DRILL_TOTAL_REPEATS" "99")
              (setq dst-marker (point-marker))))
          (with-current-buffer (find-file-noselect src-file)
            (let ((org-startup-folded nil))
              (insert "* Question :drill:\n")
              (org-mode)
              (goto-char (point-min))
              ;; Source is virgin (no rate calls).
              (org-drill--copy-scheduling-to-marker dst-marker)))
          (with-current-buffer (find-file-noselect dst-file)
            (goto-char (point-min))
            ;; Dest's TOTAL_REPEATS was stripped to nil (strip runs first
            ;; unconditionally), but no fresh data was written.
            (should-not (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

;;;; org-drill--strip-unmatched-dest-entries

(ert-deftest test-strip-unmatched-dest-entries-clears-properties ()
  "Every entry left in the table has its scheduling props stripped."
  (with-org-tempfile "* Question :drill:\n"
    (org-drill-store-item-data 10 3 1 5 3.8 2.4)
    (clrhash org-drill-dest-id-table)
    (puthash "stale-id" (point-marker) org-drill-dest-id-table)
    (org-drill--strip-unmatched-dest-entries)
    (goto-char (point-min))
    (should-not (org-entry-get (point) "DRILL_LAST_INTERVAL"))
    (should-not (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))

(provide 'test-org-drill-orchestration-helpers)

;;; test-org-drill-orchestration-helpers.el ends here
