;;; test-org-drill-navigation-and-leitner-rebox.el --- Tests for navigation helpers and leitner-rebox  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for:
;;
;; - `org-drill-goto-entry': switches to a marker's buffer and position
;; - `org-drill-goto-drill-entry-heading': moves up the outline until
;;   it finds the heading that owns the :drill: tag (handles being
;;   called from a sub-heading)
;; - `org-drill-command-keybinding-to-string': human-readable key
;;   description, used in messages like "Press C-c d to resume"
;; - `org-drill-push-end': in-place list-end push macro
;; - `org-drill-leitner-rebox': the rating loop for Leitner cards.
;;   Tests drive ratings 0..5 by mocking `read-key-sequence' and
;;   verify the DRILL_LEITNER_BOX property updates correctly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fresh-drill-entry (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\nbody\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-leitner-entry-in-box (box-num &rest body)
  "Run BODY at point on a leitner-tagged entry currently in BOX-NUM."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert (format "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: %d\n:END:\nbody\n" ,box-num))
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defun stub-read-key-sequence-with-string (return-string)
  "Make `read-key-sequence' return the given string."
  (cl-letf (((symbol-function 'read-key-sequence)
             (lambda (_prompt) return-string)))))

;;;; org-drill-goto-entry

(ert-deftest test-org-drill-goto-entry-jumps-to-marker-position ()
  "After `goto-entry', point matches the marker's position."
  (with-temp-buffer
    (insert "abcdefghij\n")
    (let ((m (make-marker)))
      (set-marker m 5)
      (org-drill-goto-entry m)
      (should (= 5 (point))))))

;;;; org-drill-goto-drill-entry-heading

(ert-deftest test-org-drill-goto-drill-entry-heading-on-drill-stays-put ()
  "Called on the drill heading itself, point doesn't move."
  (with-fresh-drill-entry
    (let ((before (point)))
      (org-drill-goto-drill-entry-heading)
      (should (= before (point))))))

(ert-deftest test-org-drill-goto-drill-entry-heading-from-child-walks-up ()
  "Called inside a sub-heading of a drill entry, walks up to the parent."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Parent :drill:\n** Child\nchild body\n")
      (org-mode)
      (re-search-backward "^\\*\\* Child")
      (org-drill-goto-drill-entry-heading)
      (should (looking-at "^\\* Parent")))))

(ert-deftest test-org-drill-goto-drill-entry-heading-non-drill-errors ()
  "Called outside a drill entry → user-visible error."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Plain heading\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (should-error (org-drill-goto-drill-entry-heading)))))

;;;; org-drill-command-keybinding-to-string

(ert-deftest test-org-drill-command-keybinding-to-string-unbound-returns-nil ()
  "When CMD has no binding, returns nil."
  (let ((cmd (make-symbol "fictional-org-drill-command-no-binding")))
    (should (null (org-drill-command-keybinding-to-string cmd)))))

(ert-deftest test-org-drill-command-keybinding-to-string-bound-returns-string ()
  (let ((cmd (make-symbol "fictional-test-cmd")))
    (defalias cmd #'ignore)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c x") cmd)
      (let ((overriding-local-map map))
        (let ((result (org-drill-command-keybinding-to-string cmd)))
          (should (stringp result))
          (should (string-match-p "C-c x" result)))))))

;;;; org-drill-push-end

(ert-deftest test-org-drill-push-end-appends-to-list-place ()
  (let ((lst (list 1 2 3)))
    (org-drill-push-end 4 lst)
    (should (equal '(1 2 3 4) lst))))

(ert-deftest test-org-drill-push-end-on-empty-list ()
  (let ((lst nil))
    (org-drill-push-end 'a lst)
    (should (equal '(a) lst))))

;;;; org-drill-leitner-rebox

(ert-deftest test-org-drill-leitner-rebox-rating-0-resets-to-box-1 ()
  "Quality 0 demotes the card all the way down to box 1."
  (with-leitner-entry-in-box 4
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "0"))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-leitner-rebox session)))
          (should (eq t result))
          (should (equal "1" (org-entry-get (point) "DRILL_LEITNER_BOX"))))))))

(ert-deftest test-org-drill-leitner-rebox-rating-1-decrements-by-one ()
  "Quality 1 demotes by one box."
  (with-leitner-entry-in-box 3
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "1"))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-leitner-rebox session)
        (should (equal "2" (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(ert-deftest test-org-drill-leitner-rebox-rating-1-from-box-1-stays-at-box-1 ()
  "Quality 1 from box 1 stays at box 1 — can't go lower."
  (with-leitner-entry-in-box 1
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "1"))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-leitner-rebox session)
        (should (equal "1" (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(ert-deftest test-org-drill-leitner-rebox-rating-2-stays-in-current-box ()
  "Quality 2 leaves the box unchanged."
  (with-leitner-entry-in-box 3
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "2"))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-leitner-rebox session)
        (should (equal "3" (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(ert-deftest test-org-drill-leitner-rebox-rating-3-promotes-by-one ()
  (with-leitner-entry-in-box 2
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "3"))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-leitner-rebox session)
        (should (equal "3" (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(ert-deftest test-org-drill-leitner-rebox-rating-5-promotes-by-one ()
  (with-leitner-entry-in-box 3
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) "5"))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-leitner-rebox session)
        (should (equal "4" (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(ert-deftest test-org-drill-leitner-rebox-quit-key-returns-quit ()
  "Pressing the configured quit key returns the symbol `quit'."
  (with-leitner-entry-in-box 2
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-key-sequence)
                 (lambda (_prompt) (string org-drill--quit-key)))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-leitner-rebox session)))
          (should (eq 'quit result)))))))

(provide 'test-org-drill-navigation-and-leitner-rebox)

;;; test-org-drill-navigation-and-leitner-rebox.el ends here
