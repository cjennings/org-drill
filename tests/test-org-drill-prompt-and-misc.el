;;; test-org-drill-prompt-and-misc.el --- Tests for prompt formatting and miscellaneous helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for several smaller helpers users see indirectly:
;;
;; - `org-drill--make-minibuffer-prompt': formats the live status line
;;   that appears at the top of every drill prompt — `Y 3 0 5 12 ...'
;;   showing card status / done / failed / mature / new / prompt.
;; - `org-drill-relearn-item': resets the current item's interval to 0
;;   so it gets drilled again.
;; - `org-drill-progress-message': progress bar shown during the
;;   collection scan, only updates every 50 scans.

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

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defun make-marker-at (pos)
  (let ((m (make-marker))) (set-marker m pos) m))

;;;; org-drill--make-minibuffer-prompt

(ert-deftest test-org-drill--make-minibuffer-prompt-includes-status-character ()
  "A :new entry shows `N' as the leading status char."
  (with-fresh-drill-entry
    (with-fixed-now
      (let* ((session (org-drill-session))
             (prompt (org-drill--make-minibuffer-prompt session "test")))
        (should (string-match-p "\\bN\\b" (substring-no-properties prompt)))))))

(ert-deftest test-org-drill--make-minibuffer-prompt-cram-shows-C ()
  "Cram mode displays `C' regardless of the underlying status."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((session (org-drill-session)))
        (oset session cram-mode t)
        (let ((prompt (org-drill--make-minibuffer-prompt session "test")))
          (should (string-match-p "\\bC\\b" (substring-no-properties prompt))))))))

(ert-deftest test-org-drill--make-minibuffer-prompt-failed-shows-F ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (org-schedule nil "2026-04-30")
      (org-set-property "DRILL_LAST_QUALITY" "1")
      (org-set-property "DRILL_LAST_INTERVAL" "5")
      (org-set-property "DRILL_TOTAL_REPEATS" "3")
      (with-fixed-now
        (let* ((session (org-drill-session))
               (prompt (org-drill--make-minibuffer-prompt session "test")))
          (should (string-match-p "\\bF\\b" (substring-no-properties prompt))))))))

(ert-deftest test-org-drill--make-minibuffer-prompt-includes-done-count ()
  "Done count appears in the prompt as a numeric field."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((session (org-drill-session)))
        (oset session done-entries
              (list (make-marker-at 1) (make-marker-at 1) (make-marker-at 1)))
        (let ((prompt (org-drill--make-minibuffer-prompt session "...")))
          ;; 3 done items should appear as " 3 " somewhere
          (should (string-match-p "\\b3\\b" (substring-no-properties prompt))))))))

(ert-deftest test-org-drill--make-minibuffer-prompt-passes-through-prompt-text ()
  "The user's prompt text shows up at the end."
  (with-fresh-drill-entry
    (with-fixed-now
      (let* ((session (org-drill-session))
             (result (org-drill--make-minibuffer-prompt session "type-your-answer")))
        (should (string-match-p "type-your-answer" result))))))

;;;; org-drill-relearn-item

(ert-deftest test-org-drill-relearn-item-resets-interval-to-zero ()
  "Relearning sets DRILL_LAST_INTERVAL back to 0 — the card behaves as new."
  (with-fresh-drill-entry
    (org-drill-store-item-data 30 5 0 5 4.0 2.5)
    (with-fixed-now
      (org-drill-relearn-item))
    ;; After relearn: last-interval should be 0
    (should (= 0 (string-to-number
                  (org-entry-get (point) "DRILL_LAST_INTERVAL"))))))

(ert-deftest test-org-drill-relearn-item-removes-scheduled-stamp ()
  "Relearning unschedules the entry (days-ahead = 0 path)."
  (with-fresh-drill-entry
    (org-drill-store-item-data 30 5 0 5 4.0 2.5)
    (org-schedule nil "2026-06-01")
    (with-fixed-now
      (org-drill-relearn-item))
    (should (null (org-entry-get (point) "SCHEDULED")))))

;;;; org-drill-progress-message

(ert-deftest test-org-drill-progress-message-on-multiple-of-50-emits-message ()
  "Every 50th scan triggers a `Collecting due drill items...' message."
  (let ((message-log-max nil)
        (got-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq got-message (apply #'format fmt args)))))
      (org-drill-progress-message 5 50)
      (should got-message)
      (should (string-match-p "Collecting due drill items" got-message)))))

(ert-deftest test-org-drill-progress-message-on-non-multiple-stays-silent ()
  "Scans that aren't multiples of 50 don't update the message."
  (let ((got-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest _) (setq got-message fmt))))
      (org-drill-progress-message 5 17)   ; 17 % 50 != 0
      (should (null got-message)))))

(ert-deftest test-org-drill-progress-message-includes-collected-count ()
  "The message includes the COLLECTED count."
  (let ((got-message nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq got-message (apply #'format fmt args)))))
      (org-drill-progress-message 42 50)
      (should (string-match-p "42" got-message)))))

(provide 'test-org-drill-prompt-and-misc)

;;; test-org-drill-prompt-and-misc.el ends here
