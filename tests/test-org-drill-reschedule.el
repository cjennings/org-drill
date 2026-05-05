;;; test-org-drill-reschedule.el --- Tests for org-drill-reschedule rating loop  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the rating loop in `org-drill-reschedule', the function
;; that prompts the user to rate their recall and schedules the next
;; review.  Drives the loop via mocked `read-key-sequence' returning
;; specific keystrokes.
;;
;; Mock pattern: replace `read-key-sequence' with a stub that returns
;; the desired key string.  The function then walks its rating cond
;; and calls `org-drill-smart-reschedule' just like in production.

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

(defmacro with-key-input (key &rest body)
  "Run BODY with `read-key-sequence' returning KEY (a string)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-key-sequence)
              (lambda (_prompt) ,key))
             ((symbol-function 'sit-for) #'ignore))
     ,@body))

;;;; Quality 0..5 returns the integer rating

(ert-deftest test-org-drill-reschedule-quality-5-returns-5 ()
  "Pressing `5' returns the integer 5 — the user-visible quality rating."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "5"
        (let ((session (org-drill-session)))
          (should (= 5 (org-drill-reschedule session))))))))

(ert-deftest test-org-drill-reschedule-quality-0-returns-0 ()
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "0"
        (let ((session (org-drill-session)))
          (should (= 0 (org-drill-reschedule session))))))))

(ert-deftest test-org-drill-reschedule-quality-3-returns-3 ()
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "3"
        (let ((session (org-drill-session)))
          (should (= 3 (org-drill-reschedule session))))))))

;;;; Quit / edit non-numeric returns

(ert-deftest test-org-drill-reschedule-quit-key-returns-nil ()
  "Pressing the configured quit key returns nil."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input (string org-drill--quit-key)
        (let ((session (org-drill-session)))
          (should (null (org-drill-reschedule session))))))))

(ert-deftest test-org-drill-reschedule-edit-key-returns-edit ()
  "Pressing the configured edit key returns the symbol `edit'."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input (string org-drill--edit-key)
        (let ((session (org-drill-session)))
          (should (eq 'edit (org-drill-reschedule session))))))))

;;;; Side effects on the entry

(ert-deftest test-org-drill-reschedule-rating-records-quality-in-session ()
  "After a rating, the quality is pushed onto the session's qualities slot."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "4"
        (let ((session (org-drill-session)))
          (org-drill-reschedule session)
          (should (member 4 (oref session qualities))))))))

(ert-deftest test-org-drill-reschedule-rating-schedules-next-review ()
  "After a non-failure rating, the entry has a SCHEDULED stamp."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "5"
        (let ((session (org-drill-session)))
          (org-drill-reschedule session)
          (should (org-entry-get (point) "SCHEDULED")))))))

(ert-deftest test-org-drill-reschedule-cram-mode-doesnt-reschedule ()
  "Cram mode skips the reschedule — only ratings are recorded."
  (with-fresh-drill-entry
    (with-fixed-now
      (with-key-input "5"
        (let ((session (org-drill-session)))
          (oset session cram-mode t)
          (org-drill-reschedule session)
          ;; No SCHEDULED was set in cram mode.
          (should (null (org-entry-get (point) "SCHEDULED"))))))))

;;;; Failure threshold → leech tagging

(ert-deftest test-org-drill-reschedule-failure-over-threshold-tags-leech ()
  "Many failures (>= leech-failure-threshold) flips the entry to a leech."
  (with-fresh-drill-entry
    (org-set-property "DRILL_FAILURE_COUNT" "5")        ; 5 prior failures
    (with-fixed-now
      (with-key-input "0"
        (let ((session (org-drill-session))
              (org-drill-leech-failure-threshold 3))
          (org-drill-reschedule session)
          (should (member "leech" (org-get-tags nil t))))))))

(ert-deftest test-org-drill-reschedule-failure-under-threshold-no-leech ()
  "A handful of failures (< threshold) doesn't mark the entry leech."
  (with-fresh-drill-entry
    (org-set-property "DRILL_FAILURE_COUNT" "0")
    (with-fixed-now
      (with-key-input "0"
        (let ((session (org-drill-session))
              (org-drill-leech-failure-threshold 5))
          (org-drill-reschedule session)
          (should-not (member "leech" (org-get-tags nil t))))))))

(provide 'test-org-drill-reschedule)

;;; test-org-drill-reschedule.el ends here
