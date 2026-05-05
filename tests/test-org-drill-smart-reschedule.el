;;; test-org-drill-smart-reschedule.el --- Tests for smart-reschedule  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-smart-reschedule', the function that runs after a
;; user rates a card.  It updates the card's DRILL_* properties via
;; store-item-data and then sets a fresh SCHEDULED stamp.
;;
;; Three days-ahead branches:
;;
;; - 0   → unschedule (treat as new again)
;; - <0  → schedule for today (right now)
;; - >0  → schedule N days from today
;; - nil → use the algorithm-computed next-interval (this branch was
;;         broken before the fix — `(= 0 nil)' errored).
;;
;; The user-facing contract: rate a card, see the next-review date
;; advance to a sensible point.

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
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defun current-scheduled-time-string ()
  "Return SCHEDULED stamp on entry at point or nil."
  (org-entry-get (point) "SCHEDULED"))

;;;; Branch: days-ahead = 0 (unschedule)

(ert-deftest test-org-drill-smart-reschedule-zero-days-ahead-unschedules ()
  "Passing 0 removes the SCHEDULED stamp — the entry is treated as new again."
  (with-fresh-drill-entry
    (org-schedule nil "2026-05-01")
    (with-fixed-now
      (org-drill-smart-reschedule 4 0))
    (should (null (current-scheduled-time-string)))))

;;;; Branch: days-ahead < 0 (today)

(ert-deftest test-org-drill-smart-reschedule-negative-schedules-today ()
  "Negative days-ahead schedules for current-time (today)."
  (with-fresh-drill-entry
    (with-fixed-now
      (org-drill-smart-reschedule 4 -1))
    (let ((scheduled (current-scheduled-time-string)))
      (should scheduled)
      ;; Scheduled stamp matches today's date.
      (should (string-match-p "2026-05-05" scheduled)))))

;;;; Branch: days-ahead > 0 (N days from today)

(ert-deftest test-org-drill-smart-reschedule-positive-schedules-n-days-ahead ()
  "Positive days-ahead schedules exactly N days into the future."
  (with-fresh-drill-entry
    (with-fixed-now
      (org-drill-smart-reschedule 4 7))
    (let ((scheduled (current-scheduled-time-string)))
      (should scheduled)
      ;; 2026-05-05 + 7 days = 2026-05-12.
      (should (string-match-p "2026-05-12" scheduled)))))

;;;; Branch: days-ahead = nil (use algorithm)

(ert-deftest test-org-drill-smart-reschedule-nil-days-ahead-uses-algorithm ()
  "Without days-ahead, fall back to the scheduler's computed next-interval.
Pre-fix this branch crashed with `Wrong type argument: number-or-marker-p, nil'
because the cond compared nil with `=' before the type-guard."
  (with-fresh-drill-entry
    (with-fixed-now
      ;; Should not error.  The scheduled date should be in the future
      ;; (some sensible interval).
      (org-drill-smart-reschedule 5)
      (let ((scheduled (current-scheduled-time-string)))
        (should scheduled)))))

;;;; Algorithm dispatch (covers the cl-case branches in smart-reschedule)

(ert-deftest test-org-drill-smart-reschedule-sm2-algorithm-schedules ()
  "With sm2 selected, smart-reschedule still produces a SCHEDULED stamp."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((org-drill-spaced-repetition-algorithm 'sm2))
        (org-drill-smart-reschedule 5))
      (should (current-scheduled-time-string)))))

(ert-deftest test-org-drill-smart-reschedule-simple8-algorithm-schedules ()
  "With simple8 selected, smart-reschedule still produces a SCHEDULED stamp."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((org-drill-spaced-repetition-algorithm 'simple8))
        (org-drill-smart-reschedule 5))
      (should (current-scheduled-time-string)))))

;;;; Property side-effects

(ert-deftest test-org-drill-smart-reschedule-writes-drill-properties ()
  "Rescheduling also writes DRILL_* properties via store-item-data."
  (with-fresh-drill-entry
    (with-fixed-now
      (org-drill-smart-reschedule 5 7))
    (should (org-entry-get (point) "DRILL_LAST_INTERVAL"))
    (should (org-entry-get (point) "DRILL_TOTAL_REPEATS"))
    (should (org-entry-get (point) "DRILL_EASE"))))

(ert-deftest test-org-drill-smart-reschedule-increments-total-repeats ()
  "Each rating increments DRILL_TOTAL_REPEATS."
  (with-fresh-drill-entry
    (with-fixed-now
      (org-drill-smart-reschedule 4 7)
      (let ((after-first (string-to-number
                          (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))
        (org-drill-smart-reschedule 4 7)
        (let ((after-second (string-to-number
                             (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))
          (should (= (1+ after-first) after-second)))))))

(provide 'test-org-drill-smart-reschedule)

;;; test-org-drill-smart-reschedule.el ends here
