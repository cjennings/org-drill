;;; test-org-drill-session-state.el --- Tests for session queue predicates  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the predicates and accessors that drive the drill loop's
;; main control flow:
;;
;; - `org-drill-entries-pending-p': are there cards left to drill?
;; - `org-drill-pending-entry-count': how many?
;; - `org-drill-maximum-duration-reached-p': did we hit the time limit?
;; - `org-drill-maximum-item-count-reached-p': did we hit the count limit?
;; - `org-drill--entry-lapsed-p': has this entry crossed the lapse
;;   threshold (very-old, very-overdue)?
;; - `org-drill-free-markers': clean up markers at session end.
;;
;; The user-facing contract: when I start a session, drill until I hit
;; my configured limits or run out of cards, then stop cleanly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defun make-marker-at (point-or-pos)
  "Make a marker pointing at the given position."
  (let ((m (make-marker)))
    (set-marker m point-or-pos)
    m))

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

;;;; org-drill-entries-pending-p

(ert-deftest test-org-drill-entries-pending-p-empty-session-returns-nil ()
  "A fresh session with no entries in any queue is not pending."
  (let ((session (org-drill-session)))
    (should-not (org-drill-entries-pending-p session))))

(ert-deftest test-org-drill-entries-pending-p-current-item-counts ()
  "If there's a current-item being drilled, the session is still pending."
  (let ((session (org-drill-session)))
    (oset session current-item (make-marker-at 1))
    (should (org-drill-entries-pending-p session))))

(ert-deftest test-org-drill-entries-pending-p-again-entries-bypasses-limits ()
  "Items in `again-entries' (failed earlier this session) keep the session
pending even when item-count limits are hit — re-drilling those is mandatory."
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 1))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (oset session again-entries (list (make-marker-at 3)))
    (should (org-drill-entries-pending-p session))))

(ert-deftest test-org-drill-entries-pending-p-respects-item-count-limit ()
  "When max-items is reached and only new-entries remain, no longer pending."
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 2))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (oset session new-entries (list (make-marker-at 3)))
    (should-not (org-drill-entries-pending-p session))))

(ert-deftest test-org-drill-entries-pending-p-non-empty-new-queue ()
  "Cards in any of the prioritized queues count as pending.
The fresh session needs a real start-time — the default initform of
0.0 (epoch) plus the default 20-minute duration limit makes the
session look long-expired."
  (dolist (slot '(new-entries failed-entries young-mature-entries
                  old-mature-entries overdue-entries))
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (eieio-oset session slot (list (make-marker-at 1)))
      (should (org-drill-entries-pending-p session)))))

;;;; org-drill-pending-entry-count

(ert-deftest test-org-drill-pending-entry-count-empty-session-zero ()
  (let ((session (org-drill-session)))
    (should (= 0 (org-drill-pending-entry-count session)))))

(ert-deftest test-org-drill-pending-entry-count-sums-all-queues ()
  "Count includes current-item plus every queue."
  (let ((session (org-drill-session)))
    (oset session current-item (make-marker-at 1))
    (oset session new-entries (list (make-marker-at 2) (make-marker-at 3)))
    (oset session failed-entries (list (make-marker-at 4)))
    (oset session young-mature-entries (list (make-marker-at 5)))
    (oset session old-mature-entries (list (make-marker-at 6) (make-marker-at 7)))
    (oset session overdue-entries (list (make-marker-at 8)))
    (oset session again-entries (list (make-marker-at 9)))
    (should (= 9 (org-drill-pending-entry-count session)))))

(ert-deftest test-org-drill-pending-entry-count-current-item-only-when-marker ()
  "Current-item only contributes 1 when it's a marker (not nil, not other)."
  (let ((session (org-drill-session)))
    (oset session current-item nil)
    (should (= 0 (org-drill-pending-entry-count session)))
    (oset session current-item (make-marker-at 1))
    (should (= 1 (org-drill-pending-entry-count session)))))

;;;; org-drill-maximum-duration-reached-p

(ert-deftest test-org-drill-maximum-duration-reached-p-not-set-returns-nil ()
  "When `org-drill-maximum-duration' is nil, never time out."
  (let ((session (org-drill-session))
        (org-drill-maximum-duration nil))
    (oset session start-time 0.0)            ; epoch start, very long ago
    (should-not (org-drill-maximum-duration-reached-p session))))

(ert-deftest test-org-drill-maximum-duration-reached-p-cram-mode-bypassed ()
  "Cram mode ignores the duration limit — cram all you want."
  (with-fixed-now
    (let ((session (org-drill-session))
          (org-drill-maximum-duration 1))    ; 1 minute
      (oset session start-time 0.0)
      (oset session cram-mode t)
      (should-not (org-drill-maximum-duration-reached-p session)))))

(ert-deftest test-org-drill-maximum-duration-reached-p-fresh-session-not-reached ()
  "A session that just started hasn't hit the duration limit."
  (with-fixed-now
    (let ((session (org-drill-session))
          (org-drill-maximum-duration 30))   ; 30 minutes
      (oset session start-time (float-time (current-time)))
      (should-not (org-drill-maximum-duration-reached-p session)))))

(ert-deftest test-org-drill-maximum-duration-reached-p-old-session-reached ()
  "A session that started far in the past has hit the duration limit."
  (with-fixed-now
    (let ((session (org-drill-session))
          (org-drill-maximum-duration 1))    ; 1 minute
      (oset session start-time 0.0)         ; epoch — way more than 1 min ago
      (should (org-drill-maximum-duration-reached-p session)))))

;;;; org-drill-maximum-item-count-reached-p

(ert-deftest test-org-drill-maximum-item-count-reached-p-not-set-returns-nil ()
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session nil))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (should-not (org-drill-maximum-item-count-reached-p session))))

(ert-deftest test-org-drill-maximum-item-count-reached-p-cram-mode-bypassed ()
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 1))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (oset session cram-mode t)
    (should-not (org-drill-maximum-item-count-reached-p session))))

(ert-deftest test-org-drill-maximum-item-count-reached-p-under-limit ()
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 5))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (should-not (org-drill-maximum-item-count-reached-p session))))

(ert-deftest test-org-drill-maximum-item-count-reached-p-at-limit ()
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 2))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (should (org-drill-maximum-item-count-reached-p session))))

(ert-deftest test-org-drill-maximum-item-count-reached-p-includes-failed-when-flag-set ()
  "When `org-drill-item-count-includes-failed-items-p' is t, again-entries
count toward the limit."
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 3)
        (org-drill-item-count-includes-failed-items-p t))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (oset session again-entries (list (make-marker-at 3)))
    (should (org-drill-maximum-item-count-reached-p session))))

(ert-deftest test-org-drill-maximum-item-count-reached-p-excludes-failed-when-flag-clear ()
  "When the flag is nil, again-entries don't count toward the limit."
  (let ((session (org-drill-session))
        (org-drill-maximum-items-per-session 3)
        (org-drill-item-count-includes-failed-items-p nil))
    (oset session done-entries (list (make-marker-at 1) (make-marker-at 2)))
    (oset session again-entries (list (make-marker-at 3)))
    (should-not (org-drill-maximum-item-count-reached-p session))))

;;;; org-drill--entry-lapsed-p

(ert-deftest test-org-drill--entry-lapsed-p-feature-disabled-returns-nil ()
  "When the lapse feature flag is off, no entry is ever lapsed."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (org-schedule nil "2020-01-01")  ; far past
      (let ((session (org-drill-session))
            (org-drill--lapse-very-overdue-entries-p nil))
        (with-fixed-now
          (should-not (org-drill--entry-lapsed-p session)))))))

(ert-deftest test-org-drill--entry-lapsed-p-old-overdue-entry-flagged ()
  "With the flag on, an entry overdue past the threshold is lapsed."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (org-schedule nil "2020-01-01")  ; ~6 years overdue
      (let ((session (org-drill-session))
            (org-drill--lapse-very-overdue-entries-p t)
            (org-drill-lapse-threshold-days 90))
        (with-fixed-now
          (should (org-drill--entry-lapsed-p session)))))))

(ert-deftest test-org-drill--entry-lapsed-p-recent-entry-not-flagged ()
  "An entry only days overdue isn't lapsed."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (org-schedule nil "2026-05-01")  ; 4 days ago vs threshold 90
      (let ((session (org-drill-session))
            (org-drill--lapse-very-overdue-entries-p t)
            (org-drill-lapse-threshold-days 90))
        (with-fixed-now
          (should-not (org-drill--entry-lapsed-p session)))))))

;;;; org-drill-free-markers

(ert-deftest test-org-drill-free-markers-frees-explicit-list ()
  (with-temp-buffer
    (insert "abc\ndef\nghi\n")
    (let* ((m1 (make-marker-at 1))
           (m2 (make-marker-at 5))
           (m3 (make-marker-at 9))
           (session (org-drill-session)))
      (org-drill-free-markers session (list m1 m2))
      (should (null (marker-position m1)))
      (should (null (marker-position m2)))
      ;; m3 was not in the list — still alive
      (should (numberp (marker-position m3))))))

(ert-deftest test-org-drill-free-markers-t-frees-all-session-markers ()
  "When called with t, every marker across all session queues is freed."
  (with-temp-buffer
    (insert "abc\ndef\nghi\njkl\n")
    (let* ((m-done (make-marker-at 1))
           (m-new (make-marker-at 5))
           (m-failed (make-marker-at 9))
           (m-overdue (make-marker-at 13))
           (session (org-drill-session)))
      (oset session done-entries (list m-done))
      (oset session new-entries (list m-new))
      (oset session failed-entries (list m-failed))
      (oset session overdue-entries (list m-overdue))
      (org-drill-free-markers session t)
      (should (null (marker-position m-done)))
      (should (null (marker-position m-new)))
      (should (null (marker-position m-failed)))
      (should (null (marker-position m-overdue))))))

(provide 'test-org-drill-session-state)

;;; test-org-drill-session-state.el ends here
