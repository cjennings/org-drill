;;; test-org-drill-final-report.el --- Tests for the final-report message  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-final-report' — the wrap-up summary shown at the
;; end of every drill session.  The function builds a multiline message
;; with reviewed-count, duration, per-quality recall percentages, and
;; remaining-queue counts, then waits for a key.
;;
;; Tests mock `input-pending-p' / `read-char-exclusive' / `message' to
;; bypass the wait loop and capture the formatted message.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defmacro with-mocked-final-report-loop (&rest body)
  "Run BODY with the final-report's wait loop short-circuited after one
iteration so `message' actually fires.  Captures the last-shown prompt
to `final-report-message'."
  `(let ((final-report-message nil)
         (loop-iters 0))
     (cl-letf (((symbol-function 'input-pending-p)
                (lambda ()
                  (cl-incf loop-iters)
                  (> loop-iters 1)))
               ((symbol-function 'read-char-exclusive) (lambda (&optional _) ?x))
               ((symbol-function 'sit-for) #'ignore)
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq final-report-message (apply #'format fmt args)))))
       ,@body
       final-report-message)))

(defun make-marker-at-1 ()
  (let ((m (make-marker))) (set-marker m 1) m))

;;;; Format / content

(ert-deftest test-final-report-includes-reviewed-count ()
  "The N-items-reviewed count from done-entries appears."
  (with-temp-buffer
    (let ((session (org-drill-session)))
      (oset session done-entries
            (list (make-marker-at-1) (make-marker-at-1) (make-marker-at-1)))
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (let ((msg (with-mocked-final-report-loop
                    (org-drill-final-report session))))
          (should (string-match-p "3 items reviewed" msg)))))))

(ert-deftest test-final-report-includes-pending-counts ()
  "The remaining-queue line lists failed / overdue / new / young / old."
  (with-temp-buffer
    (let ((session (org-drill-session)))
      (oset session new-entries (list (make-marker-at-1)))
      (oset session young-mature-entries
            (list (make-marker-at-1) (make-marker-at-1)))
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (let ((msg (with-mocked-final-report-loop
                    (org-drill-final-report session))))
          (should (string-match-p "1 new" msg))
          (should (string-match-p "2 young" msg)))))))

(ert-deftest test-final-report-100-percent-pass-skips-warning ()
  "Perfect recall doesn't trigger the second `WARNING!' message."
  (with-temp-buffer
    (let ((session (org-drill-session))
          (warning-shown nil))
      (oset session qualities '(5 5 5 5))
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
                  ((symbol-function 'sit-for) #'ignore)
                  ((symbol-function 'read-char-exclusive)
                   (lambda (&optional msg)
                     (when (and msg (string-match-p "WARNING" msg))
                       (setq warning-shown t))
                     ?x))
                  ((symbol-function 'message) (lambda (&rest _))))
          (org-drill-final-report session)
          (should-not warning-shown))))))

(ert-deftest test-final-report-low-pass-percent-shows-warning ()
  "Pass rate below the forgetting-index complement triggers the warning prompt."
  (with-temp-buffer
    (let ((session (org-drill-session))
          (warning-shown nil))
      (oset session qualities '(0 0 0 0 0))   ; 0% pass
      (oset session dormant-entry-count 5)
      (oset session due-entry-count 5)
      (oset session overdue-entry-count 2)
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
                  ((symbol-function 'sit-for) #'ignore)
                  ((symbol-function 'read-char-exclusive)
                   (lambda (&optional msg)
                     (when (and msg (string-match-p "WARNING" msg))
                       (setq warning-shown t))
                     ?x))
                  ((symbol-function 'message) (lambda (&rest _))))
          (org-drill-final-report session)
          (should warning-shown))))))

(ert-deftest test-final-report-quality-counts-percentages ()
  "Per-quality counts add up: 1 of 4 quality-5 ratings → 25%.
Uses all passing ratings to avoid the warning branch's divide-by-zero
on empty dormant/due counts."
  (with-temp-buffer
    (let ((session (org-drill-session)))
      (oset session qualities '(5 4 4 4))   ; 100% pass, no warning fires
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (let ((msg (with-mocked-final-report-loop
                    (org-drill-final-report session))))
          ;; 1 of 4 = 25% Excellent
          (should (string-match-p "Excellent (5):.*25%" msg))
          ;; 3 of 4 = 75% Good
          (should (string-match-p "Good (4):.*75%" msg)))))))

(ert-deftest test-final-report-warning-branch-with-zero-dormant-and-due-survives ()
  "The warning branch divides by (dormant + due), which is 0 in some
edge cases (cram mode with zero items collected, or pure-failure
sessions on empty queues).

Pre-fix this hit `arith-error'.  The fix wraps the divisor with
`(max 1 ...)' so the percentage gracefully reports 0%."
  (with-temp-buffer
    (let ((session (org-drill-session))
          (warning-shown nil))
      (oset session qualities '(0 0 0))            ; 0% pass
      (oset session dormant-entry-count 0)         ; both zero — the bug shape
      (oset session due-entry-count 0)
      (oset session overdue-entry-count 0)
      (oset session start-time (float-time (encode-time 0 0 12 5 5 2026)))
      (with-fixed-now
        (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
                  ((symbol-function 'sit-for) #'ignore)
                  ((symbol-function 'read-char-exclusive)
                   (lambda (&optional msg)
                     (when (and msg (string-match-p "WARNING" msg))
                       (setq warning-shown t))
                     ?x))
                  ((symbol-function 'message) (lambda (&rest _))))
          ;; Pre-fix this errored out before reaching the assertion.
          (org-drill-final-report session)
          (should warning-shown))))))

(provide 'test-org-drill-final-report)

;;; test-org-drill-final-report.el ends here
