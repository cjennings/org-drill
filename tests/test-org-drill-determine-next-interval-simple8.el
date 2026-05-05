;;; test-org-drill-determine-next-interval-simple8.el --- Tests for Simple8 algorithm  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for org-drill-determine-next-interval-simple8 and its three
;; pure-math helpers (simple8-first-interval, simple8-interval-factor,
;; simple8-quality->ease).
;;
;; Simple8 differs from SM2 and SM5 in three main ways:
;; - It does not take an EF parameter.  The ease is recomputed from meanq
;;   each call via the polynomial in `org-drill-simple8-quality->ease'.
;; - It returns a 6-element list (not 7).  No optimal-factor matrix.
;; - It supports an optional delta-days parameter for adjusting intervals
;;   on both early and late reviews when org-drill-adjust-intervals-for-
;;   early-and-late-repetitions-p is enabled.
;;
;; Function signature:
;;   (org-drill-determine-next-interval-simple8 last-interval repeats quality
;;                                              failures meanq totaln
;;                                              &optional delta-days)
;;
;; Returns: (NEXT-INTERVAL REPEATS EASE FAILURES AVERAGE-QUALITY TOTAL-REPEATS)

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'testutil-scheduler)

;;; Normal Cases - Successful Reviews

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-first-review-quality-4 ()
  "Normal: first review (repeats=0, last-interval=0) uses first-interval branch."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 0 0 4 0 nil 0 nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result)))
      (should (> interval 0))
      (should (= repeats 1)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-second-review-quality-4 ()
  "Normal: subsequent review uses interval-factor branch and grows the interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result)))
      (should (> interval 0))
      (should (= repeats 2)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-third-review-quality-4 ()
  "Normal: third review continues the interval-factor branch."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 10 2 4 0 4.0 2 nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result)))
      (should (> interval 0))
      (should (= repeats 3)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-quality-5-perfect-recall ()
  "Normal: quality 5 produces a higher ease than the same call with quality 3."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result-q5 (org-drill-determine-next-interval-simple8 10 2 5 0 5.0 2 nil))
           (result-q3 (org-drill-determine-next-interval-simple8 10 2 3 0 3.0 2 nil))
           (ease-q5 (test-scheduler--extract-ease result-q5))
           (ease-q3 (test-scheduler--extract-ease result-q3)))
      (should (> ease-q5 ease-q3)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-quality-3-adequate-recall ()
  "Normal: quality 3 passes (above failure threshold) and increments repeats."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 10 2 3 0 3.0 2 nil))
           (repeats (test-scheduler--extract-repeats result)))
      (should (= repeats 3)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-failure-quality-0 ()
  "Normal: failed review with quality 0 resets interval to -1 and repeats to 0."
  (let* ((result (org-drill-determine-next-interval-simple8 10 3 0 0 nil 3 nil))
         (interval (test-scheduler--extract-interval result))
         (repeats (test-scheduler--extract-repeats result))
         (failures (test-scheduler--extract-failures result)))
    (should (= interval -1))
    (should (= repeats 0))
    (should (= failures 1))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-failure-quality-1 ()
  "Normal: failed review with quality 1 resets interval and increments failures."
  (let* ((result (org-drill-determine-next-interval-simple8 15 5 1 2 3.5 5 nil))
         (interval (test-scheduler--extract-interval result))
         (failures (test-scheduler--extract-failures result)))
    (should (= interval -1))
    (should (= failures 3))))

(ert-deftest test-org-drill-determine-next-interval-simple8-normal-failure-quality-2 ()
  "Normal: quality 2 (= default org-drill-failure-quality) triggers failure path."
  (let* ((result (org-drill-determine-next-interval-simple8 10 3 2 0 nil 3 nil))
         (interval (test-scheduler--extract-interval result)))
    (should (= interval -1))))

;;; Boundary Cases

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-nil-meanq-uses-quality ()
  "Boundary: nil meanq initializes to current quality."
  (let* ((quality 4)
         (result (org-drill-determine-next-interval-simple8 0 0 quality 0 nil 0 nil))
         (meanq (test-scheduler--extract-meanq result)))
    (should (= meanq quality))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-zero-repeats-forces-first-interval ()
  "Boundary: zero repeats forces first-interval branch (last-interval > 0)."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 100 0 4 0 nil 0 nil))
           (interval (test-scheduler--extract-interval result)))
      ;; First-interval is small (around 2.4849), not last-interval * factor.
      (should (> interval 0))
      (should (< interval 10)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-zero-last-interval-forces-first-interval ()
  "Boundary: zero last-interval forces first-interval branch (repeats > 0)."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 0 5 4 0 4.0 5 nil))
           (interval (test-scheduler--extract-interval result)))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-quality-5-maximum ()
  "Boundary: maximum quality 5 is accepted and produces a positive interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 4 1 5 0 nil 1 nil))
           (interval (test-scheduler--extract-interval result)))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-quality-0-minimum ()
  "Boundary: minimum quality 0 triggers the failure path with interval -1."
  (let* ((result (org-drill-determine-next-interval-simple8 10 3 0 0 nil 3 nil))
         (interval (test-scheduler--extract-interval result)))
    (should (= interval -1))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-meanq-weighted-average ()
  "Boundary: meanq is correctly calculated as a weighted average over totaln."
  (let* ((quality 4)
         (meanq 3.0)
         (totaln 10)
         (result (org-drill-determine-next-interval-simple8 10 3 quality 0 meanq totaln nil))
         (new-meanq (test-scheduler--extract-meanq result))
         (expected (/ (+ quality (* meanq totaln 1.0)) (1+ totaln))))
    (should (< (abs (- new-meanq expected)) 0.0001))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-delta-days-nil ()
  "Boundary: nil delta-days (default) skips both early and late adjustments."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 nil))
           (interval (test-scheduler--extract-interval result)))
      (should (numberp interval))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-delta-days-positive-flag-disabled ()
  "Boundary: positive delta-days with flag disabled does not change the interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p nil))
    (let* ((result-no-adjust (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 5))
           (result-no-delta (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 nil))
           (interval-no-adjust (test-scheduler--extract-interval result-no-adjust))
           (interval-no-delta (test-scheduler--extract-interval result-no-delta)))
      (should (= interval-no-adjust interval-no-delta)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-delta-days-positive-flag-enabled ()
  "Boundary: positive delta-days with flag enabled runs late-review adjustment."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))
    (let* ((result (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 5))
           (interval (test-scheduler--extract-interval result)))
      (should (numberp interval))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-delta-days-negative-flag-disabled ()
  "Boundary: negative delta-days with flag disabled does not change the interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p nil))
    (let* ((result-no-adjust (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 -3))
           (result-no-delta (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 nil))
           (interval-no-adjust (test-scheduler--extract-interval result-no-adjust))
           (interval-no-delta (test-scheduler--extract-interval result-no-delta)))
      (should (= interval-no-adjust interval-no-delta)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-delta-days-negative-flag-enabled ()
  "Boundary: negative delta-days with flag enabled runs early-interval-factor."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))
    (let* ((result (org-drill-determine-next-interval-simple8 4 1 4 0 nil 1 -3))
           (interval (test-scheduler--extract-interval result)))
      (should (numberp interval))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-simple8-boundary-totaln-increments-on-both-paths ()
  "Boundary: totaln increments on both success and failure paths.
This matches the SM2 and SM5 behavior so DRILL_TOTAL_REPEATS counts every
review attempt regardless of which scheduling algorithm produced it."
  (let* ((totaln 42)
         (result-success (org-drill-determine-next-interval-simple8 0 0 4 0 nil totaln nil))
         (result-failure (org-drill-determine-next-interval-simple8 10 3 0 0 nil totaln nil)))
    (should (= (test-scheduler--extract-total-repeats result-success) (1+ totaln)))
    (should (= (test-scheduler--extract-total-repeats result-failure) (1+ totaln)))))

;;; Error Cases - cl-assert violations

(defmacro test-scheduler--should-cl-assert (&rest body)
  "Assert BODY signals a cl-assertion-failed via condition-case.

ERT 29.4 installs `ert--should-signal-hook' as `signal-hook-function'
around the entire `ert-deftest' body — not just `should' forms.  That
hook intercepts every signal before any inner `condition-case' can
catch it, so the obvious (should-error FORM) and even a manual
condition-case both fail on 29.4 even though cl-assertion-failed
clearly fires (visible in the test-failure backtrace).

The fix is to shadow `signal-hook-function' to nil inside our
wrapper, letting condition-case catch normally.  The ert-fail call
on the no-error path runs after our shadowing scope exits, so it
still routes through ERT's normal failure handling.

Catches `cl-assertion-failed' by name rather than via the generic
`error' parent — inheritance varies across Emacs versions but the
symbol-name match through condition-case always works."
  `(let ((caught
          (let ((signal-hook-function nil))
            (condition-case _err
                (progn ,@body 'no-error)
              (cl-assertion-failed 'caught)))))
     (unless (eq caught 'caught)
       (ert-fail "expected cl-assertion-failed signal, got none"))))

(ert-deftest test-org-drill-determine-next-interval-simple8-error-negative-repeats ()
  "Error: repeats=-1 violates the (cl-assert (>= repeats 0)) precondition."
  (test-scheduler--should-cl-assert
   (org-drill-determine-next-interval-simple8 0 -1 4 0 nil 0 nil)))

(ert-deftest test-org-drill-determine-next-interval-simple8-error-quality-below-zero ()
  "Error: quality=-1 violates the cl-assert quality range."
  (test-scheduler--should-cl-assert
   (org-drill-determine-next-interval-simple8 0 0 -1 0 nil 0 nil)))

(ert-deftest test-org-drill-determine-next-interval-simple8-error-quality-above-five ()
  "Error: quality=6 violates the cl-assert quality range."
  (test-scheduler--should-cl-assert
   (org-drill-determine-next-interval-simple8 0 0 6 0 nil 0 nil)))

(ert-deftest test-org-drill-determine-next-interval-simple8-error-meanq-above-five ()
  "Error: meanq=10 violates the meanq cl-assert (Simple8-specific check)."
  (test-scheduler--should-cl-assert
   (org-drill-determine-next-interval-simple8 10 3 4 0 10.0 3 nil)))

(ert-deftest test-org-drill-determine-next-interval-simple8-error-meanq-below-zero ()
  "Error: meanq=-1 violates the meanq cl-assert (Simple8-specific check)."
  (test-scheduler--should-cl-assert
   (org-drill-determine-next-interval-simple8 10 3 4 0 -1.0 3 nil)))

;;; Algorithm Verification

(ert-deftest test-org-drill-determine-next-interval-simple8-algorithm-return-value-structure ()
  "Algorithm: return value is a 6-element list (not 7 like SM2/SM5)."
  (let ((result (org-drill-determine-next-interval-simple8 10 3 4 0 3.5 10 nil)))
    (should (listp result))
    (should (= (length result) 6))
    (should (numberp (nth 0 result)))     ; interval
    (should (numberp (nth 1 result)))     ; repeats
    (should (numberp (nth 2 result)))     ; ease
    (should (numberp (nth 3 result)))     ; failures
    (should (numberp (nth 4 result)))     ; meanq
    (should (numberp (nth 5 result)))))   ; totaln

(ert-deftest test-org-drill-determine-next-interval-simple8-algorithm-failure-resets-repeats-to-zero ()
  "Algorithm: failure path resets repeats to 0, regardless of input value."
  (let* ((result (org-drill-determine-next-interval-simple8 10 7 0 0 nil 7 nil))
         (repeats (test-scheduler--extract-repeats result)))
    (should (= repeats 0))))

(ert-deftest test-org-drill-determine-next-interval-simple8-algorithm-ease-derived-from-meanq ()
  "Algorithm: returned ease = simple8-quality->ease(returned-meanq)."
  (let* ((result (org-drill-determine-next-interval-simple8 10 2 4 0 4.0 2 nil))
         (returned-ease (test-scheduler--extract-ease result))
         (returned-meanq (test-scheduler--extract-meanq result))
         (expected-ease (org-drill-simple8-quality->ease returned-meanq)))
    (should (< (abs (- returned-ease expected-ease)) 0.0001))))

(ert-deftest test-org-drill-determine-next-interval-simple8-algorithm-interval-grows-over-successive-reviews ()
  "Algorithm: successive successful quality-4 reviews produce growing intervals."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result-1 (org-drill-determine-next-interval-simple8 0 0 4 0 nil 0 nil))
           (interval-1 (test-scheduler--extract-interval result-1))
           (meanq-1 (test-scheduler--extract-meanq result-1))
           (result-2 (org-drill-determine-next-interval-simple8 interval-1 1 4 0 meanq-1 1 nil))
           (interval-2 (test-scheduler--extract-interval result-2)))
      (should (> interval-2 interval-1)))))

;;; Helper Functions

(ert-deftest test-org-drill-simple8-quality->ease-zero ()
  "Helper: simple8-quality->ease(0) returns the polynomial constant term (1.4515)."
  (let ((ease (org-drill-simple8-quality->ease 0)))
    (should (< (abs (- ease 1.4515)) 0.0001))))

(ert-deftest test-org-drill-simple8-quality->ease-monotonic ()
  "Helper: simple8-quality->ease is monotonically increasing on [0, 5]."
  (let ((ease-0 (org-drill-simple8-quality->ease 0))
        (ease-5 (org-drill-simple8-quality->ease 5)))
    (should (> ease-5 ease-0))))

(ert-deftest test-org-drill-simple8-first-interval-zero-failures ()
  "Helper: simple8-first-interval(0) returns the leading coefficient (2.4849)."
  (let ((interval (org-drill-simple8-first-interval 0)))
    (should (< (abs (- interval 2.4849)) 0.0001))))

(ert-deftest test-org-drill-simple8-first-interval-decreases-with-failures ()
  "Helper: simple8-first-interval is monotonically decreasing in failures."
  (let ((interval-0 (org-drill-simple8-first-interval 0))
        (interval-10 (org-drill-simple8-first-interval 10)))
    (should (< interval-10 interval-0))))

(ert-deftest test-org-drill-simple8-interval-factor-repetition-one ()
  "Helper: simple8-interval-factor returns input ease at repetition=1.
At repetition=1, learn-fraction^log(1,2) = learn-fraction^0 = 1, so the
factor reduces to 1.2 + (ease - 1.2) * 1 = ease."
  (let* ((ease 1.5)
         (factor (org-drill-simple8-interval-factor ease 1)))
    (should (< (abs (- factor ease)) 0.0001))))

(provide 'test-org-drill-determine-next-interval-simple8)
;;; test-org-drill-determine-next-interval-simple8.el ends here
