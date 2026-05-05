;;; test-org-drill-scheduler-helpers.el --- Tests for SM2/SM5 helper math  -*- lexical-binding: t; -*-

;;; Commentary:
;; Direct unit tests for the small pure helpers underneath the SM2 and SM5
;; schedulers.  These are exercised transitively by the top-level scheduler
;; tests in test-org-drill-determine-next-interval-{sm2,sm5}.el, but a direct
;; suite gives faster feedback when a helper breaks and pins down each
;; helper's contract independently.
;;
;; Helpers covered:
;; - `org-drill-round-float'
;; - `org-drill-modify-e-factor' (SM2/SM5 e-factor update rule)
;; - `org-drill-modify-of' (optimal-factor smoothing)
;; - `org-drill-set-optimal-factor' (matrix update)
;; - `org-drill-initial-optimal-factor-sm5' (first-review default)
;; - `org-drill-get-optimal-factor-sm5' (matrix lookup with fallback)
;; - `org-drill-inter-repetition-interval-sm5' (interval calculation)
;; - `org-drill-early-interval-factor' (early-review adjustment)
;; - `org-drill-random-dispersal-factor' (bounded random distribution)
;; - `org-drill--safe-read-learn-data' (safe LEARN_DATA parser)

;;; Code:

(require 'ert)
(require 'org-drill)

;;;; org-drill-round-float

(ert-deftest test-org-drill-round-float-normal-three-decimals ()
  (should (equal 3.568 (org-drill-round-float 3.56755765 3))))

(ert-deftest test-org-drill-round-float-normal-zero-decimals ()
  (should (equal 4.0 (org-drill-round-float 3.56755765 0))))

(ert-deftest test-org-drill-round-float-boundary-already-clean ()
  (should (equal 1.5 (org-drill-round-float 1.5 2))))

(ert-deftest test-org-drill-round-float-boundary-negative-number ()
  (should (equal -2.346 (org-drill-round-float -2.34567 3))))

(ert-deftest test-org-drill-round-float-boundary-zero-input ()
  (should (equal 0.0 (org-drill-round-float 0.0 3))))

;;;; org-drill-modify-e-factor

(ert-deftest test-org-drill-modify-e-factor-normal-quality-5-increases-ef ()
  "Quality-5 (perfect recall) increases the e-factor."
  (should (> (org-drill-modify-e-factor 2.5 5) 2.5)))

(ert-deftest test-org-drill-modify-e-factor-normal-quality-3-decreases-ef ()
  "Quality-3 (correct after hesitation) decreases the e-factor."
  (should (< (org-drill-modify-e-factor 2.5 3) 2.5)))

(ert-deftest test-org-drill-modify-e-factor-normal-quality-4-near-stable ()
  "Quality-4 keeps the e-factor approximately stable."
  (let ((result (org-drill-modify-e-factor 2.5 4)))
    (should (and (> result 2.49) (< result 2.51)))))

(ert-deftest test-org-drill-modify-e-factor-boundary-floor-at-1.3 ()
  "EF below 1.3 is clamped up to 1.3."
  (should (equal 1.3 (org-drill-modify-e-factor 1.0 3)))
  (should (equal 1.3 (org-drill-modify-e-factor 0.5 5)))
  (should (equal 1.3 (org-drill-modify-e-factor 1.29 5))))

(ert-deftest test-org-drill-modify-e-factor-boundary-ef-at-floor-passes-through ()
  "EF exactly 1.3 takes the normal update path (not clamped)."
  ;; Note: the implementation uses `<' not `<=' for the floor check,
  ;; so 1.3 passes through to the formula.
  (let ((result (org-drill-modify-e-factor 1.3 5)))
    (should (numberp result))
    (should (>= result 1.3))))

;;;; org-drill-modify-of

(ert-deftest test-org-drill-modify-of-normal-fraction-0-returns-of ()
  "Fraction 0 means \"no movement\" — return OF unchanged."
  (should (equal 2.5 (org-drill-modify-of 2.5 4 0))))

(ert-deftest test-org-drill-modify-of-normal-fraction-1-returns-temp ()
  "Fraction 1 means \"all the way to the new value\" — return temp."
  ;; temp = of * (0.72 + q * 0.07) = 2.5 * (0.72 + 4 * 0.07) = 2.5 * 1.0 = 2.5
  (should (equal 2.5 (org-drill-modify-of 2.5 4 1))))

(ert-deftest test-org-drill-modify-of-boundary-fraction-half ()
  "Fraction 0.5 is the midpoint between OF and temp."
  ;; of = 2.5, q = 5: temp = 2.5 * (0.72 + 0.35) = 2.5 * 1.07 = 2.675
  ;; midpoint = (2.5 + 2.675) / 2 = 2.5875
  (let ((result (org-drill-modify-of 2.5 5 0.5)))
    (should (and (> result 2.58) (< result 2.59)))))

;;;; org-drill-set-optimal-factor

(ert-deftest test-org-drill-set-optimal-factor-normal-new-n-creates-entry ()
  "Setting OF for a fresh N adds it to the matrix."
  (let* ((m '((1 (2.5 . 1.0))))
         (result (org-drill-set-optimal-factor 2 2.5 m 1.5)))
    (should (assoc 2 result))
    (should (equal 1.5 (cdr (assoc 2.5 (cdr (assoc 2 result))))))))

(ert-deftest test-org-drill-set-optimal-factor-normal-existing-n-new-ef ()
  "Setting OF for an existing N but new EF adds the (EF . OF) pair."
  (let* ((m (list (list 1 (cons 2.5 1.0))))
         (result (org-drill-set-optimal-factor 1 3.0 m 1.7)))
    (should (equal 1.7 (cdr (assoc 3.0 (cdr (assoc 1 result)))))) ))

(ert-deftest test-org-drill-set-optimal-factor-normal-existing-n-and-ef-updates ()
  "Setting OF for an existing (N, EF) pair updates the value in place."
  (let* ((m (list (list 1 (cons 2.5 1.0))))
         (result (org-drill-set-optimal-factor 1 2.5 m 1.7)))
    (should (equal 1.7 (cdr (assoc 2.5 (cdr (assoc 1 result))))))))

;;;; org-drill-initial-optimal-factor-sm5

(ert-deftest test-org-drill-initial-optimal-factor-sm5-n-equals-1-uses-initial-interval ()
  "First-review (n=1) returns the configured initial interval."
  (should (equal org-drill-sm5-initial-interval
                 (org-drill-initial-optimal-factor-sm5 1 2.5))))

(ert-deftest test-org-drill-initial-optimal-factor-sm5-n-greater-than-1-returns-ef ()
  "After the first review, the initial OF is just the e-factor."
  (should (equal 2.5 (org-drill-initial-optimal-factor-sm5 2 2.5)))
  (should (equal 1.8 (org-drill-initial-optimal-factor-sm5 5 1.8))))

;;;; org-drill-get-optimal-factor-sm5

(ert-deftest test-org-drill-get-optimal-factor-sm5-normal-found-in-matrix ()
  "When (N, EF) is present in the matrix, return the stored OF."
  (let ((m '((1 (2.5 . 1.5))
             (2 (2.5 . 2.0)))))
    (should (equal 1.5 (org-drill-get-optimal-factor-sm5 1 2.5 m)))
    (should (equal 2.0 (org-drill-get-optimal-factor-sm5 2 2.5 m)))))

(ert-deftest test-org-drill-get-optimal-factor-sm5-boundary-n-missing-falls-back ()
  "When N isn't in the matrix, fall back to the initial-OF function."
  (let ((m '((1 (2.5 . 1.5)))))
    (should (equal 3.0 (org-drill-get-optimal-factor-sm5 5 3.0 m)))))

(ert-deftest test-org-drill-get-optimal-factor-sm5-boundary-ef-missing-falls-back ()
  "When N is in the matrix but EF isn't, fall back to the initial-OF function.
For N>1 the initial-OF function returns the passed EF unchanged."
  (let ((m '((2 (2.5 . 1.5)))))
    (should (equal 3.0 (org-drill-get-optimal-factor-sm5 2 3.0 m)))))

;;;; org-drill-inter-repetition-interval-sm5

(ert-deftest test-org-drill-inter-repetition-interval-sm5-normal-first-review ()
  "First review (n=1) returns the OF directly."
  ;; With n=1 in a matrix that has (1 (2.5 . 4.0)), result should be 4.0
  (let ((m '((1 (2.5 . 4.0)))))
    (should (equal 4.0 (org-drill-inter-repetition-interval-sm5 nil 1 2.5 m)))))

(ert-deftest test-org-drill-inter-repetition-interval-sm5-normal-subsequent-multiplies ()
  "After the first review, interval = OF * last-interval."
  (let ((m '((2 (2.5 . 1.6)))))
    (should (equal 16.0 (org-drill-inter-repetition-interval-sm5 10 2 2.5 m)))))

;;;; org-drill-early-interval-factor

(ert-deftest test-org-drill-early-interval-factor-normal-zero-days-ahead ()
  "Reviewed on time (0 days ahead) returns the optimal factor unchanged."
  (should (equal 2.5 (org-drill-early-interval-factor 2.5 10 0))))

(ert-deftest test-org-drill-early-interval-factor-normal-early-review-reduces-factor ()
  "Reviewing N days early (positive days-ahead) returns a factor below optimal."
  (should (< (org-drill-early-interval-factor 2.5 10 5) 2.5)))

;;;; org-drill-random-dispersal-factor

(ert-deftest test-org-drill-random-dispersal-factor-bounded-output ()
  "Output is in the expected (~0.5, ~1.5) range across many samples."
  ;; The docstring says the function returns a number between 0.5 and 1.5.
  (let ((min-val 1.0)
        (max-val 1.0))
    (dotimes (_ 200)
      (let ((v (org-drill-random-dispersal-factor)))
        (when (< v min-val) (setq min-val v))
        (when (> v max-val) (setq max-val v))))
    (should (and (> min-val 0.45) (< min-val 1.0)))
    (should (and (> max-val 1.0) (< max-val 1.55)))))

;;;; org-drill--safe-read-learn-data

(ert-deftest test-org-drill--safe-read-learn-data-normal-valid-three-element-list ()
  "A valid LEARN_DATA list of three numbers is parsed and returned."
  (should (equal '(2.5 1 0.5)
                 (org-drill--safe-read-learn-data "(2.5 1 0.5)"))))

(ert-deftest test-org-drill--safe-read-learn-data-normal-valid-longer-list ()
  "Lists longer than 3 elements parse fine — only the first 3 are validated."
  (should (equal '(2.5 1 0.5 4)
                 (org-drill--safe-read-learn-data "(2.5 1 0.5 4)"))))

(ert-deftest test-org-drill--safe-read-learn-data-error-nil-input ()
  (should (null (org-drill--safe-read-learn-data nil))))

(ert-deftest test-org-drill--safe-read-learn-data-error-non-string-input ()
  (should (null (org-drill--safe-read-learn-data 42)))
  (should (null (org-drill--safe-read-learn-data '(1 2 3)))))

(ert-deftest test-org-drill--safe-read-learn-data-error-empty-string ()
  (should (null (org-drill--safe-read-learn-data ""))))

(ert-deftest test-org-drill--safe-read-learn-data-error-non-list-data ()
  (should (null (org-drill--safe-read-learn-data "42")))
  (should (null (org-drill--safe-read-learn-data "\"hello\""))))

(ert-deftest test-org-drill--safe-read-learn-data-error-list-too-short ()
  (should (null (org-drill--safe-read-learn-data "(1 2)"))))

(ert-deftest test-org-drill--safe-read-learn-data-error-non-numeric-elements ()
  (should (null (org-drill--safe-read-learn-data "(\"a\" 1 2)")))
  (should (null (org-drill--safe-read-learn-data "(1 \"b\" 2)")))
  (should (null (org-drill--safe-read-learn-data "(1 2 \"c\")"))))

(ert-deftest test-org-drill--safe-read-learn-data-error-malformed-input ()
  "Malformed s-expressions return nil rather than erroring."
  (should (null (org-drill--safe-read-learn-data "((((")))
  (should (null (org-drill--safe-read-learn-data "not-a-list"))))

(provide 'test-org-drill-scheduler-helpers)

;;; test-org-drill-scheduler-helpers.el ends here
