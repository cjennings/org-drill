;;; test-org-drill-determine-next-interval-sm2.el --- Tests for SM2 algorithm

;;; Commentary:
;; Unit tests for org-drill-determine-next-interval-sm2, the SuperMemo 2 (SM2)
;; spaced repetition algorithm implementation.
;;
;; The SM2 algorithm calculates the next review interval based on:
;; - Last interval (days since last review)
;; - Number of successful repeats (n)
;; - Easiness factor (EF) - modified by recall quality
;; - Quality of recall (0-5 scale)
;; - Failure count and statistics
;;
;; Function signature:
;;   (org-drill-determine-next-interval-sm2 last-interval n ef quality
;;                                          failures meanq total-repeats)
;;
;; Returns: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX)

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data and Constants

;; Default SM2 parameters
(defconst test-sm2-default-ef 2.5
  "Default easiness factor for new items.")

(defconst test-sm2-min-ef 1.3
  "Minimum easiness factor (SM2 floor).")

;;; Helper Functions

(defun test-sm2--extract-interval (result)
  "Extract interval from SM2 result list."
  (nth 0 result))

(defun test-sm2--extract-repeats (result)
  "Extract repeats from SM2 result list."
  (nth 1 result))

(defun test-sm2--extract-ef (result)
  "Extract easiness factor from SM2 result list."
  (nth 2 result))

(defun test-sm2--extract-failures (result)
  "Extract failures from SM2 result list."
  (nth 3 result))

(defun test-sm2--extract-meanq (result)
  "Extract mean quality from SM2 result list."
  (nth 4 result))

(defun test-sm2--extract-total-repeats (result)
  "Extract total repeats from SM2 result list."
  (nth 5 result))

;;; Normal Cases - Successful Reviews

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-first-review-quality-4 ()
  "Test first successful review with quality 4.
First review (n=1) should return interval of 1 day."
  (let* ((result (org-drill-determine-next-interval-sm2 0 1 nil 4 0 nil 0))
         (interval (test-sm2--extract-interval result))
         (repeats (test-sm2--extract-repeats result))
         (ef (test-sm2--extract-ef result)))
    (should (= interval 1))
    (should (= repeats 2))
    (should ef))) ; EF should be calculated

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-second-review-quality-4 ()
  "Test second successful review with quality 4.
Second review (n=2) should return interval of 6 days (no random noise)."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm2 1 2 2.5 4 0 nil 0))
           (interval (test-sm2--extract-interval result))
           (repeats (test-sm2--extract-repeats result)))
      (should (= interval 6))
      (should (= repeats 3)))))

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-third-review-quality-4 ()
  "Test third successful review with quality 4.
Third review (n=3) uses formula: last-interval * EF."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((ef 2.5)
           (last-interval 6)
           (result (org-drill-determine-next-interval-sm2 last-interval 3 ef 4 0 nil 0))
           (interval (test-sm2--extract-interval result))
           (new-ef (test-sm2--extract-ef result)))
      ;; Interval should be approximately last-interval * new-ef
      (should (> interval last-interval))
      (should (= (test-sm2--extract-repeats result) 4)))))

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-quality-5-perfect-recall ()
  "Test review with perfect recall (quality 5).
Quality 5 should increase EF and result in longer intervals."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm2 10 3 2.5 5 0 nil 0))
           (ef (test-sm2--extract-ef result)))
      ;; Quality 5 should maintain or increase EF
      (should (>= ef 2.5)))))

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-quality-3-adequate-recall ()
  "Test review with adequate recall (quality 3).
Quality 3 should maintain relatively stable EF."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm2 10 3 2.5 3 0 nil 0))
           (ef (test-sm2--extract-ef result)))
      ;; Quality 3 should result in moderate EF
      (should (> ef test-sm2-min-ef))
      (should (< ef 2.5))))) ; EF likely decreases slightly

;;; Normal Cases - Failed Reviews

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-failure-quality-0 ()
  "Test failed review with quality 0.
Quality 0 (<= org-drill-failure-quality) should reset interval to -1."
  (let* ((result (org-drill-determine-next-interval-sm2 10 3 2.5 0 0 nil 0))
         (interval (test-sm2--extract-interval result))
         (repeats (test-sm2--extract-repeats result))
         (ef (test-sm2--extract-ef result))
         (failures (test-sm2--extract-failures result)))
    (should (= interval -1))
    (should (= repeats 1)) ; Reset to 1
    (should (= ef 2.5)) ; EF unchanged on failure
    (should (= failures 1)))) ; Failure count incremented

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-failure-quality-1 ()
  "Test failed review with quality 1.
Quality 1 (<= org-drill-failure-quality) should reset interval."
  (let* ((result (org-drill-determine-next-interval-sm2 15 5 2.6 1 2 3.5 10))
         (interval (test-sm2--extract-interval result))
         (ef (test-sm2--extract-ef result))
         (failures (test-sm2--extract-failures result)))
    (should (= interval -1))
    (should (= ef 2.6)) ; EF unchanged
    (should (= failures 3)))) ; Previous failures + 1

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-failure-quality-2 ()
  "Test failed review with quality 2.
Quality 2 (= org-drill-failure-quality default) should reset interval."
  (let* ((result (org-drill-determine-next-interval-sm2 10 3 2.5 2 0 nil 0))
         (interval (test-sm2--extract-interval result)))
    (should (= interval -1))))

;;; Boundary Cases - Default Values

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-nil-ef-uses-default ()
  "Test that nil EF defaults to 2.5."
  (let* ((result (org-drill-determine-next-interval-sm2 0 1 nil 4 0 nil 0))
         (ef (test-sm2--extract-ef result)))
    (should ef) ; EF should be set (modified from default 2.5)
    (should (> ef 0))))

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-zero-n-becomes-one ()
  "Test that n=0 is treated as n=1."
  (let* ((result (org-drill-determine-next-interval-sm2 0 0 2.5 4 0 nil 0))
         (interval (test-sm2--extract-interval result))
         (repeats (test-sm2--extract-repeats result)))
    (should (= interval 1)) ; First review interval
    (should (= repeats 2)))) ; n incremented from 1 to 2

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-nil-meanq-uses-quality ()
  "Test that nil meanq initializes to current quality."
  (let* ((quality 4)
         (result (org-drill-determine-next-interval-sm2 0 1 2.5 quality 0 nil 0))
         (meanq (test-sm2--extract-meanq result)))
    (should (= meanq quality))))

;;; Boundary Cases - Quality Extremes

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-quality-5-maximum ()
  "Test maximum quality (5) - perfect recall."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm2 1 2 2.5 5 0 nil 0))
           (interval (test-sm2--extract-interval result)))
      (should (= interval 6))))) ; Second review with quality 5

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-quality-0-minimum ()
  "Test minimum quality (0) - complete failure."
  (let* ((result (org-drill-determine-next-interval-sm2 10 3 2.5 0 0 nil 0))
         (interval (test-sm2--extract-interval result)))
    (should (= interval -1)))) ; Failed review

;;; Boundary Cases - Repeat Count

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-n-equals-1 ()
  "Test first successful review (n=1) returns interval of 1."
  (let* ((result (org-drill-determine-next-interval-sm2 0 1 2.5 4 0 nil 0))
         (interval (test-sm2--extract-interval result)))
    (should (= interval 1))))

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-n-equals-2-no-noise ()
  "Test second review (n=2) with no random noise.
Should return interval of 6 days regardless of quality (if passing)."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm2 1 2 2.5 4 0 nil 0))
           (interval (test-sm2--extract-interval result)))
      (should (= interval 6)))))

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-n-equals-2-with-noise ()
  "Test second review (n=2) with random noise enabled.
Interval should vary by quality, with quality 5 having highest base interval (6)."
  (let ((org-drill-add-random-noise-to-intervals-p t))
    (let* ((result-q5 (org-drill-determine-next-interval-sm2 1 2 2.5 5 0 nil 0))
           (result-q4 (org-drill-determine-next-interval-sm2 1 2 2.5 4 0 nil 0))
           (result-q3 (org-drill-determine-next-interval-sm2 1 2 2.5 3 0 nil 0))
           (interval-q5 (test-sm2--extract-interval result-q5))
           (interval-q4 (test-sm2--extract-interval result-q4))
           (interval-q3 (test-sm2--extract-interval result-q3)))
      ;; Base intervals before noise are: Q5=6, Q4=4, Q3=3
      ;; After random noise, verify intervals are positive and roughly in expected ranges
      (should (> interval-q5 0))
      (should (> interval-q4 0))
      (should (> interval-q3 0))
      ;; Quality 5 base interval (6) should generally be higher than quality 3 base (3)
      ;; even with noise applied
      (should (> interval-q5 2)))))

;;; Boundary Cases - Mean Quality Calculation

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-meanq-weighted-average ()
  "Test that meanq is correctly calculated as weighted average.
meanq = (quality + meanq * total-repeats) / (total-repeats + 1)"
  (let* ((quality 4)
         (meanq 3.0)
         (total-repeats 10)
         (result (org-drill-determine-next-interval-sm2 10 3 2.5 quality 0 meanq total-repeats))
         (new-meanq (test-sm2--extract-meanq result))
         (expected-meanq (/ (+ quality (* meanq total-repeats 1.0))
                            (1+ total-repeats))))
    (should (< (abs (- new-meanq expected-meanq)) 0.0001)))) ; Floating point comparison

;;; Boundary Cases - EF Modification

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-ef-minimum-floor ()
  "Test that EF floor of 1.3 is applied to input EF when it's below 1.3.
If input EF < 1.3, org-drill-modify-e-factor returns 1.3 exactly."
  (let* ((result (org-drill-determine-next-interval-sm2 10 3 1.0 4 0 nil 0))
         (ef (test-sm2--extract-ef result)))
    ;; Input EF was 1.0 < 1.3, so it should be raised to 1.3 first,
    ;; then modified based on quality 4 (which increases EF)
    (should (>= ef 1.3))))

;;; Boundary Cases - Total Repeats

(ert-deftest test-org-drill-determine-next-interval-sm2-boundary-total-repeats-increments ()
  "Test that total-repeats is always incremented by 1."
  (let* ((total-repeats 42)
         (result (org-drill-determine-next-interval-sm2 10 3 2.5 4 0 3.5 total-repeats))
         (new-total (test-sm2--extract-total-repeats result)))
    (should (= new-total (1+ total-repeats)))))

;;; Return Value Structure

(ert-deftest test-org-drill-determine-next-interval-sm2-normal-return-value-structure ()
  "Test that return value has correct structure.
Should return 7-element list: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX)"
  (let ((result (org-drill-determine-next-interval-sm2 10 3 2.5 4 0 3.5 10)))
    (should (listp result))
    (should (= (length result) 7))
    ;; Verify first 6 elements are numbers
    (should (numberp (nth 0 result))) ; interval
    (should (numberp (nth 1 result))) ; repeats
    (should (numberp (nth 2 result))) ; ef
    (should (numberp (nth 3 result))) ; failures
    (should (numberp (nth 4 result))) ; meanq
    (should (numberp (nth 5 result))) ; total-repeats
    ;; 7th element (index 6) is OFMATRIX - can be nil or a matrix
    ;; Just verify list has 7 elements (already checked above)
    ))

;;; Algorithm Verification

(ert-deftest test-org-drill-determine-next-interval-sm2-algorithm-ef-increases-with-quality ()
  "Test that higher quality results in higher EF (or maintains it).
Quality 5 should result in EF >= initial EF."
  (let* ((initial-ef 2.5)
         (result (org-drill-determine-next-interval-sm2 10 3 initial-ef 5 0 nil 0))
         (new-ef (test-sm2--extract-ef result)))
    (should (>= new-ef initial-ef))))

(ert-deftest test-org-drill-determine-next-interval-sm2-algorithm-ef-decreases-with-low-quality ()
  "Test that low quality results in decreased EF.
Quality 3 should result in EF < initial EF."
  (let* ((initial-ef 2.5)
         (result (org-drill-determine-next-interval-sm2 10 3 initial-ef 3 0 nil 0))
         (new-ef (test-sm2--extract-ef result)))
    (should (< new-ef initial-ef))))

(ert-deftest test-org-drill-determine-next-interval-sm2-algorithm-interval-grows-exponentially ()
  "Test that intervals grow approximately exponentially for consistent quality.
Third review interval should be significantly larger than second."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((ef 2.5)
           (result-2nd (org-drill-determine-next-interval-sm2 1 2 ef 4 0 nil 0))
           (interval-2nd (test-sm2--extract-interval result-2nd))
           (ef-2nd (test-sm2--extract-ef result-2nd))
           (result-3rd (org-drill-determine-next-interval-sm2 interval-2nd 3 ef-2nd 4 0 nil 1))
           (interval-3rd (test-sm2--extract-interval result-3rd)))
      (should (> interval-3rd (* interval-2nd 1.5)))))) ; Significant growth

(provide 'test-org-drill-determine-next-interval-sm2)
;;; test-org-drill-determine-next-interval-sm2.el ends here
