;;; test-org-drill-determine-next-interval-sm5.el --- Tests for SM5 algorithm  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for org-drill-determine-next-interval-sm5, the SuperMemo 5
;; spaced-repetition algorithm implementation.
;;
;; SM5 differs from SM2 in two main ways:
;; - It maintains a 2D "optimal factor" matrix indexed by (n, ef) -> of,
;;   updated on each call.
;; - It supports an optional delta-days parameter for adjusting intervals
;;   on early reviews when org-drill-adjust-intervals-for-early-and-late-
;;   repetitions-p is enabled.
;;
;; Function signature:
;;   (org-drill-determine-next-interval-sm5 last-interval n ef quality
;;                                          failures meanq total-repeats
;;                                          of-matrix &optional delta-days)
;;
;; Returns: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX)

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'testutil-scheduler)

;;; Test Data and Constants

(defconst test-sm5-default-ef 2.5
  "Default easiness factor for new items.")

(defconst test-sm5-min-ef 1.3
  "Minimum easiness factor.
The SM5 floor is shared with SM2 via `org-drill-modify-e-factor'.")

;;; Normal Cases - Successful Reviews

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-first-review-quality-4 ()
  "Normal: first successful review with quality 4 returns positive interval and increments repeats."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 0 1 nil 4 0 nil 0 nil nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result))
           (ef (test-scheduler--extract-ef result)))
      (should (> interval 0))
      (should (= repeats 2))
      (should ef))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-second-review-quality-4 ()
  "Normal: second successful review with quality 4 increments repeats to 3."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 1 2 2.5 4 0 nil 0 nil nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result)))
      (should (> interval 0))
      (should (= repeats 3)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-third-review-quality-4 ()
  "Normal: third successful review uses OF-matrix lookup and returns positive interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil nil))
           (interval (test-scheduler--extract-interval result))
           (repeats (test-scheduler--extract-repeats result)))
      (should (> interval 0))
      (should (= repeats 4)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-quality-5-perfect-recall ()
  "Normal: quality 5 maintains or increases EF on successful path."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 10 3 2.5 5 0 nil 0 nil nil))
           (ef (test-scheduler--extract-ef result)))
      (should (>= ef 2.5)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-quality-3-adequate-recall ()
  "Normal: quality 3 decreases EF below initial but keeps it above the floor."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 10 3 2.5 3 0 nil 0 nil nil))
           (ef (test-scheduler--extract-ef result)))
      (should (> ef test-sm5-min-ef))
      (should (< ef 2.5)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-failure-quality-0 ()
  "Normal: failed review with quality 0 resets interval to -1, repeats to 1, increments failures."
  (let* ((result (org-drill-determine-next-interval-sm5 10 3 2.5 0 0 nil 0 nil nil))
         (interval (test-scheduler--extract-interval result))
         (repeats (test-scheduler--extract-repeats result))
         (ef (test-scheduler--extract-ef result))
         (failures (test-scheduler--extract-failures result)))
    (should (= interval -1))
    (should (= repeats 1))
    (should (= ef 2.5))
    (should (= failures 1))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-failure-quality-1 ()
  "Normal: failed review with quality 1 resets interval, preserves input EF, increments failures."
  (let* ((result (org-drill-determine-next-interval-sm5 15 5 2.6 1 2 3.5 10 nil nil))
         (interval (test-scheduler--extract-interval result))
         (ef (test-scheduler--extract-ef result))
         (failures (test-scheduler--extract-failures result)))
    (should (= interval -1))
    (should (= ef 2.6))
    (should (= failures 3))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-failure-quality-2 ()
  "Normal: quality 2 (= default org-drill-failure-quality) triggers failure path."
  (let* ((result (org-drill-determine-next-interval-sm5 10 3 2.5 2 0 nil 0 nil nil))
         (interval (test-scheduler--extract-interval result)))
    (should (= interval -1))))

(ert-deftest test-org-drill-determine-next-interval-sm5-normal-failure-preserves-old-ef ()
  "Normal: SM5 failure path returns the input EF (OLD-EF), not the modified one."
  (let* ((input-ef 2.7)
         (result (org-drill-determine-next-interval-sm5 10 3 input-ef 0 0 nil 0 nil nil))
         (returned-ef (test-scheduler--extract-ef result)))
    ;; modify-e-factor would have computed a different value, but the failure
    ;; branch returns old-ef, which is the input ef.
    (should (= returned-ef input-ef))))

;;; Boundary Cases - Default and Sentinel Values

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-nil-ef-uses-default ()
  "Boundary: nil EF defaults to 2.5."
  (let* ((result (org-drill-determine-next-interval-sm5 0 1 nil 4 0 nil 0 nil nil))
         (ef (test-scheduler--extract-ef result)))
    (should ef)
    (should (> ef 0))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-zero-n-becomes-one ()
  "Boundary: n=0 is treated as n=1, so returned repeats is 2."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 0 0 2.5 4 0 nil 0 nil nil))
           (repeats (test-scheduler--extract-repeats result)))
      (should (= repeats 2)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-nil-meanq-uses-quality ()
  "Boundary: nil meanq initializes to current quality."
  (let* ((quality 4)
         (result (org-drill-determine-next-interval-sm5 0 1 2.5 quality 0 nil 0 nil nil))
         (meanq (test-scheduler--extract-meanq result)))
    (should (= meanq quality))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-quality-5-maximum ()
  "Boundary: maximum quality 5 is accepted and produces a positive interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 1 2 2.5 5 0 nil 0 nil nil))
           (interval (test-scheduler--extract-interval result)))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-quality-0-minimum ()
  "Boundary: minimum quality 0 triggers the failure path with interval -1."
  (let* ((result (org-drill-determine-next-interval-sm5 10 3 2.5 0 0 nil 0 nil nil))
         (interval (test-scheduler--extract-interval result)))
    (should (= interval -1))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-meanq-weighted-average ()
  "Boundary: meanq is correctly calculated as a weighted average over total-repeats."
  (let* ((quality 4)
         (meanq 3.0)
         (total-repeats 10)
         (result (org-drill-determine-next-interval-sm5 10 3 2.5 quality 0 meanq total-repeats nil nil))
         (new-meanq (test-scheduler--extract-meanq result))
         (expected (/ (+ quality (* meanq total-repeats 1.0))
                      (1+ total-repeats))))
    (should (< (abs (- new-meanq expected)) 0.0001))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-ef-below-floor ()
  "Boundary: input EF below 1.3 is raised to 1.3 by org-drill-modify-e-factor."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 10 3 1.0 4 0 nil 0 nil nil))
           (ef (test-scheduler--extract-ef result)))
      (should (>= ef test-sm5-min-ef)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-ef-at-floor ()
  "Boundary: EF exactly at floor (1.3) does not crash and remains >= 1.3 with quality 4."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 10 3 1.3 4 0 4.0 10 nil nil))
           (ef (test-scheduler--extract-ef result)))
      (should (>= ef test-sm5-min-ef)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-total-repeats-increments ()
  "Boundary: total-repeats is always incremented by 1, including on failure."
  (let* ((total-repeats 42)
         (result-success (org-drill-determine-next-interval-sm5 10 3 2.5 4 0 3.5 total-repeats nil nil))
         (result-failure (org-drill-determine-next-interval-sm5 10 3 2.5 0 0 3.5 total-repeats nil nil)))
    (should (= (test-scheduler--extract-total-repeats result-success) (1+ total-repeats)))
    (should (= (test-scheduler--extract-total-repeats result-failure) (1+ total-repeats)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-nil-of-matrix-uses-default ()
  "Boundary: nil of-matrix defaults to org-drill-sm5-optimal-factor-matrix and returns a list."
  (let* ((result (org-drill-determine-next-interval-sm5 0 1 2.5 4 0 nil 0 nil nil))
         (matrix (test-scheduler--extract-of-matrix result)))
    (should (listp matrix))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-input-matrix-not-mutated ()
  "Boundary: a non-nil input of-matrix is copied, not mutated, by the call."
  ;; Construct the input fresh on each run rather than using a quoted literal,
  ;; so a future regression that drops the production-side `copy-tree' can't
  ;; silently corrupt the literal across test runs.
  (let* ((input-matrix (list (list 10 (cons 1.0 1.0))))
         (snapshot (copy-tree input-matrix)))
    (org-drill-determine-next-interval-sm5 0 1 2.5 4 0 nil 0 input-matrix nil)
    (should (equal input-matrix snapshot))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-delta-days-nil ()
  "Boundary: nil delta-days (the default) skips the early-review adjustment."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil nil))
           (interval (test-scheduler--extract-interval result)))
      (should (numberp interval))
      (should (> interval 0)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-delta-days-positive-no-adjustment ()
  "Boundary: positive delta-days (late review) does not trigger the early-adjustment path."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))
    (let* ((result-late (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil 5))
           (result-no-delta (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil nil))
           (interval-late (test-scheduler--extract-interval result-late))
           (interval-no-delta (test-scheduler--extract-interval result-no-delta)))
      (should (= interval-late interval-no-delta)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-delta-days-negative-flag-disabled ()
  "Boundary: negative delta-days with adjust-flag disabled does not change the interval."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p nil))
    (let* ((result-no-adjust (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil -5))
           (result-no-delta (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil nil))
           (interval-no-adjust (test-scheduler--extract-interval result-no-adjust))
           (interval-no-delta (test-scheduler--extract-interval result-no-delta)))
      (should (= interval-no-adjust interval-no-delta)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-boundary-delta-days-negative-flag-enabled ()
  "Boundary: negative delta-days with adjust-flag enabled runs the early-interval-factor branch."
  (let ((org-drill-add-random-noise-to-intervals-p nil)
        (org-drill-adjust-intervals-for-early-and-late-repetitions-p t))
    (let* ((result (org-drill-determine-next-interval-sm5 6 3 2.5 4 0 nil 0 nil -5))
           (interval (test-scheduler--extract-interval result)))
      (should (numberp interval))
      (should (> interval 0)))))

;;; Error Cases - cl-assert violations

(ert-deftest test-org-drill-determine-next-interval-sm5-error-negative-n ()
  "Error: n=-1 violates the (cl-assert (> n 0)) precondition."
  (should-error
   (org-drill-determine-next-interval-sm5 0 -1 2.5 4 0 nil 0 nil nil)
   :type 'cl-assertion-failed))

(ert-deftest test-org-drill-determine-next-interval-sm5-error-quality-below-zero ()
  "Error: quality=-1 violates the cl-assert quality range."
  (should-error
   (org-drill-determine-next-interval-sm5 0 1 2.5 -1 0 nil 0 nil nil)
   :type 'cl-assertion-failed))

(ert-deftest test-org-drill-determine-next-interval-sm5-error-quality-above-five ()
  "Error: quality=6 violates the cl-assert quality range."
  (should-error
   (org-drill-determine-next-interval-sm5 0 1 2.5 6 0 nil 0 nil nil)
   :type 'cl-assertion-failed))

;;; Algorithm Verification

(ert-deftest test-org-drill-determine-next-interval-sm5-algorithm-return-value-structure ()
  "Algorithm: return value is a 7-element list with the documented field types."
  (let ((result (org-drill-determine-next-interval-sm5 10 3 2.5 4 0 3.5 10 nil nil)))
    (should (listp result))
    (should (= (length result) 7))
    (should (numberp (nth 0 result)))     ; interval
    (should (numberp (nth 1 result)))     ; repeats
    (should (numberp (nth 2 result)))     ; ef
    (should (numberp (nth 3 result)))     ; failures
    (should (numberp (nth 4 result)))     ; meanq
    (should (numberp (nth 5 result)))     ; total-repeats
    (should (listp (nth 6 result)))))     ; of-matrix

(ert-deftest test-org-drill-determine-next-interval-sm5-algorithm-ef-increases-with-quality-5 ()
  "Algorithm: quality 5 results in EF >= initial EF on the success path."
  (let* ((initial-ef 2.5)
         (result (org-drill-determine-next-interval-sm5 10 3 initial-ef 5 0 nil 0 nil nil))
         (new-ef (test-scheduler--extract-ef result)))
    (should (>= new-ef initial-ef))))

(ert-deftest test-org-drill-determine-next-interval-sm5-algorithm-ef-decreases-with-quality-3 ()
  "Algorithm: quality 3 results in EF < initial EF on the success path."
  (let* ((initial-ef 2.5)
         (result (org-drill-determine-next-interval-sm5 10 3 initial-ef 3 0 nil 0 nil nil))
         (new-ef (test-scheduler--extract-ef result)))
    (should (< new-ef initial-ef))))

(ert-deftest test-org-drill-determine-next-interval-sm5-algorithm-interval-grows-over-successive-reviews ()
  "Algorithm: successive successful quality-4 reviews produce strictly growing intervals."
  (let ((org-drill-add-random-noise-to-intervals-p nil))
    (let* ((result-2 (org-drill-determine-next-interval-sm5 1 2 2.5 4 0 nil 0 nil nil))
           (interval-2 (test-scheduler--extract-interval result-2))
           (ef-2 (test-scheduler--extract-ef result-2))
           (matrix-2 (test-scheduler--extract-of-matrix result-2))
           (result-3 (org-drill-determine-next-interval-sm5 interval-2 3 ef-2 4 0 nil 1 matrix-2 nil))
           (interval-3 (test-scheduler--extract-interval result-3)))
      (should (> interval-3 interval-2)))))

(ert-deftest test-org-drill-determine-next-interval-sm5-algorithm-of-matrix-gains-entry ()
  "Algorithm: of-matrix gains an entry for n after a successful call, preserving prior entries."
  (let* ((input-matrix '((10 (1.0 . 1.0))))
         (result (org-drill-determine-next-interval-sm5 0 1 2.5 4 0 nil 0 input-matrix nil))
         (output-matrix (test-scheduler--extract-of-matrix result)))
    (should (assoc 1 output-matrix))    ; new entry for n=1 was added
    (should (assoc 10 output-matrix)))) ; unrelated entry for n=10 preserved

(provide 'test-org-drill-determine-next-interval-sm5)
;;; test-org-drill-determine-next-interval-sm5.el ends here
