;;; test-org-drill-statistics-distribution.el --- Tests for distribution statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard distribution block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(ert-deftest test-org-drill-statistics-distribution-subheading ()
  "The rendered section opens with the Quality Distribution subheading."
  (let ((out (org-drill-statistics--render-distribution [1 0 2 3 5 4])))
    (should (string-prefix-p "** Quality Distribution" out))))

(ert-deftest test-org-drill-statistics-distribution-all-six-rows ()
  "Every quality 0..5 gets a row even when its count is zero."
  (let* ((out (org-drill-statistics--render-distribution [0 0 0 0 0 0]))
         (lines (split-string out "\n" t)))
    ;; subheading + note line + six quality rows.
    (dotimes (q 6)
      (should (string-match-p (format "^%d  " q) out)))))

(ert-deftest test-org-drill-statistics-distribution-counts-and-percent ()
  "Each row shows the absolute count and the percent of total."
  ;; Total 10: quality 5 has 5 (50%), quality 0 has 1 (10%).
  (let ((out (org-drill-statistics--render-distribution [1 1 1 1 1 5])))
    (should (string-match-p "^5  .*   5   50%$" out))
    (should (string-match-p "^0  .*   1   10%$" out))))

(ert-deftest test-org-drill-statistics-distribution-total-line ()
  "A non-empty histogram reports the total rating count."
  (let ((out (org-drill-statistics--render-distribution [2 0 0 0 0 3])))
    (should (string-match-p "Total ratings: 5" out))))

(ert-deftest test-org-drill-statistics-distribution-empty-note ()
  "An all-zero histogram emits the empty note and 0%% percentages."
  (let ((out (org-drill-statistics--render-distribution [0 0 0 0 0 0])))
    (should (string-match-p "No quality ratings recorded yet\\." out))
    (should (string-match-p "^3  .*   0    0%$" out))))

(ert-deftest test-org-drill-statistics-distribution-bar-scales-to-max ()
  "The largest count fills the full bar width; smaller counts scale down."
  (let* ((org-drill-statistics-distribution-bar-width 10)
         (out (org-drill-statistics--render-distribution [10 0 0 0 0 5]))
         (block (char-to-string ?\x2588)))
    ;; Quality 0 (count 10, the max) fills all 10 blocks.
    (should (string-match-p
             (concat "^0  " (regexp-quote (make-string 10 ?\x2588)))
             out))
    ;; Quality 5 (count 5, half the max) fills 5 blocks.
    (should (string-match-p
             (concat "^5  " (regexp-quote (make-string 5 ?\x2588)) " ")
             out))))

(ert-deftest test-org-drill-statistics-distribution-pure-no-buffer ()
  "Rendering returns a string and does not switch or create buffers."
  (let ((before (buffer-list)))
    (should (stringp (org-drill-statistics--render-distribution [1 2 3 4 5 6])))
    (should (equal before (buffer-list)))))

(ert-deftest test-org-drill-statistics-distribution-from-histogram-helper ()
  "Renderer composes with the quality-histogram aggregator over a fixture log.
Components integrated:
- org-drill-statistics--quality-histogram (real)
- org-drill-statistics--render-distribution (real, entry point)
Validates the count for a known fixture flows through to the rendered row."
  (let* ((rec (make-org-drill-session-record
               :start-time (float-time)
               :end-time (float-time)
               :qualities [5 5 5 0]))
         (hist (org-drill-statistics--quality-histogram (list rec)))
         (out (org-drill-statistics--render-distribution hist)))
    (should (string-match-p "Total ratings: 4" out))
    ;; Quality 5 appears three times (75%).
    (should (string-match-p "^5  .*   3   75%$" out))
    ;; Quality 0 appears once (25%).
    (should (string-match-p "^0  .*   1   25%$" out))))

(provide 'test-org-drill-statistics-distribution)

;;; test-org-drill-statistics-distribution.el ends here
