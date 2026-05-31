;;; test-org-drill-statistics-sparkline.el --- Tests for sparkline statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard sparkline block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(ert-deftest test-org-drill-statistics-sparkline-empty ()
  "An empty sequence renders as the empty string."
  (should (equal (org-drill-statistics--sparkline '()) ""))
  (should (equal (org-drill-statistics--sparkline []) "")))

(ert-deftest test-org-drill-statistics-sparkline-single-value ()
  "A single positive value renders as the full block.
With a one-element sequence and no explicit MAX, the value equals the
derived max and maps to the tallest glyph."
  (should (equal (org-drill-statistics--sparkline '(5)) "█")))

(ert-deftest test-org-drill-statistics-sparkline-single-zero ()
  "A single zero value renders as the lowest block, not an error.
The derived max is zero, so the all-zero branch applies."
  (should (equal (org-drill-statistics--sparkline '(0)) "▁")))

(ert-deftest test-org-drill-statistics-sparkline-all-equal ()
  "All-equal positive values all render as the full block.
Each value equals the derived max, so every glyph is the tallest."
  (should (equal (org-drill-statistics--sparkline '(3 3 3 3)) "████")))

(ert-deftest test-org-drill-statistics-sparkline-all-zero ()
  "All-zero values render as the lowest block, one per entry.
Max is zero, so the division-by-zero guard returns the lowest glyph for
every value instead of erroring."
  (should (equal (org-drill-statistics--sparkline '(0 0 0)) "▁▁▁")))

(ert-deftest test-org-drill-statistics-sparkline-known-ramp ()
  "A 0..7 ramp against MAX 7 maps to each glyph in order.
With value I and MAX 7, the index is (round (* (/ I 7.0) 7)) = I, so the
ramp walks the charset from lowest to highest exactly once."
  (should (equal (org-drill-statistics--sparkline '(0 1 2 3 4 5 6 7) 7)
                 "▁▂▃▄▅▆▇█")))

(ert-deftest test-org-drill-statistics-sparkline-nil-entries ()
  "Nil entries render as spaces and do not affect the derived max.
The values 1 and 2 against derived max 2 scale to indices 4 (1/2*7
rounds to 4) and 7."
  (should (equal (org-drill-statistics--sparkline '(nil 1 nil 2 nil))
                 " ▅ █ ")))

(ert-deftest test-org-drill-statistics-sparkline-all-nil ()
  "An all-nil sequence renders as one space per entry."
  (should (equal (org-drill-statistics--sparkline '(nil nil nil)) "   ")))

(ert-deftest test-org-drill-statistics-sparkline-explicit-max ()
  "An explicit MAX scales values against it, not the sequence max.
With MAX 10, value 10 is the full block, 0 is the lowest, and 5 scales
to 5/10*7 = 3.5 which rounds to index 4."
  (should (equal (org-drill-statistics--sparkline '(0 5 10) 10) "▁▅█")))

(ert-deftest test-org-drill-statistics-sparkline-explicit-zero-max ()
  "An explicit MAX of zero renders every value as the lowest block.
The guard treats a zero ceiling like the all-zero case rather than
dividing by zero."
  (should (equal (org-drill-statistics--sparkline '(0 1 2) 0) "▁▁▁")))

(ert-deftest test-org-drill-statistics-sparkline-value-above-max ()
  "A value exceeding MAX clamps to the full block instead of overflowing.
Without the clamp the computed index would exceed the charset length.
The 5 scales to 5/10*7 = 3.5 which rounds to index 4."
  (should (equal (org-drill-statistics--sparkline '(20) 10) "█"))
  (should (equal (org-drill-statistics--sparkline '(5 20) 10) "▅█")))

(ert-deftest test-org-drill-statistics-sparkline-vector-input ()
  "A vector argument is accepted and rendered like a list.
The helper coerces any sequence, so callers may pass either."
  (should (equal (org-drill-statistics--sparkline [0 7] 7) "▁█")))

(provide 'test-org-drill-statistics-sparkline)

;;; test-org-drill-statistics-sparkline.el ends here
