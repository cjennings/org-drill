;;; test-org-drill-statistics-render-trends.el --- Tests for render-trends statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard render-trends block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; Tests for the Trends render helper (render 2/5).


(defun org-drill-statistics-test--record (start-day-offset qualities
                                                           &optional duration-min)
  "Build an `org-drill-session-record' for trends tests.
START-DAY-OFFSET is an integer day offset from today (0 is today, -7 is
a week ago).  QUALITIES is a list of int qualities, stored as a vector.
DURATION-MIN defaults to 10 minutes.  The record's start-time and
end-time floats land on the requested day; only the day component is
load bearing for these tests."
  (let* ((duration (or duration-min 10))
         (today-secs (float-time (current-time)))
         (start (+ today-secs (* start-day-offset 86400.0)))
         (end (+ start (* duration 60.0))))
    (make-org-drill-session-record
     :start-time start
     :end-time end
     :scope 'file
     :algorithm 'sm5
     :qualities (vconcat qualities)
     :pass-percent (org-drill--compute-pass-percent (vconcat qualities))
     :new-count 0
     :mature-count (length qualities)
     :failed-count 0
     :cram-mode nil)))

(ert-deftest test-org-drill-statistics-render-trends-has-subheading ()
  "The rendered section opens with the \"* Trends\" org subheading."
  (let ((out (org-drill-statistics--render-trends nil)))
    (should (string-prefix-p "* Trends\n" out))))

(ert-deftest test-org-drill-statistics-render-trends-empty-log ()
  "An empty log still renders both sparkline lines and a table header.
The sparklines are all-space (no data) and the table has only its header
and separator rows."
  (let ((out (org-drill-statistics--render-trends nil)))
    (should (string-match-p "Reviews/day (last 90):" out))
    (should (string-match-p "Pass rate/day (last 90):" out))
    (should (string-match-p "| Week | Reviews | Pass % | Avg min |" out))
    (should (string-match-p "|------" out))))

(ert-deftest test-org-drill-statistics-render-trends-sparkline-glyph ()
  "A record today puts a non-space block glyph in the reviews sparkline.
The final sparkline column (today) must be a quadrant block, not the
space rendered for empty days."
  (let* ((log (list (org-drill-statistics-test--record 0 '(5 4 3))))
         (out (org-drill-statistics--render-trends log))
         (line (car (seq-filter
                     (lambda (l) (string-prefix-p "Reviews/day" l))
                     (split-string out "\n")))))
    ;; The sparkline glyph for the busiest day is the full block, since
    ;; today is the only day with data so it scales to the ceiling.
    (should (string-match-p "█" line))))

(ert-deftest test-org-drill-statistics-render-trends-weekly-row ()
  "A this-week session produces a body row with its counts.
The row carries the Monday date of this week, the review count, the
pass percentage, and a one-decimal average duration."
  (let* ((today (time-to-days (current-time)))
         (week-start (org-drill-statistics--week-start-day today))
         (expected-date (org-drill-statistics--format-week-start week-start))
         ;; Three qualities, two passes (> failure-quality default 2).
         (log (list (org-drill-statistics-test--record 0 '(5 4 1) 20)))
         (out (org-drill-statistics--render-trends log)))
    ;; Pass percent: 2 of 3 -> 67.  Avg duration: 20.0 minutes.
    (should (string-match-p
             (regexp-quote (format "| %s | 3 | 67 | 20.0 |" expected-date))
             out))))

(ert-deftest test-org-drill-statistics-render-trends-twelve-week-rows ()
  "The table body has exactly 12 week rows, one per week in the window."
  (let* ((out (org-drill-statistics--render-trends nil))
         (lines (split-string out "\n" t))
         (body (seq-filter
                (lambda (l)
                  (and (string-prefix-p "| " l)
                       (not (string-match-p "Week" l))))
                lines)))
    (should (= (length body) 12))))

(ert-deftest test-org-drill-statistics-render-trends-algorithm-filter ()
  "Passing an algorithm filters records out of the aggregates.
A record under `sm5' is excluded when the section filters for `sm2',
leaving an empty (all-zero) this-week row."
  (let* ((today (time-to-days (current-time)))
         (week-start (org-drill-statistics--week-start-day today))
         (date (org-drill-statistics--format-week-start week-start))
         (log (list (org-drill-statistics-test--record 0 '(5 4 3) 15)))
         (out (org-drill-statistics--render-trends log 'sm2)))
    ;; sm5 record is filtered out, so this week's row is zeroed.
    (should (string-match-p
             (regexp-quote (format "| %s | 0 | 0 | 0.0 |" date))
             out))
    ;; And the unfiltered render keeps it.
    (let ((unfiltered (org-drill-statistics--render-trends log)))
      (should (string-match-p
               (regexp-quote (format "| %s | 3 |" date))
               unfiltered)))))

(ert-deftest test-org-drill-statistics-render-trends-pass-rate-absolute-scale ()
  "The pass-rate sparkline scales against 100, not the window peak.
A day with a 50 percent pass rate must render a mid-height glyph, not
the full block it would reach if scaled to its own maximum."
  (let* ((log (list (org-drill-statistics-test--record 0 '(5 1))))
         (out (org-drill-statistics--render-trends log))
         (line (car (seq-filter
                     (lambda (l) (string-prefix-p "Pass rate/day" l))
                     (split-string out "\n")))))
    ;; 1 pass of 2 -> 50 percent.  Scaled to 100 over an 8-glyph charset,
    ;; round(50/100 * 7) = 4 -> the 5th glyph, not the full block.
    (should-not (string-match-p "█" line))
    (should (string-match-p "▅" line))))

(provide 'test-org-drill-statistics-render-trends)

;;; test-org-drill-statistics-render-trends.el ends here
