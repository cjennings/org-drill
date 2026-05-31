;;; test-org-drill-statistics-render-forecast.el --- Tests for render-forecast statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard render-forecast block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(defun test-org-drill-statistics--make-record
    (start-day-offset &optional qualities)
  "Build an `org-drill-session-record' START-DAY-OFFSET days from now.
QUALITIES defaults to a single passing quality.  Helper for forecast
render tests, though the forecast itself reads org entries, not the log;
kept minimal and self-contained."
  (let ((start (float-time
                (time-add (current-time)
                          (days-to-time start-day-offset)))))
    (make-org-drill-session-record
     :start-time start
     :end-time (+ start 60.0)
     :scope 'file
     :algorithm 'sm5
     :qualities (or qualities (vector 5))
     :pass-percent 100
     :new-count 0
     :mature-count 0
     :failed-count 0
     :cram-mode nil)))

(ert-deftest test-org-drill-statistics-render-forecast-has-subheading ()
  "The rendered section starts with the Forecast subheading."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '(0 0 0 0 0 0 0))))
    (let ((out (org-drill-statistics--render-forecast)))
      (should (string-prefix-p "** Forecast\n" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-header-labels ()
  "The header row labels Today and the +1..+6 offsets in order."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '(0 0 0 0 0 0 0))))
    (let ((out (org-drill-statistics--render-forecast)))
      (should (string-match-p
               "| Today | \\+1 | \\+2 | \\+3 | \\+4 | \\+5 | \\+6 |"
               out)))))

(ert-deftest test-org-drill-statistics-render-forecast-counts-row ()
  "The counts row reflects the forecast helper's per-day values."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '(3 1 0 5 0 2 4))))
    (let ((out (org-drill-statistics--render-forecast)))
      (should (string-match-p "| 3 | 1 | 0 | 5 | 0 | 2 | 4 |" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-trailing-newline ()
  "The section ends with a newline so sections concatenate cleanly."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '(0 0 0 0 0 0 0))))
    (let ((out (org-drill-statistics--render-forecast)))
      (should (string-suffix-p "\n" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-honors-days ()
  "A custom DAYS yields a header and row sized to the forecast length."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&optional _scope _days) '(2 7 1))))
    (let ((out (org-drill-statistics--render-forecast nil 3)))
      (should (string-match-p "| Today | \\+1 | \\+2 |" out))
      (should (string-match-p "| 2 | 7 | 1 |" out))
      ;; No fourth column should appear.
      (should-not (string-match-p "\\+3" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-empty-window ()
  "A zero-length forecast renders a note instead of a table."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '())))
    (let ((out (org-drill-statistics--render-forecast nil 0)))
      (should (string-prefix-p "** Forecast\n" out))
      (should (string-match-p "No forecast window configured." out))
      (should-not (string-match-p "|" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-single-day ()
  "A one-day forecast renders just the Today column."
  (cl-letf (((symbol-function 'org-drill-statistics--forecast)
             (lambda (&rest _) '(9)))
            (org-drill-statistics-forecast-days 1))
    (let ((out (org-drill-statistics--render-forecast)))
      (should (string-match-p "| Today |" out))
      (should (string-match-p "| 9 |" out))
      (should-not (string-match-p "\\+1" out)))))

(ert-deftest test-org-drill-statistics-render-forecast-default-days ()
  "With no DAYS argument the section spans the configured default."
  (let ((org-drill-statistics-forecast-days 7))
    (cl-letf (((symbol-function 'org-drill-statistics--forecast)
               (lambda (&optional _scope days)
                 ;; Echo the resolved day count as a flat list so the
                 ;; renderer's column count is observable.
                 (make-list (or days org-drill-statistics-forecast-days)
                            0))))
      (let ((out (org-drill-statistics--render-forecast)))
        (should (string-match-p "\\+6 |" out))
        (should-not (string-match-p "\\+7" out))))))

(ert-deftest test-org-drill-statistics-render-forecast-with-scope-buffer ()
  "End to end through the real forecast helper against a temp org buffer.
Two cards are scheduled today and one is scheduled three days out; the
rendered counts row must reflect that bucketing."
  (let ((today (format-time-string "%Y-%m-%d" (current-time)))
        (plus3 (format-time-string
                "%Y-%m-%d" (time-add (current-time) (days-to-time 3)))))
    (with-temp-buffer
      ;; Cards carry the drill tag and put SCHEDULED right after the
      ;; heading so the traversal sees them; scope is 'file (a buffer
      ;; list would be read as a list of file paths and fail).
      (insert (format "* Drill cards\n"))
      (insert (format "** Card one :drill:\nSCHEDULED: <%s>\n:PROPERTIES:\n:DRILL_CARD_TYPE: simple\n:END:\nfront\n" today))
      (insert (format "** Card two :drill:\nSCHEDULED: <%s>\n:PROPERTIES:\n:DRILL_CARD_TYPE: simple\n:END:\nfront\n" today))
      (insert (format "** Card three :drill:\nSCHEDULED: <%s>\n:PROPERTIES:\n:DRILL_CARD_TYPE: simple\n:END:\nfront\n" plus3))
      (org-mode)
      (let* ((org-drill-scope 'file)
             (org-drill-question-tag "drill")
             (org-drill-match nil)
             (out (org-drill-statistics--render-forecast 'file 7)))
        ;; Today column = 2, +3 column = 1, others 0.
        (should (string-match-p "| 2 | 0 | 0 | 1 | 0 | 0 | 0 |" out))))))

(provide 'test-org-drill-statistics-render-forecast)

;;; test-org-drill-statistics-render-forecast.el ends here
