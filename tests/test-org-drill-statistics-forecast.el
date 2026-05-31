;;; test-org-drill-statistics-forecast.el --- Tests for forecast statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard forecast block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; Tests for org-drill-statistics--forecast and its pure bucketer.
;;
;; The bucketing math is exercised directly against
;; `org-drill-statistics--bucket-forecast-days' with synthetic day
;; numbers (no org buffer needed).  The org-reading path is exercised
;; through a with-temp-buffer fixture whose SCHEDULED dates are derived
;; relative to (current-time), never hardcoded.


(defun test-org-drill-statistics--scheduled-offset-string (days-from-today)
  "Return an active org SCHEDULED stamp DAYS-FROM-TODAY days out.
Derived from `current-time' so the fixture never hardcodes a date."
  (format-time-string
   "<%Y-%m-%d %a>"
   (time-add (current-time) (days-to-time days-from-today))))

(defun test-org-drill-statistics--forecast-fixture (offsets)
  "Insert one scheduled drill card per integer in OFFSETS.
Each card is tagged `drill' and scheduled that many days from today.
Returns nothing; call inside a `with-temp-buffer' after `org-mode'."
  (let ((n 0))
    (dolist (off offsets)
      (setq n (1+ n))
      (insert (format "* Card %d :drill:\nSCHEDULED: %s\n"
                      n
                      (test-org-drill-statistics--scheduled-offset-string off))))))

;; Normal cases --------------------------------------------------------

(ert-deftest test-org-drill-statistics-forecast-bucket-counts-by-offset ()
  "Bucketer tallies each day-offset into the matching index."
  (let ((today 1000))
    ;; Two due today, one due tomorrow, one due in 3 days.
    (should (equal (org-drill-statistics--bucket-forecast-days
                    (list 1000 1000 1001 1003) today 7)
                   '(2 1 0 1 0 0 0)))))

(ert-deftest test-org-drill-statistics-forecast-bucket-empty-input ()
  "Bucketer returns an all-zero vector of the requested length."
  (should (equal (org-drill-statistics--bucket-forecast-days nil 500 5)
                 '(0 0 0 0 0))))

(ert-deftest test-org-drill-statistics-forecast-reads-scheduled-entries ()
  "Forecast over a fixture counts cards by their SCHEDULED day."
  (with-temp-buffer
    (org-mode)
    ;; today, today, tomorrow, day-after, plus a far-future card.
    (test-org-drill-statistics--forecast-fixture '(0 0 1 2 30))
    (let ((org-drill-scope 'file)
          (org-drill-question-tag "drill")
          (org-drill-match nil))
      ;; Default window of 7: the day-30 card falls outside it.
      ;; Offsets 0,0,1,2 bucket to today=2, +1=1, +2=1; +30 dropped.
      (should (equal (org-drill-statistics--forecast 'file 7)
                     '(2 1 1 0 0 0 0))))))

;; Boundary cases ------------------------------------------------------

(ert-deftest test-org-drill-statistics-forecast-bucket-window-edges ()
  "Cards on the last in-window day count; the next day does not."
  (let ((today 0))
    ;; offset 6 is the last bucket of a 7-day window; offset 7 is out.
    (should (equal (org-drill-statistics--bucket-forecast-days
                    (list 6 7) today 7)
                   '(0 0 0 0 0 0 1)))))

(ert-deftest test-org-drill-statistics-forecast-bucket-past-and-future-dropped ()
  "Days before today and beyond the window are not counted."
  (let ((today 100))
    (should (equal (org-drill-statistics--bucket-forecast-days
                    (list 99 100 110) today 3)
                   '(1 0 0)))))

(ert-deftest test-org-drill-statistics-forecast-bucket-single-day-window ()
  "A one-day window yields a single bucket counting today's cards."
  ;; Two cards due today, one due tomorrow; a window of 1 keeps only the
  ;; today bucket, so the tomorrow card is dropped.
  (should (equal (org-drill-statistics--bucket-forecast-days
                  (list 50 50 51) 50 1)
                 '(2))))

(ert-deftest test-org-drill-statistics-forecast-skips-unscheduled-entries ()
  "Entries without a SCHEDULED time are ignored by the reader."
  (with-temp-buffer
    (org-mode)
    (insert "* Scheduled card :drill:\nSCHEDULED: "
            (test-org-drill-statistics--scheduled-offset-string 0)
            "\n* No-schedule card :drill:\n")
    (let ((org-drill-scope 'file)
          (org-drill-question-tag "drill")
          (org-drill-match nil))
      (should (equal (org-drill-statistics--scheduled-days 'file)
                     (list (org-drill-statistics--today-day))))
      (should (equal (org-drill-statistics--forecast 'file 3)
                     '(1 0 0))))))

;; Error / degenerate cases -------------------------------------------

(ert-deftest test-org-drill-statistics-forecast-bucket-zero-days ()
  "A zero-length window yields the empty list."
  (should (equal (org-drill-statistics--bucket-forecast-days
                  (list 10 11) 10 0)
                 nil)))

(ert-deftest test-org-drill-statistics-forecast-bucket-negative-days ()
  "A negative window length is treated as empty, not an error."
  (should (equal (org-drill-statistics--bucket-forecast-days
                  (list 10 11) 10 -3)
                 nil)))

(ert-deftest test-org-drill-statistics-forecast-empty-scope ()
  "A scope with no drill entries forecasts all zeros."
  (with-temp-buffer
    (org-mode)
    (insert "* Not a card\nSome prose.\n")
    (let ((org-drill-scope 'file)
          (org-drill-question-tag "drill")
          (org-drill-match nil))
      (should (equal (org-drill-statistics--forecast 'file 4)
                     '(0 0 0 0))))))

(provide 'test-org-drill-statistics-forecast)

;;; test-org-drill-statistics-forecast.el ends here
