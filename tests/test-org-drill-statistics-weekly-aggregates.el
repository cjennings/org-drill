;;; test-org-drill-statistics-weekly-aggregates.el --- Tests for weekly-aggregates statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard weekly-aggregates block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; Tests for org-drill-statistics--weekly-aggregates.
;;
;; All tests redefine `org-drill-statistics--today-day' to a fixed
;; absolute day so the window is deterministic and never anchored to the
;; real clock.  The chosen day, 739767, is Sunday 2026-05-31; its
;; Monday-based week start is 739761.  Fixtures build records at noon of
;; a chosen absolute day, which round-trips cleanly through the
;; `time-to-days' path that `org-drill-statistics--record-day' uses.

(defun test-org-drill-statistics-weekly--abs-to-float (abs hour)
  "Return a float-time for HOUR (local) on absolute day ABS."
  (let ((g (calendar-gregorian-from-absolute abs)))
    (float-time
     (encode-time (list 0 0 hour
                        (calendar-extract-day g)
                        (calendar-extract-month g)
                        (calendar-extract-year g)
                        nil -1 nil)))))

(defun test-org-drill-statistics-weekly--rec (abs qualities &optional dur-min)
  "Build a record starting at noon on ABS, lasting DUR-MIN minutes.
QUALITIES is a sequence of integers; DUR-MIN defaults to 10."
  (let ((start (test-org-drill-statistics-weekly--abs-to-float abs 12)))
    (make-org-drill-session-record
     :start-time start
     :end-time (+ start (* 60 (or dur-min 10)))
     :qualities (vconcat qualities)
     :algorithm 'sm5)))

(defmacro test-org-drill-statistics-weekly--with-today (abs &rest body)
  "Run BODY with `org-drill-statistics--today-day' fixed to ABS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-drill-statistics--today-day)
              (lambda () ,abs)))
     ,@body))

(defconst test-org-drill-statistics-weekly--today 739767
  "Fixed today for tests: Sunday 2026-05-31, absolute day number.")
(defconst test-org-drill-statistics-weekly--this-mon 739761
  "Monday starting the week of `test-org-drill-statistics-weekly--today'.")

;;; ---- Normal cases ----

(ert-deftest test-org-drill-statistics-weekly-default-span ()
  "Default WEEKS is 12, oldest-first, with each week 7 days apart."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let ((agg (org-drill-statistics--weekly-aggregates nil)))
      (should (= 12 (length agg)))
      (should (= (- test-org-drill-statistics-weekly--this-mon (* 7 11))
                 (plist-get (car agg) :week-start)))
      (should (= test-org-drill-statistics-weekly--this-mon
                 (plist-get (car (last agg)) :week-start)))
      (cl-loop for (a b) on agg while b
               do (should (= 7 (- (plist-get b :week-start)
                                  (plist-get a :week-start))))))))

(ert-deftest test-org-drill-statistics-weekly-pooled-pass-percent ()
  "Reviews sum pooled qualities; pass-percent is pooled, not averaged."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    ;; Two sessions this week: pooled (5 4 1 2) -> 2 pass of 4 -> 50.
    (let* ((log (list (test-org-drill-statistics-weekly--rec
                       test-org-drill-statistics-weekly--today '(5 4) 10)
                      (test-org-drill-statistics-weekly--rec
                       (1+ test-org-drill-statistics-weekly--this-mon)
                       '(1 2) 30)))
           (agg (org-drill-statistics--weekly-aggregates log))
           (this (car (last agg))))
      (should (= test-org-drill-statistics-weekly--this-mon
                 (plist-get this :week-start)))
      (should (= 4 (plist-get this :reviews)))
      (should (= 50 (plist-get this :pass-percent)))
      (should (= 20.0 (plist-get this :avg-duration-min))))))

(ert-deftest test-org-drill-statistics-weekly-records-spread-weeks ()
  "Records land in their own Monday-based week buckets."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let* ((mon test-org-drill-statistics-weekly--this-mon)
           (log (list (test-org-drill-statistics-weekly--rec
                       test-org-drill-statistics-weekly--today '(5))
                      (test-org-drill-statistics-weekly--rec
                       (- mon 7) '(0 0))
                      (test-org-drill-statistics-weekly--rec
                       (- mon 14) '(4))))
           (agg (org-drill-statistics--weekly-aggregates log 12))
           (by-start (mapcar (lambda (p) (cons (plist-get p :week-start)
                                               (plist-get p :reviews)))
                             agg)))
      (should (= 1 (cdr (assoc mon by-start))))
      (should (= 2 (cdr (assoc (- mon 7) by-start))))
      (should (= 1 (cdr (assoc (- mon 14) by-start)))))))

;;; ---- Boundary cases ----

(ert-deftest test-org-drill-statistics-weekly-empty-log ()
  "An empty log yields WEEKS all-zero plists."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let ((agg (org-drill-statistics--weekly-aggregates nil 3)))
      (should (= 3 (length agg)))
      (dolist (p agg)
        (should (= 0 (plist-get p :reviews)))
        (should (= 0 (plist-get p :pass-percent)))
        (should (= 0.0 (plist-get p :avg-duration-min)))))))

(ert-deftest test-org-drill-statistics-weekly-single-week ()
  "WEEKS = 1 keeps only the current week's records."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let* ((log (list (test-org-drill-statistics-weekly--rec
                       test-org-drill-statistics-weekly--today '(5 5))
                      (test-org-drill-statistics-weekly--rec
                       (- test-org-drill-statistics-weekly--this-mon 7) '(0))))
           (agg (org-drill-statistics--weekly-aggregates log 1)))
      (should (= 1 (length agg)))
      (should (= test-org-drill-statistics-weekly--this-mon
                 (plist-get (car agg) :week-start)))
      (should (= 2 (plist-get (car agg) :reviews)))
      (should (= 100 (plist-get (car agg) :pass-percent))))))

(ert-deftest test-org-drill-statistics-weekly-out-of-window-dropped ()
  "Records older than the window are not bucketed."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let* ((log (list (test-org-drill-statistics-weekly--rec
                       (- test-org-drill-statistics-weekly--this-mon (* 7 5))
                       '(5))))
           (agg (org-drill-statistics--weekly-aggregates log 3)))
      (should (cl-every (lambda (p) (= 0 (plist-get p :reviews))) agg)))))

(ert-deftest test-org-drill-statistics-weekly-week-boundary-monday ()
  "A Monday session counts in its week; the Sunday before is the prior week."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let* ((mon test-org-drill-statistics-weekly--this-mon)
           (log (list (test-org-drill-statistics-weekly--rec mon '(5))
                      (test-org-drill-statistics-weekly--rec (1- mon) '(4))))
           (agg (org-drill-statistics--weekly-aggregates log 2))
           (prior (car agg))
           (this (cadr agg)))
      (should (= (- mon 7) (plist-get prior :week-start)))
      (should (= 1 (plist-get prior :reviews)))
      (should (= mon (plist-get this :week-start)))
      (should (= 1 (plist-get this :reviews))))))

;;; ---- Error cases ----

(ert-deftest test-org-drill-statistics-weekly-non-positive-weeks-errors ()
  "WEEKS below 1 signals an error."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (should-error (org-drill-statistics--weekly-aggregates nil 0))
    (should-error (org-drill-statistics--weekly-aggregates nil -3))))

(ert-deftest test-org-drill-statistics-weekly-empty-qualities-record ()
  "A record with no qualities adds 0 reviews but still counts toward the
week's average duration."
  (test-org-drill-statistics-weekly--with-today
      test-org-drill-statistics-weekly--today
    (let* ((log (list (test-org-drill-statistics-weekly--rec
                       test-org-drill-statistics-weekly--today [] 10)))
           (agg (org-drill-statistics--weekly-aggregates log 1))
           (this (car agg)))
      (should (= 0 (plist-get this :reviews)))
      (should (= 0 (plist-get this :pass-percent)))
      (should (= 10.0 (plist-get this :avg-duration-min))))))

(provide 'test-org-drill-statistics-weekly-aggregates)

;;; test-org-drill-statistics-weekly-aggregates.el ends here
