;;; test-org-drill-statistics-reviews-by-day.el --- Tests for reviews-by-day statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard reviews-by-day block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;; Shared fixture: build a session record whose start day is a fixed
;; offset from today, so `org-drill-statistics--record-day' maps it
;; back to the intended absolute day without depending on a literal
;; date.  The start-time is noon local time on that calendar day, which
;; keeps `time-to-days' off any DST or midnight boundary.

(defun org-drill-statistics-test--record-at (day-offset n-qualities)
  "Build a session record starting DAY-OFFSET days before today.
The start-time is noon local time on that calendar day, so
`org-drill-statistics--record-day' maps it back to the intended day.
N-QUALITIES sets the length of the qualities vector, the review count
the record contributes; 0 yields an empty vector."
  (let* ((target-day (- (org-drill-statistics--today-day) day-offset))
         (greg (calendar-gregorian-from-absolute target-day))
         (ts (encode-time 0 0 12 (nth 1 greg) (nth 0 greg) (nth 2 greg))))
    (make-org-drill-session-record
     :start-time (float-time ts)
     :qualities (make-vector n-qualities 3))))

;; Normal cases

(ert-deftest test-org-drill-statistics-reviews-by-day-normal-counts-per-day ()
  "Reviews land in the correct oldest-to-newest slots by start day."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at 0 2)
                  (org-drill-statistics-test--record-at 1 3)
                  (org-drill-statistics-test--record-at 6 1))
            7)))
    (should (= (length v) 7))
    (should (= (aref v 6) 2))
    (should (= (aref v 5) 3))
    (should (= (aref v 0) 1))
    (should (= (apply #'+ (append v nil)) 6))))

(ert-deftest test-org-drill-statistics-reviews-by-day-normal-same-day-accumulates ()
  "Multiple records on the same day sum their quality counts."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at 0 2)
                  (org-drill-statistics-test--record-at 0 3))
            3)))
    (should (= (aref v 2) 5))))

(ert-deftest test-org-drill-statistics-reviews-by-day-normal-default-days ()
  "Omitting DAYS uses `org-drill-statistics-trend-days' for the length."
  (let ((org-drill-statistics-trend-days 30))
    (should (= (length (org-drill-statistics--reviews-by-day nil)) 30))))

;; Boundary cases

(ert-deftest test-org-drill-statistics-reviews-by-day-boundary-empty-log ()
  "An empty log yields an all-zero vector of the requested length."
  (let ((v (org-drill-statistics--reviews-by-day nil 5)))
    (should (= (length v) 5))
    (should (= (apply #'+ (append v nil)) 0))))

(ert-deftest test-org-drill-statistics-reviews-by-day-boundary-single-day ()
  "DAYS of 1 yields a one-slot vector holding today's count."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at 0 4)) 1)))
    (should (= (length v) 1))
    (should (= (aref v 0) 4))))

(ert-deftest test-org-drill-statistics-reviews-by-day-boundary-edges-of-window ()
  "The oldest in-window day fills slot 0; one day older is dropped."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at 6 2)
                  (org-drill-statistics-test--record-at 7 9))
            7)))
    (should (= (aref v 0) 2))
    (should (= (apply #'+ (append v nil)) 2))))

(ert-deftest test-org-drill-statistics-reviews-by-day-boundary-empty-qualities ()
  "A record with an empty qualities vector contributes zero."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at 0 0)) 3)))
    (should (= (apply #'+ (append v nil)) 0))))

;; Error cases

(ert-deftest test-org-drill-statistics-reviews-by-day-error-future-record-ignored ()
  "A record dated in the future falls outside the window and is ignored."
  (let ((v (org-drill-statistics--reviews-by-day
            (list (org-drill-statistics-test--record-at -3 5)) 7)))
    (should (= (apply #'+ (append v nil)) 0))))

(ert-deftest test-org-drill-statistics-reviews-by-day-error-nonpositive-days-clamped ()
  "DAYS of 0 or negative is clamped to a single today slot."
  (let ((v0 (org-drill-statistics--reviews-by-day
             (list (org-drill-statistics-test--record-at 0 2)) 0))
        (vn (org-drill-statistics--reviews-by-day
             (list (org-drill-statistics-test--record-at 0 3)) -5)))
    (should (= (length v0) 1))
    (should (= (aref v0 0) 2))
    (should (= (length vn) 1))
    (should (= (aref vn 0) 3))))

(provide 'test-org-drill-statistics-reviews-by-day)

;;; test-org-drill-statistics-reviews-by-day.el ends here
