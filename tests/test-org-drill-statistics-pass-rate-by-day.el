;;; test-org-drill-statistics-pass-rate-by-day.el --- Tests for pass-rate-by-day statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard pass-rate-by-day block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(defun test-org-drill-statistics--fixed-today (day)
  "Install a stub for `org-drill-statistics--today-day' returning DAY.
Returns nothing useful, used for its side effect inside a fixture."
  (advice-add 'org-drill-statistics--today-day :override
              (lambda () day) '((name . test-fixed-today))))

(defun test-org-drill-statistics--clear-today ()
  "Remove the fixed-today stub installed by the helper above."
  (advice-remove 'org-drill-statistics--today-day 'test-fixed-today))

(defun test-org-drill-statistics--record-on-day (day qualities)
  "Build a session record started on absolute DAY with QUALITIES vector.
DAY is an absolute day number as from `time-to-days'.  QUALITIES is a
vector of integer qualities.  The start-time is the float-time at noon
of that day, so day bucketing is unambiguous."
  (let ((start (+ (float-time
                   (encode-time 0 0 0 1 1 2000))
                  (* (- day (time-to-days
                             (encode-time 0 0 0 1 1 2000)))
                     86400)
                  (* 12 3600))))
    (make-org-drill-session-record
     :start-time start
     :end-time start
     :scope 'directory
     :algorithm 'sm5
     :qualities qualities
     :pass-percent 0
     :new-count 0
     :mature-count 0
     :failed-count 0
     :cram-mode nil)))

;; Normal: a multi-day log produces per-day pass rates in chronological
;; order, with the failure-quality threshold deciding pass vs fail.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-basic ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          ;; today: qualities 5 5 1 0 -> 2 pass of 4 -> 50
          ;; yesterday: qualities 4 3 -> 2 pass of 2 -> 100
          (let* ((log (list
                       (test-org-drill-statistics--record-on-day
                        today [5 5 1 0])
                       (test-org-drill-statistics--record-on-day
                        (1- today) [4 3])))
                 (v (org-drill-statistics--pass-rate-by-day log 3)))
            (should (= (length v) 3))
            (should (null (aref v 0)))          ; two days ago, no data
            (should (= (aref v 1) 100))         ; yesterday
            (should (= (aref v 2) 50))))        ; today
      (test-org-drill-statistics--clear-today))))

;; Normal: multiple records on the same day aggregate together.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-same-day-merge ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          ;; today across two records: [5 1] and [4 0 2]
          ;; passes: 5,4 -> 2 ; total 5 -> 40
          (let* ((log (list
                       (test-org-drill-statistics--record-on-day
                        today [5 1])
                       (test-org-drill-statistics--record-on-day
                        today [4 0 2])))
                 (v (org-drill-statistics--pass-rate-by-day log 1)))
            (should (= (length v) 1))
            (should (= (aref v 0) 40))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: empty log yields an all-nil vector of the requested length.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-empty-log ()
  (let ((today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (let ((v (org-drill-statistics--pass-rate-by-day nil 5)))
            (should (= (length v) 5))
            (should (cl-every #'null (append v nil)))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: a record outside the window is ignored; one at the oldest
;; edge of the window is counted at index 0.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-window-edges ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (let* ((days 3)
                 (oldest (- today (1- days)))      ; today-2
                 (log (list
                       ;; just outside the window (too old): ignored
                       (test-org-drill-statistics--record-on-day
                        (1- oldest) [5 5])
                       ;; oldest day in the window: index 0
                       (test-org-drill-statistics--record-on-day
                        oldest [5 0])))
                 (v (org-drill-statistics--pass-rate-by-day log days)))
            (should (= (length v) days))
            (should (= (aref v 0) 50))           ; oldest in-window day
            (should (null (aref v 1)))
            (should (null (aref v 2)))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: a record dated in the future relative to today is ignored.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-future-ignored ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (let* ((log (list
                       (test-org-drill-statistics--record-on-day
                        (1+ today) [5 5 5])))
                 (v (org-drill-statistics--pass-rate-by-day log 3)))
            (should (cl-every #'null (append v nil)))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: threshold edge. A quality equal to the threshold is a fail;
;; one above it is a pass.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-threshold-edge ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          ;; qualities 2 (fail) and 3 (pass) -> 1 of 2 -> 50
          (let* ((log (list
                       (test-org-drill-statistics--record-on-day
                        today [2 3])))
                 (v (org-drill-statistics--pass-rate-by-day log 1)))
            (should (= (aref v 0) 50))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: a record whose qualities vector is empty contributes no
;; total, leaving that day as no-data rather than a division by zero.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-empty-qualities ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (let* ((log (list
                       (test-org-drill-statistics--record-on-day
                        today [])))
                 (v (org-drill-statistics--pass-rate-by-day log 1)))
            (should (null (aref v 0)))))
      (test-org-drill-statistics--clear-today))))

;; Boundary: DAYS defaults to `org-drill-statistics-trend-days' when
;; omitted, and a non-positive DAYS is clamped to a length-1 vector.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-days-arg ()
  (let ((org-drill-statistics-trend-days 12)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (should (= (length
                      (org-drill-statistics--pass-rate-by-day nil))
                     12))
          (should (= (length
                      (org-drill-statistics--pass-rate-by-day nil 0))
                     1)))
      (test-org-drill-statistics--clear-today))))

;; Error: a nil qualities slot is tolerated as no-data, not a crash.
(ert-deftest test-org-drill-statistics-pass-rate-by-day-nil-qualities ()
  (let ((org-drill-failure-quality 2)
        (today 700000))
    (unwind-protect
        (progn
          (test-org-drill-statistics--fixed-today today)
          (let* ((rec (test-org-drill-statistics--record-on-day
                       today [5 5]))
                 (_ (setf (org-drill-session-record-qualities rec) nil))
                 (v (org-drill-statistics--pass-rate-by-day
                     (list rec) 1)))
            (should (null (aref v 0)))))
      (test-org-drill-statistics--clear-today))))

(provide 'test-org-drill-statistics-pass-rate-by-day)

;;; test-org-drill-statistics-pass-rate-by-day.el ends here
