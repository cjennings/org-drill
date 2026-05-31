;;; test-org-drill-statistics-primitives.el --- Tests for primitives statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard primitives block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; Tests for statistics primitives.  Require 'org-drill and 'cl-lib.
;;; Fixtures are deterministic: start-time values are fixed floats, and
;;; day numbers are derived, never hardcoded against today's date.


(defun test-org-drill-statistics--make-record (start-time algorithm)
  "Build a minimal `org-drill-session-record' for tests.
START-TIME is a float; ALGORITHM is a symbol.  Other slots get inert
placeholder values sufficient for the primitive under test."
  (make-org-drill-session-record
   :start-time start-time
   :end-time (+ start-time 60.0)
   :scope 'directory
   :algorithm algorithm
   :qualities (vector 5 4 3)
   :pass-percent 67
   :new-count 1
   :mature-count 1
   :failed-count 1
   :cram-mode nil))

;;; org-drill-statistics--today-day

(ert-deftest test-org-drill-statistics-today-day-matches-time-to-days ()
  "`--today-day' equals `time-to-days' of the current time."
  (should (= (org-drill-statistics--today-day)
             (time-to-days (current-time)))))

(ert-deftest test-org-drill-statistics-today-day-redefinable ()
  "`--today-day' can be redefined to a fixed day for deterministic tests."
  (cl-letf (((symbol-function 'org-drill-statistics--today-day)
             (lambda () 700000)))
    (should (= (org-drill-statistics--today-day) 700000))))

;;; org-drill-statistics--record-day

(ert-deftest test-org-drill-statistics-record-day-derives-from-start-time ()
  "`--record-day' returns the day number of the record's start time."
  (let* ((now (float-time))
         (record (test-org-drill-statistics--make-record now 'sm5)))
    (should (= (org-drill-statistics--record-day record)
               (time-to-days (seconds-to-time now))))))

(ert-deftest test-org-drill-statistics-record-day-earlier-is-smaller ()
  "A record started a day earlier has a day number one less."
  (let* ((now (float-time))
         (today (test-org-drill-statistics--make-record now 'sm5))
         (yesterday (test-org-drill-statistics--make-record
                     (- now 86400.0) 'sm5)))
    (should (= (- (org-drill-statistics--record-day today)
                  (org-drill-statistics--record-day yesterday))
               1))))

;;; org-drill-statistics--filter-log

(ert-deftest test-org-drill-statistics-filter-log-nil-returns-all ()
  "Filtering with nil algorithm returns the log unchanged."
  (let ((log (list (test-org-drill-statistics--make-record 100.0 'sm5)
                   (test-org-drill-statistics--make-record 200.0 'simple8))))
    (should (equal (org-drill-statistics--filter-log log nil) log))))

(ert-deftest test-org-drill-statistics-filter-log-keeps-matching ()
  "Filtering keeps only records whose algorithm matches."
  (let* ((a (test-org-drill-statistics--make-record 100.0 'sm5))
         (b (test-org-drill-statistics--make-record 200.0 'simple8))
         (c (test-org-drill-statistics--make-record 300.0 'sm5))
         (log (list a b c))
         (result (org-drill-statistics--filter-log log 'sm5)))
    (should (equal result (list a c)))))

(ert-deftest test-org-drill-statistics-filter-log-no-match-returns-empty ()
  "Filtering on an absent algorithm returns an empty list."
  (let ((log (list (test-org-drill-statistics--make-record 100.0 'sm5))))
    (should (null (org-drill-statistics--filter-log log 'simple8)))))

(ert-deftest test-org-drill-statistics-filter-log-empty-log ()
  "Filtering an empty log returns an empty list for any algorithm."
  (should (null (org-drill-statistics--filter-log nil 'sm5)))
  (should (null (org-drill-statistics--filter-log nil nil))))

(ert-deftest test-org-drill-statistics-filter-log-does-not-mutate ()
  "Filtering leaves the input list intact."
  (let* ((log (list (test-org-drill-statistics--make-record 100.0 'sm5)
                    (test-org-drill-statistics--make-record 200.0 'simple8)))
         (copy (copy-sequence log)))
    (org-drill-statistics--filter-log log 'sm5)
    (should (equal log copy))))

;;; org-drill-statistics--log-since

(ert-deftest test-org-drill-statistics-log-since-keeps-at-or-after-cutoff ()
  "Records at or after the cutoff are kept; earlier ones dropped."
  (let* ((before (test-org-drill-statistics--make-record 100.0 'sm5))
         (at (test-org-drill-statistics--make-record 200.0 'sm5))
         (after (test-org-drill-statistics--make-record 300.0 'sm5))
         (log (list before at after))
         (result (org-drill-statistics--log-since log 200.0)))
    (should (equal result (list at after)))))

(ert-deftest test-org-drill-statistics-log-since-all-before-cutoff ()
  "When every record predates the cutoff, the result is empty."
  (let ((log (list (test-org-drill-statistics--make-record 100.0 'sm5)
                   (test-org-drill-statistics--make-record 150.0 'sm5))))
    (should (null (org-drill-statistics--log-since log 200.0)))))

(ert-deftest test-org-drill-statistics-log-since-all-after-cutoff ()
  "When every record is at or after the cutoff, all are kept."
  (let* ((log (list (test-org-drill-statistics--make-record 300.0 'sm5)
                    (test-org-drill-statistics--make-record 400.0 'sm5)))
         (result (org-drill-statistics--log-since log 200.0)))
    (should (equal result log))))

(ert-deftest test-org-drill-statistics-log-since-empty-log ()
  "Filtering an empty log by cutoff returns an empty list."
  (should (null (org-drill-statistics--log-since nil 200.0))))

(ert-deftest test-org-drill-statistics-log-since-does-not-mutate ()
  "Cutoff filtering leaves the input list intact."
  (let* ((log (list (test-org-drill-statistics--make-record 100.0 'sm5)
                    (test-org-drill-statistics--make-record 300.0 'sm5)))
         (copy (copy-sequence log)))
    (org-drill-statistics--log-since log 200.0)
    (should (equal log copy))))

(provide 'test-org-drill-statistics-primitives)

;;; test-org-drill-statistics-primitives.el ends here
