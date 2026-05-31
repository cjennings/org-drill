;;; test-org-drill-statistics-quality-histogram.el --- Tests for quality-histogram statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard quality-histogram block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(defun test-org-drill-statistics--make-record (qualities)
  "Build a minimal `org-drill-session-record' carrying QUALITIES.
QUALITIES is a vector of ints.  Other slots are filled with inert
defaults so the histogram tests stay focused on the qualities slot."
  (make-org-drill-session-record
   :start-time 0.0
   :end-time 0.0
   :scope nil
   :algorithm 'sm5
   :qualities qualities
   :pass-percent 0
   :new-count 0
   :mature-count 0
   :failed-count 0
   :cram-mode nil))

;; Normal cases.

(ert-deftest test-org-drill-statistics-quality-histogram-single-record ()
  "A single record's qualities are tallied into the right buckets."
  (let* ((record (test-org-drill-statistics--make-record [0 3 3 5 3]))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [1 0 0 3 0 1]))))

(ert-deftest test-org-drill-statistics-quality-histogram-multiple-records ()
  "Counts sum across every record in the log."
  (let* ((r1 (test-org-drill-statistics--make-record [0 1 2]))
         (r2 (test-org-drill-statistics--make-record [3 4 5]))
         (r3 (test-org-drill-statistics--make-record [0 5 5]))
         (result (org-drill-statistics--quality-histogram (list r1 r2 r3))))
    (should (equal result [2 1 1 1 1 3]))))

(ert-deftest test-org-drill-statistics-quality-histogram-all-same-quality ()
  "A record with every entry the same quality concentrates in one bucket."
  (let* ((record (test-org-drill-statistics--make-record [4 4 4 4]))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [0 0 0 0 4 0]))))

;; Boundary cases.

(ert-deftest test-org-drill-statistics-quality-histogram-empty-log ()
  "An empty log yields an all-zero histogram, never nil."
  (let ((result (org-drill-statistics--quality-histogram '())))
    (should (equal result [0 0 0 0 0 0]))))

(ert-deftest test-org-drill-statistics-quality-histogram-empty-qualities ()
  "A record with an empty qualities vector contributes nothing."
  (let* ((record (test-org-drill-statistics--make-record []))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [0 0 0 0 0 0]))))

(ert-deftest test-org-drill-statistics-quality-histogram-nil-qualities ()
  "A record whose qualities slot is nil is skipped without error."
  (let* ((r1 (test-org-drill-statistics--make-record nil))
         (r2 (test-org-drill-statistics--make-record [2 2]))
         (result (org-drill-statistics--quality-histogram (list r1 r2))))
    (should (equal result [0 0 2 0 0 0]))))

(ert-deftest test-org-drill-statistics-quality-histogram-extreme-buckets ()
  "Quality 0 and quality 5, the range endpoints, both land correctly."
  (let* ((record (test-org-drill-statistics--make-record [0 0 5 5 5]))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [2 0 0 0 0 3]))))

;; Error cases.

(ert-deftest test-org-drill-statistics-quality-histogram-out-of-range-ignored ()
  "Qualities outside 0..5 are dropped, valid ones still counted."
  (let* ((record (test-org-drill-statistics--make-record [-1 6 3 99 2]))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [0 0 1 1 0 0]))))

(ert-deftest test-org-drill-statistics-quality-histogram-non-integer-ignored ()
  "Non-integer quality entries are ignored rather than signalling."
  (let* ((record (test-org-drill-statistics--make-record [2 nil 2.5 3]))
         (result (org-drill-statistics--quality-histogram (list record))))
    (should (equal result [0 0 1 1 0 0]))))

(ert-deftest test-org-drill-statistics-quality-histogram-does-not-mutate-input ()
  "The qualities vectors are read, never written."
  (let* ((qualities (vector 1 2 3))
         (record (test-org-drill-statistics--make-record qualities)))
    (org-drill-statistics--quality-histogram (list record))
    (should (equal qualities [1 2 3]))))

(provide 'test-org-drill-statistics-quality-histogram)

;;; test-org-drill-statistics-quality-histogram.el ends here
