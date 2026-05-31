;;; test-org-drill-statistics-attention-data.el --- Tests for attention-data statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard attention-data block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; ERT tests for the needs-attention selectors.
;;
;; The org-traversal collector and the public selectors are exercised
;; through a `with-temp-buffer' fixture with deterministic data.  Day
;; offsets are computed relative to (current-time) so the fixture never
;; hardcodes a calendar date.  The pure predicates and the row cap are
;; tested directly on structs without any buffer.


(defun test-org-drill-statistics--inactive-stamp (days-ago)
  "Return an inactive org timestamp string DAYS-AGO before today.
Derived from (current-time) so the fixture stays date-independent."
  (org-drill-time-to-inactive-org-timestamp
   (time-subtract (current-time) (days-to-time days-ago))))

(defun test-org-drill-statistics--mkdata (&rest kw)
  "Build an entry-attention-data struct from keyword args KW.
Defaults: failure 0, avg nil, review nil, added nil, repeats 0, pos 1."
  (org-drill-statistics--make-entry-attention-data
   :heading (or (plist-get kw :heading) "card")
   :pos (or (plist-get kw :pos) 1)
   :failure-count (or (plist-get kw :failure-count) 0)
   :avg-quality (plist-get kw :avg-quality)
   :days-since-review (plist-get kw :days-since-review)
   :days-since-added (plist-get kw :days-since-added)
   :total-repeats (or (plist-get kw :total-repeats) 0)))

;;; ---- Normal cases: predicates ----

(ert-deftest test-org-drill-statistics-leech-predicate-flags-low-quality-failer ()
  "A card over the failure threshold with low avg quality is a leech."
  (let ((org-drill-leech-failure-threshold 3)
        (org-drill-statistics-leech-quality-threshold 2.5))
    (should (org-drill-statistics--leech-candidate-p
             (test-org-drill-statistics--mkdata
              :failure-count 4 :avg-quality 1.8)))))

(ert-deftest test-org-drill-statistics-long-overdue-predicate-flags-stale-review ()
  "A review older than the lapse threshold is long overdue."
  (let ((org-drill-lapse-threshold-days 30))
    (should (org-drill-statistics--long-overdue-p
             (test-org-drill-statistics--mkdata :days-since-review 45)))))

(ert-deftest test-org-drill-statistics-forgotten-new-predicate-flags-unrepeated-old ()
  "A card added 20 days ago with zero repeats is forgotten-new."
  (should (org-drill-statistics--forgotten-new-p
           (test-org-drill-statistics--mkdata
            :days-since-added 20 :total-repeats 0))))

;;; ---- Boundary cases: predicates ----

(ert-deftest test-org-drill-statistics-leech-predicate-quality-at-threshold-excluded ()
  "Average quality exactly at the ceiling is not a leech (strict <)."
  (let ((org-drill-leech-failure-threshold 3)
        (org-drill-statistics-leech-quality-threshold 2.5))
    (should-not (org-drill-statistics--leech-candidate-p
                 (test-org-drill-statistics--mkdata
                  :failure-count 5 :avg-quality 2.5)))))

(ert-deftest test-org-drill-statistics-leech-predicate-failures-at-threshold-included ()
  "Failure count equal to the threshold satisfies the >= test."
  (let ((org-drill-leech-failure-threshold 3)
        (org-drill-statistics-leech-quality-threshold 2.5))
    (should (org-drill-statistics--leech-candidate-p
             (test-org-drill-statistics--mkdata
              :failure-count 3 :avg-quality 2.0)))))

(ert-deftest test-org-drill-statistics-long-overdue-predicate-equal-threshold-excluded ()
  "Exactly the lapse threshold is not yet over it (strict >)."
  (let ((org-drill-lapse-threshold-days 30))
    (should-not (org-drill-statistics--long-overdue-p
                 (test-org-drill-statistics--mkdata :days-since-review 30)))))

(ert-deftest test-org-drill-statistics-forgotten-new-predicate-exactly-14-days-included ()
  "Added exactly 14 days ago meets the >= 14 day floor."
  (should (org-drill-statistics--forgotten-new-p
           (test-org-drill-statistics--mkdata
            :days-since-added 14 :total-repeats 0))))

(ert-deftest test-org-drill-statistics-forgotten-new-predicate-13-days-excluded ()
  "Added 13 days ago is below the 14-day floor."
  (should-not (org-drill-statistics--forgotten-new-p
               (test-org-drill-statistics--mkdata
                :days-since-added 13 :total-repeats 0))))

;;; ---- Error / absent-data cases: predicates ----

(ert-deftest test-org-drill-statistics-leech-predicate-missing-quality-excluded ()
  "A card with no recorded average quality is not a leech."
  (let ((org-drill-leech-failure-threshold 3)
        (org-drill-statistics-leech-quality-threshold 2.5))
    (should-not (org-drill-statistics--leech-candidate-p
                 (test-org-drill-statistics--mkdata
                  :failure-count 9 :avg-quality nil)))))

(ert-deftest test-org-drill-statistics-long-overdue-predicate-never-reviewed-excluded ()
  "A never-reviewed card (nil days) is not long overdue."
  (let ((org-drill-lapse-threshold-days 30))
    (should-not (org-drill-statistics--long-overdue-p
                 (test-org-drill-statistics--mkdata :days-since-review nil)))))

(ert-deftest test-org-drill-statistics-forgotten-new-predicate-missing-add-date-excluded ()
  "A card with no add date is not forgotten-new."
  (should-not (org-drill-statistics--forgotten-new-p
               (test-org-drill-statistics--mkdata
                :days-since-added nil :total-repeats 0))))

(ert-deftest test-org-drill-statistics-forgotten-new-predicate-repeated-excluded ()
  "An old card that has been repeated is not forgotten-new."
  (should-not (org-drill-statistics--forgotten-new-p
               (test-org-drill-statistics--mkdata
                :days-since-added 30 :total-repeats 2))))

;;; ---- Row cap ----

(ert-deftest test-org-drill-statistics-cap-rows-under-limit-unchanged ()
  "A list shorter than the limit is returned unchanged."
  (let ((org-drill-statistics-attention-row-limit 10))
    (should (equal '(a b c) (org-drill-statistics--cap-rows '(a b c))))))

(ert-deftest test-org-drill-statistics-cap-rows-over-limit-truncated ()
  "A list longer than the limit is truncated to the limit length."
  (let ((org-drill-statistics-attention-row-limit 3))
    (should (equal '(a b c)
                   (org-drill-statistics--cap-rows '(a b c d e))))))

(ert-deftest test-org-drill-statistics-cap-rows-empty-stays-empty ()
  "An empty list caps to empty."
  (let ((org-drill-statistics-attention-row-limit 5))
    (should (null (org-drill-statistics--cap-rows '())))))

;;; ---- timestamp helper ----

(ert-deftest test-org-drill-statistics-days-since-timestamp-nil-returns-nil ()
  "A nil timestamp yields nil days."
  (should (null (org-drill-statistics--days-since-org-timestamp nil 1000))))

(ert-deftest test-org-drill-statistics-days-since-timestamp-malformed-returns-nil ()
  "A malformed timestamp is caught and yields nil rather than erroring."
  (should (null (org-drill-statistics--days-since-org-timestamp
                 "not-a-date" 1000))))

;;; ---- Integration via with-temp-buffer fixture ----

(defmacro test-org-drill-statistics--with-cards (&rest body)
  "Run BODY in a temp org buffer holding drill cards.
The buffer holds one card per needs-attention category plus a clean
card.  Standard thresholds are bound so the predicates have stable
inputs.  Dates are relative to today."
  `(let ((org-drill-leech-failure-threshold 3)
         (org-drill-statistics-leech-quality-threshold 2.5)
         (org-drill-lapse-threshold-days 30)
         (org-drill-statistics-attention-row-limit 10)
         (org-drill-question-tag "drill")
         (org-drill-scope 'file)
         (org-drill-match nil))
     (with-temp-buffer
       (org-mode)
       (insert
        "* Leech card :drill:\n"
        ":PROPERTIES:\n"
        ":DRILL_FAILURE_COUNT: 5\n"
        ":DRILL_AVERAGE_QUALITY: 1.2\n"
        ":DRILL_LAST_REVIEWED: " (test-org-drill-statistics--inactive-stamp 2) "\n"
        ":DRILL_TOTAL_REPEATS: 7\n"
        ":END:\n"
        "* Overdue card :drill:\n"
        ":PROPERTIES:\n"
        ":DRILL_LAST_REVIEWED: " (test-org-drill-statistics--inactive-stamp 60) "\n"
        ":DRILL_TOTAL_REPEATS: 3\n"
        ":END:\n"
        "* Forgotten new card :drill:\n"
        ":PROPERTIES:\n"
        ":DATE_ADDED: " (test-org-drill-statistics--inactive-stamp 20) "\n"
        ":END:\n"
        "* Healthy card :drill:\n"
        ":PROPERTIES:\n"
        ":DRILL_FAILURE_COUNT: 0\n"
        ":DRILL_AVERAGE_QUALITY: 4.8\n"
        ":DRILL_LAST_REVIEWED: " (test-org-drill-statistics--inactive-stamp 1) "\n"
        ":DATE_ADDED: " (test-org-drill-statistics--inactive-stamp 1) "\n"
        ":DRILL_TOTAL_REPEATS: 12\n"
        ":END:\n")
       ,@body)))

(ert-deftest test-org-drill-statistics-leech-candidates-selects-leech-only ()
  "Only the leech card is returned by the leech selector."
  (test-org-drill-statistics--with-cards
   (let ((result (org-drill-statistics--leech-candidates)))
     (should (equal '("Leech card") (mapcar #'car result)))
     (should (integerp (cdr (car result)))))))

(ert-deftest test-org-drill-statistics-long-overdue-selects-overdue-only ()
  "Only the overdue card is returned by the overdue selector."
  (test-org-drill-statistics--with-cards
   (should (equal '("Overdue card")
                  (mapcar #'car (org-drill-statistics--long-overdue))))))

(ert-deftest test-org-drill-statistics-forgotten-new-selects-forgotten-only ()
  "Only the forgotten-new card is returned by that selector."
  (test-org-drill-statistics--with-cards
   (should (equal '("Forgotten new card")
                  (mapcar #'car (org-drill-statistics--forgotten-new))))))

(ert-deftest test-org-drill-statistics-long-overdue-sorted-most-overdue-first ()
  "The overdue list is ordered by descending staleness."
  (let ((org-drill-lapse-threshold-days 10)
        (org-drill-question-tag "drill")
        (org-drill-scope 'file)
        (org-drill-statistics-attention-row-limit 10)
        (org-drill-match nil))
    (with-temp-buffer
      (org-mode)
      (insert
       "* Mild :drill:\n:PROPERTIES:\n:DRILL_LAST_REVIEWED: "
       (test-org-drill-statistics--inactive-stamp 15) "\n:END:\n"
       "* Severe :drill:\n:PROPERTIES:\n:DRILL_LAST_REVIEWED: "
       (test-org-drill-statistics--inactive-stamp 90) "\n:END:\n")
      (should (equal '("Severe" "Mild")
                     (mapcar #'car (org-drill-statistics--long-overdue)))))))

(ert-deftest test-org-drill-statistics-leech-candidates-empty-buffer-returns-nil ()
  "A buffer with no drill entries yields no leech candidates."
  (let ((org-drill-question-tag "drill")
        (org-drill-scope 'file)
        (org-drill-statistics-attention-row-limit 10)
        (org-drill-match nil))
    (with-temp-buffer
      (org-mode)
      (insert "* Just a heading\nNo drill tag here.\n")
      (should (null (org-drill-statistics--leech-candidates))))))

(ert-deftest test-org-drill-statistics-leech-candidates-respects-row-limit ()
  "More leeches than the limit are truncated to the limit count."
  (let ((org-drill-leech-failure-threshold 3)
        (org-drill-statistics-leech-quality-threshold 2.5)
        (org-drill-statistics-attention-row-limit 2)
        (org-drill-question-tag "drill")
        (org-drill-scope 'file)
        (org-drill-match nil))
    (with-temp-buffer
      (org-mode)
      (dotimes (i 4)
        (insert
         (format "* Leech %d :drill:\n:PROPERTIES:\n:DRILL_FAILURE_COUNT: 4\n:DRILL_AVERAGE_QUALITY: %s\n:END:\n"
                 i (+ 1.0 (* i 0.1)))))
      (should (= 2 (length (org-drill-statistics--leech-candidates)))))))

(provide 'test-org-drill-statistics-attention-data)

;;; test-org-drill-statistics-attention-data.el ends here
