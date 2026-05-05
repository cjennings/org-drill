;;; test-org-drill-entry-properties.el --- Tests for entry property accessors  -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the small org-property reader functions that surface a
;; drill entry's persisted state:
;;
;; - `org-drill-entry-last-quality'
;; - `org-drill-entry-failure-count'
;; - `org-drill-entry-average-quality'
;; - `org-drill-entry-last-interval'
;; - `org-drill-entry-repeats-since-fail'
;; - `org-drill-entry-total-repeats'
;; - `org-drill-entry-ease'
;; - `org-drill-entry-leech-p'
;; - `org-drill-entry-new-p'
;;
;; All but `entry-leech-p' read a single DRILL_* org-mode property and parse
;; it as a number, with a configurable default when the property is absent.
;; `entry-leech-p' checks for the "leech" tag.  `entry-new-p' returns t when
;; the entry has no SCHEDULED time stamp.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-drill-entry-fixture (properties &rest body)
  "Run BODY at point on a drill-tagged entry whose PROPERTIES are set.
PROPERTIES is a list of (NAME . VALUE) cons cells.  Each value is set
via `org-set-property'."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       (dolist (prop ,properties)
         (org-set-property (car prop) (cdr prop)))
       (goto-char (point-min))
       ,@body)))

(defmacro with-non-drill-entry-fixture (&rest body)
  "Run BODY at point on a heading without the :drill: tag."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Plain heading\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

;;;; org-drill-entry-last-quality

(ert-deftest test-org-drill-entry-last-quality-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_LAST_QUALITY" . "4"))
    (should (equal 4 (org-drill-entry-last-quality)))))

(ert-deftest test-org-drill-entry-last-quality-normal-property-zero ()
  (with-drill-entry-fixture '(("DRILL_LAST_QUALITY" . "0"))
    (should (equal 0 (org-drill-entry-last-quality)))))

(ert-deftest test-org-drill-entry-last-quality-boundary-no-property-no-default ()
  (with-drill-entry-fixture nil
    (should (null (org-drill-entry-last-quality)))))

(ert-deftest test-org-drill-entry-last-quality-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 'no-quality (org-drill-entry-last-quality 'no-quality)))))

(ert-deftest test-org-drill-entry-last-quality-normal-property-overrides-default ()
  "When the property is set the default is ignored."
  (with-drill-entry-fixture '(("DRILL_LAST_QUALITY" . "3"))
    (should (equal 3 (org-drill-entry-last-quality 'fallback)))))

;;;; org-drill-entry-failure-count

(ert-deftest test-org-drill-entry-failure-count-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_FAILURE_COUNT" . "7"))
    (should (equal 7 (org-drill-entry-failure-count)))))

(ert-deftest test-org-drill-entry-failure-count-boundary-no-property-returns-zero ()
  "Failure-count's missing-property fallback is hardcoded to 0, not nil."
  (with-drill-entry-fixture nil
    (should (equal 0 (org-drill-entry-failure-count)))))

(ert-deftest test-org-drill-entry-failure-count-boundary-property-zero ()
  (with-drill-entry-fixture '(("DRILL_FAILURE_COUNT" . "0"))
    (should (equal 0 (org-drill-entry-failure-count)))))

;;;; org-drill-entry-average-quality

(ert-deftest test-org-drill-entry-average-quality-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_AVERAGE_QUALITY" . "3.75"))
    (should (equal 3.75 (org-drill-entry-average-quality)))))

(ert-deftest test-org-drill-entry-average-quality-boundary-no-property-no-default ()
  (with-drill-entry-fixture nil
    (should (null (org-drill-entry-average-quality)))))

(ert-deftest test-org-drill-entry-average-quality-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 2.5 (org-drill-entry-average-quality 2.5)))))

;;;; org-drill-entry-last-interval

(ert-deftest test-org-drill-entry-last-interval-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_LAST_INTERVAL" . "14"))
    (should (equal 14 (org-drill-entry-last-interval)))))

(ert-deftest test-org-drill-entry-last-interval-normal-fractional-interval ()
  (with-drill-entry-fixture '(("DRILL_LAST_INTERVAL" . "0.5"))
    (should (equal 0.5 (org-drill-entry-last-interval)))))

(ert-deftest test-org-drill-entry-last-interval-boundary-no-property-default-zero ()
  "Last-interval's missing-property fallback is 0, not nil."
  (with-drill-entry-fixture nil
    (should (equal 0 (org-drill-entry-last-interval)))))

(ert-deftest test-org-drill-entry-last-interval-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 99 (org-drill-entry-last-interval 99)))))

;;;; org-drill-entry-repeats-since-fail

(ert-deftest test-org-drill-entry-repeats-since-fail-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_REPEATS_SINCE_FAIL" . "5"))
    (should (equal 5 (org-drill-entry-repeats-since-fail)))))

(ert-deftest test-org-drill-entry-repeats-since-fail-boundary-no-property-default-zero ()
  (with-drill-entry-fixture nil
    (should (equal 0 (org-drill-entry-repeats-since-fail)))))

(ert-deftest test-org-drill-entry-repeats-since-fail-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 12 (org-drill-entry-repeats-since-fail 12)))))

;;;; org-drill-entry-total-repeats

(ert-deftest test-org-drill-entry-total-repeats-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_TOTAL_REPEATS" . "23"))
    (should (equal 23 (org-drill-entry-total-repeats)))))

(ert-deftest test-org-drill-entry-total-repeats-boundary-no-property-default-zero ()
  (with-drill-entry-fixture nil
    (should (equal 0 (org-drill-entry-total-repeats)))))

(ert-deftest test-org-drill-entry-total-repeats-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 100 (org-drill-entry-total-repeats 100)))))

;;;; org-drill-entry-ease

(ert-deftest test-org-drill-entry-ease-normal-property-set ()
  (with-drill-entry-fixture '(("DRILL_EASE" . "2.5"))
    (should (equal 2.5 (org-drill-entry-ease)))))

(ert-deftest test-org-drill-entry-ease-boundary-no-property-no-default ()
  (with-drill-entry-fixture nil
    (should (null (org-drill-entry-ease)))))

(ert-deftest test-org-drill-entry-ease-boundary-no-property-with-default ()
  (with-drill-entry-fixture nil
    (should (equal 1.3 (org-drill-entry-ease 1.3)))))

;;;; org-drill-entry-leech-p

(ert-deftest test-org-drill-entry-leech-p-normal-leech-tag-on-drill-entry-returns-t ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Hard one :drill:leech:\n")
      (org-mode)
      (goto-char (point-min))
      (should (org-drill-entry-leech-p)))))

(ert-deftest test-org-drill-entry-leech-p-normal-no-leech-tag-returns-nil ()
  (with-drill-entry-fixture nil
    (should-not (org-drill-entry-leech-p))))

(ert-deftest test-org-drill-entry-leech-p-error-leech-tag-without-drill-tag ()
  "An entry tagged leech but without the drill tag is not a leech (must be a drill entry first)."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Just leech :leech:\n")
      (org-mode)
      (goto-char (point-min))
      (should-not (org-drill-entry-leech-p)))))

;;;; org-drill-entry-new-p

(ert-deftest test-org-drill-entry-new-p-normal-no-scheduled-stamp-returns-t ()
  "A drill entry with no SCHEDULED timestamp is new."
  (with-drill-entry-fixture nil
    (should (org-drill-entry-new-p))))

(ert-deftest test-org-drill-entry-new-p-normal-with-scheduled-stamp-returns-nil ()
  "A drill entry with a SCHEDULED timestamp is not new."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nSCHEDULED: <2026-05-05 Tue>\n")
      (org-mode)
      (goto-char (point-min))
      (should-not (org-drill-entry-new-p)))))

(ert-deftest test-org-drill-entry-new-p-error-non-drill-entry-returns-nil ()
  "A non-drill entry is not \"new\" — there's no drill state to consider."
  (with-non-drill-entry-fixture
    (should-not (org-drill-entry-new-p))))

(provide 'test-org-drill-entry-properties)

;;; test-org-drill-entry-properties.el ends here
