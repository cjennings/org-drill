;;; test-org-drill-item-data-roundtrip.el --- Tests for item-data save/load  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the core persistence pair `org-drill-get-item-data' and
;; `org-drill-store-item-data'.  These two functions are the boundary
;; between in-memory drill state and the on-disk org buffer — every rating
;; passes through them, so the user-facing contract is:
;;
;;   "When I rate a card, my progress is saved.  When I open the file
;;    tomorrow, my progress is still there."
;;
;; The contract has three branches:
;;
;; 1. *Modern format*: store writes six DRILL_* properties; get reads
;;    them back.  The default path.
;;
;; 2. *Legacy LEARN_DATA*: older org-drill files stored interval,
;;    repeats, and ease as a single LEARN_DATA s-expression.  get
;;    transparently reads either format.  Backward compat — a 2015-era
;;    deck should still drill.
;;
;; 3. *Virgin item*: a card that's never been rated returns
;;    `(0 0 0 0 nil nil)' so the scheduler knows to treat it as new.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fresh-drill-entry (&rest body)
  "Run BODY at point on a fresh drill entry with no DRILL_* or LEARN_DATA properties."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-modern-drill-entry (props &rest body)
  "Run BODY at point on a drill entry with DRILL_* PROPS set.
PROPS is a list of (NAME . STRING-VALUE) cons cells."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       (dolist (p ,props)
         (org-set-property (car p) (cdr p)))
       (goto-char (point-min))
       ,@body)))

;;;; Virgin items

(ert-deftest test-org-drill-get-item-data-virgin-returns-zero-list ()
  "A drill entry with no persisted state returns the virgin sentinel.
Six elements: zero interval, zero repeats, zero failures, zero total,
nil meanq, nil ease.  This is the value the scheduler reads on a
never-rated card."
  (with-fresh-drill-entry
    (should (equal '(0 0 0 0 nil nil) (org-drill-get-item-data)))))

;;;; Modern format — read-side

(ert-deftest test-org-drill-get-item-data-modern-reads-all-six-fields ()
  "When all DRILL_* properties are set, get-item-data reads all six."
  (with-modern-drill-entry '(("DRILL_LAST_INTERVAL" . "10")
                             ("DRILL_REPEATS_SINCE_FAIL" . "3")
                             ("DRILL_FAILURE_COUNT" . "1")
                             ("DRILL_TOTAL_REPEATS" . "5")
                             ("DRILL_AVERAGE_QUALITY" . "3.8")
                             ("DRILL_EASE" . "2.4"))
    (should (equal '(10 3 1 5 3.8 2.4) (org-drill-get-item-data)))))

(ert-deftest test-org-drill-get-item-data-modern-partial-properties-fall-back-to-defaults ()
  "Missing DRILL_* properties take their per-field defaults — but the
presence of any DRILL_* property still puts the function in modern mode."
  (with-modern-drill-entry '(("DRILL_TOTAL_REPEATS" . "3"))
    (let ((result (org-drill-get-item-data)))
      ;; total-repeats → 3 (set), others take their defaults
      (should (equal 0 (nth 0 result)))   ; last-interval default 0
      (should (equal 0 (nth 1 result)))   ; repeats default 0
      (should (equal 0 (nth 2 result)))   ; failures default 0
      (should (equal 3 (nth 3 result)))   ; total-repeats from property
      (should (null (nth 4 result)))      ; meanq default nil
      (should (null (nth 5 result))))))   ; ease default nil

;;;; Modern format — write-side

(ert-deftest test-org-drill-store-item-data-writes-all-six-properties ()
  "store-item-data sets all six DRILL_* properties on the entry at point."
  (with-fresh-drill-entry
    (org-drill-store-item-data 10 3 1 5 3.8 2.4)
    (should (equal "10.0" (org-entry-get (point) "DRILL_LAST_INTERVAL")))
    (should (equal "3" (org-entry-get (point) "DRILL_REPEATS_SINCE_FAIL")))
    (should (equal "1" (org-entry-get (point) "DRILL_FAILURE_COUNT")))
    (should (equal "5" (org-entry-get (point) "DRILL_TOTAL_REPEATS")))
    (should (equal "3.8" (org-entry-get (point) "DRILL_AVERAGE_QUALITY")))
    (should (equal "2.4" (org-entry-get (point) "DRILL_EASE")))))

(ert-deftest test-org-drill-store-item-data-rounds-floats ()
  "Floating-point fields are rounded — interval to 4dp, meanq/ease to 3dp.
Keeps the buffer tidy and avoids stray precision noise like 2.4999999998."
  (with-fresh-drill-entry
    (org-drill-store-item-data 10.123456789 3 1 5 3.8765432 2.4567899)
    (should (equal "10.1235" (org-entry-get (point) "DRILL_LAST_INTERVAL")))
    (should (equal "3.877" (org-entry-get (point) "DRILL_AVERAGE_QUALITY")))
    (should (equal "2.457" (org-entry-get (point) "DRILL_EASE")))))

(ert-deftest test-org-drill-store-item-data-deletes-legacy-LEARN_DATA ()
  "Storing modern format wipes any legacy LEARN_DATA on the entry.
Otherwise get-item-data would still read the stale legacy value first."
  (with-modern-drill-entry '(("LEARN_DATA" . "(2.5 1 0.5)"))
    (org-drill-store-item-data 10 3 1 5 3.8 2.4)
    (should (null (org-entry-get (point) "LEARN_DATA")))))

;;;; Round-trip — the core user-facing assertion

(ert-deftest test-org-drill-item-data-roundtrip-preserves-values ()
  "Storing then reading returns equivalent values — the save/load cycle is lossless.
This is the assertion users actually care about: rate a card today, see
the same state tomorrow.

Note on types: rounded fields (LAST-INTERVAL, MEANQ, EASE) come back as
floats because `org-drill-round-float' returns float; counters (REPEATS,
FAILURES, TOTAL-REPEATS) come back as ints.  Numerically the round-trip
is lossless; the scheduler accepts both."
  (with-fresh-drill-entry
    (org-drill-store-item-data 10 3 1 5 3.8 2.4)
    (should (equal '(10.0 3 1 5 3.8 2.4) (org-drill-get-item-data)))))

(ert-deftest test-org-drill-item-data-roundtrip-preserves-zero-values ()
  "A first-rating round-trip with mostly zeros survives intact.
Same type-mixing pattern as the non-zero round-trip — see that test's
note for why LAST-INTERVAL is 0.0 and the counters are integer 0."
  (with-fresh-drill-entry
    (org-drill-store-item-data 0 0 0 1 5.0 2.5)
    (should (equal '(0.0 0 0 1 5.0 2.5) (org-drill-get-item-data)))))

;;;; Legacy LEARN_DATA — backward compat

(ert-deftest test-org-drill-get-item-data-legacy-LEARN_DATA-takes-precedence ()
  "When LEARN_DATA exists, it wins over the modern fields.
Layout: (LAST-INTERVAL REPEATS EASE).  failures and last-quality come
from their separate DRILL_* properties for the legacy path."
  (with-modern-drill-entry '(("LEARN_DATA" . "(7 2 2.6)")
                             ("DRILL_FAILURE_COUNT" . "0")
                             ("DRILL_LAST_QUALITY" . "4"))
    ;; Returned: (interval, repeats, failures, repeats-again, last-quality, ease)
    (should (equal '(7 2 0 2 4 2.6) (org-drill-get-item-data)))))

(ert-deftest test-org-drill-get-item-data-invalid-LEARN_DATA-falls-through-to-modern ()
  "If LEARN_DATA is malformed, fall through to the modern DRILL_* fields.
Defends against a corrupted legacy entry from breaking a session."
  (with-modern-drill-entry '(("LEARN_DATA" . "((((not-a-list")
                             ("DRILL_LAST_INTERVAL" . "12")
                             ("DRILL_REPEATS_SINCE_FAIL" . "4")
                             ("DRILL_FAILURE_COUNT" . "2")
                             ("DRILL_TOTAL_REPEATS" . "6")
                             ("DRILL_AVERAGE_QUALITY" . "3.5")
                             ("DRILL_EASE" . "2.1"))
    (should (equal '(12 4 2 6 3.5 2.1) (org-drill-get-item-data)))))

(ert-deftest test-org-drill-get-item-data-invalid-LEARN_DATA-and-no-modern-returns-virgin ()
  "Malformed LEARN_DATA on an entry with no DRILL_* fallback returns virgin sentinel.
This matters: a corrupted-only legacy entry shouldn't crash the session,
just be treated as never rated."
  (with-modern-drill-entry '(("LEARN_DATA" . "garbage"))
    (should (equal '(0 0 0 0 nil nil) (org-drill-get-item-data)))))

(provide 'test-org-drill-item-data-roundtrip)

;;; test-org-drill-item-data-roundtrip.el ends here
