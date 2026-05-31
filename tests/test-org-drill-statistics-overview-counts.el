;;; test-org-drill-statistics-overview-counts.el --- Tests for overview-counts statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard overview-counts block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; test-org-drill-statistics-overview-counts.el --- overview-counts tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-statistics--overview-counts': walk the drill
;; entries in a scope and bucket them into a (:total :new :mature
;; :lapsed) plist via `org-drill-entry-status'.

;;; Code:


(defmacro with-fixed-now (&rest body)
  "Run BODY with `current-time' pinned to 2026-05-05 12:00."
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defmacro with-overview-buffer (content &rest body)
  "Insert CONTENT into a temp org buffer, then run BODY at point-min."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

;;;; Normal cases

(ert-deftest test-org-drill-statistics-overview-counts-mixed-population ()
  "A buffer with one new, one mature (young), and one lapsed card buckets
each correctly and totals three."
  (with-overview-buffer
      (concat
       "* New card :drill:\nbody of a brand new card\n"
       "* Young card :drill:\n"
       "SCHEDULED: <2026-05-04 Mon>\n"
       ":PROPERTIES:\n"
       ":DRILL_LAST_QUALITY: 5\n:DRILL_LAST_INTERVAL: 3\n"
       ":DRILL_TOTAL_REPEATS: 2\n:END:\nbody\n"
       "* Lapsed card :drill:\n"
       "SCHEDULED: <2026-04-30 Thu>\n"
       ":PROPERTIES:\n"
       ":DRILL_LAST_QUALITY: 1\n:DRILL_LAST_INTERVAL: 5\n"
       ":DRILL_TOTAL_REPEATS: 3\n:END:\nbody\n")
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 3 (plist-get counts :total)))
        (should (= 1 (plist-get counts :new)))
        (should (= 1 (plist-get counts :mature)))
        (should (= 1 (plist-get counts :lapsed)))))))

(ert-deftest test-org-drill-statistics-overview-counts-overdue-counts-as-mature ()
  "An :overdue card lands in the mature bucket, not its own."
  (with-overview-buffer
      (concat
       "* Very overdue :drill:\n"
       "SCHEDULED: <2026-04-15 Wed>\n"
       ":PROPERTIES:\n"
       ":DRILL_LAST_QUALITY: 5\n:DRILL_LAST_INTERVAL: 5\n"
       ":DRILL_TOTAL_REPEATS: 3\n:END:\nbody\n")
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 1 (plist-get counts :total)))
        (should (= 0 (plist-get counts :new)))
        (should (= 1 (plist-get counts :mature)))
        (should (= 0 (plist-get counts :lapsed)))))))

(ert-deftest test-org-drill-statistics-overview-counts-future-counts-in-total-only ()
  "A future-scheduled card counts toward :total but not new/mature/lapsed."
  (with-overview-buffer
      (concat
       "* Future card :drill:\n"
       "SCHEDULED: <2026-05-10 Sun>\n"
       "body\n")
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 1 (plist-get counts :total)))
        (should (= 0 (plist-get counts :new)))
        (should (= 0 (plist-get counts :mature)))
        (should (= 0 (plist-get counts :lapsed)))))))

;;;; Boundary cases

(ert-deftest test-org-drill-statistics-overview-counts-empty-buffer-all-zero ()
  "No headings at all yields zero across every bucket."
  (with-overview-buffer "Just some prose, no headings.\n"
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 0 (plist-get counts :total)))
        (should (= 0 (plist-get counts :new)))
        (should (= 0 (plist-get counts :mature)))
        (should (= 0 (plist-get counts :lapsed)))))))

(ert-deftest test-org-drill-statistics-overview-counts-non-drill-headings-ignored ()
  "Plain headings without the drill tag never reach :total."
  (with-overview-buffer
      (concat
       "* Plain heading one\nbody\n"
       "* The only drill :drill:\nbody of a new card\n"
       "* Plain heading two\nbody\n")
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 1 (plist-get counts :total)))
        (should (= 1 (plist-get counts :new)))))))

(ert-deftest test-org-drill-statistics-overview-counts-single-new-card ()
  "A single new card: total and new are one, the rest zero."
  (with-overview-buffer "* Lonely :drill:\nbody of the only card\n"
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 1 (plist-get counts :total)))
        (should (= 1 (plist-get counts :new)))
        (should (= 0 (plist-get counts :mature)))
        (should (= 0 (plist-get counts :lapsed)))))))

(ert-deftest test-org-drill-statistics-overview-counts-empty-drill-card-not-counted ()
  "A drill-tagged heading with an empty body and a default card type has
status nil and is excluded from :total."
  (with-overview-buffer
      "* Empty drill :drill:\n:PROPERTIES:\n:ID: x\n:END:\n"
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (= 0 (plist-get counts :total)))))))

;;;; Error / robustness cases

(ert-deftest test-org-drill-statistics-overview-counts-returns-plist-shape ()
  "The return value always carries the four documented keys, even when
the buffer holds no cards."
  (with-overview-buffer "no headings here\n"
    (with-fixed-now
      (let ((counts (org-drill-statistics--overview-counts 'file)))
        (should (plist-member counts :total))
        (should (plist-member counts :new))
        (should (plist-member counts :mature))
        (should (plist-member counts :lapsed))
        (should (cl-every #'integerp
                          (list (plist-get counts :total)
                                (plist-get counts :new)
                                (plist-get counts :mature)
                                (plist-get counts :lapsed))))))))

(ert-deftest test-org-drill-statistics-overview-counts-totals-are-additive ()
  "New + mature + lapsed never exceeds :total, and the dormant remainder
(:total minus the three actionable buckets) is non-negative."
  (with-overview-buffer
      (concat
       "* New :drill:\nbody one\n"
       "* Young :drill:\n"
       "SCHEDULED: <2026-05-04 Mon>\n"
       ":PROPERTIES:\n"
       ":DRILL_LAST_QUALITY: 5\n:DRILL_LAST_INTERVAL: 3\n"
       ":DRILL_TOTAL_REPEATS: 2\n:END:\nbody\n"
       "* Future :drill:\n"
       "SCHEDULED: <2026-05-10 Sun>\n"
       "body\n")
    (with-fixed-now
      (let* ((counts (org-drill-statistics--overview-counts 'file))
             (actionable (+ (plist-get counts :new)
                            (plist-get counts :mature)
                            (plist-get counts :lapsed))))
        (should (>= (plist-get counts :total) actionable))
        ;; one new + one young + one future dormant = total 3, actionable 2
        (should (= 3 (plist-get counts :total)))
        (should (= 2 actionable))))))

(provide 'test-org-drill-statistics-overview-counts)

;;; test-org-drill-statistics-overview-counts.el ends here
