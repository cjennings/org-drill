;;; test-org-drill-entry-status.el --- Tests for entry classification & creation-age & overdue ordering  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the dispatcher that decides which queue a card belongs in:
;;
;; - `org-drill-entry-status': returns (STATUS DUE AGE) where STATUS is
;;   one of nil / :unscheduled / :future / :new / :failed / :overdue /
;;   :young / :old.  Every drill iteration runs this on every candidate.
;; - `org-drill-entry-days-since-creation': how old is the card?  Used
;;   for the :unscheduled / :failed / :overdue / :young / :old call.
;; - `org-drill-order-overdue-entries': sort the overdue queue with
;;   recently-overdue first, lapsed-very-old behind.
;;
;; The user-facing contract: every card lands in exactly one bucket per
;; iteration, and bucket choice is deterministic given the card's state.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-org-buffer (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defun status-of (session)
  "Return the STATUS element of `(org-drill-entry-status session)' at point."
  (car (org-drill-entry-status session)))

(defun make-marker-at (pos)
  (let ((m (make-marker))) (set-marker m pos) m))

;;;; org-drill-entry-status

(ert-deftest test-org-drill-entry-status-non-drill-entry-returns-nil ()
  (with-org-buffer "* Plain heading\nbody\n"
    (with-fixed-now
      (should (null (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-empty-drill-entry-default-card-type-returns-nil ()
  "An entry tagged drill but with no body and no card-type that allows
empty bodies is silently skipped — status nil."
  (with-org-buffer "* Question :drill:\n:PROPERTIES:\n:ID: x\n:END:\n"
    (with-fixed-now
      (should (null (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-virgin-unscheduled-with-body-returns-new ()
  "A drill entry with body and no schedule is :new."
  (with-org-buffer "* Question :drill:\nThis is the body of the question.\n"
    (with-fixed-now
      (should (eq :new (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-future-scheduled-returns-future ()
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-05-10")
    (with-fixed-now
      (should (eq :future (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-past-with-failed-quality-returns-failed ()
  "An entry with last-quality below the failure threshold is :failed —
this status overrides young/old/overdue."
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-04-30")
    (org-set-property "DRILL_LAST_QUALITY" "1")  ; below default failure-quality
    (org-set-property "DRILL_LAST_INTERVAL" "5")
    (org-set-property "DRILL_TOTAL_REPEATS" "3")  ; not virgin
    (with-fixed-now
      (should (eq :failed (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-due-and-young-returns-young ()
  "Past-scheduled entry, good last-quality, short last-interval → :young."
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-05-04")  ; one day overdue
    (org-set-property "DRILL_LAST_QUALITY" "5")
    (org-set-property "DRILL_LAST_INTERVAL" "3")
    (org-set-property "DRILL_TOTAL_REPEATS" "2")
    (with-fixed-now
      ;; 1 day overdue out of 3 last-interval → not overdue (factor < 1.5)
      (should (eq :young (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-due-and-old-returns-old ()
  "Long last-interval (>= org-drill-days-before-old) makes a non-overdue
due card :old."
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-05-04")
    (org-set-property "DRILL_LAST_QUALITY" "5")
    (org-set-property "DRILL_LAST_INTERVAL" "30")
    (org-set-property "DRILL_TOTAL_REPEATS" "10")
    (let ((org-drill-days-before-old 10))   ; default is 10 in org-drill
      (with-fixed-now
        (should (eq :old (status-of (org-drill-session))))))))

(ert-deftest test-org-drill-entry-status-very-overdue-returns-overdue ()
  "Past-scheduled by much more than the last-interval factor → :overdue."
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-04-15")  ; 20 days overdue
    (org-set-property "DRILL_LAST_QUALITY" "5")
    (org-set-property "DRILL_LAST_INTERVAL" "5")
    (org-set-property "DRILL_TOTAL_REPEATS" "3")
    (with-fixed-now
      (should (eq :overdue (status-of (org-drill-session)))))))

(ert-deftest test-org-drill-entry-status-skipped-leech-returns-unscheduled ()
  "When `org-drill-leech-method' is `skip', a leech card returns :unscheduled
because its days-overdue computes nil."
  (with-org-buffer "* Hard one :drill:leech:\nbody\n"
    (org-schedule nil "2026-04-30")
    (let ((org-drill-leech-method 'skip))
      (with-fixed-now
        (should (eq :unscheduled (status-of (org-drill-session))))))))

(ert-deftest test-org-drill-entry-status-returns-three-element-list ()
  "Return shape is always (STATUS DUE AGE) — three elements."
  (with-org-buffer "* Question :drill:\nbody\n"
    (with-fixed-now
      (let ((result (org-drill-entry-status (org-drill-session))))
        (should (= 3 (length result)))))))

;;;; org-drill-entry-days-since-creation

(ert-deftest test-org-drill-entry-days-since-creation-with-date-added ()
  "DATE_ADDED set → returns days since that date."
  (with-org-buffer "* Question :drill:\n"
    (org-set-property "DATE_ADDED" "<2026-04-01 Wed>")
    (with-fixed-now
      (let ((days (org-drill-entry-days-since-creation (org-drill-session))))
        (should (numberp days))
        (should (>= days 33))    ; ~34 days from 4-01 to 5-05
        (should (<= days 35))))))

(ert-deftest test-org-drill-entry-days-since-creation-no-date-no-flag-returns-nil ()
  "DATE_ADDED missing, USE-LAST-INTERVAL-P nil → returns nil."
  (with-org-buffer "* Question :drill:\n"
    (with-fixed-now
      (should (null (org-drill-entry-days-since-creation (org-drill-session)))))))

(ert-deftest test-org-drill-entry-days-since-creation-no-date-with-flag-uses-interval ()
  "DATE_ADDED missing, USE-LAST-INTERVAL-P t → days-overdue + last-interval."
  (with-org-buffer "* Question :drill:\nbody\n"
    (org-schedule nil "2026-05-02")  ; 3 days overdue
    (org-set-property "DRILL_LAST_INTERVAL" "5")
    (with-fixed-now
      ;; Expected: 3 (overdue) + 5 (last-interval) = 8
      (should (= 8 (org-drill-entry-days-since-creation (org-drill-session) t))))))

;;;; org-drill-order-overdue-entries

(ert-deftest test-org-drill-order-overdue-entries-empty-stays-empty ()
  (let ((session (org-drill-session)))
    (oset session overdue-data nil)
    (org-drill-order-overdue-entries session)
    (should (null (oref session overdue-entries)))))

(ert-deftest test-org-drill-order-overdue-entries-non-lapsed-sort-by-due-descending ()
  "Recently-overdue cards: most-overdue-first.
Each overdue-data entry is (POS DUE AGE).  Sorting puts higher DUE first."
  (with-temp-buffer
    (insert "abc\ndef\nghi\n")
    (let* ((m1 (make-marker-at 1))
           (m2 (make-marker-at 5))
           (m3 (make-marker-at 9))
           (session (org-drill-session))
           (org-drill--lapse-very-overdue-entries-p nil))
      ;; 3 entries, due 5/2/8 days, all young (low age)
      (oset session overdue-data
            `((,m1 5 1) (,m2 2 1) (,m3 8 1)))
      (org-drill-order-overdue-entries session)
      ;; Expected order: m3 (due=8), m1 (due=5), m2 (due=2)
      (should (equal (list m3 m1 m2) (oref session overdue-entries))))))

(ert-deftest test-org-drill-order-overdue-entries-lapsed-go-after-non-lapsed ()
  "Lapsed (very-overdue) entries form a separate group sorted by AGE,
appearing after the non-lapsed ones.

Note: lapse split is by DUE (days overdue), not AGE — matches the
semantic of `org-drill--entry-lapsed-p' which gates on days-overdue
crossing the threshold.  Inside the lapsed group, secondary sort is
by AGE descending (oldest cards first)."
  (with-temp-buffer
    (insert "abc\ndef\nghi\njkl\n")
    (let* ((m-fresh (make-marker-at 1))
           (m-lapsed-old (make-marker-at 5))
           (m-lapsed-older (make-marker-at 9))
           (session (org-drill-session))
           (org-drill--lapse-very-overdue-entries-p t)
           (org-drill-lapse-threshold-days 90))
      (oset session overdue-data
            `((,m-fresh 5 30)               ; non-lapsed (DUE 5 < 90)
              (,m-lapsed-old 100 50)        ; lapsed (DUE 100 > 90)
              (,m-lapsed-older 100 200)))   ; lapsed too, but older
      (org-drill-order-overdue-entries session)
      ;; Non-lapsed first, then lapsed sorted by AGE descending.
      (should (equal (list m-fresh m-lapsed-older m-lapsed-old)
                     (oref session overdue-entries))))))

(provide 'test-org-drill-entry-status)

;;; test-org-drill-entry-status.el ends here
