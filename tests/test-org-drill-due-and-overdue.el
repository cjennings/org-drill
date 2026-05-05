;;; test-org-drill-due-and-overdue.el --- Tests for due/overdue predicates  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the predicates that decide whether a card should appear in
;; today's drill session:
;;
;; - `org-drill-days-since-last-review' / `org-drill-hours-since-last-review':
;;   read DRILL_LAST_REVIEWED and convert to elapsed days/hours.
;; - `org-drill-entry-days-overdue' (session-aware): how far past or
;;   future the SCHEDULED date is.
;; - `org-drill-entry-due-p' (session-aware): is this card due today?
;; - `org-drill-entry-overdue-p' (session-aware): is this card overdue
;;   *enough* to be flagged for the special overdue queue?
;; - `org-drill-current-scope': translate user-facing scope keywords
;;   (file, directory, etc.) to org-map-entries scope values.
;;
;; Time-based tests pin "now" to a fixed instant via `cl-letf' on
;; `current-time' so the computation is deterministic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fresh-drill-entry (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-non-drill-entry (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Plain heading\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defun test-tts-fixed-now ()
  "A fixed reference instant: 2026-05-05 12:00:00 local time."
  (encode-time 0 0 12 5 5 2026))

(defmacro with-fixed-now (&rest body)
  "Run BODY with `current-time' returning the test's fixed instant."
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (test-tts-fixed-now))))
     ,@body))

(defun set-drill-last-reviewed (timestamp-string)
  "Set DRILL_LAST_REVIEWED on the entry at point."
  (org-set-property "DRILL_LAST_REVIEWED" timestamp-string))

(defun set-scheduled (timestamp-string)
  "Set SCHEDULED on the entry at point."
  (org-schedule nil timestamp-string))

;;;; org-drill-days-since-last-review

(ert-deftest test-org-drill-days-since-last-review-reviewed-today-returns-zero ()
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-05 Tue 09:00]")
    (with-fixed-now
      (should (equal 0 (org-drill-days-since-last-review))))))

(ert-deftest test-org-drill-days-since-last-review-reviewed-yesterday-returns-one ()
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-04 Mon 09:00]")
    (with-fixed-now
      (should (equal 1 (org-drill-days-since-last-review))))))

(ert-deftest test-org-drill-days-since-last-review-reviewed-week-ago-returns-seven ()
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-04-28 Tue 12:00]")
    (with-fixed-now
      (should (equal 7 (org-drill-days-since-last-review))))))

(ert-deftest test-org-drill-days-since-last-review-no-property-returns-nil ()
  "A card without DRILL_LAST_REVIEWED returns nil — never reviewed."
  (with-fresh-drill-entry
    (should (null (org-drill-days-since-last-review)))))

(ert-deftest test-org-drill-days-since-last-review-future-date-returns-negative ()
  "A future timestamp returns negative — should never happen, but it's
defined."
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-10 Sun 12:00]")
    (with-fixed-now
      (should (equal -5 (org-drill-days-since-last-review))))))

;;;; org-drill-hours-since-last-review

(ert-deftest test-org-drill-hours-since-last-review-reviewed-three-hours-ago ()
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-05 Tue 09:00]")
    (with-fixed-now
      (should (equal 3 (org-drill-hours-since-last-review))))))

(ert-deftest test-org-drill-hours-since-last-review-no-property-returns-nil ()
  (with-fresh-drill-entry
    (should (null (org-drill-hours-since-last-review)))))

(ert-deftest test-org-drill-hours-since-last-review-floors-fractional-hours ()
  "30 minutes elapsed reads as 0 hours — value is floored."
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-05 Tue 11:30]")
    (with-fixed-now
      (should (equal 0 (org-drill-hours-since-last-review))))))

;;;; org-drill-entry-days-overdue (normal-mode session)

(ert-deftest test-org-drill-entry-days-overdue-non-drill-entry-returns-nil ()
  "Entries without :drill: tag are not in a drill session."
  (with-non-drill-entry
    (with-fixed-now
      (should (null (org-drill-entry-days-overdue (org-drill-session)))))))

(ert-deftest test-org-drill-entry-days-overdue-unscheduled-drill-entry-returns-zero ()
  "A drill entry with no SCHEDULED is treated as due now (zero days overdue)."
  (with-fresh-drill-entry
    (with-fixed-now
      (should (equal 0 (org-drill-entry-days-overdue (org-drill-session)))))))

(ert-deftest test-org-drill-entry-days-overdue-scheduled-future-returns-negative ()
  "Scheduled tomorrow → -1 (one day in the future)."
  (with-fresh-drill-entry
    (set-scheduled "2026-05-06")
    (with-fixed-now
      (should (equal -1 (org-drill-entry-days-overdue (org-drill-session)))))))

(ert-deftest test-org-drill-entry-days-overdue-scheduled-past-returns-positive ()
  "Scheduled three days ago → 3 days overdue."
  (with-fresh-drill-entry
    (set-scheduled "2026-05-02")
    (with-fixed-now
      (should (equal 3 (org-drill-entry-days-overdue (org-drill-session)))))))

(ert-deftest test-org-drill-entry-days-overdue-leech-skipped-when-method-skip ()
  "When `org-drill-leech-method' is `skip', leech entries are dropped (return nil)."
  (with-temp-buffer
    (let ((org-startup-folded nil)
          (org-drill-leech-method 'skip))
      (insert "* Hard one :drill:leech:\n")
      (org-mode)
      (goto-char (point-min))
      (set-scheduled "2026-05-02")
      (with-fixed-now
        (should (null (org-drill-entry-days-overdue (org-drill-session))))))))

;;;; org-drill-entry-days-overdue (cram-mode session)

(ert-deftest test-org-drill-entry-days-overdue-cram-recently-reviewed-returns-nil ()
  "Cram mode: a card reviewed within `org-drill-cram-hours' is skipped."
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-05 Tue 11:00]")  ; one hour ago
    (let ((session (org-drill-session))
          (org-drill-cram-hours 12))                     ; 12-hour cram window
      (oset session cram-mode t)
      (with-fixed-now
        (should (null (org-drill-entry-days-overdue session)))))))

(ert-deftest test-org-drill-entry-days-overdue-cram-old-review-returns-zero ()
  "Cram mode: a card last reviewed beyond `org-drill-cram-hours' is due (returns 0)."
  (with-fresh-drill-entry
    (set-drill-last-reviewed "[2026-05-04 Mon 09:00]")  ; ~27 hours ago
    (let ((session (org-drill-session))
          (org-drill-cram-hours 12))
      (oset session cram-mode t)
      (with-fixed-now
        (should (equal 0 (org-drill-entry-days-overdue session)))))))

(ert-deftest test-org-drill-entry-days-overdue-cram-never-reviewed-returns-zero ()
  "Cram mode on a never-reviewed card: due now."
  (with-fresh-drill-entry
    (let ((session (org-drill-session)))
      (oset session cram-mode t)
      (with-fixed-now
        (should (equal 0 (org-drill-entry-days-overdue session)))))))

;;;; org-drill-entry-due-p

(ert-deftest test-org-drill-entry-due-p-unscheduled-returns-t ()
  (with-fresh-drill-entry
    (with-fixed-now
      (should (org-drill-entry-due-p (org-drill-session))))))

(ert-deftest test-org-drill-entry-due-p-past-scheduled-returns-t ()
  (with-fresh-drill-entry
    (set-scheduled "2026-05-02")
    (with-fixed-now
      (should (org-drill-entry-due-p (org-drill-session))))))

(ert-deftest test-org-drill-entry-due-p-future-scheduled-returns-nil ()
  "Scheduled in the future → not due (negative days-overdue)."
  (with-fresh-drill-entry
    (set-scheduled "2026-05-10")
    (with-fixed-now
      (should-not (org-drill-entry-due-p (org-drill-session))))))

(ert-deftest test-org-drill-entry-due-p-non-drill-returns-nil ()
  (with-non-drill-entry
    (with-fixed-now
      (should-not (org-drill-entry-due-p (org-drill-session))))))

;;;; org-drill-entry-overdue-p (session, days-overdue, last-interval)

(ert-deftest test-org-drill-entry-overdue-p-not-very-overdue-returns-nil ()
  "Just one day past schedule on a 7-day interval → not flagged as overdue.
overdue-interval-factor defaults to 1.2, so for last-int=7 we need
days-overdue/(7+...) > 0.2 — i.e. days-overdue >= 2 ish."
  (with-fresh-drill-entry
    (with-fixed-now
      (should-not (org-drill-entry-overdue-p (org-drill-session) 1 7)))))

(ert-deftest test-org-drill-entry-overdue-p-very-overdue-returns-t ()
  "20 days past schedule on a 7-day interval → flagged as overdue."
  (with-fresh-drill-entry
    (with-fixed-now
      (should (org-drill-entry-overdue-p (org-drill-session) 20 7)))))

(ert-deftest test-org-drill-entry-overdue-p-zero-last-interval-returns-nil ()
  "Last-interval = 0 (virgin item) is never \"overdue\" — it's just due."
  (with-fresh-drill-entry
    (with-fixed-now
      (should-not (org-drill-entry-overdue-p (org-drill-session) 5 0)))))

;;;; org-drill-current-scope

(ert-deftest test-org-drill-current-scope-nil-defaults-to-customizable-scope ()
  "When called with nil, falls back to the `org-drill-scope' defcustom value."
  (let ((org-drill-scope 'tree))
    (should (eq 'tree (org-drill-current-scope nil)))))

(ert-deftest test-org-drill-current-scope-symbol-file-becomes-nil ()
  "The user-facing `file' scope translates to nil for org-map-entries."
  (should (null (org-drill-current-scope 'file))))

(ert-deftest test-org-drill-current-scope-symbol-file-no-restriction-becomes-file ()
  "`file-no-restriction' translates to the `file' symbol for org-map-entries."
  (should (eq 'file (org-drill-current-scope 'file-no-restriction))))

(ert-deftest test-org-drill-current-scope-passthrough-for-other-symbols ()
  "Unknown scope symbols are passed through unchanged."
  (should (eq 'tree (org-drill-current-scope 'tree)))
  (should (eq 'agenda (org-drill-current-scope 'agenda)))
  (should (equal '("a.org" "b.org") (org-drill-current-scope '("a.org" "b.org")))))

(provide 'test-org-drill-due-and-overdue)

;;; test-org-drill-due-and-overdue.el ends here
