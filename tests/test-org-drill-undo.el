;;; test-org-drill-undo.el --- Tests for undo/retry of the last rating  -*- lexical-binding: t; -*-

;;; Commentary:
;; Undo lets the user take back a misclicked rating.  Before a rating is
;; applied, the entry's scheduling state is snapshotted onto the session's
;; undo-stack (capped at org-drill-undo-limit).  org-drill-undo-last-rating
;; restores that state, drops the recorded quality, and re-queues the card so
;; it comes around again.

;;; Code:

(require 'ert)
(require 'org-drill)

;;;; defcustoms

(ert-deftest test-org-drill-undo-limit-defaults-to-3 ()
  (should (eq 3 (default-value 'org-drill-undo-limit))))

(ert-deftest test-org-drill-undo-key-is-customizable-and-defaults-to-u ()
  (should (custom-variable-p 'org-drill--undo-key))
  (should (eq ?u (default-value 'org-drill--undo-key))))

;;;; snapshot / restore round-trip

(ert-deftest test-org-drill-snapshot-restore-round-trips-scheduling-data ()
  "Restoring a snapshot puts changed properties back to their captured values."
  (with-temp-buffer
    (insert "* Card :drill:\n")
    (org-mode)
    (goto-char (point-min))
    (org-set-property "DRILL_LAST_QUALITY" "4")
    (org-set-property "DRILL_EASE" "2.5")
    (let ((snap (org-drill--snapshot-entry-data)))
      (org-set-property "DRILL_LAST_QUALITY" "1")
      (org-set-property "DRILL_EASE" "1.8")
      (org-drill--restore-entry-data snap)
      (should (equal "4" (org-entry-get (point) "DRILL_LAST_QUALITY")))
      (should (equal "2.5" (org-entry-get (point) "DRILL_EASE"))))))

(ert-deftest test-org-drill-restore-deletes-properties-absent-at-snapshot ()
  "A property that did not exist when snapshotted is removed on restore."
  (with-temp-buffer
    (insert "* Card :drill:\n")
    (org-mode)
    (goto-char (point-min))
    (let ((snap (org-drill--snapshot-entry-data)))
      (org-set-property "DRILL_LAST_QUALITY" "3")
      (org-drill--restore-entry-data snap)
      (should (null (org-entry-get (point) "DRILL_LAST_QUALITY"))))))

;;;; org-drill-undo-last-rating

(ert-deftest test-org-drill-undo-last-rating-restores-pops-and-requeues ()
  "Undo restores the entry data, drops the recorded quality, and re-queues
the card onto the again list."
  (with-temp-buffer
    (insert "* Card :drill:\n")
    (org-mode)
    (goto-char (point-min))
    (org-set-property "DRILL_LAST_QUALITY" "4")
    (let ((session (org-drill-session)))
      (push (org-drill--snapshot-entry-data) (oref session undo-stack))
      (org-set-property "DRILL_LAST_QUALITY" "1")
      (push 1 (oref session qualities))
      (org-drill-undo-last-rating session)
      (should (equal "4" (org-entry-get (point) "DRILL_LAST_QUALITY")))
      (should (null (oref session qualities)))
      (should (= 1 (length (oref session again-entries)))))))

(ert-deftest test-org-drill-undo-last-rating-empty-stack-is-noop ()
  "Undo with nothing recorded does not error and changes no session state."
  (let ((session (org-drill-session)))
    (org-drill-undo-last-rating session)
    (should (null (oref session qualities)))
    (should (null (oref session again-entries)))))

;;;; undo-key routing through the rating prompt

(ert-deftest test-org-drill-reschedule-undo-key-takes-back-previous-rating ()
  "Pressing the undo key at the prompt restores the previous card and
re-queues it, then the prompt loop continues (here, a quit ends it).

Components integrated:
- org-drill-reschedule (entry point, real)
- org-drill--read-rating-key (MOCKED — returns undo key then quit key)
- org-drill-undo-last-rating + snapshot/restore (real)

Validates the undo key is handled in the prompt loop rather than treated
as a rating, and that the loop keeps prompting afterward."
  (with-temp-buffer
    (insert "* Prev :drill:\n* Current :drill:\n")
    (org-mode)
    (let ((session (org-drill-session)))
      (goto-char (point-min))
      (org-set-property "DRILL_LAST_QUALITY" "5")
      (push (org-drill--snapshot-entry-data) (oref session undo-stack))
      (org-set-property "DRILL_LAST_QUALITY" "0")  ; the misrating to take back
      (push 0 (oref session qualities))
      (goto-char (point-min))
      (re-search-forward "Current")
      (let ((keys (list org-drill--undo-key org-drill--quit-key)))
        (cl-letf (((symbol-function 'org-drill--read-rating-key)
                   (lambda (&rest _) (pop keys)))
                  ((symbol-function 'sit-for) #'ignore))
          (should (null (org-drill-reschedule session)))))
      (goto-char (point-min))
      (should (equal "5" (org-entry-get (point) "DRILL_LAST_QUALITY")))
      (should (null (oref session qualities)))
      (should (= 1 (length (oref session again-entries)))))))

(provide 'test-org-drill-undo)

;;; test-org-drill-undo.el ends here
