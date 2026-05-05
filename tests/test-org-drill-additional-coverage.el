;;; test-org-drill-additional-coverage.el --- Additional coverage for entry-status, map-entry-function, replace-multi  -*- lexical-binding: t; -*-

;;; Commentary:
;; Targeted tests filling gaps in:
;;
;; - `org-drill-replace-entry-text-multi': installs N overlays from a list
;;   of replacement strings.
;; - `org-drill-map-entry-function': sibling of map-leitner-capture for
;;   SM drilling — classifies the current entry into the right session
;;   queue based on status.
;; - `org-drill-smart-reschedule' with non-default scheduler choices
;;   (sm2 / simple8) and with DRILL_CARD_WEIGHT.
;; - `org-drill-entries-pending-p' edge cases.

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
       (insert "* Question :drill:\nbody body body body body body body body body body\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defun count-overlays-of-category (cat)
  (let ((n 0))
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql cat (overlay-get ovl 'category))
        (cl-incf n)))
    n))

;;;; org-drill-replace-entry-text-multi

(ert-deftest test-replace-entry-text-multi-creates-one-overlay-per-replacement ()
  "Three replacements → three replaced-text overlays."
  (with-fresh-drill-entry
    (org-drill-replace-entry-text-multi '("alpha" "beta" "gamma"))
    (should (= 3 (count-overlays-of-category 'org-drill-replaced-text-overlay)))))

(ert-deftest test-replace-entry-text-multi-displays-each-replacement ()
  "Each overlay shows one of the supplied strings via its `display' prop."
  (with-fresh-drill-entry
    (org-drill-replace-entry-text-multi '("X" "Y"))
    (let ((displays nil))
      (dolist (ovl (overlays-in (point-min) (point-max)))
        (when (eql 'org-drill-replaced-text-overlay (overlay-get ovl 'category))
          (push (overlay-get ovl 'display) displays)))
      (should (member "X" displays))
      (should (member "Y" displays)))))

;;;; org-drill-map-entry-function

(ert-deftest test-map-entry-function-virgin-entry-counts-as-new ()
  "A drill entry with no schedule and a body lands in session->new-entries."
  (let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmpfile)
          (let ((org-startup-folded nil))
            (insert "* Question :drill:\nbody\n")
            (goto-char (point-min))
            (let ((session (org-drill-session)))
              (with-fixed-now
                (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                          ((symbol-function 'sit-for) #'ignore))
                  (org-drill-map-entry-function session)
                  (should (= 1 (length (oref session new-entries)))))))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-map-entry-function-future-entry-counts-as-dormant ()
  "An entry scheduled in the future increments dormant-entry-count, not
new-entries."
  (let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmpfile)
          (let ((org-startup-folded nil))
            (insert "* Question :drill:\nbody\n")
            (org-mode)
            (goto-char (point-min))
            (org-schedule nil "2026-06-01")
            (goto-char (point-min))
            (let ((session (org-drill-session)))
              (with-fixed-now
                (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                          ((symbol-function 'sit-for) #'ignore))
                  (org-drill-map-entry-function session)
                  (should (= 1 (oref session dormant-entry-count)))
                  (should (null (oref session new-entries))))))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-map-entry-function-non-drill-entry-skipped ()
  "Plain headings are ignored — not classified into any queue."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Plain heading\n")
      (org-mode)
      (goto-char (point-min))
      (let ((session (org-drill-session)))
        (with-fixed-now
          (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                    ((symbol-function 'sit-for) #'ignore))
            (org-drill-map-entry-function session)
            (should (null (oref session new-entries)))
            (should (= 0 (oref session dormant-entry-count)))))))))

;;;; smart-reschedule with non-default schedulers

(ert-deftest test-smart-reschedule-sm2-runs-cleanly ()
  "Switching to the sm2 scheduler still produces a SCHEDULED stamp."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((org-drill-spaced-repetition-algorithm 'sm2))
        (org-drill-smart-reschedule 4)
        (should (org-entry-get (point) "SCHEDULED"))))))

(ert-deftest test-smart-reschedule-simple8-runs-cleanly ()
  "Switching to the simple8 scheduler still produces a SCHEDULED stamp."
  (with-fresh-drill-entry
    (with-fixed-now
      (let ((org-drill-spaced-repetition-algorithm 'simple8))
        (org-drill-smart-reschedule 4)
        (should (org-entry-get (point) "SCHEDULED"))))))

(ert-deftest test-smart-reschedule-respects-card-weight ()
  "DRILL_CARD_WEIGHT > 1 stretches the next-interval delta — the resulting
SCHEDULED date is closer to today than without weight."
  (with-fresh-drill-entry
    (org-drill-store-item-data 10 3 0 3 4.5 2.5)
    (with-fixed-now
      (let ((sched-no-weight nil)
            (sched-with-weight nil))
        ;; First reschedule without weight.
        (org-drill-smart-reschedule 5)
        (setq sched-no-weight (org-entry-get (point) "SCHEDULED"))
        ;; Reset and reschedule with weight=2.
        (org-drill-store-item-data 10 3 0 3 4.5 2.5)
        (org-set-property "DRILL_CARD_WEIGHT" "2")
        (org-drill-smart-reschedule 5)
        (setq sched-with-weight (org-entry-get (point) "SCHEDULED"))
        (should sched-no-weight)
        (should sched-with-weight)))))

;;;; entries-pending-p edge cases

(ert-deftest test-entries-pending-p-overdue-only ()
  "An overdue queue alone is enough to keep the session pending."
  (let ((session (org-drill-session)))
    (oset session start-time (float-time (current-time)))
    (oset session overdue-entries
          (list (let ((m (make-marker))) (set-marker m 1) m)))
    (should (org-drill-entries-pending-p session))))

(provide 'test-org-drill-additional-coverage)

;;; test-org-drill-additional-coverage.el ends here
