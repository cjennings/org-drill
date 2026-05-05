;;; test-org-drill-resume-clears-end-pos.el --- Regression for #33  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #33 (2020-05).  When a user interrupted a drill
;; session to edit or capture, `org-drill-entries' set the session's
;; `end-pos' slot.  The end-of-`org-drill' cond branched on `end-pos':
;; if set, show "you can resume" and skip `org-drill-final-report'.
;;
;; That worked for the first interruption.  But on subsequent
;; `org-drill-resume', the session was reused with `end-pos' still
;; carrying the marker from before — so even when the resumed session
;; completed normally, `final-report' was still skipped.
;;
;; Fix: clear `end-pos' at the top of `org-drill' when resume-p is
;; non-nil, so the session can again reach the final-report branch.
;; This matches Markus's proposed patch on the upstream issue.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression — #33

(ert-deftest test-org-drill-resume-clears-stale-end-pos ()
  "An org-drill resume on a session whose end-pos is still set from a
prior interruption should clear end-pos so the resumed session can
reach the normal final-report branch."
  (let ((session (org-drill-session))
        (final-report-called nil))
    ;; Simulate a session that was interrupted by edit (end-pos = marker).
    (let ((stale-marker (with-temp-buffer (point-marker))))
      (oset session end-pos stale-marker))
    (oset session start-time (float-time (current-time)))
    ;; Set up org-drill-last-session for the resume path to find.
    (let ((org-drill-last-session session))
      (cl-letf (((symbol-function 'org-drill-final-report)
                 (lambda (_) (setq final-report-called t)))
                ((symbol-function 'org-drill--setup-display) #'ignore)
                ((symbol-function 'org-drill--restore-display) #'ignore)
                ((symbol-function 'org-drill-map-entries) #'ignore)
                ((symbol-function 'org-drill-order-overdue-entries) #'ignore)
                ((symbol-function 'org-drill-entries)
                 (lambda (_session &optional _resume) nil))
                ((symbol-function 'org-drill-free-markers) #'ignore)
                ((symbol-function 'persist-save) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill nil nil t nil)
        ;; The resumed session completed normally — end-pos cleared,
        ;; final-report fired.
        (should final-report-called)))))

(provide 'test-org-drill-resume-clears-end-pos)

;;; test-org-drill-resume-clears-end-pos.el ends here
