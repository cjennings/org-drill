;;; test-org-drill-resume-nil-session.el --- Regression for nil last-session  -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-drill-again' and `org-drill-resume' bind a local `session' to
;; `org-drill-last-session' without checking for nil.  On the first
;; ever invocation (or after Emacs restart with no active session),
;; org-drill-last-session is nil and both functions throw — `again'
;; on `(setf (oref session cram-mode) nil)' (eieio-oset on nil),
;; `resume' on `org-drill-entries-pending-p' walking nil's slots.
;;
;; Fixed by adding an early `(unless session (user-error ...))' to
;; both.  A clear user-visible message replaces the obscure type
;; errors.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression — org-drill-again

(ert-deftest test-org-drill-again-nil-last-session-raises-user-error ()
  "Calling org-drill-again with no prior session triggers a user-error,
not an obscure eieio type error."
  (let ((org-drill-last-session nil))
    (should-error (org-drill-again) :type 'user-error)))

;;;; Regression — org-drill-resume

(ert-deftest test-org-drill-resume-nil-last-session-raises-user-error ()
  "Calling org-drill-resume with no prior session triggers a user-error."
  (let ((org-drill-last-session nil))
    (should-error (org-drill-resume) :type 'user-error)))

(provide 'test-org-drill-resume-nil-session)

;;; test-org-drill-resume-nil-session.el ends here
