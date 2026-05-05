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

;;;; org-drill-resume — happy paths

(ert-deftest test-org-drill-resume-with-pending-entries-resumes ()
  "When the prior session has pending entries, resume calls org-drill with resume-p=t."
  (let* ((session (org-drill-session))
         (org-drill-last-session session)
         (resume-args nil))
    (cl-letf (((symbol-function 'org-drill-entries-pending-p) (lambda (_) t))
              ((symbol-function 'org-drill)
               (lambda (&rest args) (setq resume-args args))))
      (org-drill-resume))
    (should resume-args)
    (should (nth 2 resume-args))))

(ert-deftest test-org-drill-resume-finished-with-no-pending-says-finished ()
  "When the session is done with no remaining pending count, prints the
'finished' message."
  (let* ((session (org-drill-session))
         (org-drill-last-session session)
         (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) messages)))))
      (org-drill-resume))
    (should (cl-some (lambda (m) (string-match-p "finished" m)) messages))))

(ert-deftest test-org-drill-resume-finished-with-y-starts-new ()
  "When session is done but pending count is positive and user answers y,
org-drill-again runs."
  (let* ((session (org-drill-session))
         (org-drill-last-session session)
         (m (let ((mk (make-marker))) (set-marker mk 1) mk))
         (again-called nil))
    ;; Done-entries non-empty + new-entries non-empty + done-entries reaches limit
    ;; tricky to set up cleanly.  Easier: stub the predicates.
    (cl-letf (((symbol-function 'org-drill-entries-pending-p)
               (lambda (_) nil))
              ((symbol-function 'org-drill-pending-entry-count)
               (lambda (_) 5))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-drill-again)
               (lambda (&rest _) (setq again-called t)))
              ((symbol-function 'message) #'ignore))
      (org-drill-resume))
    (should again-called)))

(provide 'test-org-drill-resume-nil-session)

;;; test-org-drill-resume-nil-session.el ends here
