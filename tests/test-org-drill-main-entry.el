;;; test-org-drill-main-entry.el --- Tests for the main org-drill orchestrator  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the public `org-drill' command — the main orchestrator that
;; sets up a session, scans for entries, and dispatches into the entry
;; loop.  We mock the recursive-edit-driven `org-drill-entries' to keep
;; the tests batch-friendly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-org-drill-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-main-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-org-drill-empty-buffer-messages-no-pending ()
  "An empty buffer takes the `queues-empty-p' branch and messages no items."
  (with-org-drill-tempfile "Plain text, no drill entries.\n"
    (let ((messages nil)
          (org-drill-save-buffers-after-drill-sessions-p nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt
                     (push (apply #'format fmt args) messages))))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'org-drill--setup-display) #'ignore)
                ((symbol-function 'org-drill--restore-display) #'ignore)
                ((symbol-function 'persist-save) #'ignore))
        (org-drill 'file))
      (should (cl-some (lambda (m)
                         (string-match-p "did not find any pending" m))
                       messages)))))

(ert-deftest test-org-drill-with-entries-runs-entry-loop ()
  "A buffer with a drill entry calls `org-drill-entries' and finishes cleanly."
  (with-org-drill-tempfile "* Card :drill:\nbody\n"
    (let ((entries-called nil)
          (messages nil)
          (org-drill-save-buffers-after-drill-sessions-p nil))
      (cl-letf (((symbol-function 'org-drill-entries)
                 (lambda (&rest _) (setq entries-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt
                     (push (apply #'format fmt args) messages))))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'org-drill--setup-display) #'ignore)
                ((symbol-function 'org-drill--restore-display) #'ignore)
                ((symbol-function 'persist-save) #'ignore))
        (org-drill 'file))
      (should entries-called)
      (should (cl-some (lambda (m) (string-match-p "Drill session finished" m))
                       messages)))))

(ert-deftest test-org-drill-cram-flag-sets-session-cram-mode ()
  "Calling org-drill with cram=t leaves cram-mode bound during the session."
  (with-org-drill-tempfile "* Card :drill:\nbody\n"
    (let ((seen-cram nil)
          (org-drill-save-buffers-after-drill-sessions-p nil))
      (cl-letf (((symbol-function 'org-drill-entries)
                 (lambda (s &rest _)
                   (setq seen-cram (oref s cram-mode))))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'org-drill--setup-display) #'ignore)
                ((symbol-function 'org-drill--restore-display) #'ignore)
                ((symbol-function 'persist-save) #'ignore))
        (org-drill 'file nil nil t))
      (should seen-cram))))

(ert-deftest test-org-drill-resume-p-skips-collection ()
  "When resume-p is non-nil, the entry collection step is skipped."
  (with-org-drill-tempfile "* Card :drill:\nbody\n"
    (let ((collect-called nil)
          (org-drill-save-buffers-after-drill-sessions-p nil)
          (org-drill-last-session (org-drill-session)))
      (oset org-drill-last-session start-time (float-time (current-time)))
      (oset org-drill-last-session new-entries
            (list (let ((m (make-marker))) (set-marker m 1 (current-buffer)) m)))
      (cl-letf (((symbol-function 'org-drill--collect-entries)
                 (lambda (&rest _) (setq collect-called t)))
                ((symbol-function 'org-drill-entries) #'ignore)
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'org-drill--setup-display) #'ignore)
                ((symbol-function 'org-drill--restore-display) #'ignore)
                ((symbol-function 'persist-save) #'ignore))
        (org-drill 'file nil t))
      (should-not collect-called))))

(provide 'test-org-drill-main-entry)
;;; test-org-drill-main-entry.el ends here
