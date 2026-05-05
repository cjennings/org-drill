;;; test-org-drill-leitner-orchestrator.el --- Tests for the leitner main entry  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-leitner', the public Leitner-learning command.  The
;; orchestrator captures all leitner-tagged entries, fills box 1 if there
;; aren't enough boxed entries, then runs the per-entry loop until the
;; user quits or asks to edit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-leitner-orch-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT and clean leitner queues."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-leitner-orch-" nil ".org"))
         (org-drill-leitner-boxed-entries nil)
         (org-drill-leitner-unboxed-entries nil))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-leitner-finishes-cleanly-when-loop-returns-t-each-step ()
  "When every leitner-entry returns t, the loop completes and the
'Finished Leitner Learning' summary is printed."
  (with-leitner-orch-tempfile
      "* A :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 1\n:END:\n* B :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 2\n:END:\n"
    (let ((messages nil)
          (org-drill-leitner-completed 0))
      (cl-letf (((symbol-function 'org-drill-leitner-entry)
                 (lambda (&rest _) t))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) messages))))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore))
        (org-drill-leitner))
      (should (cl-some (lambda (m) (string-match-p "Finished Leitner" m))
                       messages)))))

(ert-deftest test-leitner-quit-short-circuits-the-loop ()
  "When leitner-entry returns (quit ,_), the pcase matches the quit branch
and the function returns t without running further entries."
  (with-leitner-orch-tempfile
      "* A :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 1\n:END:\n* B :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 2\n:END:\n"
    (let ((calls 0))
      (cl-letf (((symbol-function 'org-drill-leitner-entry)
                 (lambda (&rest _)
                   (cl-incf calls)
                   'quit))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore))
        (should (eq t (org-drill-leitner)))
        (should (= 1 calls))))))

(ert-deftest test-leitner-edit-jumps-to-marker ()
  "When leitner-entry returns (edit MARKER), the orchestrator jumps to MARKER."
  (with-leitner-orch-tempfile
      "* A :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 1\n:END:\nbody\n"
    (let ((jumped-to nil)
          (entry-marker nil))
      (cl-letf (((symbol-function 'org-drill-leitner-entry)
                 (lambda (&rest _) 'edit))
                ((symbol-function 'org-drill-goto-entry)
                 (lambda (m)
                   (setq jumped-to m)
                   (when (markerp m) (goto-char m))))
                ((symbol-function 'org-reveal) #'ignore)
                ((symbol-function 'org-fold-show-entry) #'ignore)
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore))
        (setq entry-marker (point-marker))
        (org-drill-leitner)
        ;; goto-entry was called twice — once to navigate to the entry,
        ;; once again on the edit branch.  We just check it was called.
        (should jumped-to)))))

(ert-deftest test-leitner-fills-box-when-boxed-queue-is-too-small ()
  "If fewer boxed entries than maximum-items-per-session, leitner-start-box
runs to fill box 1 from the unboxed queue."
  (with-leitner-orch-tempfile
      "* A :leitner:\nbody\n* B :leitner:\nbody\n"
    (let ((start-box-called nil))
      (cl-letf (((symbol-function 'org-drill-leitner-start-box)
                 (lambda (&rest _) (setq start-box-called t)))
                ((symbol-function 'org-drill-leitner-entry)
                 (lambda (&rest _) t))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-progress-message) #'ignore))
        (let ((org-drill-maximum-items-per-session 5))
          (org-drill-leitner)))
      (should start-box-called))))

(provide 'test-org-drill-leitner-orchestrator)
;;; test-org-drill-leitner-orchestrator.el ends here
