;;; test-org-drill-autoloads.el --- Autoload-cookie coverage  -*- lexical-binding: t; -*-

;;; Commentary:
;; The user-facing entry-point commands must carry an `;;;###autoload' cookie
;; so they work from a fresh package install before org-drill is loaded.
;; Without it, `M-x org-drill-resume' (etc.) fails with "command not found"
;; until something pulls the file in.
;;
;; This reads the source via `find-library-name' (the .el, not a compiled
;; .elc which would have the cookies stripped) and checks each command.

;;; Code:

(require 'ert)
(require 'org-drill)

(defconst test-org-drill-autoloaded-commands
  '("org-drill"
    "org-drill-cram"
    "org-drill-cram-tree"
    "org-drill-tree"
    "org-drill-directory"
    "org-drill-again"
    "org-drill-resume"
    "org-drill-relearn-item"
    "org-drill-strip-all-data"
    "org-drill-merge-buffers")
  "Entry-point commands that should be autoloaded.")

(ert-deftest test-org-drill-entry-commands-carry-autoload-cookie ()
  "Each user-facing entry-point command is preceded by an autoload cookie."
  (let ((src (with-temp-buffer
               (insert-file-contents (find-library-name "org-drill"))
               (buffer-string))))
    (dolist (cmd test-org-drill-autoloaded-commands)
      (should (string-match-p
               (concat ";;;###autoload\n(defun " (regexp-quote cmd) " ")
               src)))))

(ert-deftest test-org-drill-entry-commands-are-interactive ()
  "Every command in the autoload list is actually an interactive command."
  (dolist (cmd test-org-drill-autoloaded-commands)
    (should (commandp (intern cmd)))))

(provide 'test-org-drill-autoloads)

;;; test-org-drill-autoloads.el ends here
