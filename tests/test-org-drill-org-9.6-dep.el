;;; test-org-drill-org-9.6-dep.el --- Test Org dep declarations match runtime needs  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-drill calls `org-fold-show-entry' and `org-fold-show-subtree'
;; from seven sites without `fboundp' guards.  Both functions arrived
;; in Org 9.6.  Pre-fix the package declared `(org "9.3")' /
;; `(depends-on "org" "9.2")', so users on older Org would silently
;; void-function at runtime instead of getting a clear install-time
;; error.
;;
;; Bumping the dep to 9.6 makes the requirement explicit and lets us
;; call the new APIs without per-site guards.  Tests verify the
;; declared deps and that the API symbols are actually bound.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Dep declarations

(ert-deftest test-org-drill-package-requires-org-9.6-or-newer ()
  "The Package-Requires header declares org 9.6+ — matches the runtime
need for org-fold-show-entry/-subtree."
  (let ((header-line
         (with-temp-buffer
           (insert-file-contents
            (locate-library "org-drill") nil 0 4096)
           (goto-char (point-min))
           (when (re-search-forward "Package-Requires:.*$" nil t)
             (match-string 0)))))
    (should header-line)
    (should (string-match-p "(org \"9\\.6\\|9\\.[7-9]\\|[1-9][0-9]" header-line))))

(ert-deftest test-org-drill-org-fold-functions-bound ()
  "On any supported Org version (>= 9.6), the org-fold APIs we call
without guards are actually bound."
  (should (fboundp 'org-fold-show-entry))
  (should (fboundp 'org-fold-show-subtree)))

(provide 'test-org-drill-org-9.6-dep)

;;; test-org-drill-org-9.6-dep.el ends here
