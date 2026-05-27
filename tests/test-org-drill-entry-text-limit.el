;;; test-org-drill-entry-text-limit.el --- Tests for the entry-text line limit  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-drill-get-entry-text asks org-agenda for at most N lines of an entry's
;; text.  That limit used to be a hardcoded 100; it is now the defcustom
;; org-drill-entry-text-max-lines so a user with very long cards can raise it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-drill)

(ert-deftest test-org-drill-entry-text-max-lines-defaults-to-100 ()
  "The limit keeps its historical default of 100 lines."
  (should (eq 100 (default-value 'org-drill-entry-text-max-lines))))

(ert-deftest test-org-drill-get-entry-text-uses-the-custom-limit ()
  "get-entry-text passes the defcustom value as the line limit, not a
hardcoded number."
  (let ((org-drill-entry-text-max-lines 7)
        (captured nil))
    (cl-letf (((symbol-function 'org-agenda-get-some-entry-text)
               (lambda (_marker n &rest _) (setq captured n) "text")))
      (with-temp-buffer
        (insert "* Card :drill:\nbody\n")
        (org-mode)
        (goto-char (point-min))
        (org-drill-get-entry-text))
      (should (eq 7 captured)))))

(provide 'test-org-drill-entry-text-limit)

;;; test-org-drill-entry-text-limit.el ends here
