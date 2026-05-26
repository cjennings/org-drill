;;; test-org-drill-version.el --- Tests for org-drill-version  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-drill-version is both a constant (the package version string) and an
;; interactive command that reports it, so bug reporters don't have to open
;; the file header.

;;; Code:

(require 'ert)
(require 'org-drill)

(ert-deftest test-org-drill-version-is-a-version-string ()
  "The constant is a non-empty dotted version string."
  (should (stringp org-drill-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+" org-drill-version)))

(ert-deftest test-org-drill-version-command-returns-the-version ()
  "Calling the command returns the version string."
  (should (equal org-drill-version (org-drill-version))))

(ert-deftest test-org-drill-version-command-is-interactive ()
  "The version reporter is an interactive command."
  (should (commandp 'org-drill-version)))

(provide 'test-org-drill-version)

;;; test-org-drill-version.el ends here
