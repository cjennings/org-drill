;;; test-org-drill-session-keys.el --- Tests for customizable session keys  -*- lexical-binding: t; -*-

;;; Commentary:
;; The session-control keys (quit/edit/help/skip/tags) are defcustoms so they
;; can be rebound through customize-group (upstream issue #35), while keeping
;; their historical default characters.

;;; Code:

(require 'ert)
(require 'org-drill)

(defconst test-org-drill-session-key-defaults
  '((org-drill--quit-key . ?q)
    (org-drill--edit-key . ?e)
    (org-drill--help-key . ??)
    (org-drill--skip-key . ?s)
    (org-drill--tags-key . ?t))
  "Each session-control key variable and its historical default character.")

(ert-deftest test-org-drill-session-keys-are-customizable ()
  "Each session-control key is a defcustom, so customize-group can set it."
  (dolist (cell test-org-drill-session-key-defaults)
    (should (custom-variable-p (car cell)))))

(ert-deftest test-org-drill-session-keys-keep-their-defaults ()
  "Promoting to defcustom does not change the default bindings."
  (dolist (cell test-org-drill-session-key-defaults)
    (should (eq (cdr cell) (default-value (car cell))))))

(provide 'test-org-drill-session-keys)

;;; test-org-drill-session-keys.el ends here
