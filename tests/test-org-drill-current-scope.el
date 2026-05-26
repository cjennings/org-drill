;;; test-org-drill-current-scope.el --- Tests for org-drill-current-scope  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-drill-current-scope translates a drill scope into the scope argument
;; org-map-entries expects, and org-drill-map-entries delegates to it.  The
;; map-entries mapping itself and drill-match filtering are covered in
;; test-org-drill.el (find-entries / find-tagged-entries); this file covers
;; the scope-translation branches, which had no tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-drill)

(ert-deftest test-org-drill-current-scope-file-returns-nil ()
  "Scope `file' maps to nil — the current buffer, respecting narrowing."
  (should (null (org-drill-current-scope 'file))))

(ert-deftest test-org-drill-current-scope-file-no-restriction-returns-file ()
  "Scope `file-no-restriction' maps to `file' — whole buffer, ignoring narrowing."
  (should (eq 'file (org-drill-current-scope 'file-no-restriction))))

(ert-deftest test-org-drill-current-scope-custom-symbol-passes-through ()
  "An unrecognized scope such as `tree' is returned unchanged."
  (should (eq 'tree (org-drill-current-scope 'tree))))

(ert-deftest test-org-drill-current-scope-nil-falls-back-to-org-drill-scope ()
  "A nil scope falls back to the value of `org-drill-scope'."
  (let ((org-drill-scope 'tree))
    (should (eq 'tree (org-drill-current-scope nil)))))

(ert-deftest test-org-drill-current-scope-directory-lists-org-files ()
  "Scope `directory' returns the .org files in the current file's directory."
  (let (captured-dir)
    (cl-letf (((symbol-function 'buffer-file-name)
               (lambda (&rest _) "/tmp/deck/cards.org"))
              ((symbol-function 'directory-files)
               (lambda (dir _full _match)
                 (setq captured-dir dir)
                 '("/tmp/deck/a.org" "/tmp/deck/b.org"))))
      (let ((result (org-drill-current-scope 'directory)))
        (should (equal "/tmp/deck/" captured-dir))
        (should (equal '("/tmp/deck/a.org" "/tmp/deck/b.org") result))))))

(provide 'test-org-drill-current-scope)

;;; test-org-drill-current-scope.el ends here
