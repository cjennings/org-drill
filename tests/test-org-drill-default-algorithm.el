;;; test-org-drill-default-algorithm.el --- Pin the default scheduler  -*- lexical-binding: t; -*-

;;; Commentary:
;; The default for `org-drill-spaced-repetition-algorithm' is the algorithm a
;; brand-new user gets out of the box.  Issue #46 asked for that choice to be
;; reconsidered; the ADR-style comment above the defcustom in `org-drill.el'
;; records the reasoning.  Pinning the value in a test means an accidental
;; flip back surfaces in CI rather than as drift discovered months later.

;;; Code:

(require 'ert)
(require 'org-drill)

(ert-deftest test-org-drill-default-algorithm-is-simple8 ()
  "The default scheduler is Simple8.  See the ADR comment in org-drill.el."
  (should (eq 'simple8 (default-value 'org-drill-spaced-repetition-algorithm))))

(provide 'test-org-drill-default-algorithm)
;;; test-org-drill-default-algorithm.el ends here
