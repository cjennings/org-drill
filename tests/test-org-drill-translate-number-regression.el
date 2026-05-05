;;; test-org-drill-translate-number-regression.el --- Regression test for issue #43  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #43 (2021-08-31).  The card-type alist mapped
;; "translate_number" to `org-drill-present-translate-number', a function
;; that no longer exists in the file.  Cards with
;; `:DRILL_CARD_TYPE: translate_number:' hit a void-function error during
;; the drill loop instead of being skipped gracefully.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression — issue #43

(ert-deftest test-translate-number-card-type-handled-gracefully ()
  "A card with `:DRILL_CARD_TYPE: translate_number:' is skipped with
a user-visible message instead of crashing with void-function.

Pre-fix: org-drill-card-type-alist still mapped translate_number to
the missing org-drill-present-translate-number; entry-f's
`(funcall presentation-fn session)' threw void-function.

Post-fix: the alist entry is removed; entry-f's no-presentation-fn
branch fires and returns 'skip after messaging the user."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: translate_number\n:END:\nbody body body\n")
      (org-mode)
      (goto-char (point-min))
      (let ((session (org-drill-session)))
        (cl-letf (((symbol-function 'sit-for) #'ignore))
          (let ((result (org-drill-entry-f session #'ignore)))
            (should (eq 'skip result))))))))

(ert-deftest test-translate-number-not-in-card-type-alist ()
  "The translate_number → org-drill-present-translate-number entry is gone
from the card-type alist.  No code path can dispatch into the missing
function any more."
  (should-not (assoc "translate_number" org-drill-card-type-alist)))

(provide 'test-org-drill-translate-number-regression)

;;; test-org-drill-translate-number-regression.el ends here
