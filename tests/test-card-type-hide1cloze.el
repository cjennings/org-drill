;;; test-card-type-hide1cloze.el --- Tests for hide1cloze card type

;;; Commentary:
;; Tests for the hide1cloze card type in org-drill.

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Normal Cases - Card Recognition

(ert-deftest test-card-type-hide1cloze-normal-card-type-property ()
  "Test that hide1cloze cards have correct DRILL_CARD_TYPE property."
  (with-temp-buffer
    (org-mode)
    (insert "* Basic Cloze Card :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: hide1cloze\n:END:\n\nThe capital of France is Paris.\n")
    (goto-char (point-min))
    (should (org-drill-entry-p))
    (should (equal "hide1cloze" (org-entry-get (point) "DRILL_CARD_TYPE")))))

(ert-deftest test-card-type-hide1cloze-normal-has-presentation-function ()
  "Test that hide1cloze card type has correct presentation function."
  (let ((entry (assoc "hide1cloze" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-hide1))))

(ert-deftest test-card-type-hide1cloze-normal-multicloze-alias ()
  "Test that multicloze is an alias for hide1cloze."
  (let ((hide1-entry (assoc "hide1cloze" org-drill-card-type-alist))
        (multi-entry (assoc "multicloze" org-drill-card-type-alist)))
    (should hide1-entry)
    (should multi-entry)
    (should (eq (cadr hide1-entry) (cadr multi-entry)))
    (should (eq (cadr multi-entry) 'org-drill-present-multicloze-hide1))))

(ert-deftest test-card-type-hide1cloze-normal-different-from-show1cloze ()
  "Test that hide1cloze and show1cloze use different functions."
  (let ((hide1-fn (cadr (assoc "hide1cloze" org-drill-card-type-alist)))
        (show1-fn (cadr (assoc "show1cloze" org-drill-card-type-alist))))
    (should-not (eq hide1-fn show1-fn))
    (should (eq hide1-fn 'org-drill-present-multicloze-hide1))
    (should (eq show1-fn 'org-drill-present-multicloze-show1))))

(provide 'test-card-type-hide1cloze)
;;; test-card-type-hide1cloze.el ends here
