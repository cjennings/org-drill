;;; test-integration-card-types-integration-test.el --- Integration test for card types

;;; Commentary:
;; Integration test verifying that the card type system works correctly.

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Integration Tests - Card Type System

(ert-deftest test-integration-card-types-integration-card-type-system-complete ()
  "Test that card type system handles all expected types.
Verifies all card types are registered in org-drill-card-type-alist."
  (let ((expected-types '(nil "simple" "simpletyped" "twosided" "multisided"
                          "hide1cloze" "hide2cloze" "show1cloze" "show2cloze"
                          "multicloze" "hidefirst" "hidelast"
                          "hide1_firstmore" "show1_lastmore" "show1_firstless"
                          "conjugate" "decline_noun" "spanish_verb" "translate_number")))
    (dolist (type expected-types)
      (let ((entry (assoc type org-drill-card-type-alist)))
        (should entry)
        (should (cadr entry))))))

(ert-deftest test-integration-card-types-integration-simple-vs-twosided ()
  "Test that simple and twosided cards use different presentation functions."
  (let ((simple-fn (cadr (assoc "simple" org-drill-card-type-alist)))
        (twosided-fn (cadr (assoc "twosided" org-drill-card-type-alist))))
    (should simple-fn)
    (should twosided-fn)
    (should-not (eq simple-fn twosided-fn))))

(ert-deftest test-integration-card-types-integration-hide-vs-show-cloze ()
  "Test that hide and show cloze variants use different functions."
  (let ((hide1-fn (cadr (assoc "hide1cloze" org-drill-card-type-alist)))
        (show1-fn (cadr (assoc "show1cloze" org-drill-card-type-alist)))
        (hide2-fn (cadr (assoc "hide2cloze" org-drill-card-type-alist)))
        (show2-fn (cadr (assoc "show2cloze" org-drill-card-type-alist))))
    (should hide1-fn)
    (should show1-fn)
    (should hide2-fn)
    (should show2-fn)
    ;; All different
    (should-not (eq hide1-fn show1-fn))
    (should-not (eq hide1-fn hide2-fn))
    (should-not (eq show1-fn show2-fn))))

(ert-deftest test-integration-card-types-integration-default-equals-simple ()
  "Test that default (nil) card type maps to same function as simple."
  (let ((default-fn (cadr (assoc nil org-drill-card-type-alist)))
        (simple-fn (cadr (assoc "simple" org-drill-card-type-alist))))
    (should default-fn)
    (should simple-fn)
    (should (eq default-fn simple-fn))
    (should (eq default-fn 'org-drill-present-simple-card))))

(ert-deftest test-integration-card-types-integration-presentation-functions-exist ()
  "Test that all presentation functions are defined."
  (let ((all-types '("simple" "twosided" "hide1cloze" "show1cloze" "multicloze")))
    (dolist (type all-types)
      (let* ((entry (assoc type org-drill-card-type-alist))
             (fn (cadr entry)))
        (should fn)
        (should (fboundp fn))))))

(provide 'test-integration-card-types-integration-test)
;;; test-integration-card-types-integration-test.el ends here
