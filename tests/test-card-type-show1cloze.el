;;; test-card-type-show1cloze.el --- Tests for show1cloze card type

;;; Commentary:
;; Tests for the show1cloze card type in org-drill.
;;
;; show1cloze cards show exactly one piece of cloze-marked text,
;; chosen at random, while hiding all other cloze markers.
;; This is the inverse of hide1cloze.
;;
;; Card type: "show1cloze"
;; Presentation function: org-drill-present-multicloze-show1

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Normal Cases - Card Recognition

(ert-deftest test-card-type-show1cloze-normal-card-type-property ()
  "Test that show1cloze cards have correct DRILL_CARD_TYPE property."
  (with-temp-buffer
    (org-mode)
    (insert "* Show Cloze Card :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: show1cloze\n:END:\n\nThe capital of France is Paris.\n")
    (goto-char (point-min))
    (should (org-drill-entry-p))
    (should (equal "show1cloze" (org-entry-get (point) "DRILL_CARD_TYPE")))))

(ert-deftest test-card-type-show1cloze-normal-has-presentation-function ()
  "Test that show1cloze card type has correct presentation function."
  (let ((entry (assoc "show1cloze" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-show1))))

(ert-deftest test-card-type-show1cloze-normal-different-from-hide1cloze ()
  "Test that show1cloze and hide1cloze use different functions."
  (let ((show1-fn (cadr (assoc "show1cloze" org-drill-card-type-alist)))
        (hide1-fn (cadr (assoc "hide1cloze" org-drill-card-type-alist))))
    (should-not (eq show1-fn hide1-fn))
    (should (eq show1-fn 'org-drill-present-multicloze-show1))
    (should (eq hide1-fn 'org-drill-present-multicloze-hide1))))

(ert-deftest test-card-type-show1cloze-normal-show2cloze-exists ()
  "Test that show2cloze variant also exists."
  (let ((show1-entry (assoc "show1cloze" org-drill-card-type-alist))
        (show2-entry (assoc "show2cloze" org-drill-card-type-alist)))
    (should show1-entry)
    (should show2-entry)
    ;; Different presentation functions
    (should-not (eq (cadr show1-entry) (cadr show2-entry)))
    (should (eq (cadr show2-entry) 'org-drill-present-multicloze-show2))))

;;; Semantics - Inverse of hide1cloze

(ert-deftest test-card-type-show1cloze-normal-inverse-semantics ()
  "Test that show1cloze has inverse semantics to hide1cloze.
hide1cloze: hide 1, show rest
show1cloze: show 1, hide rest"
  (let ((hide1-fn (cadr (assoc "hide1cloze" org-drill-card-type-alist)))
        (show1-fn (cadr (assoc "show1cloze" org-drill-card-type-alist))))
    ;; Both functions exist
    (should hide1-fn)
    (should show1-fn)
    ;; They are different functions
    (should-not (eq hide1-fn show1-fn))
    ;; Both are multicloze variants
    (should (string-match-p "multicloze" (symbol-name hide1-fn)))
    (should (string-match-p "multicloze" (symbol-name show1-fn)))))

(ert-deftest test-card-type-show1cloze-normal-use-case-list-learning ()
  "Test typical use case for show1cloze: learning from one shown item.
When learning a list, showing one item while hiding others helps
learn the context around each item."
  (with-temp-buffer
    (org-mode)
    (insert "* Programming Languages :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: show1cloze\n:END:\n\nLearn these languages.\n")
    (goto-char (point-min))
    (should (org-drill-entry-p))
    (should (equal "show1cloze" (org-entry-get (point) "DRILL_CARD_TYPE")))))

(provide 'test-card-type-show1cloze)
;;; test-card-type-show1cloze.el ends here
