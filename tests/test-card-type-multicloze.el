;;; test-card-type-multicloze.el --- Tests for multicloze card type variants

;;; Commentary:
;; Tests for various multicloze card type variants in org-drill.
;;
;; Multicloze variants:
;; - multicloze (alias for hide1cloze)
;; - hide1cloze, hide2cloze (hide N clozes)
;; - show1cloze, show2cloze (show N clozes, hide rest)
;; - hidefirst, hidelast (hide specific positions)
;; - hide1_firstmore, show1_lastmore, show1_firstless (weighted variants)

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Normal Cases - Multicloze Variants

(ert-deftest test-card-type-multicloze-normal-hide2cloze-exists ()
  "Test that hide2cloze variant exists."
  (let ((entry (assoc "hide2cloze" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-hide2))))

(ert-deftest test-card-type-multicloze-normal-show2cloze-exists ()
  "Test that show2cloze variant exists."
  (let ((entry (assoc "show2cloze" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-show2))))

(ert-deftest test-card-type-multicloze-normal-hidefirst-exists ()
  "Test that hidefirst variant exists."
  (let ((entry (assoc "hidefirst" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-hide-first))))

(ert-deftest test-card-type-multicloze-normal-hidelast-exists ()
  "Test that hidelast variant exists."
  (let ((entry (assoc "hidelast" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-hide-last))))

;;; Normal Cases - Weighted Variants

(ert-deftest test-card-type-multicloze-normal-hide1-firstmore-exists ()
  "Test that hide1_firstmore variant exists."
  (let ((entry (assoc "hide1_firstmore" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-hide1-firstmore))))

(ert-deftest test-card-type-multicloze-normal-show1-lastmore-exists ()
  "Test that show1_lastmore variant exists."
  (let ((entry (assoc "show1_lastmore" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-show1-lastmore))))

(ert-deftest test-card-type-multicloze-normal-show1-firstless-exists ()
  "Test that show1_firstless variant exists."
  (let ((entry (assoc "show1_firstless" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-multicloze-show1-firstless))))

;;; Boundary Cases - Hide vs Show Counts

(ert-deftest test-card-type-multicloze-boundary-hide1-vs-hide2 ()
  "Test that hide1cloze and hide2cloze use different functions."
  (let ((hide1-fn (cadr (assoc "hide1cloze" org-drill-card-type-alist)))
        (hide2-fn (cadr (assoc "hide2cloze" org-drill-card-type-alist))))
    (should-not (eq hide1-fn hide2-fn))
    (should (eq hide1-fn 'org-drill-present-multicloze-hide1))
    (should (eq hide2-fn 'org-drill-present-multicloze-hide2))))

(ert-deftest test-card-type-multicloze-boundary-show1-vs-show2 ()
  "Test that show1cloze and show2cloze use different functions."
  (let ((show1-fn (cadr (assoc "show1cloze" org-drill-card-type-alist)))
        (show2-fn (cadr (assoc "show2cloze" org-drill-card-type-alist))))
    (should-not (eq show1-fn show2-fn))
    (should (eq show1-fn 'org-drill-present-multicloze-show1))
    (should (eq show2-fn 'org-drill-present-multicloze-show2))))

;;; Boundary Cases - Position-based Variants

(ert-deftest test-card-type-multicloze-boundary-hidefirst-vs-hidelast ()
  "Test that hidefirst and hidelast use different functions."
  (let ((first-fn (cadr (assoc "hidefirst" org-drill-card-type-alist)))
        (last-fn (cadr (assoc "hidelast" org-drill-card-type-alist))))
    (should-not (eq first-fn last-fn))
    (should (eq first-fn 'org-drill-present-multicloze-hide-first))
    (should (eq last-fn 'org-drill-present-multicloze-hide-last))))

;;; Semantics - Variant Coverage

(ert-deftest test-card-type-multicloze-normal-all-variants-registered ()
  "Test that all documented multicloze variants are registered.
This ensures the card type system is complete."
  (let ((variants '("multicloze" "hide1cloze" "hide2cloze"
                    "show1cloze" "show2cloze"
                    "hidefirst" "hidelast"
                    "hide1_firstmore" "show1_lastmore" "show1_firstless")))
    (dolist (variant variants)
      (let ((entry (assoc variant org-drill-card-type-alist)))
        (should entry)
        (should (cadr entry))))))

(ert-deftest test-card-type-multicloze-normal-function-naming-convention ()
  "Test that multicloze functions follow naming convention.
All should start with org-drill-present-multicloze-"
  (let ((multicloze-types '("hide1cloze" "hide2cloze" "show1cloze" "show2cloze"
                            "hidefirst" "hidelast")))
    (dolist (type multicloze-types)
      (let* ((entry (assoc type org-drill-card-type-alist))
             (fn (cadr entry))
             (fn-name (symbol-name fn)))
        (should (string-prefix-p "org-drill-present-multicloze-" fn-name))))))

(provide 'test-card-type-multicloze)
;;; test-card-type-multicloze.el ends here
