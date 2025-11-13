;;; test-card-type-twosided.el --- Tests for two-sided card type

;;; Commentary:
;; Tests for the two-sided card type in org-drill.
;;
;; Two-sided cards have two subheadings representing two "sides" of a card.
;; When presented, one of the first two subheadings is randomly chosen
;; and shown as the question, while the other serves as the answer.
;;
;; Card type: "twosided"
;; Presentation function: org-drill-present-two-sided-card

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data

(defconst test-card-type-twosided-basic-card
  "* Two-Sided Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side 1

English: Apple

** Side 2

Spanish: Manzana
"
  "Basic two-sided card for vocabulary.")

(defconst test-card-type-twosided-with-extra-sides
  "* Card with Extra Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side A

First side content.

** Side B

Second side content.

** Side C

Third side (should not be shown in two-sided mode).

** Side D

Fourth side (should not be shown in two-sided mode).
"
  "Two-sided card with more than 2 subheadings.")

(defconst test-card-type-twosided-single-side
  "* Card with Single Side :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Only Side

Only one subheading present.
"
  "Two-sided card with only one subheading.")

(defconst test-card-type-twosided-empty-sides
  "* Card with Empty Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side 1

** Side 2

"
  "Two-sided card with empty side content.")

;;; Helper Functions

(defun test-card-type-twosided--with-card (content callback)
  "Execute CALLBACK in temp buffer with drill card CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (funcall callback)))

(defun test-card-type-twosided--count-subheadings ()
  "Count number of level-2 subheadings under current entry."
  (save-excursion
    (org-back-to-heading)
    (let ((count 0)
          (end (save-excursion (org-end-of-subtree t t))))
      (while (and (re-search-forward "^\\*\\* " end t))
        (cl-incf count))
      count)))

;;; Normal Cases - Card Recognition

(ert-deftest test-card-type-twosided-normal-card-type-property ()
  "Test that two-sided cards have correct DRILL_CARD_TYPE property."
  (test-card-type-twosided--with-card
   test-card-type-twosided-basic-card
   (lambda ()
     (should (org-drill-entry-p))
     (should (equal "twosided" (org-entry-get (point) "DRILL_CARD_TYPE"))))))

(ert-deftest test-card-type-twosided-normal-has-presentation-function ()
  "Test that twosided card type has correct presentation function registered."
  (let ((entry (assoc "twosided" org-drill-card-type-alist)))
    (should entry)
    (should (eq (cadr entry) 'org-drill-present-two-sided-card))))

(ert-deftest test-card-type-twosided-normal-drill-empty-p-flag ()
  "Test that twosided card type has drill-empty-p flag set.
This allows two-sided cards to be presented even if main body is empty."
  (let ((entry (assoc "twosided" org-drill-card-type-alist)))
    (should entry)
    ;; Fourth element is drill-empty-p flag
    (should (nth 3 entry))))

;;; Normal Cases - Card Structure

(ert-deftest test-card-type-twosided-normal-two-subheadings ()
  "Test two-sided card with exactly two subheadings."
  (test-card-type-twosided--with-card
   test-card-type-twosided-basic-card
   (lambda ()
     (should (org-drill-entry-p))
     (should (= 2 (test-card-type-twosided--count-subheadings))))))

(ert-deftest test-card-type-twosided-normal-more-than-two-subheadings ()
  "Test two-sided card with more than two subheadings.
Only first two should be used for presentation."
  (test-card-type-twosided--with-card
   test-card-type-twosided-with-extra-sides
   (lambda ()
     (should (org-drill-entry-p))
     (should (> (test-card-type-twosided--count-subheadings) 2)))))

;;; Boundary Cases

(ert-deftest test-card-type-twosided-boundary-single-subheading ()
  "Test two-sided card with only one subheading.
Should still be a valid card, though not ideal."
  (test-card-type-twosided--with-card
   test-card-type-twosided-single-side
   (lambda ()
     (should (org-drill-entry-p))
     (should (= 1 (test-card-type-twosided--count-subheadings))))))

(ert-deftest test-card-type-twosided-boundary-empty-side-content ()
  "Test two-sided card where sides have no content."
  (test-card-type-twosided--with-card
   test-card-type-twosided-empty-sides
   (lambda ()
     (should (org-drill-entry-p))
     (should (= 2 (test-card-type-twosided--count-subheadings))))))

(ert-deftest test-card-type-twosided-boundary-no-main-body ()
  "Test two-sided card with no main body content.
This is valid because twosided has drill-empty-p flag."
  (let ((content "* No Body :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n\n** A\n\nContent A\n\n** B\n\nContent B\n"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (equal "twosided" (org-entry-get (point) "DRILL_CARD_TYPE")))))))

;;; Card Type Semantics

(ert-deftest test-card-type-twosided-normal-vocabulary-use-case ()
  "Test typical vocabulary card use case.
Two sides for word translation pairs."
  (test-card-type-twosided--with-card
   test-card-type-twosided-basic-card
   (lambda ()
     (should (org-drill-entry-p))
     ;; Verify both sides exist
     (let ((sides nil))
       (save-excursion
         (org-back-to-heading)
         (while (re-search-forward "^\\*\\* " nil t)
           (let ((heading-text (buffer-substring
                                (point)
                                (line-end-position))))
             (push heading-text sides))))
       (should (= 2 (length sides)))
       (should (member "Side 1" sides))
       (should (member "Side 2" sides))))))

(ert-deftest test-card-type-twosided-normal-concept-definition-use-case ()
  "Test concept/definition card use case."
  (let ((content "* Concept Card :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n\n** Concept\n\nPolymorphism\n\n** Definition\n\nAbility of different objects to respond to same message in different ways.\n"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-normal-different-from-simple ()
  "Test that twosided is registered differently from simple."
  (let ((twosided-fn (cadr (assoc "twosided" org-drill-card-type-alist)))
        (simple-fn (cadr (assoc "simple" org-drill-card-type-alist))))
    (should-not (eq twosided-fn simple-fn))
    (should (eq twosided-fn 'org-drill-present-two-sided-card))
    (should (eq simple-fn 'org-drill-present-simple-card))))

(provide 'test-card-type-twosided)
;;; test-card-type-twosided.el ends here
