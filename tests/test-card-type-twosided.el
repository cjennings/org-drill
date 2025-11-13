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

;;; Aggressive Edge Cases

(ert-deftest test-card-type-twosided-edge-unicode-in-sides ()
  "Test two-sided card with Unicode in both sides.
Unicode characters should be preserved correctly."
  (let ((content "* Unicode Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** English

Hello

** 日本語

こんにちは
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-very-long-side-content ()
  "Test two-sided card with very long content in sides.
Should handle large amounts of text without issues."
  (let* ((long-text (make-string 5000 ?x))
         (content (format "* Long Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side A

%s

** Side B

%s
" long-text long-text)))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-lists ()
  "Test two-sided card with org-mode lists in sides."
  (let ((content "* Lists Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question

What are primary colors?

** Answer

- Red
- Blue
- Yellow
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-tables ()
  "Test two-sided card with org-mode tables in sides."
  (let ((content "* Table Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question

Temperature conversion?

** Table

| Celsius | Fahrenheit |
|---------+------------|
|       0 |         32 |
|     100 |        212 |
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-code-blocks ()
  "Test two-sided card with source code blocks in sides."
  (let ((content "* Code Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question

Python hello world?

** Code

#+BEGIN_SRC python
print(\"Hello, World!\")
#+END_SRC
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-links ()
  "Test two-sided card with org-mode links in sides."
  (let ((content "* Links Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Term

Emacs

** Definition

A [[https://www.gnu.org/software/emacs/][powerful text editor]].
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-formatting ()
  "Test two-sided card with org-mode text formatting in sides."
  (let ((content "* Formatted Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question

What is *bold*, /italic/, =code=?

** Answer

These are *org-mode* /text/ =formatting= options.
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-tags ()
  "Test two-sided card where sides have tags.
Tags on subheadings should not interfere with card."
  (let ((content "* Card with Tagged Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question :important:

What is the answer?

** Answer :note:

42
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-nested-subheadings-in-sides ()
  "Test two-sided card with nested subheadings within sides.
Nested content should be part of the side."
  (let ((content "* Nested Content :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side A

Content A

*** Nested Under A

More details.

** Side B

Content B
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-empty-headings ()
  "Test two-sided card where heading text is minimal."
  (let ((content "* Minimal Headings :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** A

Full content for A.

** B

Full content for B.
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-special-characters-in-sides ()
  "Test two-sided card with special characters in side content."
  (let ((content "* Special Chars :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Question

What are these: @#$%^&*()?

** Answer

Special programming characters.
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-many-extra-sides ()
  "Test two-sided card with many subheadings (10+).
Only first two should be used for presentation."
  (let ((content "* Many Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side 1

Content 1

** Side 2

Content 2

** Side 3

Extra

** Side 4

Extra

** Side 5

Extra

** Side 6

Extra

** Side 7

Extra

** Side 8

Extra

** Side 9

Extra

** Side 10

Extra
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (>= (test-card-type-twosided--count-subheadings) 10))))))

(ert-deftest test-card-type-twosided-edge-no-subheadings ()
  "Test two-sided card with no subheadings at all.
Card is invalid but should not crash."
  (let ((content "* No Sides :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

Just main body, no subheadings.
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 0 (test-card-type-twosided--count-subheadings)))))))

(ert-deftest test-card-type-twosided-edge-sides-with-drawers ()
  "Test two-sided card with drawers in sides.
Drawers should be preserved."
  (let ((content "* Drawer Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** Side A

:LOGBOOK:
- Note taken on [2024-01-01]
:END:

Content A

** Side B

Content B
"))
    (test-card-type-twosided--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))
       (should (= 2 (test-card-type-twosided--count-subheadings)))))))

(provide 'test-card-type-twosided)
;;; test-card-type-twosided.el ends here
