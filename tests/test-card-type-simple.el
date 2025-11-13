;;; test-card-type-simple.el --- Tests for simple card type

;;; Commentary:
;; Tests for the simple card type in org-drill.
;;
;; Simple cards are the default card type. They show the question
;; (entry body) and hide subheadings. The answer is revealed in
;; subheadings or can be specified in DRILL_ANSWER property.
;;
;; Card type: nil or "simple"
;; Presentation function: org-drill-present-simple-card

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data

(defconst test-card-type-simple-basic-card
  "* Simple Card :drill:

Question: What is the capital of France?

** Answer

Paris
"
  "Basic simple card with question and answer in subheading.")

(defconst test-card-type-simple-with-property
  "* Simple Card with Property :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: simple
:DRILL_ANSWER: The answer is 42
:END:

Question: What is the meaning of life?
"
  "Simple card with DRILL_ANSWER property.")

(defconst test-card-type-simple-multiple-subheadings
  "* Card with Multiple Subheadings :drill:

Main question content here.

** First Answer

First part of answer.

** Second Answer

Second part of answer.

** Notes

Additional notes that should be hidden.
"
  "Simple card with multiple subheadings.")

(defconst test-card-type-simple-no-answer
  "* Card without Answer :drill:

Just a question, no answer subheading.
"
  "Simple card with no answer subheading.")

;;; Helper Functions

(defun test-card-type-simple--with-card (content callback)
  "Execute CALLBACK in temp buffer with drill card CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (funcall callback)))

(defun test-card-type-simple--card-has-type-p (card-type)
  "Check if current entry has CARD-TYPE."
  (let ((entry-type (org-entry-get (point) "DRILL_CARD_TYPE" t)))
    (or (and (null card-type) (null entry-type))
        (equal card-type entry-type))))

;;; Normal Cases - Card Recognition

(ert-deftest test-card-type-simple-normal-default-card-type ()
  "Test that cards without DRILL_CARD_TYPE are treated as simple cards."
  (test-card-type-simple--with-card
   test-card-type-simple-basic-card
   (lambda ()
     (should (org-drill-entry-p))
     (should (null (org-entry-get (point) "DRILL_CARD_TYPE"))))))

(ert-deftest test-card-type-simple-normal-explicit-simple-type ()
  "Test card with explicit 'simple' type in DRILL_CARD_TYPE property."
  (test-card-type-simple--with-card
   test-card-type-simple-with-property
   (lambda ()
     (should (org-drill-entry-p))
     (should (equal "simple" (org-entry-get (point) "DRILL_CARD_TYPE"))))))

(ert-deftest test-card-type-simple-normal-has-presentation-function ()
  "Test that simple card type has correct presentation function registered."
  (let ((simple-entry (assoc "simple" org-drill-card-type-alist))
        (nil-entry (assoc nil org-drill-card-type-alist)))
    ;; Both nil and "simple" should map to org-drill-present-simple-card
    (should simple-entry)
    (should nil-entry)
    (should (eq (cadr simple-entry) 'org-drill-present-simple-card))
    (should (eq (cadr nil-entry) 'org-drill-present-simple-card))))

;;; Normal Cases - Card Structure

(ert-deftest test-card-type-simple-normal-single-answer-subheading ()
  "Test simple card with single answer subheading."
  (test-card-type-simple--with-card
   test-card-type-simple-basic-card
   (lambda ()
     (should (org-drill-entry-p))
     ;; Should have at least one subheading
     (save-excursion
       (org-back-to-heading)
       (let ((has-subheading nil))
         (org-map-entries
          (lambda () (setq has-subheading t))
          nil 'tree)
         (should has-subheading))))))

(ert-deftest test-card-type-simple-normal-multiple-answer-subheadings ()
  "Test simple card with multiple subheadings."
  (test-card-type-simple--with-card
   test-card-type-simple-multiple-subheadings
   (lambda ()
     (should (org-drill-entry-p))
     ;; Count subheadings
     (save-excursion
       (org-back-to-heading)
       (let ((subheading-count 0))
         (org-map-entries
          (lambda ()
            (when (> (org-current-level) 1)
              (cl-incf subheading-count)))
          nil 'tree)
         (should (>= subheading-count 3)))))))

;;; Normal Cases - DRILL_ANSWER Property

(ert-deftest test-card-type-simple-normal-drill-answer-property ()
  "Test simple card with DRILL_ANSWER property."
  (test-card-type-simple--with-card
   test-card-type-simple-with-property
   (lambda ()
     (should (org-drill-entry-p))
     (let ((answer (org-entry-get (point) "DRILL_ANSWER")))
       (should answer)
       (should (string-match-p "42" answer))))))

;;; Boundary Cases

(ert-deftest test-card-type-simple-boundary-no-answer-subheading ()
  "Test simple card without answer subheading.
Card should still be valid, answer would come from DRILL_ANSWER property."
  (test-card-type-simple--with-card
   test-card-type-simple-no-answer
   (lambda ()
     (should (org-drill-entry-p))
     ;; No subheadings
     (save-excursion
       (org-back-to-heading)
       (let ((has-subheading nil))
         (org-map-entries
          (lambda ()
            (when (> (org-current-level) 1)
              (setq has-subheading t)))
          nil 'tree)
         (should-not has-subheading))))))

(ert-deftest test-card-type-simple-boundary-empty-question ()
  "Test simple card with empty question body."
  (let ((content "* Empty Question :drill:\n\n** Answer\n\nSome answer\n"))
    (test-card-type-simple--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))))))

(ert-deftest test-card-type-simple-boundary-very-long-question ()
  "Test simple card with very long question text."
  (let* ((long-text (make-string 5000 ?x))
         (content (format "* Long Question :drill:\n\n%s\n\n** Answer\n\nYes\n" long-text)))
    (test-card-type-simple--with-card
     content
     (lambda ()
       (should (org-drill-entry-p))))))

;;; Card Type Lookup

(ert-deftest test-card-type-simple-normal-card-type-lookup ()
  "Test that card type lookup works for simple cards."
  (let ((presentation-fn (cadr (assoc nil org-drill-card-type-alist))))
    (should (eq presentation-fn 'org-drill-present-simple-card)))
  (let ((presentation-fn (cadr (assoc "simple" org-drill-card-type-alist))))
    (should (eq presentation-fn 'org-drill-present-simple-card))))

(ert-deftest test-card-type-simple-normal-default-vs-explicit ()
  "Test that default (nil) and explicit 'simple' type behave identically."
  (let ((default-fn (cadr (assoc nil org-drill-card-type-alist)))
        (explicit-fn (cadr (assoc "simple" org-drill-card-type-alist))))
    (should (eq default-fn explicit-fn))
    (should (eq default-fn 'org-drill-present-simple-card))))

(provide 'test-card-type-simple)
;;; test-card-type-simple.el ends here
