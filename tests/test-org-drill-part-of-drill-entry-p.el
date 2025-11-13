;;; test-org-drill-part-of-drill-entry-p.el --- Tests for org-drill-part-of-drill-entry-p

;;; Commentary:
;; Unit tests for org-drill-part-of-drill-entry-p, which determines if point
;; is at a drill entry heading OR within a subheading that inherits the drill tag.
;;
;; This function differs from org-drill-entry-p in that it checks BOTH:
;; 1. Direct drill tag on current heading (via org-drill-entry-p)
;; 2. Inherited drill tag from parent headings (via org-get-tags)

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data

(defconst test-org-drill-part-of-drill-entry-p-parent-drill
  "* Parent heading :drill:

Parent content.

** Child heading

Child content.

*** Grandchild heading

Grandchild content."
  "Drill entry with nested children that inherit the tag.")

(defconst test-org-drill-part-of-drill-entry-p-no-drill
  "* Parent heading

Parent content.

** Child heading

Child content."
  "Headings without drill tag.")

(defconst test-org-drill-part-of-drill-entry-p-child-has-drill
  "* Parent heading

Parent content.

** Child heading :drill:

Child content."
  "Child has drill tag, parent doesn't.")

(defconst test-org-drill-part-of-drill-entry-p-deep-nesting
  "* Level 1 :drill:

Content 1.

** Level 2

Content 2.

*** Level 3

Content 3.

**** Level 4

Content 4."
  "Deeply nested structure with drill tag on top level.")

;;; Helper Functions

(defun test-org-drill-part-of-drill-entry-p--with-org-buffer (content callback)
  "Execute CALLBACK in temporary org-mode buffer with CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (funcall callback)))

;;; Normal Cases

(ert-deftest test-org-drill-part-of-drill-entry-p-normal-at-parent-returns-true ()
  "Test that function returns true when at parent heading with drill tag."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; Point at parent heading
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-normal-at-child-returns-true ()
  "Test that function returns true when at child heading that inherits drill tag."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; Move to child heading
     (re-search-forward "^\\*\\* Child heading")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-normal-no-drill-returns-nil ()
  "Test that function returns nil when no drill tag present or inherited."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-no-drill
   (lambda ()
     (should-not (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-normal-child-not-parent-has-drill ()
  "Test when child has drill tag but parent doesn't.
Child heading itself should return true."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-child-has-drill
   (lambda ()
     ;; Move to child heading with drill tag
     (re-search-forward "^\\*\\* Child heading")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-normal-parent-no-drill-returns-nil ()
  "Test that parent without drill tag returns nil."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-child-has-drill
   (lambda ()
     ;; At parent heading (no drill tag)
     (should-not (org-drill-part-of-drill-entry-p)))))

;;; Boundary Cases

(ert-deftest test-org-drill-part-of-drill-entry-p-boundary-grandchild-inherits ()
  "Test that grandchild heading inherits drill tag from grandparent."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; Move to grandchild heading
     (re-search-forward "^\\*\\*\\* Grandchild heading")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-boundary-deep-nesting-all-inherit ()
  "Test that deeply nested headings all inherit drill tag.
All levels 2, 3, and 4 should return true."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-deep-nesting
   (lambda ()
     ;; Test level 2
     (re-search-forward "^\\*\\* Level 2")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p))

     ;; Test level 3
     (re-search-forward "^\\*\\*\\* Level 3")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p))

     ;; Test level 4
     (re-search-forward "^\\*\\*\\*\\* Level 4")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-boundary-point-in-child-body ()
  "Test behavior when point is in child body text.
Should still detect inherited drill tag."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; Move to child body content
     (re-search-forward "Child content")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-boundary-empty-child-heading ()
  "Test child heading with no content still inherits drill tag."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   "* Parent :drill:\n** Child\n"
   (lambda ()
     (re-search-forward "^\\*\\* Child")
     (beginning-of-line)
     (should (org-drill-part-of-drill-entry-p)))))

;;; Error Cases

(ert-deftest test-org-drill-part-of-drill-entry-p-error-empty-buffer ()
  "Test behavior in empty buffer.
Should not error, just return nil."
  (with-temp-buffer
    (org-mode)
    (should-not (org-drill-part-of-drill-entry-p))))

(ert-deftest test-org-drill-part-of-drill-entry-p-error-no-headings ()
  "Test behavior in buffer with only text, no headings."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   "Just some text without any headings."
   (lambda ()
     (should-not (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-error-point-before-first-heading ()
  "Test behavior when point is before any headings."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   "Some preamble text\n\n* Heading :drill:\n\nContent"
   (lambda ()
     ;; Point at preamble
     (goto-char (point-min))
     (should-not (org-drill-part-of-drill-entry-p)))))

;;; Comparison Tests (org-drill-entry-p vs org-drill-part-of-drill-entry-p)

(ert-deftest test-org-drill-part-of-drill-entry-p-comparison-child-differs ()
  "Test that org-drill-entry-p and org-drill-part-of-drill-entry-p differ at child.
At child heading: entry-p returns nil, part-of returns true."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; Move to child heading
     (re-search-forward "^\\*\\* Child heading")
     (beginning-of-line)
     ;; org-drill-entry-p should be nil (no direct tag)
     (should-not (org-drill-entry-p))
     ;; org-drill-part-of-drill-entry-p should be true (inherited tag)
     (should (org-drill-part-of-drill-entry-p)))))

(ert-deftest test-org-drill-part-of-drill-entry-p-comparison-parent-same ()
  "Test that both functions return true at parent heading with drill tag."
  (test-org-drill-part-of-drill-entry-p--with-org-buffer
   test-org-drill-part-of-drill-entry-p-parent-drill
   (lambda ()
     ;; At parent heading
     (should (org-drill-entry-p))
     (should (org-drill-part-of-drill-entry-p)))))

(provide 'test-org-drill-part-of-drill-entry-p)
;;; test-org-drill-part-of-drill-entry-p.el ends here
