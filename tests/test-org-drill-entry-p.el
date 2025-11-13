;;; test-org-drill-entry-p.el --- Tests for org-drill-entry-p function

;;; Commentary:
;; Unit tests for org-drill-entry-p, which determines if point is at a
;; drill entry heading (not a subheading within a drill entry).
;;
;; The function checks for the drill tag at the current heading only,
;; not inherited tags. Use org-drill-part-of-drill-entry-p for inherited tags.

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data

(defconst test-org-drill-entry-p-simple-entry
  "* Heading with drill tag :drill:

Body content here."
  "Simple drill entry with drill tag on main heading.")

(defconst test-org-drill-entry-p-no-tag
  "* Heading without drill tag

Body content here."
  "Heading without any drill tag.")

(defconst test-org-drill-entry-p-nested-entry
  "* Parent heading :drill:

Parent content.

** Child heading

Child content."
  "Drill entry with nested child heading.")

(defconst test-org-drill-entry-p-multiple-tags
  "* Heading :drill:important:review:

Body with multiple tags."
  "Drill entry with multiple tags including drill.")

(defconst test-org-drill-entry-p-custom-tag
  "* Heading :customtag:

Body with custom tag."
  "Heading with custom tag (not drill).")

;;; Helper Functions

(defun test-org-drill-entry-p--with-org-buffer (content callback)
  "Execute CALLBACK in temporary org-mode buffer with CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (funcall callback)))

;;; Normal Cases

(ert-deftest test-org-drill-entry-p-normal-valid-tag-returns-true ()
  "Test that heading with drill tag is identified as drill entry."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-simple-entry
   (lambda ()
     (should (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-normal-no-tag-returns-nil ()
  "Test that heading without drill tag returns nil."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-no-tag
   (lambda ()
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-normal-multiple-tags-returns-true ()
  "Test that heading with multiple tags including drill is identified."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-multiple-tags
   (lambda ()
     (should (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-normal-at-parent-heading-returns-true ()
  "Test that function works when point is at parent drill heading."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-nested-entry
   (lambda ()
     ;; Point should be at parent heading after goto-char (point-min)
     (should (org-drill-entry-p)))))

;;; Boundary Cases

(ert-deftest test-org-drill-entry-p-boundary-at-child-heading-returns-nil ()
  "Test that child heading of drill entry returns nil.
Only the heading with the actual drill tag should return true."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-nested-entry
   (lambda ()
     ;; Move to child heading
     (re-search-forward "^\\*\\* Child heading")
     (beginning-of-line)
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-boundary-empty-heading-no-tag-returns-nil ()
  "Test that empty heading without tags returns nil."
  (test-org-drill-entry-p--with-org-buffer
   "* Empty heading\n"
   (lambda ()
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-boundary-heading-with-only-whitespace-tag ()
  "Test heading with whitespace around tags."
  (test-org-drill-entry-p--with-org-buffer
   "* Heading   :drill:   \n\nContent"
   (lambda ()
     (should (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-boundary-custom-tag-not-drill ()
  "Test that custom tag (not drill) returns nil."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-custom-tag
   (lambda ()
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-boundary-drill-substring-in-other-tag ()
  "Test that drill as substring of another tag does not match.
Tag 'drilling' should not be identified as drill entry."
  (test-org-drill-entry-p--with-org-buffer
   "* Heading :drilling:\n\nContent"
   (lambda ()
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-boundary-case-sensitive-tag ()
  "Test that drill tag matching is case-sensitive.
Tag 'DRILL' should not match 'drill'."
  (test-org-drill-entry-p--with-org-buffer
   "* Heading :DRILL:\n\nContent"
   (lambda ()
     ;; org-mode tags are case-sensitive by default
     (should-not (org-drill-entry-p)))))

;;; Error Cases

(ert-deftest test-org-drill-entry-p-error-point-in-body-text ()
  "Test behavior when point is in body text, not at heading.
Function should check current heading context."
  (test-org-drill-entry-p--with-org-buffer
   test-org-drill-entry-p-simple-entry
   (lambda ()
     ;; Move to body text
     (re-search-forward "Body content")
     (beginning-of-line)
     ;; Even though we're in a drill entry's body, org-drill-entry-p
     ;; checks the current heading when point is in body
     (should (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-error-empty-buffer ()
  "Test behavior in empty buffer.
Should not error, just return nil."
  (with-temp-buffer
    (org-mode)
    (should-not (org-drill-entry-p))))

(ert-deftest test-org-drill-entry-p-error-buffer-without-headings ()
  "Test behavior in buffer with only text, no headings."
  (test-org-drill-entry-p--with-org-buffer
   "Just some text without any headings."
   (lambda ()
     (should-not (org-drill-entry-p)))))

(ert-deftest test-org-drill-entry-p-error-malformed-heading ()
  "Test behavior with malformed heading syntax.
Single asterisk should still work as heading."
  (test-org-drill-entry-p--with-org-buffer
   "* Malformed :drill:\nNo blank line before content"
   (lambda ()
     (should (org-drill-entry-p)))))

(provide 'test-org-drill-entry-p)
;;; test-org-drill-entry-p.el ends here
