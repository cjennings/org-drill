;;; test-org-drill-entry-empty-p.el --- Tests for org-drill-entry-empty-p  -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for `org-drill-entry-empty-p'.
;;
;; Upstream issue #13 (kqr, 2019-07-22) reported that drill entries
;; whose body lives inside a child sub-heading were being skipped as
;; empty.  Example: a question stored in the heading text, with the
;; answer inside `** The Answer'.
;;
;; Root cause: the function used `(outline-next-heading)' to compute
;; the search-end bound.  `outline-next-heading' lands on the next
;; heading at any level, including a child — so the search range was
;; metadata-end up to the child heading's start, which excluded the
;; child's body text.  The function returned t (empty) on entries
;; that clearly had content.
;;
;; Fix: use `org-end-of-subtree' for the bound.  That covers the
;; whole subtree (including children) and degrades gracefully at the
;; last heading in the buffer (where `outline-forward-same-level'
;; would have errored).

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-org-fixture (content &rest body)
  "Run BODY in a temp org-mode buffer containing CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

;;;; Normal cases

(ert-deftest test-org-drill-entry-empty-p-normal-empty-entry-returns-t ()
  "An entry with only metadata and no body is empty."
  (with-org-fixture "* Question :drill:
:PROPERTIES:
:ID:       abc
:END:
"
    (should (org-drill-entry-empty-p))))

(ert-deftest test-org-drill-entry-empty-p-normal-direct-body-returns-nil ()
  "An entry with body text directly under the heading is not empty."
  (with-org-fixture "* Question :drill:
:PROPERTIES:
:ID:       abc
:END:

The answer lives right here.
"
    (should-not (org-drill-entry-empty-p))))

;;;; Boundary / regression — issue #13

(ert-deftest test-org-drill-entry-empty-p-regression-body-in-child-not-empty ()
  "Issue #13: an entry with its answer inside a child sub-heading is not empty.

This is the exact shape kqr reported.  The question lives in the
heading text and the answer lives inside `** The Answer'.  Pre-fix,
this returned t because `outline-next-heading' set the search bound
to the child's start, excluding the child's body."
  (with-org-fixture "* Entry question? :drill:
SCHEDULED: <2019-04-05 Fri>
:PROPERTIES:
:ID:       def
:END:

** The Answer

Some text
"
    (should-not (org-drill-entry-empty-p))))

(ert-deftest test-org-drill-entry-empty-p-boundary-last-entry-in-buffer ()
  "An entry that's the last in the buffer is handled without error.

`outline-forward-same-level' would error at the last heading; the fix
uses `org-end-of-subtree' which handles end-of-buffer gracefully."
  (with-org-fixture "* Final question :drill:
:PROPERTIES:
:ID:       jkl
:END:

The answer.
"
    (should-not (org-drill-entry-empty-p))))

(ert-deftest test-org-drill-entry-empty-p-boundary-followed-by-sibling ()
  "An empty entry followed by a non-empty sibling stays empty.

Sanity check that the wider search bound doesn't bleed into siblings."
  (with-org-fixture "* First :drill:
:PROPERTIES:
:ID:       mno
:END:

* Second :drill:
:PROPERTIES:
:ID:       pqr
:END:

This one has body.
"
    (should (org-drill-entry-empty-p))))

;;;; Error cases

(ert-deftest test-org-drill-entry-empty-p-normal-deeply-nested-content ()
  "Content several levels deep under the entry is still found."
  (with-org-fixture "* Question :drill:
:PROPERTIES:
:ID:       stu
:END:

** Section A
*** Subsection
**** Sub-subsection

Deeply nested answer.
"
    (should-not (org-drill-entry-empty-p))))

(provide 'test-org-drill-entry-empty-p)

;;; test-org-drill-entry-empty-p.el ends here
