;;; test-org-drill-utilities-and-leitner.el --- Tests for list utils, hide helpers, Leitner promote  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for a grab-bag of small helpers:
;;
;; - `org-drill-swap' / `org-drill-shuffle': in-place list utilities
;; - `org-drill-pop-random': macro that removes a random element
;; - `org-drill-hide-comments' / `org-drill-hide-drawers': overlay hiders
;;   for buffer noise the user shouldn't see during a drill
;; - `org-drill-leitner-promote': move a card forward in the Leitner
;;   box system, including the special "graduate to drill" jump at box 5

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defun count-overlays-of-category (cat)
  (let ((n 0))
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql cat (overlay-get ovl 'category))
        (cl-incf n)))
    n))

;;;; org-drill-swap

(ert-deftest test-org-drill-swap-normal-distinct-indices ()
  "Swapping indices 0 and 2 in (a b c d) gives (c b a d)."
  (let ((lst (list 'a 'b 'c 'd)))
    (org-drill-swap lst 0 2)
    (should (equal '(c b a d) lst))))

(ert-deftest test-org-drill-swap-same-index-is-noop ()
  "Swapping an index with itself leaves the list unchanged."
  (let ((lst (list 'a 'b 'c)))
    (org-drill-swap lst 1 1)
    (should (equal '(a b c) lst))))

(ert-deftest test-org-drill-swap-end-and-start ()
  (let ((lst (list 1 2 3 4 5)))
    (org-drill-swap lst 0 4)
    (should (equal '(5 2 3 4 1) lst))))

;;;; org-drill-shuffle

(ert-deftest test-org-drill-shuffle-preserves-element-set ()
  "After shuffling, the multiset of elements is unchanged."
  (let* ((original '(1 2 3 4 5 6 7 8 9 10))
         (shuffled (org-drill-shuffle (copy-sequence original))))
    (should (equal (sort (copy-sequence shuffled) #'<)
                   original))))

(ert-deftest test-org-drill-shuffle-empty-list-is-empty ()
  (should (null (org-drill-shuffle nil)))
  (should (equal '() (org-drill-shuffle (list)))))

(ert-deftest test-org-drill-shuffle-single-element-unchanged ()
  (should (equal '(42) (org-drill-shuffle (list 42)))))

;;;; org-drill-pop-random (macro)

(ert-deftest test-org-drill-pop-random-removes-one-element ()
  (let ((lst (list 'a 'b 'c 'd)))
    (let ((popped (org-drill-pop-random lst)))
      (should (memq popped '(a b c d)))
      (should (= 3 (length lst)))
      (should-not (memq popped lst)))))

(ert-deftest test-org-drill-pop-random-on-nil-returns-nil ()
  (let ((lst nil))
    (should (null (org-drill-pop-random lst)))))

(ert-deftest test-org-drill-pop-random-on-singleton-empties-list ()
  (let ((lst (list 'only)))
    (should (eq 'only (org-drill-pop-random lst)))
    (should (null lst))))

;;;; org-drill-hide-comments

(ert-deftest test-org-drill-hide-comments-hides-each-comment-line ()
  "Each `^#' comment line gets its own hidden-text overlay."
  (with-temp-buffer
    (insert "# first comment\n# second comment\nactual body\n")
    (goto-char (point-min))
    (org-drill-hide-comments)
    (should (= 2 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

(ert-deftest test-org-drill-hide-comments-no-comments-no-overlays ()
  (with-temp-buffer
    (insert "no comments here\njust regular text\n")
    (goto-char (point-min))
    (org-drill-hide-comments)
    (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

;;;; org-drill-hide-drawers

(ert-deftest test-org-drill-hide-drawers-hides-properties-drawer ()
  "PROPERTIES drawer becomes a hidden-text overlay during drilling."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n:PROPERTIES:\n:ID: x\n:END:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-hide-drawers)
      (should (>= (count-overlays-of-category 'org-drill-hidden-text-overlay) 1)))))

(ert-deftest test-org-drill-hide-drawers-handles-multiple-drawers ()
  "Multiple drawers each get an overlay."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\n:PROPERTIES:\n:ID: x\n:END:\n:LOGBOOK:\nentry\n:END:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-hide-drawers)
      (should (>= (count-overlays-of-category 'org-drill-hidden-text-overlay) 2)))))

(ert-deftest test-org-drill-hide-drawers-no-drawer-no-overlay ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nbody only\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-hide-drawers)
      (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay))))))

;;;; org-drill-leitner-promote

(ert-deftest test-org-drill-leitner-promote-non-graduating-increments-box ()
  "Promotion from box 1 → 2, 2 → 3, etc.  No tag change, just a property bump."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 1\n:END:\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-leitner-promote 1)
      (should (equal "2" (org-entry-get (point) "DRILL_LEITNER_BOX")))
      ;; Tags unchanged.
      (should (member "leitner" (org-get-tags nil t)))
      (should-not (member "drill" (org-get-tags nil t))))))

(ert-deftest test-org-drill-leitner-promote-from-box-5-graduates-to-drill ()
  "Promotion from box 5 toggles `leitner' off and (with the flag) `drill' on."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 5\n:END:\n")
      (org-mode)
      (goto-char (point-min))
      (let ((org-drill-leitner-promote-to-drill-p t)
            (org-drill-leitner-completed 0))
        (org-drill-leitner-promote 5)
        (should-not (member "leitner" (org-get-tags nil t)))
        (should (member "drill" (org-get-tags nil t)))
        (should (= 1 org-drill-leitner-completed))))))

(ert-deftest test-org-drill-leitner-promote-from-box-5-without-promote-flag ()
  "Without the promote-to-drill flag, only `leitner' tag is removed."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 5\n:END:\n")
      (org-mode)
      (goto-char (point-min))
      (let ((org-drill-leitner-promote-to-drill-p nil)
            (org-drill-leitner-completed 0))
        (org-drill-leitner-promote 5)
        (should-not (member "leitner" (org-get-tags nil t)))
        (should-not (member "drill" (org-get-tags nil t)))
        (should (= 1 org-drill-leitner-completed))))))

(provide 'test-org-drill-utilities-and-leitner)

;;; test-org-drill-utilities-and-leitner.el ends here
