;;; test-org-drill-cloze-regex-single-line.el --- Regression for #38  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #38 (2021-01).  When `org-drill-use-visible-cloze-face-p'
;; was non-nil, the cloze face leaked onto unrelated org headings.
;; Root cause: the cloze regex's inner character class included
;; `[:cntrl:]' / `[:space:]' which contain newline.  A stray `[' could
;; match all the way to a `]' several lines later, covering org
;; headings in between with the visible-cloze face.
;;
;; Fix: restrict the inner match to non-newline characters so a cloze
;; stays within a single line — the intended scope.

;;; Code:

(require 'ert)
(require 'org-drill)

;;;; Regression — #38

(ert-deftest test-cloze-regex-doesnt-span-newlines ()
  "A `[' on one line followed by `]' two lines later should NOT match
as a single cloze.  Pre-fix this matched the entire span, which is
what caused the heading-face bleed."
  (let ((re (org-drill--compute-cloze-regexp)))
    (with-temp-buffer
      (insert "Line one [some text\nLine two\nLine three] more")
      (goto-char (point-min))
      (should-not (re-search-forward re nil t)))))

(ert-deftest test-cloze-regex-still-matches-single-line-cloze ()
  "Single-line clozes still match — the fix doesn't break the happy path."
  (let ((re (org-drill--compute-cloze-regexp)))
    (with-temp-buffer
      (insert "Capital of France is [Paris].")
      (goto-char (point-min))
      (should (re-search-forward re nil t))
      (should (equal "[Paris]" (match-string 0))))))

(ert-deftest test-cloze-regex-multiline-buffer-finds-only-line-bounded-cloze ()
  "In a buffer with multiple lines and a single-line cloze on each,
each cloze is matched independently — no spans across lines."
  (let ((re (org-drill--compute-cloze-regexp)))
    (with-temp-buffer
      (insert "Line A: [foo]\n* Heading\nLine B: [bar]\n")
      (goto-char (point-min))
      (let ((matches nil))
        (while (re-search-forward re nil t)
          (push (match-string 0) matches))
        (should (member "[foo]" matches))
        (should (member "[bar]" matches))
        ;; And exactly two matches — no spurious span across the heading.
        (should (= 2 (length matches)))))))

(provide 'test-org-drill-cloze-regex-single-line)

;;; test-org-drill-cloze-regex-single-line.el ends here
