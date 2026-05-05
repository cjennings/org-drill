;;; test-org-drill-hide-show.el --- Tests for cloze-hide / replace-text overlay functions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the buffer-overlay machinery that drives card presentation:
;;
;; - `org-drill-hide-region': make a single hidden-text overlay
;; - `org-drill-unhide-text': remove all hidden-text overlays
;; - `org-drill-hide-clozed-text' / `unhide-clozed-text': hide every
;;   `[cloze]' span in the buffer (using `org-drill-cloze-overlay-defaults'
;;   category).  This is what makes the answer disappear when the card
;;   is presented.
;; - `org-drill-hide-cloze-hints': hide just the `||hint' tail of each
;;   cloze span without hiding the rest.
;; - `org-drill-replace-entry-text' / `unreplace-entry-text': overlay
;;   the entry body with a placeholder string.
;;
;; The user-facing contract: when the card is shown, the answer is
;; hidden behind an overlay.  When the answer is revealed, every
;; overlay disappears.  Round-trip cleanliness matters.

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-cloze-buffer (content &rest body)
  "Run BODY in a temp org buffer with CONTENT, point at start, fontification
caches refreshed so the cloze regex picks up current delimiter values."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       (setq-local org-drill-cloze-regexp (org-drill--compute-cloze-regexp))
       ,@body)))

(defun count-overlays-of-category (cat)
  "Count overlays in current buffer whose `category' property equals CAT."
  (let ((n 0))
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql cat (overlay-get ovl 'category))
        (cl-incf n)))
    n))

;;;; org-drill-hide-region

(ert-deftest test-org-drill-hide-region-creates-one-overlay ()
  (with-cloze-buffer "Some plain text here."
    (org-drill-hide-region 6 11)
    (should (= 1 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

(ert-deftest test-org-drill-hide-region-respects-given-bounds ()
  (with-cloze-buffer "Some plain text here."
    (org-drill-hide-region 6 11)
    (let ((ovl (car (overlays-at 8))))
      (should (= 6 (overlay-start ovl)))
      (should (= 11 (overlay-end ovl))))))

(ert-deftest test-org-drill-hide-region-with-text-shows-replacement ()
  "When a TEXT arg is supplied, the overlay's `display' shows that string
instead of being invisible."
  (with-cloze-buffer "Some plain text here."
    (org-drill-hide-region 6 11 "[REDACTED]")
    (let ((ovl (car (overlays-at 8))))
      (should (equal "[REDACTED]" (overlay-get ovl 'display))))))

(ert-deftest test-org-drill-hide-region-without-text-uses-invisible ()
  "Without a TEXT arg, the overlay relies on the category's invisible
property — no `display' string is set."
  (with-cloze-buffer "Some plain text here."
    (org-drill-hide-region 6 11)
    (let ((ovl (car (overlays-at 8))))
      (should (null (overlay-get ovl 'display))))))

;;;; org-drill-unhide-text

(ert-deftest test-org-drill-unhide-text-removes-hidden-overlays ()
  (with-cloze-buffer "Some text and more."
    (org-drill-hide-region 1 5)
    (org-drill-hide-region 11 15)
    (should (= 2 (count-overlays-of-category 'org-drill-hidden-text-overlay)))
    (org-drill-unhide-text)
    (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

(ert-deftest test-org-drill-unhide-text-leaves-other-categories-alone ()
  "`unhide-text' only removes hidden-text-overlay; other categories survive."
  (with-cloze-buffer "Some text and more."
    (let ((ovl (make-overlay 1 5)))
      (overlay-put ovl 'category 'unrelated-overlay))
    (org-drill-hide-region 11 15)
    (org-drill-unhide-text)
    (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay)))
    (should (= 1 (count-overlays-of-category 'unrelated-overlay)))))

(ert-deftest test-org-drill-unhide-text-on-clean-buffer-is-noop ()
  (with-cloze-buffer "No overlays here."
    (org-drill-unhide-text)
    (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

;;;; org-drill-hide-clozed-text — full cloze hiding

(ert-deftest test-org-drill-hide-clozed-text-hides-every-cloze ()
  "Two clozes in the buffer → two cloze-overlay-defaults overlays after hiding."
  (with-cloze-buffer "Capital of France is [Paris] and Italy is [Rome]."
    (org-drill-hide-clozed-text)
    (should (= 2 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

(ert-deftest test-org-drill-hide-clozed-text-no-cloze-no-overlays ()
  (with-cloze-buffer "Plain prose without any cloze syntax."
    (org-drill-hide-clozed-text)
    (should (= 0 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

(ert-deftest test-org-drill-hide-clozed-text-with-hint-still-hides ()
  "A cloze with a hint `[hidden||hint]' is hidden the same way."
  (with-cloze-buffer "Capital of France is [Paris||a city of light]."
    (org-drill-hide-clozed-text)
    (should (= 1 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

;;;; org-drill-unhide-clozed-text

(ert-deftest test-org-drill-unhide-clozed-text-removes-cloze-overlays ()
  (with-cloze-buffer "Capital of France is [Paris] and Italy is [Rome]."
    (org-drill-hide-clozed-text)
    (should (= 2 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))
    (org-drill-unhide-clozed-text)
    (should (= 0 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

;;;; org-drill-hide-cloze-hints

(ert-deftest test-org-drill-hide-cloze-hints-hides-only-hint-portion ()
  "On `[hidden||hint]', only the `||hint' portion gets a hidden-text
overlay — the `[hidden' part stays visible."
  (with-cloze-buffer "Capital is [Paris||a city of light]."
    (org-drill-hide-cloze-hints)
    (should (= 1 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

(ert-deftest test-org-drill-hide-cloze-hints-no-hint-no-overlay ()
  "A cloze with no hint section is left alone."
  (with-cloze-buffer "Capital is [Paris]."
    (org-drill-hide-cloze-hints)
    (should (= 0 (count-overlays-of-category 'org-drill-hidden-text-overlay)))))

;;;; Round-trip — show then unhide cleanly

(ert-deftest test-org-drill-cloze-roundtrip-hide-then-unhide-leaves-no-overlays ()
  "Hiding clozes then unhiding leaves the buffer free of any cloze overlays.
This is the contract that lets a card be re-shown after the user reveals
the answer."
  (with-cloze-buffer "[A] then [B] then [C]."
    (org-drill-hide-clozed-text)
    (should (= 3 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))
    (org-drill-unhide-clozed-text)
    (should (= 0 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

;;;; org-drill-replace-entry-text / unreplace-entry-text

(ert-deftest test-org-drill-replace-entry-text-creates-replaced-overlay ()
  "Replacing entry text adds one `org-drill-replaced-text-overlay'."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nThe body of the entry.\n* Other heading\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-replace-entry-text "[hidden]")
      (should (= 1 (count-overlays-of-category 'org-drill-replaced-text-overlay))))))

(ert-deftest test-org-drill-replace-entry-text-displays-given-string ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nThe body of the entry.\n* Other heading\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-replace-entry-text "[hidden]")
      (let ((ovl (cl-find-if
                  (lambda (o)
                    (eq 'org-drill-replaced-text-overlay (overlay-get o 'category)))
                  (overlays-in (point-min) (point-max)))))
        (should (equal "[hidden]" (overlay-get ovl 'display)))))))

(ert-deftest test-org-drill-unreplace-entry-text-removes-replaced-overlays ()
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nThe body of the entry.\n* Other heading\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-replace-entry-text "[hidden]")
      (should (= 1 (count-overlays-of-category 'org-drill-replaced-text-overlay)))
      (org-drill-unreplace-entry-text)
      (should (= 0 (count-overlays-of-category 'org-drill-replaced-text-overlay))))))

;;;; org-drill-get-entry-text

(ert-deftest test-org-drill-get-entry-text-returns-body-text ()
  "Body text under the heading comes back as the entry text."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nThis is the body of the question.\n")
      (org-mode)
      (goto-char (point-min))
      (let ((text (org-drill-get-entry-text)))
        (should (stringp text))
        (should (string-match-p "body of the question" text))))))

(ert-deftest test-org-drill-get-entry-text-strips-text-properties-by-default ()
  "Default call returns a string with no text properties.
With KEEP-PROPERTIES-P nil (the default) the org-mode font-lock
properties are stripped before the string is returned."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:\nThe body.\n")
      (org-mode)
      (goto-char (point-min))
      (let ((text (org-drill-get-entry-text)))
        (should (null (text-properties-at 0 text)))))))

(provide 'test-org-drill-hide-show)

;;; test-org-drill-hide-show.el ends here
