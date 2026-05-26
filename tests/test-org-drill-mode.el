;;; test-org-drill-mode.el --- Tests for org-drill-mode minor mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-drill-mode' scopes cloze fontification to buffers that hold drill
;; cards instead of installing it globally via `org-font-lock-set-keywords-hook'.
;;
;; Regression target: the global hook caused org priority cookies ([#A]..[#D])
;; to match the cloze `[...]' pattern in EVERY org buffer, colliding with org's
;; headline fontification and stripping the heading's `org-level-N' face.  With
;; the mode, plain org buffers carry no cloze rule at all, so the collision
;; cannot happen there.

;;; Code:

(require 'ert)
(require 'org-drill)

;;;; Helpers

(defun tdm--face-prop-has (face prop)
  "Return non-nil if FACE is, or is a member of, the face PROP."
  (or (eq prop face)
      (and (listp prop) (memq face prop))))

(defun tdm--face-at-string (s)
  "Fontify the buffer and return the `face' text property at the first
character of the first occurrence of literal string S."
  (font-lock-ensure)
  (goto-char (point-min))
  (when (search-forward s nil t)
    (get-text-property (match-beginning 0) 'face)))

;;;; Minor mode definition

(ert-deftest test-org-drill-mode-is-buffer-local-minor-mode ()
  "`org-drill-mode' exists and is buffer-local."
  (should (fboundp 'org-drill-mode))
  (should (local-variable-if-set-p 'org-drill-mode)))

(ert-deftest test-org-drill-mode-toggle-adds-and-removes-cloze-keyword ()
  "Enabling the mode highlights a cloze; disabling stops highlighting it."
  (let ((org-drill-use-visible-cloze-face-p t))
    (with-temp-buffer
      (insert "Question [answer] tail\n")
      (org-mode)
      (org-drill-mode 1)
      (should org-drill-mode)
      (should (tdm--face-prop-has 'org-drill-visible-cloze-face
                                  (tdm--face-at-string "answer")))
      (org-drill-mode -1)
      (should-not org-drill-mode)
      (should-not (tdm--face-prop-has 'org-drill-visible-cloze-face
                                      (tdm--face-at-string "answer"))))))

;;;; Drill-buffer predicate

(ert-deftest test-org-drill-buffer-has-cards-p-true-for-drill-tag ()
  "A heading tagged with the question tag counts as a drill buffer."
  (with-temp-buffer
    (insert "* A card :drill:\nbody\n")
    (org-mode)
    (should (org-drill-buffer-has-cards-p))))

(ert-deftest test-org-drill-buffer-has-cards-p-true-for-leitner-tag ()
  "A heading tagged with the leitner tag counts as a drill buffer."
  (with-temp-buffer
    (insert "* A card :leitner:\nbody\n")
    (org-mode)
    (should (org-drill-buffer-has-cards-p))))

(ert-deftest test-org-drill-buffer-has-cards-p-false-for-plain-org ()
  "An org buffer with no drill/leitner tag is not a drill buffer."
  (with-temp-buffer
    (insert "* Just notes :work:\n** TODO [#A] A heading\nbody [bracketed] text\n")
    (org-mode)
    (should-not (org-drill-buffer-has-cards-p))))

;;;; Auto-enable on org-mode-hook

(ert-deftest test-org-drill-mode-auto-enables-in-drill-buffer ()
  "With auto-enable on, opening a drill buffer turns the mode on."
  (let ((org-drill-auto-enable-mode t))
    (with-temp-buffer
      (insert "* A card :drill:\nQ [answer] A\n")
      (org-mode)
      (should org-drill-mode))))

(ert-deftest test-org-drill-mode-does-not-auto-enable-in-plain-buffer ()
  "With auto-enable on, a plain org buffer does NOT turn the mode on."
  (let ((org-drill-auto-enable-mode t))
    (with-temp-buffer
      (insert "* Notes\n** TODO [#A] With cookie\n")
      (org-mode)
      (should-not org-drill-mode))))

(ert-deftest test-org-drill-mode-auto-enable-respects-defcustom-off ()
  "When `org-drill-auto-enable-mode' is nil, drill buffers do not auto-enable."
  (let ((org-drill-auto-enable-mode nil))
    (with-temp-buffer
      (insert "* A card :drill:\nQ [answer] A\n")
      (org-mode)
      (should-not org-drill-mode))))

(ert-deftest test-org-drill-auto-enable-mode-defaults-on ()
  "The auto-enable defcustom ships on so existing drill files keep highlighting."
  (should (eq t (default-value 'org-drill-auto-enable-mode))))

;;;; Regression — the bug

(ert-deftest test-org-drill-no-global-font-lock-keywords-hook ()
  "Cloze fontification is no longer installed globally on
`org-font-lock-set-keywords-hook' — that was the every-buffer pollution that
made priority cookies collide with the cloze pattern."
  (should-not (memq 'org-drill-add-cloze-fontification
                    (if (boundp 'org-font-lock-set-keywords-hook)
                        org-font-lock-set-keywords-hook
                      nil))))

(ert-deftest test-org-drill-priority-cookie-not-clozed-in-plain-buffer ()
  "In a plain org buffer (no drill cards) a priority cookie must not be
fontified as a cloze, regardless of `org-drill-use-visible-cloze-face-p'."
  (let ((org-drill-use-visible-cloze-face-p t)
        (org-drill-auto-enable-mode t))
    (with-temp-buffer
      (insert "* Notes\n** TODO [#A] With cookie\n")
      (org-mode)
      (should-not org-drill-mode)
      (goto-char (point-min))
      (search-forward "#A")
      (should-not (tdm--face-prop-has
                   'org-drill-visible-cloze-face
                   (get-text-property (match-beginning 0) 'face))))))

(provide 'test-org-drill-mode)

;;; test-org-drill-mode.el ends here
