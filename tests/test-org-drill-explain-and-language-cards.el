;;; test-org-drill-explain-and-language-cards.el --- Tests for explain helpers and language card info getters  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for:
;;
;; - `org-drill-explain-entry-p': checks the `explain' tag.  Used by
;;   `org-drill-get-explain-text' to walk up the outline collecting
;;   explanation prose from parent headings.
;; - `org-drill-end-of-entry-pos': simple position helper.
;; - `org-drill-get-verb-conjugation-info': read VERB_* properties for
;;   conjugate-style cards.  Returns (INFINITIVE HINT TRANSLATION TENSE MOOD).
;; - `org-drill-get-noun-info': read NOUN_* properties for declension
;;   cards.  Returns (NOUN ROOT GENDER HINT TRANSLATION).
;;
;; Both info getters use `read-from-string' on property values, so the
;; properties must be stored as Lisp-readable strings (typically
;; quoted: `"house"' rather than `house').

;;; Code:

(require 'ert)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-org-buffer (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

;;;; org-drill-explain-entry-p

(ert-deftest test-org-drill-explain-entry-p-with-explain-tag-returns-t ()
  (with-org-buffer "* Entry :explain:\n"
    (should (org-drill-explain-entry-p))))

(ert-deftest test-org-drill-explain-entry-p-without-tag-returns-nil ()
  (with-org-buffer "* Entry :drill:\n"
    (should-not (org-drill-explain-entry-p))))

(ert-deftest test-org-drill-explain-entry-p-no-tags-returns-nil ()
  (with-org-buffer "* Plain entry\n"
    (should-not (org-drill-explain-entry-p))))

(ert-deftest test-org-drill-explain-entry-p-no-inherit-flag-rejects-inherited ()
  "When NO-INHERIT is non-nil, only direct tags count — inherited ones don't."
  (with-org-buffer "* Parent :explain:\n** Child\n"
    ;; Move to the child heading
    (goto-char (point-max))
    (forward-line -1)
    (org-back-to-heading t)
    (should (org-drill-explain-entry-p))             ; default: inherit, finds parent's
    (should-not (org-drill-explain-entry-p t))))     ; no-inherit: only direct, none

;;;; org-drill-end-of-entry-pos

(ert-deftest test-org-drill-end-of-entry-pos-returns-end-of-subtree ()
  "On a single-heading buffer, returns the end of the only subtree.
`org-end-of-subtree' returns the position just before the trailing
newline, so the result lands at point-max or point-max - 1 depending
on whether the buffer ends with a newline."
  (with-org-buffer "* Entry\nbody line\n"
    (let ((got (org-drill-end-of-entry-pos)))
      (should (or (= got (point-max))
                  (= got (1- (point-max))))))))

(ert-deftest test-org-drill-end-of-entry-pos-stops-at-next-sibling ()
  "Returns the position just before the next same-level heading."
  (with-org-buffer "* First\nbody A\n* Second\nbody B\n"
    (goto-char (point-min))
    (let ((end-pos (org-drill-end-of-entry-pos)))
      (goto-char end-pos)
      ;; The end-pos should be at-or-before the start of the second heading.
      (should (<= (point) (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^\\* Second" nil t)
                            (line-beginning-position)))))))

;;;; org-drill-get-verb-conjugation-info

(ert-deftest test-org-drill-get-verb-conjugation-info-reads-all-properties ()
  "Returns (INFINITIVE HINT TRANSLATION TENSE MOOD) parsed from quoted strings."
  (with-org-buffer "* Verb :drill:\n"
    (org-set-property "VERB_INFINITIVE" "\"hablar\"")
    (org-set-property "VERB_INFINITIVE_HINT" "\"talk\"")
    (org-set-property "VERB_TRANSLATION" "\"to speak\"")
    (org-set-property "VERB_TENSE" "\"present\"")
    (org-set-property "VERB_MOOD" "\"indicative\"")
    (let ((info (org-drill-get-verb-conjugation-info)))
      (should (equal "hablar" (substring-no-properties (nth 0 info))))
      (should (equal "talk" (substring-no-properties (nth 1 info))))
      (should (equal "to speak" (substring-no-properties (nth 2 info))))
      (should (equal "present" (substring-no-properties (nth 3 info))))
      (should (equal "indicative" (substring-no-properties (nth 4 info)))))))

(ert-deftest test-org-drill-get-verb-conjugation-info-tense-only-no-mood ()
  "TENSE alone is sufficient — MOOD is optional as long as one of the two is set."
  (with-org-buffer "* Verb :drill:\n"
    (org-set-property "VERB_INFINITIVE" "\"hablar\"")
    (org-set-property "VERB_TRANSLATION" "\"to speak\"")
    (org-set-property "VERB_TENSE" "\"present\"")
    (let ((info (org-drill-get-verb-conjugation-info)))
      (should (equal "present" (substring-no-properties (nth 3 info))))
      (should (null (nth 4 info))))))

(ert-deftest test-org-drill-get-verb-conjugation-info-missing-required-errors ()
  "Missing infinitive/translation/tense+mood → user-visible error."
  (with-org-buffer "* Verb :drill:\n"
    (org-set-property "VERB_INFINITIVE" "\"hablar\"")
    ;; missing translation → should error
    (should-error (org-drill-get-verb-conjugation-info))))

(ert-deftest test-org-drill-get-verb-conjugation-info-applies-tense-color ()
  "TENSE foreground color comes from `org-drill-verb-tense-alist'."
  (with-org-buffer "* Verb :drill:\n"
    (org-set-property "VERB_INFINITIVE" "\"hablar\"")
    (org-set-property "VERB_TRANSLATION" "\"to speak\"")
    (org-set-property "VERB_TENSE" "\"present\"")
    (let* ((info (org-drill-get-verb-conjugation-info))
           (face (get-text-property 0 'face (nth 0 info))))
      ;; Face should be a plist with :foreground set to a color string.
      (should (plist-get face :foreground))
      (should (stringp (plist-get face :foreground))))))

;;;; org-drill-get-noun-info

(ert-deftest test-org-drill-get-noun-info-reads-all-properties ()
  "Returns (NOUN ROOT GENDER HINT TRANSLATION) parsed from quoted strings."
  (with-org-buffer "* Noun :drill:\n"
    (org-set-property "NOUN" "\"casa\"")
    (org-set-property "NOUN_ROOT" "\"cas\"")
    (org-set-property "NOUN_GENDER" "\"feminine\"")
    (org-set-property "NOUN_HINT" "\"home\"")
    (org-set-property "NOUN_TRANSLATION" "\"house\"")
    (let ((info (org-drill-get-noun-info)))
      (should (equal "casa" (substring-no-properties (nth 0 info))))
      (should (equal "cas" (nth 1 info)))
      (should (equal "feminine" (nth 2 info)))
      (should (equal "home" (nth 3 info)))
      (should (equal "house" (substring-no-properties (nth 4 info)))))))

(ert-deftest test-org-drill-get-noun-info-missing-noun-or-translation-errors ()
  (with-org-buffer "* Noun :drill:\n"
    (org-set-property "NOUN" "\"casa\"")
    (org-set-property "NOUN_GENDER" "\"feminine\"")
    ;; missing NOUN_TRANSLATION
    (should-error (org-drill-get-noun-info))))

(ert-deftest test-org-drill-get-noun-info-gender-color-from-alist ()
  "Feminine gender → orchid foreground via `org-drill-noun-gender-alist'."
  (with-org-buffer "* Noun :drill:\n"
    (org-set-property "NOUN" "\"casa\"")
    (org-set-property "NOUN_GENDER" "\"feminine\"")
    (org-set-property "NOUN_TRANSLATION" "\"house\"")
    (let* ((info (org-drill-get-noun-info))
           (face (get-text-property 0 'face (nth 0 info))))
      (should (equal "orchid" (plist-get face :foreground))))))

(ert-deftest test-org-drill-get-noun-info-unknown-gender-falls-back-to-red ()
  "Unrecognized gender values get the default red foreground."
  (with-org-buffer "* Noun :drill:\n"
    (org-set-property "NOUN" "\"thing\"")
    (org-set-property "NOUN_GENDER" "\"weirdkind\"")
    (org-set-property "NOUN_TRANSLATION" "\"thing\"")
    (let* ((info (org-drill-get-noun-info))
           (face (get-text-property 0 'face (nth 0 info))))
      (should (equal "red" (plist-get face :foreground))))))

(provide 'test-org-drill-explain-and-language-cards)

;;; test-org-drill-explain-and-language-cards.el ends here
