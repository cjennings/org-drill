;;; test-org-drill-language-presenters.el --- Tests for verb/noun/Spanish presenters  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the language-card presenters that wrap
;; `org-drill-present-card-using-text' and `with-replaced-entry-heading'
;; for verb conjugation and noun declension cards.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-card-buffer (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-mocked-presentation (return-val &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-drill-presentation-prompt)
              (lambda (&rest _) ,return-val))
             ((symbol-function 'org-drill-presentation-prompt-for-string)
              (lambda (&rest _) ,return-val))
             ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
             ((symbol-function 'org-display-inline-images) #'ignore)
             ((symbol-function 'org-clear-latex-preview) #'ignore))
     ,@body))

;;;; org-drill-present-verb-conjugation

(ert-deftest test-present-verb-conjugation-runs-cleanly ()
  "On a verb card with all required properties, runs through to the prompt."
  (with-card-buffer "* Verb :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: conjugate
:VERB_INFINITIVE: \"hablar\"
:VERB_TRANSLATION: \"to speak\"
:VERB_TENSE: \"present\"
:END:
"
    (with-mocked-presentation t
      (let ((result (org-drill-present-verb-conjugation (org-drill-session))))
        (should (eq t result))))))

(ert-deftest test-present-verb-conjugation-tense-and-mood ()
  "Cards with both tense and mood produce a `tense, mood' string."
  (with-card-buffer "* Verb :drill:
:PROPERTIES:
:VERB_INFINITIVE: \"hablar\"
:VERB_TRANSLATION: \"to speak\"
:VERB_TENSE: \"future\"
:VERB_MOOD: \"subjunctive\"
:END:
"
    (let ((shown-question nil))
      (cl-letf (((symbol-function 'org-drill-present-card-using-text)
                 (lambda (_session question &rest _)
                   (setq shown-question question) t))
                ((symbol-function 'cl-random) (lambda (_) 0)))
        (org-drill-present-verb-conjugation (org-drill-session))
        (should (string-match-p "future tense, subjunctive mood" shown-question))))))

;;;; org-drill-show-answer-verb-conjugation

(ert-deftest test-show-answer-verb-conjugation-runs-cleanly ()
  "Answer presenter for verbs runs cleanly."
  (with-card-buffer "* Verb :drill:
:PROPERTIES:
:VERB_INFINITIVE: \"hablar\"
:VERB_TRANSLATION: \"to speak\"
:VERB_TENSE: \"present\"
:END:
"
    (let ((reschedule-called nil))
      (with-mocked-presentation t
        (org-drill-show-answer-verb-conjugation
         (org-drill-session)
         (lambda (_) (setq reschedule-called t))))
      (should reschedule-called))))

;;;; org-drill-present-noun-declension

(ert-deftest test-present-noun-declension-runs-cleanly ()
  (with-card-buffer "* Noun :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: decline_noun
:NOUN: \"casa\"
:NOUN_GENDER: \"feminine\"
:NOUN_TRANSLATION: \"house\"
:END:
"
    (with-mocked-presentation t
      (let ((result (org-drill-present-noun-declension (org-drill-session))))
        (should (eq t result))))))

(ert-deftest test-present-noun-declension-with-definite-property ()
  "DECLINE_DEFINITE property adds `definite/indefinite' to the question text."
  (with-card-buffer "* Noun :drill:
:PROPERTIES:
:NOUN: \"casa\"
:NOUN_GENDER: \"feminine\"
:NOUN_TRANSLATION: \"house\"
:DECLINE_DEFINITE: t
:END:
"
    (let ((shown-question nil))
      (cl-letf (((symbol-function 'org-drill-present-card-using-text)
                 (lambda (_session question &rest _)
                   (setq shown-question question) t))
                ((symbol-function 'cl-random) (lambda (_) 0)))
        (org-drill-present-noun-declension (org-drill-session))
        (should (string-match-p "definite" shown-question))))))

(ert-deftest test-present-noun-declension-without-extras-skips-decline-suffix ()
  "Without DECLINE_DEFINITE / DECLINE_PLURAL, no suffix is appended."
  (with-card-buffer "* Noun :drill:
:PROPERTIES:
:NOUN: \"casa\"
:NOUN_GENDER: \"feminine\"
:NOUN_TRANSLATION: \"house\"
:END:
"
    (let ((shown-question nil))
      (cl-letf (((symbol-function 'org-drill-present-card-using-text)
                 (lambda (_session question &rest _)
                   (setq shown-question question) t))
                ((symbol-function 'cl-random) (lambda (_) 0)))
        (org-drill-present-noun-declension (org-drill-session))
        (should-not (string-match-p "definite" shown-question))
        (should-not (string-match-p "plural" shown-question))))))

;;;; org-drill-show-answer-noun-declension

(ert-deftest test-show-answer-noun-declension-runs-cleanly ()
  (with-card-buffer "* Noun :drill:
:PROPERTIES:
:NOUN: \"casa\"
:NOUN_GENDER: \"feminine\"
:NOUN_TRANSLATION: \"house\"
:END:
"
    (let ((reschedule-called nil))
      (with-mocked-presentation t
        (org-drill-show-answer-noun-declension
         (org-drill-session)
         (lambda (_) (setq reschedule-called t))))
      (should reschedule-called))))

(provide 'test-org-drill-language-presenters)

;;; test-org-drill-language-presenters.el ends here
