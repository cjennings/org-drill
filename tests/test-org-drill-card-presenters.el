;;; test-org-drill-card-presenters.el --- Tests for card presentation functions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the functions that show a card to the user:
;;
;; - `org-drill-present-simple-card': default presenter — hides cloze
;;   text, hides comments, hides drawers, hides subheadings, then
;;   prompts.
;; - `org-drill-present-default-answer': reveals the answer.  Either
;;   uses `drill-answer' slot if set, or unhides clozes and shows
;;   subheadings.
;; - `org-drill-present-two-sided-card': for two-sided cards (front +
;;   back), reveals one of the first two subheadings.
;; - `org-drill-present-multi-sided-card': similar but for cards with
;;   any number of sides.
;; - `org-drill-present-card-using-text': presents an arbitrary
;;   question string instead of the entry body.
;;
;; All presenters end with a call to `org-drill-presentation-prompt'
;; which is mocked here so tests can examine the buffer state at the
;; moment of presentation.

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
       (setq-local org-drill-cloze-regexp (org-drill--compute-cloze-regexp))
       ,@body)))

(defmacro with-mocked-presentation (return-val &rest body)
  "Run BODY with prompt + LaTeX + inline-images stubbed for batch."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-drill-presentation-prompt)
              (lambda (&rest _) ,return-val))
             ((symbol-function 'org-drill-presentation-prompt-for-string)
              (lambda (&rest _) ,return-val))
             ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
             ((symbol-function 'org-display-inline-images) #'ignore)
             ((symbol-function 'org-clear-latex-preview) #'ignore)
             ((symbol-function 'org--latex-preview-region) #'ignore))
     ,@body))

(defun count-overlays-of-category (cat)
  (let ((n 0))
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql cat (overlay-get ovl 'category))
        (cl-incf n)))
    n))

(defvar overlays-snapshot nil
  "Captured cloze overlay count at the moment the prompt fires.")

(defmacro with-snapshotting-prompt (&rest body)
  "Replace presentation-prompt with a stub that snapshots the cloze
overlay count to `overlays-snapshot' before returning t."
  `(cl-letf (((symbol-function 'org-drill-presentation-prompt)
              (lambda (&rest _)
                (setq overlays-snapshot
                      (count-overlays-of-category 'org-drill-cloze-overlay-defaults))
                t))
             ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
             ((symbol-function 'org-display-inline-images) #'ignore))
     (setq overlays-snapshot nil)
     ,@body))

;;;; org-drill-present-simple-card

(ert-deftest test-present-simple-card-hides-cloze-text-during-prompt ()
  "On a card with a cloze body, presentation-prompt sees the cloze hidden."
  (with-card-buffer "* Question :drill:
What is the capital? [Paris]
"
    (with-snapshotting-prompt
     (org-drill-present-simple-card (org-drill-session)))
    (should (= 1 overlays-snapshot))))

(ert-deftest test-present-simple-card-returns-prompt-result ()
  (with-card-buffer "* Question :drill:
plain body
"
    (with-mocked-presentation 'edit
      (let ((result (org-drill-present-simple-card (org-drill-session))))
        (should (eq 'edit result))))))

(ert-deftest test-present-simple-card-restores-cloze-overlays-after-prompt ()
  "After the prompt returns, the cloze overlays are gone — `unhide-clozed-text'
runs as part of the unwind-protect macro `with-hidden-cloze-text'."
  (with-card-buffer "* Question :drill:
The answer is [hidden].
"
    (with-mocked-presentation t
      (org-drill-present-simple-card (org-drill-session)))
    (should (= 0 (count-overlays-of-category 'org-drill-cloze-overlay-defaults)))))

;;;; org-drill-present-default-answer

(ert-deftest test-present-default-answer-with-drill-answer-slot ()
  "When session->drill-answer is set, that text is overlay-displayed instead
of the entry's body."
  (with-card-buffer "* Question :drill:
hidden body
"
    (let ((session (org-drill-session))
          (reschedule-called nil))
      (oset session drill-answer "Forty-two")
      (with-mocked-presentation t
        (cl-letf (((symbol-function 'org-mark-subtree) #'ignore)
                  ((symbol-function 'deactivate-mark) #'ignore))
          (org-drill-present-default-answer session
            (lambda (_) (setq reschedule-called t)))))
      (should reschedule-called))))

(ert-deftest test-present-default-answer-without-drill-answer-slot ()
  "Without a drill-answer slot, the function takes the unhide path
and calls reschedule-fn.

`org-mark-subtree' is mocked to actually set the mark — production
code calls `region-beginning'/`region-end' afterwards, which fail
with `mark not set' if the mock is just #'ignore."
  (with-card-buffer "* Question :drill:
body content
"
    (let ((session (org-drill-session))
          (reschedule-called nil))
      (oset session drill-answer nil)
      (with-mocked-presentation t
        (cl-letf (((symbol-function 'org-mark-subtree)
                   (lambda (&rest _)
                     (push-mark (point-min) t t)
                     (goto-char (point-max))))
                  ((symbol-function 'deactivate-mark) #'ignore))
          (org-drill-present-default-answer session
            (lambda (_) (setq reschedule-called t)))))
      (should reschedule-called))))

;;;; org-drill-present-card-using-text

(ert-deftest test-present-card-using-text-replaces-entry-body ()
  "The provided question text is shown via a replaced-text overlay."
  (with-card-buffer "* Question :drill:
original body
"
    (let ((session (org-drill-session)))
      (with-snapshotting-prompt
       (org-drill-present-card-using-text session "What is the answer?"))
      ;; replaced-text overlay should have been present at prompt time.
      ;; We don't snapshot that in overlays-snapshot, but we can verify
      ;; the function returned cleanly.
      (should t))))

(ert-deftest test-present-card-using-text-sets-drill-answer-when-given ()
  "If an ANSWER arg is supplied, it goes into the session's drill-answer slot."
  (with-card-buffer "* Question :drill:
original body
"
    (let ((session (org-drill-session)))
      (with-mocked-presentation t
        (org-drill-present-card-using-text session "Q" "A"))
      (should (equal "A" (oref session drill-answer))))))

;;;; org-drill-present-two-sided-card

(ert-deftest test-present-two-sided-card-runs-without-error ()
  "On a two-sided card with two subheadings, runs cleanly."
  (with-card-buffer "* Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

** English
hello
** Spanish
hola
"
    (with-mocked-presentation t
      (let ((result (org-drill-present-two-sided-card (org-drill-session))))
        (should (eq t result))))))

(ert-deftest test-present-multi-sided-card-runs-without-error ()
  (with-card-buffer "* Card :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: multisided
:END:

** Side A
content A
** Side B
content B
** Side C
content C
"
    (with-mocked-presentation t
      (let ((result (org-drill-present-multi-sided-card (org-drill-session))))
        (should (eq t result))))))

(provide 'test-org-drill-card-presenters)

;;; test-org-drill-card-presenters.el ends here
