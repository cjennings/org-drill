;;; test-org-drill-prompt-and-format-helpers.el --- Tests for small prompt + format helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the small helpers that build prompts and format display
;; data: leech-warning preamble, the minibuffer-timer message, and the
;; cloze-overlay length-matches branch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; org-drill--maybe-prepend-leech-warning

(ert-deftest test-maybe-prepend-leech-warning-not-leech-returns-prompt ()
  "When the entry isn't a leech, the prompt is returned unchanged."
  (cl-letf (((symbol-function 'org-drill-entry-leech-p) (lambda (&rest _) nil)))
    (let ((org-drill-leech-method 'warn))
      (should (equal "p" (org-drill--maybe-prepend-leech-warning "p"))))))

(ert-deftest test-maybe-prepend-leech-warning-method-not-warn-returns-prompt ()
  "When `org-drill-leech-method' is not `warn', no warning is prepended."
  (cl-letf (((symbol-function 'org-drill-entry-leech-p) (lambda (&rest _) t)))
    (let ((org-drill-leech-method 'skip))
      (should (equal "p" (org-drill--maybe-prepend-leech-warning "p"))))))

(ert-deftest test-maybe-prepend-leech-warning-leech-with-warn-prepends ()
  "Both conditions met → result starts with '!!! LEECH ITEM !!!' and ends with PROMPT."
  (cl-letf (((symbol-function 'org-drill-entry-leech-p) (lambda (&rest _) t)))
    (let* ((org-drill-leech-method 'warn)
           (out (org-drill--maybe-prepend-leech-warning "the-prompt")))
      (should (string-match-p "!!! LEECH ITEM !!!" out))
      (should (string-suffix-p "the-prompt" out)))))

;;;; org-drill-presentation-minibuffer-timer-function

(ert-deftest test-presentation-minibuffer-timer-function-emits-timer-prompt ()
  "The timer function messages an MM:SS prefix concatenated with the prompt."
  (let ((messages nil)
        (org-drill-presentation-timer-calls 0))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      ;; Pass an item-start-time slightly in the past.
      (org-drill-presentation-minibuffer-timer-function
       (time-subtract (current-time) 5)
       "rate this card"))
    (should (cl-some (lambda (m) (string-match-p "rate this card" m)) messages))
    (should (= 1 org-drill-presentation-timer-calls))))

(ert-deftest test-presentation-minibuffer-timer-function-cancels-after-many-calls ()
  "After 10+ ticks, the timer is auto-cancelled."
  (let ((cancelled nil))
    (cl-letf (((symbol-function 'message) #'ignore)
              ((symbol-function 'org-drill-presentation-timer-cancel)
               (lambda () (setq cancelled t))))
      (let ((org-drill-presentation-timer-calls 10))
        (org-drill-presentation-minibuffer-timer-function
         (current-time) "p")
        (should cancelled)))))

(ert-deftest test-presentation-minibuffer-timer-function-uses-plus-prefix-after-an-hour ()
  "After ≥1 hour elapsed, the prefix becomes '++:++' instead of MM:SS."
  (let ((messages nil)
        (org-drill-presentation-timer-calls 0))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (org-drill-presentation-minibuffer-timer-function
       (time-subtract (current-time) 3700)
       "p"))
    (should (cl-some (lambda (m) (string-match-p "\\+\\+:\\+\\+" m)) messages))))

;;;; cloze hide with length-matches flag

(defun count-overlays-of-cloze ()
  (cl-count-if (lambda (ov) (eq (overlay-get ov 'category)
                                'org-drill-cloze-overlay-defaults))
               (overlays-in (point-min) (point-max))))

(ert-deftest test-hide-clozed-text-with-length-flag-uses-dotted-display ()
  "With `org-drill-cloze-length-matches-hidden-text-p' t, the cloze's display
property is a string of dots."
  (with-temp-buffer
    (insert "Capital is [Paris].")
    (org-mode)
    (goto-char (point-min))
    (let ((org-drill-cloze-length-matches-hidden-text-p t))
      (org-drill-hide-clozed-text)
      (let ((found-dotted nil))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((d (overlay-get ov 'display)))
            (when (and (stringp d) (string-match-p "^\\[\\.+\\]$" d))
              (setq found-dotted t))))
        (should found-dotted)))))

;;;; simple8 random-noise branch

(ert-deftest test-determine-next-interval-simple8-with-random-noise ()
  "When `org-drill-add-random-noise-to-intervals-p' is t, the returned
next-interval is multiplied by a small dispersal factor (≠1.0 in general)."
  (let ((org-drill-add-random-noise-to-intervals-p t))
    (cl-letf (((symbol-function 'org-drill-random-dispersal-factor)
               (lambda () 1.5)))
      (let* ((result (org-drill-determine-next-interval-simple8
                      4.0 1 5 0 5.0 1 0))
             (next (nth 0 result)))
        ;; With factor 1.5, the original next-interval was scaled up.
        (should (numberp next))
        (should (> next 0))))))

(provide 'test-org-drill-prompt-and-format-helpers)
;;; test-org-drill-prompt-and-format-helpers.el ends here
