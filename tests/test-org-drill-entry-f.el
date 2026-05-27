;;; test-org-drill-entry-f.el --- Tests for org-drill-entry-f dispatcher  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-entry-f', the per-entry dispatcher.  Given a
;; card's DRILL_CARD_TYPE, looks up the presentation function in
;; `org-drill-card-type-alist' and invokes it.  Returns whatever the
;; presenter returns, with quit / edit / skip propagated and successful
;; ratings handed off to the answer function.
;;
;; Tests verify the dispatch contract: the right presentation function
;; is called for the configured card type, and unknown card types
;; result in `skip'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-drill-entry (card-type &rest body)
  "Run BODY at point on a drill entry with DRILL_CARD_TYPE = CARD-TYPE."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert (format "* Question :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: %s\n:END:\nbody body body\n"
                       ,card-type))
       (org-mode)
       (goto-char (point-min))
       ,@body)))

;;;; Unknown card type → 'skip

(ert-deftest test-entry-f-unknown-card-type-returns-skip ()
  "An unrecognised DRILL_CARD_TYPE causes the entry to be skipped."
  (with-drill-entry "no-such-card-type"
    (let ((session (org-drill-session))
          (answer-called nil))
      (cl-letf (((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-entry-f
                       session
                       (lambda (_) (setq answer-called t)))))
          (should (eq 'skip result))
          (should-not answer-called))))))

;;;; Quit propagates

(ert-deftest test-entry-f-presenter-returns-nil-propagates-as-quit ()
  "If the presenter returns nil (quit), entry-f returns nil too."
  (with-drill-entry "simple"
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'org-drill-present-simple-card)
                 (lambda (_) nil))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-entry-f session #'ignore)))
          (should (null result)))))))

;;;; Edit propagates

(ert-deftest test-entry-f-presenter-returns-edit-propagates ()
  "An 'edit return from the presenter passes through unchanged."
  (with-drill-entry "simple"
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'org-drill-present-simple-card)
                 (lambda (_) 'edit))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-entry-f session #'ignore)))
          (should (eq 'edit result)))))))

;;;; Skip propagates

(ert-deftest test-entry-f-presenter-returns-skip-propagates ()
  (with-drill-entry "simple"
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'org-drill-present-simple-card)
                 (lambda (_) 'skip))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-entry-f session #'ignore)))
          (should (eq 'skip result)))))))

;;;; Successful presenter return → answer-fn called

(ert-deftest test-entry-f-presenter-success-calls-answer-fn ()
  "When the presenter returns t, the default answer presenter runs and
the complete-func (e.g., reschedule) is invoked with the session."
  (with-drill-entry "simple"
    (let ((session (org-drill-session))
          (complete-called nil))
      (cl-letf (((symbol-function 'org-drill-present-simple-card)
                 (lambda (_) t))
                ((symbol-function 'org-drill-present-default-answer)
                 (lambda (s reschedule-fn) (funcall reschedule-fn s)))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-entry-f session
                           (lambda (_) (setq complete-called t)))
        (should complete-called)))))

;;;; org-drill--resolve-presenter

(ert-deftest test-org-drill-resolve-presenter-bare-symbol ()
  "A bare presenter symbol pairs with the default answer function."
  (let ((org-drill-card-type-alist '(("simple" . my-present))))
    (should (equal '(my-present . org-drill-present-default-answer)
                   (org-drill--resolve-presenter "simple")))))

(ert-deftest test-org-drill-resolve-presenter-list-with-answer ()
  "A list entry uses its first element as presenter and second as answer."
  (let ((org-drill-card-type-alist '(("two" my-present my-answer))))
    (should (equal '(my-present . my-answer)
                   (org-drill--resolve-presenter "two")))))

(ert-deftest test-org-drill-resolve-presenter-list-without-answer ()
  "A single-element list entry falls back to the default answer function."
  (let ((org-drill-card-type-alist '(("one" my-present))))
    (should (equal '(my-present . org-drill-present-default-answer)
                   (org-drill--resolve-presenter "one")))))

(ert-deftest test-org-drill-resolve-presenter-unknown-is-nil ()
  "An unknown card type resolves to a nil presenter (which entry-f skips)."
  (let ((org-drill-card-type-alist '(("simple" . my-present))))
    (should (equal '(nil . org-drill-present-default-answer)
                   (org-drill--resolve-presenter "no-such-type")))))

;;;; org-drill--classify-presentation-result

(ert-deftest test-org-drill-classify-result-nil-is-quit ()
  (should (eq 'quit (org-drill--classify-presentation-result nil))))

(ert-deftest test-org-drill-classify-result-edit ()
  (should (eq 'edit (org-drill--classify-presentation-result 'edit))))

(ert-deftest test-org-drill-classify-result-skip ()
  (should (eq 'skip (org-drill--classify-presentation-result 'skip))))

(ert-deftest test-org-drill-classify-result-other-is-answer ()
  "Any other non-nil value means proceed to the answer."
  (should (eq 'answer (org-drill--classify-presentation-result t)))
  (should (eq 'answer (org-drill--classify-presentation-result 5))))

(provide 'test-org-drill-entry-f)

;;; test-org-drill-entry-f.el ends here
