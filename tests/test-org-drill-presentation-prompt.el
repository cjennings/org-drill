;;; test-org-drill-presentation-prompt.el --- Tests for the card-prompt return values  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-presentation-prompt-in-mini-buffer'.  The
;; function shows the card body, runs a busy-wait loop until the user
;; presses a key, and returns:
;;
;;   nil    — quit key pressed
;;   'edit  — edit key pressed
;;   'skip  — skip key pressed
;;   t      — any other key (user wants to see the answer)
;;
;; Tests mock both `input-pending-p' (to bypass the busy-wait) and
;; `read-key-sequence' (to inject the desired key).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fresh-drill-entry (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\nbody\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-key-input (key &rest body)
  "Run BODY with the prompt's busy-wait skipped and `read-key-sequence' returning KEY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'input-pending-p) (lambda () t))
             ((symbol-function 'read-key-sequence) (lambda (_) ,key))
             ((symbol-function 'sit-for) #'ignore))
     ,@body))

;;;; Return values

(ert-deftest test-org-drill-presentation-prompt-quit-returns-nil ()
  (with-fresh-drill-entry
    (with-key-input (string org-drill--quit-key)
      (let ((session (org-drill-session)))
        (should (null (org-drill-presentation-prompt-in-mini-buffer session)))))))

(ert-deftest test-org-drill-presentation-prompt-edit-returns-edit ()
  (with-fresh-drill-entry
    (with-key-input (string org-drill--edit-key)
      (let ((session (org-drill-session)))
        (should (eq 'edit (org-drill-presentation-prompt-in-mini-buffer session)))))))

(ert-deftest test-org-drill-presentation-prompt-skip-returns-skip ()
  (with-fresh-drill-entry
    (with-key-input (string org-drill--skip-key)
      (let ((session (org-drill-session)))
        (should (eq 'skip (org-drill-presentation-prompt-in-mini-buffer session)))))))

(ert-deftest test-org-drill-presentation-prompt-answer-returns-t ()
  "Pressing any non-control key (here SPC) returns t — `show me the answer'."
  (with-fresh-drill-entry
    (with-key-input " "
      (let ((session (org-drill-session)))
        (should (eq t (org-drill-presentation-prompt-in-mini-buffer session)))))))

(ert-deftest test-org-drill-presentation-prompt-honors-explicit-prompt-arg ()
  "An explicit PROMPT arg is honored — the user's prompt text appears in
the formatted full-prompt that gets displayed.
Lets the busy-wait loop run one iteration so message actually fires."
  (with-fresh-drill-entry
    (let ((messages-seen nil)
          (loop-iterations 0))
      (cl-letf (((symbol-function 'input-pending-p)
                 (lambda ()
                   (cl-incf loop-iterations)
                   ;; Return nil first time (run loop body), then t (exit).
                   (> loop-iterations 1)))
                ((symbol-function 'read-key-sequence) (lambda (_) " "))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages-seen))))
        (let ((session (org-drill-session)))
          (org-drill-presentation-prompt-in-mini-buffer session "MY-CUSTOM-PROMPT")
          (should (cl-some (lambda (m) (string-match-p "MY-CUSTOM-PROMPT" m))
                           messages-seen)))))))

;;;; org-drill-presentation-prompt (top-level dispatcher)

(ert-deftest test-org-drill-presentation-prompt-dispatches-to-mini-buffer-by-default ()
  "When `org-drill-presentation-prompt-with-typing' is nil, the dispatcher
sends the call to the mini-buffer variant."
  (with-fresh-drill-entry
    (let ((mini-called nil)
          (buffer-called nil)
          (org-drill-presentation-prompt-with-typing nil))
      (cl-letf (((symbol-function 'org-drill-presentation-prompt-in-mini-buffer)
                 (lambda (&rest _) (setq mini-called t) t))
                ((symbol-function 'org-drill-presentation-prompt-in-buffer)
                 (lambda (&rest _) (setq buffer-called t) t)))
        (org-drill-presentation-prompt (org-drill-session))
        (should mini-called)
        (should-not buffer-called)))))

(ert-deftest test-org-drill-presentation-prompt-dispatches-to-buffer-when-typing ()
  "When `org-drill-presentation-prompt-with-typing' is non-nil, dispatcher
goes to the in-buffer variant."
  (with-fresh-drill-entry
    (let ((mini-called nil)
          (buffer-called nil)
          (org-drill-presentation-prompt-with-typing t))
      (cl-letf (((symbol-function 'org-drill-presentation-prompt-in-mini-buffer)
                 (lambda (&rest _) (setq mini-called t) t))
                ((symbol-function 'org-drill-presentation-prompt-in-buffer)
                 (lambda (&rest _) (setq buffer-called t) t)))
        (org-drill-presentation-prompt (org-drill-session))
        (should-not mini-called)
        (should buffer-called)))))

;;;; org-drill-presentation-prompt-in-buffer

(defmacro with-mocked-in-buffer-deps (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'run-with-idle-timer)
              (lambda (&rest _) 'fake-timer))
             ((symbol-function 'cancel-timer) #'ignore)
             ((symbol-function 'recursive-edit) #'ignore)
             ((symbol-function 'select-window) #'ignore)
             ((symbol-function 'display-buffer)
              (lambda (buf &rest _)
                (or (get-buffer-window buf)
                    (selected-window))))
             ((symbol-function 'org-drill--make-minibuffer-prompt)
              (lambda (_s p) p)))
     ,@body))

(ert-deftest test-presentation-prompt-in-buffer-uses-default-prompt-when-nil ()
  "When PROMPT is nil, `prompt-in-buffer' assembles a default prompt that
mentions the response keys."
  (let ((seen-prompt nil))
    (with-mocked-in-buffer-deps
      (cl-letf (((symbol-function 'org-drill--maybe-prepend-leech-warning)
                 (lambda (p) (setq seen-prompt p) p)))
        (with-fresh-drill-entry
          (org-drill-presentation-prompt-in-buffer (org-drill-session)))))
    (should (string-match-p "Type answer" seen-prompt))))

(ert-deftest test-presentation-prompt-in-buffer-with-explicit-prompt ()
  "When PROMPT is supplied, that string is the one fed to the leech-warning prepass."
  (let ((seen-prompt nil))
    (with-mocked-in-buffer-deps
      (cl-letf (((symbol-function 'org-drill--maybe-prepend-leech-warning)
                 (lambda (p) (setq seen-prompt p) p)))
        (with-fresh-drill-entry
          (org-drill-presentation-prompt-in-buffer (org-drill-session) "EXPLICIT-P"))))
    (should (equal "EXPLICIT-P" seen-prompt))))

(ert-deftest test-presentation-prompt-in-buffer-clears-drill-answer ()
  "Calling the prompt resets the session's drill-answer slot."
  (let ((session (org-drill-session)))
    (oset session drill-answer "stale")
    (with-mocked-in-buffer-deps
      (cl-letf (((symbol-function 'org-drill--maybe-prepend-leech-warning)
                 (lambda (p) p)))
        (with-fresh-drill-entry
          (org-drill-presentation-prompt-in-buffer session "p"))))
    (should (null (oref session drill-answer)))))

(ert-deftest test-presentation-prompt-in-buffer-returns-exit-kind ()
  "The function returns the session's exit-kind set by recursive-edit."
  (let ((session (org-drill-session)))
    (with-mocked-in-buffer-deps
      (cl-letf (((symbol-function 'recursive-edit)
                 (lambda () (oset session exit-kind 'mock-result)))
                ((symbol-function 'org-drill--maybe-prepend-leech-warning)
                 (lambda (p) p)))
        (with-fresh-drill-entry
          (let ((result (org-drill-presentation-prompt-in-buffer session "p")))
            (should (eq 'mock-result result))))))))

;;;; org-drill-present-simple-card-with-typed-answer

(ert-deftest test-present-simple-card-with-typed-answer-runs-prompt ()
  "The typed-answer presenter calls `prompt-for-string' and returns its value."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'org-drill-hide-all-subheadings-except) #'ignore)
              ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
              ((symbol-function 'org-display-inline-images) #'ignore)
              ((symbol-function 'org-drill-hide-drawers) #'ignore)
              ((symbol-function 'org-drill-hide-subheadings-if) #'ignore)
              ((symbol-function 'org-drill-presentation-prompt-for-string)
               (lambda (s _p) (setq called-with s) 'prompt-result)))
      (with-fresh-drill-entry
        (let* ((session (org-drill-session))
               (result (org-drill-present-simple-card-with-typed-answer session)))
          (should (eq 'prompt-result result))
          (should (eq session called-with)))))))

(provide 'test-org-drill-presentation-prompt)

;;; test-org-drill-presentation-prompt.el ends here
