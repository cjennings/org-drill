;;; test-org-drill-response-and-timer.el --- Tests for response-mode and timer  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the response-mode helpers and the presentation-timer cancel
;; helper.  These are all small functions whose only job is to flip a
;; session slot or clear a global timer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; org-drill-presentation-timer-cancel

(ert-deftest test-presentation-timer-cancel-with-no-timer-clears-counter ()
  "Cancel called with no timer just resets the counter."
  (let ((org-drill-presentation-timer nil)
        (org-drill-presentation-timer-calls 7))
    (org-drill-presentation-timer-cancel)
    (should (null org-drill-presentation-timer))
    (should (= 0 org-drill-presentation-timer-calls))))

(ert-deftest test-presentation-timer-cancel-with-active-timer-cancels-and-clears ()
  "Cancel called with an active timer cancels it and zeroes the counter."
  (let ((cancelled nil))
    (cl-letf (((symbol-function 'cancel-timer)
               (lambda (_) (setq cancelled t))))
      (let ((org-drill-presentation-timer 'fake-timer)
            (org-drill-presentation-timer-calls 5))
        (org-drill-presentation-timer-cancel)
        (should cancelled)
        (should (null org-drill-presentation-timer))
        (should (= 0 org-drill-presentation-timer-calls))))))

;;;; Response-mode exit kinds

(defmacro with-fake-response-buffer (&rest body)
  "Run BODY in a buffer where org-drill-current-session is bound and
exit-recursive-edit and kill-buffer are stubbed."
  (declare (indent 0))
  `(let ((session (org-drill-session))
         (exited nil))
     (with-temp-buffer
       (setq-local org-drill-current-session session)
       (cl-letf (((symbol-function 'kill-buffer) #'ignore)
                 ((symbol-function 'exit-recursive-edit)
                  (lambda () (setq exited t))))
         ,@body)
       (cons session exited))))

(ert-deftest test-response-quit-sets-exit-kind-quit ()
  (let* ((result (with-fake-response-buffer
                   (org-drill-response-quit)))
         (session (car result)))
    (should (eq 'quit (oref session exit-kind)))
    (should (cdr result))))

(ert-deftest test-response-edit-sets-exit-kind-edit ()
  (let* ((result (with-fake-response-buffer
                   (org-drill-response-edit)))
         (session (car result)))
    (should (eq 'edit (oref session exit-kind)))
    (should (cdr result))))

(ert-deftest test-response-skip-sets-exit-kind-skip ()
  (let* ((result (with-fake-response-buffer
                   (org-drill-response-skip)))
         (session (car result)))
    (should (eq 'skip (oref session exit-kind)))
    (should (cdr result))))

(ert-deftest test-response-tags-sets-exit-kind-tags ()
  (let* ((result (with-fake-response-buffer
                   (org-drill-response-tags)))
         (session (car result)))
    (should (eq 'tags (oref session exit-kind)))
    (should (cdr result))))

(ert-deftest test-response-rtn-stores-typed-answer-and-exits ()
  "Return key captures buffer text into typed-answer and sets exit-kind t."
  (let ((session (org-drill-session))
        (exited nil))
    (with-temp-buffer
      (insert "my-answer")
      (setq-local org-drill-current-session session)
      (cl-letf (((symbol-function 'kill-buffer) #'ignore)
                ((symbol-function 'exit-recursive-edit)
                 (lambda () (setq exited t))))
        (org-drill-response-rtn)))
    (should (equal "my-answer" (oref session typed-answer)))
    (should (eq t (oref session exit-kind)))
    (should exited)))

;;;; Response buffer creation

(ert-deftest test-response-get-buffer-create-returns-fresh-buffer ()
  "The helper returns a buffer in `org-drill-response-mode' with empty content."
  (let ((buf (org-drill-response-get-buffer-create)))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'org-drill-response-mode))
          (should (= (point-min) (point-max))))
      (kill-buffer buf))))

(ert-deftest test-response-get-buffer-create-with-active-input-method-propagates ()
  "If an input method is active, it's activated in the new buffer."
  (let ((activated-with nil))
    (cl-letf (((symbol-function 'activate-input-method)
               (lambda (im) (setq activated-with im)))
              ((symbol-function 'set-input-method) #'ignore))
      (let ((current-input-method 'pretend-im))
        (let ((buf (org-drill-response-get-buffer-create)))
          (unwind-protect
              (should (eq 'pretend-im activated-with))
            (kill-buffer buf)))))))

(provide 'test-org-drill-response-and-timer)
;;; test-org-drill-response-and-timer.el ends here
