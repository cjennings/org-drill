;;; test-org-drill-read-rating-key.el --- Tests for --read-rating-key  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill--read-rating-key', the post-presentation key loop
;; shared by `org-drill-reschedule' and `org-drill-leitner-rebox'.
;;
;; The loop reads keys via `org-drill--read-key-sequence' until it gets a
;; 0-5 char, edit, quit, or C-g.  Three input shapes are recognized:
;;
;; - String (a typed character)
;; - Vector starting with a symbol (arrow / page keys)
;; - Vector starting with an event list (mouse wheel)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-keys-and-buffer (keys &rest body)
  "Run BODY with `org-drill--read-key-sequence' returning successive KEYS.
KEYS is a list — each call returns the next element."
  (declare (indent 1))
  `(let ((__remaining__ ,keys))
     (with-temp-buffer
       (insert "* Drill :drill:\nsome body line\nanother line\n")
       (goto-char (point-min))
       (cl-letf (((symbol-function 'org-drill--read-key-sequence)
                  (lambda (&rest _)
                    (or (pop __remaining__)
                        (error "Test ran out of mock keys"))))
                 ((symbol-function 'org-set-tags-command) #'ignore))
         ,@body))))

(ert-deftest test-read-rating-key-direct-numeric-returns-char ()
  "A string input \"3\" makes the loop return the ?3 character."
  (with-keys-and-buffer '("3")
    (should (eql ?3 (org-drill--read-rating-key nil "help")))))

(ert-deftest test-read-rating-key-quit-key-returns-quit ()
  "Quit key terminates the loop and returns the quit char."
  (with-keys-and-buffer (list (string org-drill--quit-key))
    (should (eql org-drill--quit-key
                 (org-drill--read-rating-key nil "help")))))

(ert-deftest test-read-rating-key-edit-key-returns-edit ()
  "Edit key terminates the loop and returns the edit char."
  (with-keys-and-buffer (list (string org-drill--edit-key))
    (should (eql org-drill--edit-key
                 (org-drill--read-rating-key nil "help")))))

(ert-deftest test-read-rating-key-arrow-key-then-numeric-returns-numeric ()
  "An arrow-key vector is processed (no terminal effect) and the next
numeric input ends the loop."
  (with-keys-and-buffer (list (vector 'down) "5")
    (should (eql ?5 (org-drill--read-rating-key nil "help")))))

(ert-deftest test-read-rating-key-wheel-event-then-numeric ()
  "A wheel-event vector is dispatched without terminating; loop ends on
the next numeric character."
  (let ((scroll-called nil))
    (cl-letf (((symbol-function 'mwheel-scroll)
               (lambda (&rest _) (setq scroll-called t))))
      (with-keys-and-buffer (list (vector '(wheel-up nil)) "0")
        (should (eql ?0 (org-drill--read-rating-key nil "help")))))))

(ert-deftest test-read-rating-key-help-key-shows-help-then-numeric ()
  "Pressing the help key on the first iteration causes the next prompt to
include the rating-help-block, then numeric ends the loop."
  (let ((seen-prompts nil))
    (cl-letf (((symbol-function 'org-drill--read-key-sequence)
               (let ((counter 0)
                     (keys (list (string org-drill--help-key) "4")))
                 (lambda (prompt)
                   (push prompt seen-prompts)
                   (cl-incf counter)
                   (pop keys))))
              ((symbol-function 'org-set-tags-command) #'ignore))
      (with-temp-buffer
        (insert "* Drill :drill:\nbody\n")
        (goto-char (point-min))
        (should (eql ?4 (org-drill--read-rating-key nil "RATING-HELP")))
        ;; Second prompt (after help key) carries the help block.
        (should (cl-some (lambda (p) (string-match-p "RATING-HELP" p))
                         seen-prompts))))))

(ert-deftest test-read-rating-key-tags-key-invokes-tags-command ()
  "If the tags key is used to terminate the loop, org-set-tags-command runs."
  (let ((tags-called nil))
    (cl-letf (((symbol-function 'org-drill--read-key-sequence)
               (let ((keys (list (string org-drill--tags-key))))
                 (lambda (&rest _) (pop keys))))
              ((symbol-function 'org-set-tags-command)
               (lambda (&rest _) (setq tags-called t))))
      ;; tags-key is not in the loop's terminal-set, so the loop would spin —
      ;; but `--read-rating-key' terminates only on quit/edit/C-g/0-5, so
      ;; tags-key alone won't end the loop.  We feed quit-key as a follow-up.
      (cl-letf (((symbol-function 'org-drill--read-key-sequence)
                 (let ((keys (list (string org-drill--tags-key)
                                   (string org-drill--quit-key))))
                   (lambda (&rest _) (pop keys)))))
        (with-temp-buffer
          (insert "* Drill :drill:\nbody\n")
          (org-drill--read-rating-key nil "help")
          (should tags-called))))))

(ert-deftest test-read-rating-key-typed-answer-shown-in-prompt ()
  "A non-nil typed-answer arg appears in the prompt as 'Your answer: ...'."
  (let ((seen-prompt nil))
    (cl-letf (((symbol-function 'org-drill--read-key-sequence)
               (lambda (prompt) (setq seen-prompt prompt) "0"))
              ((symbol-function 'org-set-tags-command) #'ignore))
      (with-temp-buffer
        (org-drill--read-rating-key "my-typed-answer" "help"))
      (should (string-match-p "Your answer: my-typed-answer" seen-prompt)))))

(provide 'test-org-drill-read-rating-key)
;;; test-org-drill-read-rating-key.el ends here
