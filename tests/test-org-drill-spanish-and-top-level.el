;;; test-org-drill-spanish-and-top-level.el --- Tests for Spanish verb presenter and top-level commands  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for:
;;
;; - `org-drill-present-spanish-verb': six-way cl-case dispatch on
;;   cl-random, each case shows a different tense + visibility combo
;; - `org-drill-cram', `org-drill-cram-tree', `org-drill-tree',
;;   `org-drill-directory', `org-drill-again', `org-drill-resume':
;;   thin wrappers around `org-drill' with specific args.
;;
;; All call `org-drill-presentation-prompt' or `org-drill' itself,
;; which we mock to spy on the dispatch.

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

(defmacro with-mocked-org-drill (&rest body)
  "Run BODY with `org-drill' replaced by a stub that records its args
to a captured `org-drill-args' variable."
  `(let ((org-drill-args nil))
     (cl-letf (((symbol-function 'org-drill)
                (lambda (&rest args) (setq org-drill-args args))))
       ,@body)))

;;;; org-drill-present-spanish-verb (six branches)

(ert-deftest test-present-spanish-verb-branch-0-present-tense-translate ()
  (with-card-buffer "* Verb :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: spanish_verb
:END:

** Infinitive
hablar
** English
to speak
** Present Tense
hablo, hablas, ...
"
    (let ((shown-prompt nil))
      (cl-letf (((symbol-function 'cl-random) (lambda (_) 0))
                ((symbol-function 'org-drill-presentation-prompt)
                 (lambda (_session &optional prompt &rest _)
                   (setq shown-prompt prompt) t))
                ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
                ((symbol-function 'org-display-inline-images) #'ignore))
        (org-drill-present-spanish-verb (org-drill-session))
        (should (string-match-p "present.* tense" shown-prompt))
        (should (string-match-p "Translate" shown-prompt))))))

(ert-deftest test-present-spanish-verb-branch-2-past-tense-translate ()
  (with-card-buffer "* Verb :drill:\n** Infinitive\nhablar\n** English\nto speak\n** Past Tense\nfoo\n"
    (let ((shown-prompt nil))
      (cl-letf (((symbol-function 'cl-random) (lambda (_) 2))
                ((symbol-function 'org-drill-presentation-prompt)
                 (lambda (_session &optional prompt &rest _)
                   (setq shown-prompt prompt) t))
                ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
                ((symbol-function 'org-display-inline-images) #'ignore))
        (org-drill-present-spanish-verb (org-drill-session))
        (should (string-match-p "past.* tense" shown-prompt))))))

(ert-deftest test-present-spanish-verb-branch-4-future-perfect ()
  (with-card-buffer "* Verb :drill:\n** Infinitive\nhablar\n** English\nfoo\n** Future Perfect Tense\nbar\n"
    (let ((shown-prompt nil))
      (cl-letf (((symbol-function 'cl-random) (lambda (_) 4))
                ((symbol-function 'org-drill-presentation-prompt)
                 (lambda (_session &optional prompt &rest _)
                   (setq shown-prompt prompt) t))
                ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
                ((symbol-function 'org-display-inline-images) #'ignore))
        (org-drill-present-spanish-verb (org-drill-session))
        (should (string-match-p "future perfect" shown-prompt))))))

;;;; org-drill-cram and friends

(ert-deftest test-org-drill-cram-passes-cram-flag ()
  "org-drill-cram delegates to org-drill with cram=t."
  (with-mocked-org-drill
    (org-drill-cram)
    ;; Args: (scope drill-match resume-p cram) → (nil nil nil t)
    (should (equal '(nil nil nil t) org-drill-args))))

(ert-deftest test-org-drill-cram-passes-scope ()
  (with-mocked-org-drill
    (org-drill-cram 'tree "+drill")
    (should (equal '(tree "+drill" nil t) org-drill-args))))

(ert-deftest test-org-drill-cram-tree-uses-tree-scope ()
  (let ((cram-args nil))
    (cl-letf (((symbol-function 'org-drill-cram)
               (lambda (&rest args) (setq cram-args args))))
      (org-drill-cram-tree)
      (should (equal '(tree) cram-args)))))

(ert-deftest test-org-drill-tree-uses-tree-scope ()
  (with-mocked-org-drill
    (org-drill-tree)
    (should (eq 'tree (car org-drill-args)))))

(ert-deftest test-org-drill-directory-uses-directory-scope ()
  (with-mocked-org-drill
    (org-drill-directory)
    (should (eq 'directory (car org-drill-args)))))

;;;; org-drill-again

(ert-deftest test-org-drill-again-with-leftover-items-resumes ()
  "If the last session has pending entries, org-drill-again resumes (resume-p=t)."
  (let ((session (org-drill-session))
        (org-drill-args nil))
    (oset session new-entries (list (let ((m (make-marker))) (set-marker m 1) m)))
    (oset session start-time (float-time (current-time)))
    (let ((org-drill-last-session session))
      (cl-letf (((symbol-function 'org-drill)
                 (lambda (&rest args) (setq org-drill-args args))))
        (org-drill-again)
        ;; Should pass resume-p=t (third positional arg)
        (should (eq t (nth 2 org-drill-args)))))))

(ert-deftest test-org-drill-again-empty-session-starts-fresh ()
  "If the last session has no pending entries, fall through to a fresh drill."
  (let ((session (org-drill-session))
        (org-drill-args nil))
    (let ((org-drill-last-session session))
      (cl-letf (((symbol-function 'org-drill)
                 (lambda (&rest args) (setq org-drill-args args))))
        (org-drill-again)
        ;; Fresh: no resume-p flag.
        (should (or (null org-drill-args)
                    (null (nth 2 org-drill-args))))))))

(provide 'test-org-drill-spanish-and-top-level)

;;; test-org-drill-spanish-and-top-level.el ends here
