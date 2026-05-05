;;; test-org-drill-tty-safe.el --- Regression for TTY/no-window-system error  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #44 (2021-10).  Running org-drill in a TTY emacsclient
;; (e.g., inside tmux) produced "error: Window system frame should be
;; used" because the LaTeX preview helpers (`org-latex-preview',
;; `org--latex-preview-region') require a window-system frame and
;; weren't guarded.
;;
;; Fix: gate `org-drill--show-latex-fragments' on `display-graphic-p',
;; making it a no-op on TTY.  LaTeX previews are inherently graphical
;; — the right behavior in TTY is to skip the preview rather than
;; crash the session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression — #44

(ert-deftest test-show-latex-fragments-noop-on-tty ()
  "On a non-graphic display, the helper should be a silent no-op
rather than calling org-latex-preview (which requires a window
system frame)."
  (let ((latex-preview-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
              ((symbol-function 'org-clear-latex-preview)
               (lambda () (setq latex-preview-called 'cleared)))
              ((symbol-function 'org-latex-preview)
               (lambda (&rest _) (setq latex-preview-called 'displayed))))
      (org-drill--show-latex-fragments)
      (should (null latex-preview-called)))))

(ert-deftest test-show-latex-fragments-runs-on-graphic-display ()
  "On a graphic display, the helper still calls through to org-latex-preview."
  (let ((latex-preview-called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () t))
              ((symbol-function 'org-clear-latex-preview) #'ignore)
              ((symbol-function 'org-latex-preview)
               (lambda (&rest _) (setq latex-preview-called t))))
      (org-drill--show-latex-fragments)
      (should latex-preview-called))))

(provide 'test-org-drill-tty-safe)

;;; test-org-drill-tty-safe.el ends here
