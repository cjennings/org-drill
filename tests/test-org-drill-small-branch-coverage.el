;;; test-org-drill-small-branch-coverage.el --- Tests for small branch coverage  -*- lexical-binding: t; -*-

;;; Commentary:
;; Small branch-coverage gaps that don't fit a thematic file: SM5
;; interval with random-noise enabled, the `--read-key-sequence' input-
;; method dance, and `goto-drill-entry-heading''s error branch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; SM5 interval with random-noise enabled

(ert-deftest test-determine-next-interval-sm5-with-random-noise ()
  "When the random-noise flag is t, the SM5 next-interval is multiplied by
the dispersal factor."
  (let ((org-drill-add-random-noise-to-intervals-p t))
    (cl-letf (((symbol-function 'org-drill-random-dispersal-factor)
               (lambda () 1.5)))
      (let* ((result (org-drill-determine-next-interval-sm5
                      4.0 2 2.5 5 0 5.0 1
                      org-drill-sm5-optimal-factor-matrix))
             (next (nth 0 result)))
        (should (numberp next))
        (should (> next 0))))))

;;;; --read-key-sequence input-method dance

(ert-deftest test-read-key-sequence-deactivates-and-reactivates-input-method ()
  "If an input method is active, --read-key-sequence deactivates it for the
read and re-activates it on the way out."
  (let ((current-input-method 'pretend-im)
        (deactivated nil)
        (reactivated nil))
    (cl-letf (((symbol-function 'deactivate-input-method)
               (lambda () (setq deactivated t)))
              ((symbol-function 'activate-input-method)
               (lambda (im) (setq reactivated im)))
              ((symbol-function 'read-key-sequence)
               (lambda (&rest _) "x")))
      (org-drill--read-key-sequence "p"))
    (should deactivated)
    (should (eq 'pretend-im reactivated))))

(ert-deftest test-read-key-sequence-no-input-method-skips-deactivate ()
  "With no input method active, neither deactivate nor activate is called."
  (let ((current-input-method nil)
        (deactivated nil)
        (reactivated nil))
    (cl-letf (((symbol-function 'deactivate-input-method)
               (lambda () (setq deactivated t)))
              ((symbol-function 'activate-input-method)
               (lambda (_) (setq reactivated t)))
              ((symbol-function 'read-key-sequence)
               (lambda (&rest _) "x")))
      (org-drill--read-key-sequence "p"))
    (should-not deactivated)
    (should-not reactivated)))

;;;; goto-drill-entry-heading error branch

(ert-deftest test-goto-drill-entry-heading-no-drill-parent-errors ()
  "When no ancestor heading carries the drill tag, the function errors."
  (with-temp-buffer
    (insert "* Plain :tagged-but-not-drill:\n** Child\nbody\n")
    (org-mode)
    (goto-char (point-max))
    (cl-letf (((symbol-function 'org-drill-part-of-drill-entry-p)
               (lambda (&rest _) t))
              ;; Force entry-p false at every level so the while loop walks
              ;; up until org-up-heading-safe returns nil.
              ((symbol-function 'org-drill-entry-p) (lambda (&rest _) nil)))
      (should-error (org-drill-goto-drill-entry-heading)))))

;;;; org-drill-add-cloze-fontification

(ert-deftest test-add-cloze-fontification-with-flag-extends-keywords ()
  "When `org-drill-use-visible-cloze-face-p' is t, the cloze keyword spec is
added to `org-font-lock-extra-keywords'."
  (with-temp-buffer
    (let ((org-drill-use-visible-cloze-face-p t)
          (org-font-lock-extra-keywords nil))
      (org-drill-add-cloze-fontification)
      (should org-font-lock-extra-keywords))))

(ert-deftest test-add-cloze-fontification-without-flag-leaves-keywords-untouched ()
  "When the flag is nil, no entry is added to `org-font-lock-extra-keywords'."
  (with-temp-buffer
    (let ((org-drill-use-visible-cloze-face-p nil)
          (org-font-lock-extra-keywords nil))
      (org-drill-add-cloze-fontification)
      (should (null org-font-lock-extra-keywords)))))

(provide 'test-org-drill-small-branch-coverage)
;;; test-org-drill-small-branch-coverage.el ends here
