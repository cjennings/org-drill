;;; test-org-drill-display-state.el --- Tests for display-state save/restore  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill--setup-display' and `org-drill--restore-display',
;; which save the user's text scale, variable-pitch mode, and modeline at
;; session start and put them back at session end.  Also covers the
;; `directory' branch of `org-drill-current-scope'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; --save-display-state / --restore-display

(ert-deftest test-save-display-state-records-current-buffer ()
  "After save, `org-drill--saved-display-buffer' holds the buffer."
  (with-temp-buffer
    (let ((org-drill-text-size-during-session nil)
          (org-drill-use-variable-pitch nil)
          (org-drill-hide-modeline-during-session nil)
          (org-drill--saved-display-buffer nil))
      (org-drill--setup-display)
      (should (eq (current-buffer) org-drill--saved-display-buffer)))))

(ert-deftest test-save-display-state-skips-when-flags-nil ()
  "With every flag nil, save records buffer but doesn't capture face/mode state."
  (with-temp-buffer
    (let ((org-drill-text-size-during-session nil)
          (org-drill-use-variable-pitch nil)
          (org-drill-hide-modeline-during-session nil)
          (org-drill--saved-display-buffer nil)
          (org-drill--saved-text-scale 'sentinel)
          (org-drill--saved-variable-pitch-mode 'unbound)
          (org-drill--saved-modeline-format 'sentinel))
      (org-drill--setup-display)
      ;; Saved-text-scale is left untouched because the flag was nil.
      (should (eq 'sentinel org-drill--saved-text-scale))
      (should (eq 'unbound org-drill--saved-variable-pitch-mode))
      (should (eq 'sentinel org-drill--saved-modeline-format)))))

(ert-deftest test-save-display-state-captures-modeline-when-hide-flag-set ()
  "With `org-drill-hide-modeline-during-session' t, the modeline is saved and cleared."
  (with-temp-buffer
    (let ((org-drill-text-size-during-session nil)
          (org-drill-use-variable-pitch nil)
          (org-drill-hide-modeline-during-session t)
          (org-drill--saved-display-buffer nil)
          (org-drill--saved-modeline-format nil))
      (setq-local mode-line-format "marker-mode-line")
      (org-drill--setup-display)
      (should (equal "marker-mode-line" org-drill--saved-modeline-format))
      (should (null mode-line-format)))))

(ert-deftest test-restore-display-puts-modeline-back ()
  "Restore sets mode-line-format on the saved-display-buffer back to its prior value."
  (with-temp-buffer
    (let ((org-drill-text-size-during-session nil)
          (org-drill-use-variable-pitch nil)
          (org-drill-hide-modeline-during-session t)
          (org-drill--saved-display-buffer (current-buffer))
          (org-drill--saved-text-scale nil)
          (org-drill--saved-variable-pitch-mode 'unbound)
          (org-drill--saved-modeline-format "saved-modeline"))
      (setq-local mode-line-format nil)
      (org-drill--restore-display)
      (should (equal "saved-modeline" mode-line-format))
      (should (null org-drill--saved-modeline-format))
      (should (null org-drill--saved-display-buffer)))))

(ert-deftest test-restore-display-handles-killed-buffer-target ()
  "If the saved-display-buffer was killed, restore must not crash."
  (let ((tmp (generate-new-buffer " *drill-display-tmp*")))
    (let ((org-drill-text-size-during-session nil)
          (org-drill-use-variable-pitch nil)
          (org-drill-hide-modeline-during-session t)
          (org-drill--saved-display-buffer tmp)
          (org-drill--saved-text-scale nil)
          (org-drill--saved-variable-pitch-mode 'unbound)
          (org-drill--saved-modeline-format "saved-modeline"))
      (kill-buffer tmp)
      (org-drill--restore-display)
      (should (null org-drill--saved-display-buffer)))))

(ert-deftest test-restore-display-restores-variable-pitch-mode ()
  "Restore turns variable-pitch-mode back on in the saved-display-buffer
when it was active at session start."
  (with-temp-buffer
    (let ((toggled nil))
      (cl-letf (((symbol-function 'variable-pitch-mode)
                 (lambda (arg) (setq toggled arg))))
        (let ((org-drill-text-size-during-session nil)
              (org-drill-use-variable-pitch t)
              (org-drill-hide-modeline-during-session nil)
              (org-drill--saved-display-buffer (current-buffer))
              (org-drill--saved-text-scale nil)
              (org-drill--saved-variable-pitch-mode t)
              (org-drill--saved-modeline-format nil))
          (org-drill--restore-display)
          (should (= 1 toggled))
          (should (null org-drill--saved-variable-pitch-mode)))))))

(ert-deftest test-restore-display-disables-variable-pitch-when-it-was-off ()
  "Restore turns variable-pitch-mode off when it was off at session start."
  (with-temp-buffer
    (let ((toggled nil))
      (cl-letf (((symbol-function 'variable-pitch-mode)
                 (lambda (arg) (setq toggled arg))))
        (let ((org-drill-text-size-during-session nil)
              (org-drill-use-variable-pitch t)
              (org-drill-hide-modeline-during-session nil)
              (org-drill--saved-display-buffer (current-buffer))
              (org-drill--saved-text-scale nil)
              (org-drill--saved-variable-pitch-mode nil)
              (org-drill--saved-modeline-format nil))
          (org-drill--restore-display)
          (should (= -1 toggled)))))))

(ert-deftest test-restore-display-restores-text-scale ()
  "Restore puts the default face's :height back to the saved value."
  (let ((set-args nil))
    (cl-letf (((symbol-function 'set-face-attribute)
               (lambda (&rest args) (push args set-args))))
      (let ((org-drill-text-size-during-session 14)
            (org-drill-use-variable-pitch nil)
            (org-drill-hide-modeline-during-session nil)
            (org-drill--saved-display-buffer (current-buffer))
            (org-drill--saved-text-scale 100)
            (org-drill--saved-variable-pitch-mode 'unbound)
            (org-drill--saved-modeline-format nil))
        (org-drill--restore-display)
        (should (cl-some (lambda (a) (memq 100 a)) set-args))
        (should (null org-drill--saved-text-scale))))))

;;;; org-drill-current-scope (directory branch)

(ert-deftest test-org-drill-current-scope-directory-returns-org-files-list ()
  "The `directory' scope expands to a list of .org files in the buffer's directory."
  (let* ((tmpdir (make-temp-file "org-drill-scope-" t))
         (a (expand-file-name "a.org" tmpdir))
         (b (expand-file-name "b.org" tmpdir))
         (skip (expand-file-name ".hidden.org" tmpdir))
         (other (expand-file-name "c.txt" tmpdir)))
    (unwind-protect
        (progn
          (write-region "" nil a)
          (write-region "" nil b)
          (write-region "" nil skip)
          (write-region "" nil other)
          (with-current-buffer (find-file-noselect a)
            (let ((files (org-drill-current-scope 'directory)))
              (should (member a files))
              (should (member b files))
              (should-not (member skip files))
              (should-not (member other files)))))
      (when (file-exists-p a) (delete-file a))
      (when (file-exists-p b) (delete-file b))
      (when (file-exists-p skip) (delete-file skip))
      (when (file-exists-p other) (delete-file other))
      (delete-directory tmpdir))))

(provide 'test-org-drill-display-state)
;;; test-org-drill-display-state.el ends here
