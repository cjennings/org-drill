;;; test-org-drill-display-restore.el --- Regression for cross-buffer display restore  -*- lexical-binding: t; -*-

;;; Commentary:
;; The display setup / restore pair saved buffer-local state (mode-line,
;; variable-pitch-mode) to global defvars and restored via plain
;; `setq-local'.  If the user switched buffers between setup and
;; restore, the restore wrote to the wrong buffer — leaving the
;; original drill buffer with its modeline still hidden, and trampling
;; the destination buffer's modeline.
;;
;; The fix tracks the buffer at setup and uses `with-current-buffer'
;; to restore in the original.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Cross-buffer modeline restore

(ert-deftest test-display-restore-modeline-in-original-buffer-after-buffer-switch ()
  "Setup in buffer A, restore in buffer B — A's mode-line should be restored,
B's mode-line untouched."
  (let ((buffer-a (generate-new-buffer "*org-drill-test-A*"))
        (buffer-b (generate-new-buffer "*org-drill-test-B*"))
        (org-drill-hide-modeline-during-session t)
        (org-drill-text-size-during-session nil)
        (org-drill-use-variable-pitch nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer-a
            (setq-local mode-line-format "A-original")
            (org-drill--setup-display)
            ;; A's modeline is now hidden (nil).
            (should (null mode-line-format)))
          (with-current-buffer buffer-b
            (setq-local mode-line-format "B-original")
            ;; Restore runs from buffer B (simulating a buffer switch
            ;; mid-session).
            (org-drill--restore-display))
          ;; After restore: A's modeline back to original, B's untouched.
          (with-current-buffer buffer-a
            (should (equal "A-original" mode-line-format)))
          (with-current-buffer buffer-b
            (should (equal "B-original" mode-line-format))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

(provide 'test-org-drill-display-restore)

;;; test-org-drill-display-restore.el ends here
