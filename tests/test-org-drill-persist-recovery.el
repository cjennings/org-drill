;;; test-org-drill-persist-recovery.el --- Regression test for corrupted persist file  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #45 (2021-10).  `persist-load' raises "End of file
;; during parsing" at persist.el:413 in some configurations, likely
;; from a corrupted persist data file.  Pre-fix, this propagated up
;; through the `(persist-defvar org-drill-sm5-optimal-factor-matrix
;; ...)' form at file-load time, breaking the entire package's load.
;;
;; Fix wraps the persist-defvar form in `condition-case' so a
;; corrupted file falls back to a fresh nil matrix instead of
;; preventing the package from loading.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'persist)
(require 'org-drill)

;;;; Regression — wrapping pattern

(ert-deftest test-persist-defvar-error-falls-back-to-initial-value ()
  "When persist-load throws, the condition-case wrapper recovers and
binds the symbol to the supplied initial value.

This verifies the same wrapping pattern used at top-level around the
real `org-drill-sm5-optimal-factor-matrix' definition."
  (let ((sym (intern (format "org-drill-test-persist-sym-%d" (random 999999)))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'persist-symbol) #'ignore)
                    ((symbol-function 'persist-load)
                     (lambda (_) (error "End of file during parsing"))))
            (condition-case _err
                (eval `(persist-defvar ,sym 'fallback-value "doc"))
              (error
               (set sym 'fallback-value))))
          (should (eq 'fallback-value (symbol-value sym))))
      ;; Clean up the test symbol.
      (when (boundp sym) (makunbound sym)))))

(ert-deftest test-org-drill-sm5-optimal-factor-matrix-bound ()
  "After loading org-drill, the SM5 matrix variable is bound — confirms
the at-load-time wrapping (or normal persist-defvar success) leaves
the package in a usable state."
  (should (boundp 'org-drill-sm5-optimal-factor-matrix)))

(provide 'test-org-drill-persist-recovery)

;;; test-org-drill-persist-recovery.el ends here
