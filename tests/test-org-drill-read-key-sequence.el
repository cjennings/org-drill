;;; test-org-drill-read-key-sequence.el --- Tests for input-method-safe key reading  -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for `org-drill--read-key-sequence'.
;;
;; Upstream issues #52 (breadncup, 2023-11-24) and #58 (breadncup,
;; 2024-12-31) reported that running an org-drill session silently
;; nulled out the user's `default-input-method'.
;;
;; Root cause: the wrapper called `(set-input-method nil)' to disable
;; the input method during the key read.  In Emacs, calling
;; `set-input-method' with nil while no input method is active has the
;; documented side effect of clearing `default-input-method' as well —
;; clobbering the user's persistent setting.  The fix is to use the
;; primitive that's specifically scoped to current state:
;; `deactivate-input-method' on the way out, and `activate-input-method'
;; on the way back.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-drill)

;;;; Helpers

(defmacro with-stubbed-key-sequence (return-value &rest body)
  "Run BODY with `read-key-sequence' replaced by a stub that returns RETURN-VALUE.
Avoids the interactive prompt during batch test runs."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'read-key-sequence)
              (lambda (_prompt) ,return-value)))
     ,@body))

;;;; Normal cases

(ert-deftest test-org-drill-read-key-sequence-normal-no-input-method-set ()
  "When neither default- nor current-input-method is set, both stay nil."
  (let ((default-input-method nil)
        (current-input-method nil))
    (with-stubbed-key-sequence "x"
      (org-drill--read-key-sequence "prompt: "))
    (should (null default-input-method))
    (should (null current-input-method))))

(ert-deftest test-org-drill-read-key-sequence-normal-returns-stubbed-value ()
  "The wrapper returns whatever `read-key-sequence' returns."
  (let ((default-input-method nil)
        (current-input-method nil))
    (should (equal "answer"
                   (with-stubbed-key-sequence "answer"
                     (org-drill--read-key-sequence "prompt: "))))))

;;;; Boundary / regression — issues #52 and #58

(ert-deftest test-org-drill-read-key-sequence-preserves-default-when-no-current ()
  "Issue #52/#58 regression: default-input-method must survive the call.

The reporter (breadncup) had `default-input-method' set to a Korean
input method but no input method active at session start.  Each
org-drill rating prompt would silently clear `default-input-method'."
  (let ((default-input-method "TeX")
        (current-input-method nil))
    (with-stubbed-key-sequence "x"
      (org-drill--read-key-sequence "prompt: "))
    (should (equal "TeX" default-input-method))))

(ert-deftest test-org-drill-read-key-sequence-preserves-default-across-multiple-calls ()
  "Repeated calls during a session don't accumulate damage to default-input-method."
  (let ((default-input-method "TeX")
        (current-input-method nil))
    (with-stubbed-key-sequence "x"
      (dotimes (_ 5)
        (org-drill--read-key-sequence "prompt: ")))
    (should (equal "TeX" default-input-method))))

;;;; Error cases

(ert-deftest test-org-drill-read-key-sequence-error-restores-default-after-throw ()
  "If `read-key-sequence' errors, unwind-protect must still leave default-input-method intact."
  (let ((default-input-method "TeX")
        (current-input-method nil))
    (cl-letf (((symbol-function 'read-key-sequence)
               (lambda (_prompt) (error "simulated read failure"))))
      (should-error (org-drill--read-key-sequence "prompt: ")))
    (should (equal "TeX" default-input-method))))

;;;; Same bug pattern in org-drill-response-get-buffer-create

(ert-deftest test-org-drill-response-get-buffer-create-preserves-default-input-method ()
  "Issue #52/#58 regression in the response-buffer path.

`org-drill-response-get-buffer-create' captures `current-input-method'
from the caller, then propagates it to the new *Org-Drill* buffer via
`set-input-method'.  When the caller has no input method active, the
captured value is nil — and `(set-input-method nil)' clears the global
`default-input-method'.  Same bug pattern, different function."
  (let ((default-input-method "TeX")
        (current-input-method nil))
    (unwind-protect
        (org-drill-response-get-buffer-create)
      (when (get-buffer "*Org-Drill*")
        (kill-buffer "*Org-Drill*")))
    (should (equal "TeX" default-input-method))))

(provide 'test-org-drill-read-key-sequence)

;;; test-org-drill-read-key-sequence.el ends here
