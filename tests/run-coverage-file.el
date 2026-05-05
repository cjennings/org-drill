;;; run-coverage-file.el --- Undercover setup for per-file coverage runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by `make coverage' before each test file runs, BEFORE
;; org-drill.el is loaded.  Instrumenting must happen first so the
;; subsequent `(require 'org-drill)' or `-l org-drill.el' picks up the
;; instrumented source.
;;
;; Coverage data is merged across per-file invocations into a single
;; simplecov JSON at .coverage/simplecov.json.

;;; Code:

(unless (require 'undercover nil t)
  (message "")
  (message "ERROR: undercover not installed.")
  (message "Add `undercover' to Cask as a development dep, then run `make setup'.")
  (message "")
  (kill-emacs 1))

;; Force coverage collection in non-CI environments.  Must be set after
;; loading undercover because the library's top-level form
;; `(setq undercover-force-coverage (getenv "UNDERCOVER_FORCE"))' would
;; otherwise overwrite the value.
(setq undercover-force-coverage t)

(undercover "org-drill.el"
            (:report-format 'simplecov)
            (:report-file ".coverage/simplecov.json")
            (:merge-report t)
            (:send-report nil))

;;; run-coverage-file.el ends here
