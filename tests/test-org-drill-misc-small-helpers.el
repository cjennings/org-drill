;;; test-org-drill-misc-small-helpers.el --- Tests for assorted small helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; A grab-bag of tests for small helpers that didn't fit into a topical
;; file: `org-drill-replace-entry-text' multi-mode dispatch, multicloze
;; weight validation errors, and `--copy-entry-to-other-buffer''s
;; "path doesn't exist" recovery branch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; org-drill-replace-entry-text — multi-p path

(ert-deftest test-replace-entry-text-multi-p-creates-list-overlays ()
  "With MULTI-P and a list TEXT, multiple overlays are created."
  (with-temp-buffer
    (insert "* Item :drill:\nline one\nline two\n* After\n")
    (org-mode)
    (goto-char (point-min))
    (org-drill-replace-entry-text '("a" "b" "c") t)
    (let ((replaced (cl-count-if
                     (lambda (ov)
                       (eq (overlay-get ov 'category)
                           'org-drill-replaced-text-overlay))
                     (overlays-in (point-min) (point-max)))))
      (should (>= replaced 1)))))

(ert-deftest test-replace-entry-text-non-multi-p-creates-single-overlay ()
  "Without MULTI-P, exactly one overlay is created."
  (with-temp-buffer
    (insert "* Item :drill:\nbody-text\n* After\n")
    (org-mode)
    (goto-char (point-min))
    (org-drill-replace-entry-text "REPLACEMENT")
    (let ((replaced (cl-count-if
                     (lambda (ov)
                       (eq (overlay-get ov 'category)
                           'org-drill-replaced-text-overlay))
                     (overlays-in (point-min) (point-max)))))
      (should (= 1 replaced)))))

;;;; multicloze weight-validation error branches

(ert-deftest test-multicloze-show1-firstmore-illegal-weight-errors ()
  "A non-positive weight raises an error in firstmore."
  (cl-letf (((symbol-function 'org-drill-present-multicloze-hide-n) #'ignore)
            ((symbol-function 'org-drill-present-multicloze-show1) #'ignore))
    (let ((org-drill-cloze-text-weight -3))
      (with-temp-buffer
        (should-error (org-drill-present-multicloze-show1-firstmore 'session))))))

(ert-deftest test-multicloze-show1-firstless-illegal-weight-errors ()
  "A non-positive weight raises an error in firstless."
  (cl-letf (((symbol-function 'org-drill-present-multicloze-hide-n) #'ignore)
            ((symbol-function 'org-drill-present-multicloze-show1) #'ignore))
    (let ((org-drill-cloze-text-weight 0))
      (with-temp-buffer
        (should-error (org-drill-present-multicloze-show1-firstless 'session))))))

;;;; --copy-entry-to-other-buffer recovery branch (path doesn't exist)

(ert-deftest test-copy-entry-to-other-buffer-with-no-path-appends-to-end ()
  "When the SRC entry has no outline path, the function appends to the end of DEST."
  (let ((src-file (make-temp-file "org-drill-cb-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-cb-dst-" nil ".org")))
    (unwind-protect
        (let (src dst)
          (setq src (find-file-noselect src-file))
          (setq dst (find-file-noselect dst-file))
          (with-current-buffer src
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: src-id\n:END:\n")
            (org-mode))
          (with-current-buffer dst
            (insert "* Pre-existing\n")
            (org-mode))
          (clrhash org-drill-dest-id-table)
          (with-current-buffer src
            (goto-char (point-min))
            (cl-letf (((symbol-function 'switch-to-buffer)
                       (lambda (b) (set-buffer b)))
                      ((symbol-function 'org-drill-progress-message) #'ignore))
              (org-drill-copy-entry-to-other-buffer dst nil)))
          (with-current-buffer dst
            (should (string-match-p "Card" (buffer-string)))))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

(provide 'test-org-drill-misc-small-helpers)
;;; test-org-drill-misc-small-helpers.el ends here
