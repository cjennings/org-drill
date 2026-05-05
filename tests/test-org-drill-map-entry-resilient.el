;;; test-org-drill-map-entry-resilient.el --- Regression for #53  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #53 (2024-02).  Running org-drill on a buffer with a
;; new (no-ID) entry threw "Wrong Type Argument: hash-table-p, nil",
;; and — worse — stopped the scan so subsequent items were silently
;; skipped.  User had to re-run org-drill once per item.
;;
;; The exact source of the hash-table error depends on Emacs/Org/Doom
;; configuration, but the user-facing failure mode is recoverable: one
;; entry's error shouldn't kill the whole collection scan.
;;
;; Fix: wrap the body of `org-drill-map-entry-function' in
;; `condition-case' so an error on one entry surfaces a message and
;; the scan continues with the next entry.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression — #53

(ert-deftest test-map-entry-function-survives-error-on-one-entry ()
  "If one entry triggers an error during ID/state work, subsequent
entries are still processed.  Pre-fix the error escaped and the
whole scan stopped."
  (let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmpfile)
          (let ((org-startup-folded nil))
            (insert "* First :drill:\nbody one\n* Second :drill:\nbody two\n")
            (goto-char (point-min))
            (let ((session (org-drill-session))
                  (call-count 0))
              (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                        ((symbol-function 'sit-for) #'ignore)
                        ((symbol-function 'org-drill-id-get-create-with-warning)
                         (lambda (_)
                           (cl-incf call-count)
                           ;; Fail only on the first entry.
                           (when (= call-count 1)
                             (signal 'wrong-type-argument
                                     '(hash-table-p nil))))))
                (org-drill-map-entries
                 (apply-partially #'org-drill-map-entry-function session)
                 nil)
                ;; Both entries processed (call-count = 2), and the
                ;; second one made it through to the session queue.
                (should (= 2 call-count))
                (should (= 1 (length (oref session new-entries))))))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(provide 'test-org-drill-map-entry-resilient)

;;; test-org-drill-map-entry-resilient.el ends here
