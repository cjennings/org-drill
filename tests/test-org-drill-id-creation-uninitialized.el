;;; test-org-drill-id-creation-uninitialized.el --- Regression for #53  -*- lexical-binding: t; -*-

;;; Commentary:
;; Upstream issue #53 (2024-02).  Running `org-drill' on a buffer with a
;; new (no-ID) entry would throw "Wrong Type Argument: hash-table-p,
;; nil" when `org-id-locations' hadn't been initialized yet.  The item
;; still got an ID (org-id-get-create succeeded for that one), but the
;; thrown error stopped session collection — subsequent items were
;; silently skipped.
;;
;; Fix: ensure org-id is loaded and org-id-locations is a hash table
;; before calling org-id-get-create from
;; `org-drill-id-get-create-with-warning'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'org-drill)

;;;; Regression — #53

(ert-deftest test-id-get-create-with-warning-survives-uninitialized-id-locations ()
  "When org-id-locations is nil, org-id-get-create normally fails with
hash-table-p, nil.  The fix ensures it's initialized before the call."
  (let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmpfile)
          (let ((org-startup-folded nil))
            (insert "* Question :drill:\nbody\n")
            (goto-char (point-min))
            (let ((session (org-drill-session))
                  ;; Force the bug condition: org-id-locations as nil.
                  (org-id-locations nil))
              (cl-letf (((symbol-function 'sit-for) #'ignore))
                ;; Should not error.
                (let ((id (org-drill-id-get-create-with-warning session)))
                  (should (stringp id)))))))
      (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(provide 'test-org-drill-id-creation-uninitialized)

;;; test-org-drill-id-creation-uninitialized.el ends here
