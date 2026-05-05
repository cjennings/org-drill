;;; test-org-drill-migrate-from-source.el --- Tests for --migrate-from-source  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill--migrate-from-source', which walks SRC and copies
;; scheduling data into matching DEST entries (matched by ID).  The cond
;; has three branches:
;;
;; - SRC entry has no ID, or isn't a drill entry → skip
;; - SRC entry's ID matches a DEST ID → copy scheduling, drop from table
;; - SRC entry has an ID with no DEST match → either copy as new item, or
;;   skip when ignore-new-items-p is t

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-two-org-tempfiles (src-content dest-content &rest body)
  "Run BODY with SRC-BUF and DEST-BUF bound to org-mode buffers.
SRC-CONTENT is loaded into SRC-BUF, DEST-CONTENT into DEST-BUF."
  (declare (indent 2))
  `(let ((src-file (make-temp-file "org-drill-mig-src-" nil ".org"))
         (dest-file (make-temp-file "org-drill-mig-dst-" nil ".org"))
         src-buf dest-buf)
     (unwind-protect
         (progn
           (setq src-buf (find-file-noselect src-file))
           (setq dest-buf (find-file-noselect dest-file))
           (with-current-buffer src-buf
             (let ((org-startup-folded nil))
               (insert ,src-content)
               (org-mode)))
           (with-current-buffer dest-buf
             (let ((org-startup-folded nil))
               (insert ,dest-content)
               (org-mode)))
           ,@body)
       (when (file-exists-p src-file) (delete-file src-file))
       (when (file-exists-p dest-file) (delete-file dest-file)))))

(ert-deftest test-migrate-matching-id-copies-scheduling ()
  "When SRC has an ID matching DEST, scheduling lands on DEST."
  (with-two-org-tempfiles
   "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:DRILL_LAST_INTERVAL: 12.0\n:DRILL_TOTAL_REPEATS: 4\n:DRILL_REPEATS_SINCE_FAIL: 3\n:DRILL_FAILURE_COUNT: 1\n:DRILL_AVERAGE_QUALITY: 3.5\n:DRILL_EASE: 2.5\n:END:\n"
   "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:END:\n"
   (clrhash org-drill-dest-id-table)
   (org-drill--build-dest-id-table dest-buf)
   (should (gethash "shared" org-drill-dest-id-table))
   (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
     (org-drill--migrate-from-source src-buf dest-buf nil))
   ;; ID was consumed off the table.
   (should-not (gethash "shared" org-drill-dest-id-table))
   ;; DEST got the scheduling data.
   (with-current-buffer dest-buf
     (goto-char (point-min))
     (should (equal "12.0" (org-entry-get (point) "DRILL_LAST_INTERVAL")))
     (should (equal "4" (org-entry-get (point) "DRILL_TOTAL_REPEATS"))))))

(ert-deftest test-migrate-no-id-on-src-is-skipped ()
  "SRC entries without an ID don't touch DEST and don't drain the table."
  (with-two-org-tempfiles
   "* Card :drill:\n:PROPERTIES:\n:DRILL_LAST_INTERVAL: 5.0\n:END:\n"
   "* Card :drill:\n:PROPERTIES:\n:ID: untouched\n:END:\n"
   (clrhash org-drill-dest-id-table)
   (org-drill--build-dest-id-table dest-buf)
   (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
     (org-drill--migrate-from-source src-buf dest-buf nil))
   (should (gethash "untouched" org-drill-dest-id-table))))

(ert-deftest test-migrate-non-drill-entry-skipped ()
  "Non-drill entries in SRC are skipped even when they have an ID."
  (with-two-org-tempfiles
   "* Note (no drill tag)\n:PROPERTIES:\n:ID: shared\n:END:\n"
   "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:END:\n"
   (clrhash org-drill-dest-id-table)
   (org-drill--build-dest-id-table dest-buf)
   (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
     (org-drill--migrate-from-source src-buf dest-buf nil))
   ;; Non-drill SRC didn't match, so the ID stays in the table.
   (should (gethash "shared" org-drill-dest-id-table))))

(ert-deftest test-migrate-ignore-new-items-skips-unmatched-src ()
  "When ignore-new-items-p is t, unmatched SRC entries don't get copied."
  (with-two-org-tempfiles
   "* New card :drill:\n:PROPERTIES:\n:ID: src-only\n:END:\n"
   "* Existing :drill:\n:PROPERTIES:\n:ID: dest-only\n:END:\n"
   (clrhash org-drill-dest-id-table)
   (org-drill--build-dest-id-table dest-buf)
   (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
             ((symbol-function 'org-drill-copy-entry-to-other-buffer)
              (lambda (&rest _) (error "should not be called"))))
     (org-drill--migrate-from-source src-buf dest-buf t))
   ;; dest-only is still in the table because nothing migrated to consume it.
   (should (gethash "dest-only" org-drill-dest-id-table))))

(provide 'test-org-drill-migrate-from-source)
;;; test-org-drill-migrate-from-source.el ends here
