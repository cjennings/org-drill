;;; test-org-drill-merge-and-leitner-orchestration.el --- Tests for orchestrators  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for two short orchestrator entry-points:
;;
;; - `org-drill-merge-buffers' — combines build-dest-id-table, migrate, and
;;   strip-unmatched into a confirm-then-merge user command.
;; - `org-drill-all-leitner-capture' — wraps `map-leitner-capture' over a
;;   scope so the boxed/unboxed queues end up populated and reversed.
;; - `org-drill-leitner-vs-drill-entries' — message helper that totals both
;;   queues plus the drill-entry count.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-leitner-scoped-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT and clean leitner queues."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-leitner-orch-" nil ".org"))
         (org-drill-leitner-boxed-entries nil)
         (org-drill-leitner-unboxed-entries nil))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

;;;; org-drill-all-leitner-capture

(ert-deftest test-all-leitner-capture-fills-and-reverses-queues ()
  "Capture walks the buffer, populating boxed and unboxed queues and reversing them."
  (with-leitner-scoped-tempfile
      "* A :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 1\n:END:\n* B :leitner:\nbody\n"
    (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
      (org-drill-all-leitner-capture 'file))
    (should (= 1 (length org-drill-leitner-boxed-entries)))
    (should (= 1 (length org-drill-leitner-unboxed-entries)))))

(ert-deftest test-all-leitner-capture-empty-buffer-leaves-queues-empty ()
  "No leitner-tagged entries means both queues stay empty."
  (with-leitner-scoped-tempfile "* Plain :drill:\nbody\n"
    (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
      (org-drill-all-leitner-capture 'file))
    (should (null org-drill-leitner-boxed-entries))
    (should (null org-drill-leitner-unboxed-entries))))

;;;; org-drill-leitner-vs-drill-entries

(ert-deftest test-leitner-vs-drill-entries-emits-summary-message ()
  "The helper messages a totals summary including drill and leitner counts."
  (with-leitner-scoped-tempfile
      "* Drill A :drill:\nbody\n* Leitner B :leitner:\nbody\n"
    (let ((messages nil))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (org-drill-leitner-vs-drill-entries))
      (should (cl-some (lambda (m) (string-match-p "drill entries" m)) messages))
      (should (cl-some (lambda (m) (string-match-p "leitner entries" m)) messages)))))

;;;; org-drill-merge-buffers

(ert-deftest test-merge-buffers-aborts-when-user-says-no ()
  "If yes-or-no-p returns nil, no migration runs."
  (let ((src-file (make-temp-file "org-drill-merge-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-merge-dst-" nil ".org")))
    (unwind-protect
        (let (src dst migrated)
          (setq src (find-file-noselect src-file))
          (setq dst (find-file-noselect dst-file))
          (with-current-buffer src
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:DRILL_LAST_INTERVAL: 9.0\n:END:\n")
            (org-mode))
          (with-current-buffer dst
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:END:\n")
            (org-mode))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                    ((symbol-function 'org-drill--migrate-from-source)
                     (lambda (&rest _) (setq migrated t))))
            (org-drill-merge-buffers src dst nil))
          (should-not migrated))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

(ert-deftest test-merge-buffers-with-yes-runs-full-pipeline ()
  "When yes-or-no-p is yes, build-table + migrate + strip all execute."
  (let ((src-file (make-temp-file "org-drill-merge-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-merge-dst-" nil ".org")))
    (unwind-protect
        (let (src dst)
          (setq src (find-file-noselect src-file))
          (setq dst (find-file-noselect dst-file))
          (with-current-buffer src
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:DRILL_LAST_INTERVAL: 9.0\n:DRILL_TOTAL_REPEATS: 3\n:DRILL_REPEATS_SINCE_FAIL: 2\n:DRILL_FAILURE_COUNT: 0\n:DRILL_AVERAGE_QUALITY: 4.0\n:DRILL_EASE: 2.5\n:END:\n")
            (org-mode))
          (with-current-buffer dst
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: shared\n:END:\n")
            (org-mode))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'org-drill-progress-message) #'ignore))
            (org-drill-merge-buffers src dst nil))
          (with-current-buffer dst
            (goto-char (point-min))
            (should (equal "9.0" (org-entry-get (point) "DRILL_LAST_INTERVAL")))))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

(ert-deftest test-merge-buffers-defaults-dest-to-current-buffer ()
  "When dest is omitted, the current buffer is used."
  (let ((src-file (make-temp-file "org-drill-merge-src-" nil ".org"))
        (dst-file (make-temp-file "org-drill-merge-dst-" nil ".org"))
        (chosen-dest nil))
    (unwind-protect
        (let (src dst)
          (setq src (find-file-noselect src-file))
          (setq dst (find-file-noselect dst-file))
          (with-current-buffer src
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: x\n:END:\n")
            (org-mode))
          (with-current-buffer dst
            (insert "* Card :drill:\n:PROPERTIES:\n:ID: x\n:END:\n")
            (org-mode))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'org-drill-progress-message) #'ignore)
                    ((symbol-function 'org-drill--build-dest-id-table)
                     (lambda (d) (setq chosen-dest d))))
            (with-current-buffer dst
              (org-drill-merge-buffers src nil nil)))
          (should (eq chosen-dest dst)))
      (when (file-exists-p src-file) (delete-file src-file))
      (when (file-exists-p dst-file) (delete-file dst-file)))))

(provide 'test-org-drill-merge-and-leitner-orchestration)
;;; test-org-drill-merge-and-leitner-orchestration.el ends here
