;;; test-org-drill-leitner-start-box.el --- Tests for leitner-start-box  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-leitner-start-box', which moves N entries from the
;; unboxed queue into box 1 by setting their DRILL_LEITNER_BOX property.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-leitner-unboxed-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT.
Initializes leitner queues empty, mocks `sit-for' and `message' to ignore."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-start-box-" nil ".org"))
         (org-drill-leitner-boxed-entries nil)
         (org-drill-leitner-unboxed-entries nil))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             (cl-letf (((symbol-function 'sit-for) #'ignore)
                       ((symbol-function 'message) #'ignore))
               ,@body)))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-leitner-start-box-sets-box-property-on-each-entry ()
  "Each unboxed entry processed gets DRILL_LEITNER_BOX = \"1\"."
  (with-leitner-unboxed-tempfile
      "* A :leitner:\nbody\n* B :leitner:\nbody\n"
    (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
      (let ((org-drill-question-tag org-drill-leitner-tag))
        (org-drill-all-leitner-capture 'file))
      (should (= 2 (length org-drill-leitner-unboxed-entries)))
      (org-drill-leitner-start-box 2)
      (let ((boxed-count 0))
        (org-map-entries
         (lambda ()
           (when (equal "1" (org-entry-get (point) "DRILL_LEITNER_BOX"))
             (cl-incf boxed-count))))
        (should (= 2 boxed-count))))))

(ert-deftest test-leitner-start-box-respects-number-arg ()
  "Asking for fewer entries than available only boxes that many."
  (with-leitner-unboxed-tempfile
      "* A :leitner:\nbody\n* B :leitner:\nbody\n* C :leitner:\nbody\n"
    (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
      (let ((org-drill-question-tag org-drill-leitner-tag))
        (org-drill-all-leitner-capture 'file))
      (org-drill-leitner-start-box 1)
      (let ((boxed-count 0))
        (org-map-entries
         (lambda ()
           (when (equal "1" (org-entry-get (point) "DRILL_LEITNER_BOX"))
             (cl-incf boxed-count))))
        (should (= 1 boxed-count))))))

(ert-deftest test-leitner-start-box-zero-is-noop ()
  "Asking for zero entries doesn't touch any."
  (with-leitner-unboxed-tempfile
      "* A :leitner:\nbody\n"
    (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
      (let ((org-drill-question-tag org-drill-leitner-tag))
        (org-drill-all-leitner-capture 'file))
      (org-drill-leitner-start-box 0)
      (let ((boxed-count 0))
        (org-map-entries
         (lambda ()
           (when (equal "1" (org-entry-get (point) "DRILL_LEITNER_BOX"))
             (cl-incf boxed-count))))
        (should (= 0 boxed-count))))))

(provide 'test-org-drill-leitner-start-box)
;;; test-org-drill-leitner-start-box.el ends here
