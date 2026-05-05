;;; test-org-drill-map-leitner-capture.el --- Tests for map-leitner-capture  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-map-leitner-capture', the per-entry callback that
;; routes a leitner-tagged heading into either `org-drill-leitner-boxed-entries'
;; (when DRILL_LEITNER_BOX is set 0-5) or `org-drill-leitner-unboxed-entries'
;; (when the property is missing).  Box values >5 are graduates and are
;; ignored.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defmacro with-leitner-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT, with empty leitner queues."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-leitner-test-" nil ".org"))
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

(ert-deftest test-map-leitner-capture-no-box-property-pushes-unboxed ()
  "Entry with no DRILL_LEITNER_BOX lands in the unboxed queue."
  (with-leitner-tempfile "* Card :leitner:\nbody\n"
    (let ((session (org-drill-session))
          (org-drill-question-tag org-drill-leitner-tag))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
        (org-drill-map-leitner-capture session))
      (should (= 1 (length org-drill-leitner-unboxed-entries)))
      (should (= 0 (length org-drill-leitner-boxed-entries))))))

(ert-deftest test-map-leitner-capture-box-in-range-pushes-boxed ()
  "Entry with DRILL_LEITNER_BOX 0-5 lands in the boxed queue."
  (dolist (box '("0" "1" "2" "3" "4" "5"))
    (with-leitner-tempfile (format "* Card :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: %s\n:END:\nbody\n" box)
      (let ((session (org-drill-session))
            (org-drill-question-tag org-drill-leitner-tag))
        (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
          (org-drill-map-leitner-capture session))
        (should (= 1 (length org-drill-leitner-boxed-entries)))
        (should (= 0 (length org-drill-leitner-unboxed-entries)))))))

(ert-deftest test-map-leitner-capture-graduate-box-skipped ()
  "Entry with DRILL_LEITNER_BOX > 5 is ignored — already graduated."
  (with-leitner-tempfile "* Card :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 6\n:END:\nbody\n"
    (let ((session (org-drill-session))
          (org-drill-question-tag org-drill-leitner-tag))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
        (org-drill-map-leitner-capture session))
      (should (= 0 (length org-drill-leitner-boxed-entries)))
      (should (= 0 (length org-drill-leitner-unboxed-entries))))))

(ert-deftest test-map-leitner-capture-non-drill-entry-skipped ()
  "Non-drill heading is skipped entirely."
  (with-leitner-tempfile "Just text, no headlines\n"
    (let ((session (org-drill-session))
          (org-drill-question-tag org-drill-leitner-tag))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
        (org-drill-map-leitner-capture session))
      (should (= 0 (length org-drill-leitner-boxed-entries)))
      (should (= 0 (length org-drill-leitner-unboxed-entries))))))

(ert-deftest test-map-leitner-capture-increments-session-counter ()
  "Each call bumps the session's `cnt' slot."
  (with-leitner-tempfile "* Card :leitner:\nbody\n"
    (let ((session (org-drill-session)))
      (oset session cnt 0)
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore))
        (org-drill-map-leitner-capture session)
        (org-drill-map-leitner-capture session))
      (should (= 2 (oref session cnt))))))

(provide 'test-org-drill-map-leitner-capture)
;;; test-org-drill-map-leitner-capture.el ends here
