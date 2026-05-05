;;; test-org-drill-final-helpers.el --- Tests for prompt-for-string, leitner capture, and resume  -*- lexical-binding: t; -*-

;;; Commentary:
;; Final batch of unit tests for:
;;
;; - `org-drill-presentation-prompt-for-string': typed-answer prompt
;;   (uses `read-string').
;; - `org-drill-map-leitner-capture': classifies a leitner-tagged entry
;;   into either boxed or unboxed lists based on DRILL_LEITNER_BOX.
;; - `org-drill-resume': continues a suspended session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-org-buffer (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-fixed-now (&rest body)
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time 0 0 12 5 5 2026))))
     ,@body))

(defun make-marker-at (pos)
  (let ((m (make-marker))) (set-marker m pos) m))

;;;; org-drill-presentation-prompt-for-string

(ert-deftest test-prompt-for-string-stores-typed-answer-in-session ()
  "The string the user typed is captured into session->drill-answer."
  (with-org-buffer "* Question :drill:\nbody\n"
    (let ((session (org-drill-session)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "user-typed-this")))
        (org-drill-presentation-prompt-for-string session "Type:")
        (should (equal "user-typed-this" (oref session drill-answer)))))))

(ert-deftest test-prompt-for-string-uses-default-prompt-when-nil ()
  (with-org-buffer "* Question :drill:\nbody\n"
    (let ((session (org-drill-session))
          (passed-prompt nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (setq passed-prompt prompt) "x")))
        (org-drill-presentation-prompt-for-string session nil)
        (should (string-match-p "Type your answer" passed-prompt))))))

;;;; org-drill-map-leitner-capture

(defmacro with-leitner-tempfile-buffer (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT and the
leitner tag bound as the active question tag (so org-drill-entry-p
matches leitner-tagged entries)."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil)
                 (org-drill-question-tag org-drill-leitner-tag))
             (insert ,content)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(ert-deftest test-map-leitner-capture-unboxed-entry-pushed-to-unboxed-list ()
  "An entry without DRILL_LEITNER_BOX goes into org-drill-leitner-unboxed-entries."
  (with-leitner-tempfile-buffer "* Question :leitner:\n"
    (let ((session (org-drill-session))
          (org-drill-leitner-boxed-entries nil)
          (org-drill-leitner-unboxed-entries nil))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-map-leitner-capture session)
        (should (= 1 (length org-drill-leitner-unboxed-entries)))
        (should (null org-drill-leitner-boxed-entries))))))

(ert-deftest test-map-leitner-capture-box-3-entry-pushed-to-boxed-list ()
  "An entry with DRILL_LEITNER_BOX = 3 goes into the boxed list."
  (with-leitner-tempfile-buffer
      "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 3\n:END:\n"
    (let ((session (org-drill-session))
          (org-drill-leitner-boxed-entries nil)
          (org-drill-leitner-unboxed-entries nil))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-map-leitner-capture session)
        (should (= 1 (length org-drill-leitner-boxed-entries)))
        (should (null org-drill-leitner-unboxed-entries))))))

(ert-deftest test-map-leitner-capture-box-greater-than-5-skipped ()
  "An entry with DRILL_LEITNER_BOX > 5 (already graduated) is captured into neither list."
  (with-leitner-tempfile-buffer
      "* Question :leitner:\n:PROPERTIES:\n:DRILL_LEITNER_BOX: 6\n:END:\n"
    (let ((session (org-drill-session))
          (org-drill-leitner-boxed-entries nil)
          (org-drill-leitner-unboxed-entries nil))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-map-leitner-capture session)
        (should (null org-drill-leitner-boxed-entries))
        (should (null org-drill-leitner-unboxed-entries))))))

(ert-deftest test-map-leitner-capture-non-drill-entry-skipped ()
  "Non-drill entries (no :drill: tag inheritance) are skipped silently."
  (with-org-buffer "* Plain heading\n"
    (let ((session (org-drill-session))
          (org-drill-leitner-boxed-entries nil)
          (org-drill-leitner-unboxed-entries nil))
      (cl-letf (((symbol-function 'org-drill-progress-message) #'ignore)
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-map-leitner-capture session)
        (should (null org-drill-leitner-unboxed-entries))
        (should (null org-drill-leitner-boxed-entries))))))

;;;; org-drill-resume

(ert-deftest test-org-drill-resume-pending-entries-resumes ()
  "When the prior session has pending entries, resume calls org-drill
with resume-p=t."
  (let ((session (org-drill-session))
        (org-drill-args nil))
    (oset session new-entries (list (make-marker-at 1)))
    (oset session start-time (float-time (current-time)))
    (let ((org-drill-last-session session))
      (cl-letf (((symbol-function 'org-drill)
                 (lambda (&rest args) (setq org-drill-args args))))
        (org-drill-resume)
        (should (eq t (nth 2 org-drill-args)))))))

(provide 'test-org-drill-final-helpers)

;;; test-org-drill-final-helpers.el ends here
