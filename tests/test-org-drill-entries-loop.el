;;; test-org-drill-entries-loop.el --- Tests for org-drill-entries main loop  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill-entries' — the main session loop that walks
;; through pending cards and routes the user's rating into the right
;; queue (done / again).
;;
;; The loop calls `org-drill-entry' on each card; we mock it to return
;; controlled values (nil = quit, 'edit, 'skip, 0..5 = rating) so the
;; test can verify the queue-routing logic without driving a real
;; interactive prompt.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-org-drill-tempfile (content &rest body)
  "Run BODY in a tempfile-backed org buffer with CONTENT.
Markers used for queue testing must point to real headings in a real
file because pop-next-pending-entry calls org-drill-entry-p on each."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "org-drill-test-" nil ".org")))
     (unwind-protect
         (with-current-buffer (find-file-noselect tmpfile)
           (let ((org-startup-folded nil))
             (insert ,content)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (when (file-exists-p tmpfile) (delete-file tmpfile)))))

(defun heading-marker (regex)
  "Make a marker at the start of the first heading matching REGEX."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regex nil t)
    (line-beginning-position)
    (let ((m (make-marker))) (set-marker m (line-beginning-position)) m)))

;;;; Quit return → end-pos := :quit and loop exits

(ert-deftest test-org-drill-entries-quit-stops-loop ()
  "When org-drill-entry returns nil (quit), the loop exits and end-pos becomes :quit."
  (with-org-drill-tempfile "* First :drill:\nbody one\n* Second :drill:\nbody two\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (oset session new-entries
            (list (heading-marker "^\\* First")
                  (heading-marker "^\\* Second")))
      (cl-letf (((symbol-function 'org-drill-entry) (lambda (_) nil))
                ((symbol-function 'sit-for) #'ignore))
        (let ((result (org-drill-entries session)))
          (should (null result))
          (should (eq :quit (oref session end-pos))))))))

;;;; Edit return → end-pos := point-marker, loop exits

(ert-deftest test-org-drill-entries-edit-stops-loop-records-position ()
  "When org-drill-entry returns 'edit, the loop exits and end-pos is a marker."
  (with-org-drill-tempfile "* First :drill:\nbody one\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (oset session new-entries (list (heading-marker "^\\* First")))
      (cl-letf (((symbol-function 'org-drill-entry) (lambda (_) 'edit))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-entries session)
        (should (markerp (oref session end-pos)))))))

;;;; Successful rating → entry goes to done-entries, current-item cleared

(ert-deftest test-org-drill-entries-passing-rating-pushes-to-done ()
  "A passing rating (>failure-quality) routes the marker into done-entries."
  (with-org-drill-tempfile "* First :drill:\nbody\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (oset session new-entries (list (heading-marker "^\\* First")))
      (cl-letf (((symbol-function 'org-drill-entry) (lambda (_) 5))
                ((symbol-function 'sit-for) #'ignore))
        (org-drill-entries session)
        (should (= 1 (length (oref session done-entries))))
        (should (null (oref session current-item)))))))

;;;; Failure rating → entry goes to again-entries, not done

(ert-deftest test-org-drill-entries-failing-rating-pushes-to-again ()
  "A failing rating (<= failure-quality) routes the marker into again-entries."
  (with-org-drill-tempfile "* First :drill:\nbody\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (oset session new-entries (list (heading-marker "^\\* First")))
      ;; First call returns 0 (fail), second returns nil to terminate
      ;; the loop after the again-entry repeats.
      (let ((calls 0))
        (cl-letf (((symbol-function 'org-drill-entry)
                   (lambda (_)
                     (cl-incf calls)
                     (if (= calls 1) 0 nil)))
                  ((symbol-function 'sit-for) #'ignore))
          (org-drill-entries session)
          ;; The entry that was failed lives in again-entries (or got
          ;; re-presented and then quit).  Either way, done-entries is
          ;; empty.
          (should (null (oref session done-entries))))))))

;;;; skip return → current-item nil-ed, marker not added to any queue

(ert-deftest test-org-drill-entries-skip-clears-current-without-queueing ()
  "Skip returns nil current-item, doesn't add to done or again."
  (with-org-drill-tempfile "* First :drill:\nbody\n"
    (let ((session (org-drill-session)))
      (oset session start-time (float-time (current-time)))
      (oset session new-entries (list (heading-marker "^\\* First")))
      (let ((calls 0))
        (cl-letf (((symbol-function 'org-drill-entry)
                   (lambda (_)
                     (cl-incf calls)
                     (if (= calls 1) 'skip nil)))
                  ((symbol-function 'sit-for) #'ignore))
          (org-drill-entries session)
          (should (null (oref session done-entries)))
          (should (null (oref session again-entries))))))))

(provide 'test-org-drill-entries-loop)

;;; test-org-drill-entries-loop.el ends here
