;;; test-org-drill-route-rating-result.el --- Tests for --route-rating-result  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `org-drill--route-rating-result', which translates the return
;; value of `org-drill-entry' into one of `quit', `edit', `skip', `next'
;; while updating the session queues.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

(defun make-marker-for-test ()
  (let ((m (make-marker))) (set-marker m 1) m))

(ert-deftest test-route-rating-result-nil-returns-quit-and-stashes-end-pos ()
  "When the entry returned nil (user quit), result is `quit' and end-pos is :quit."
  (let ((session (org-drill-session))
        (m (make-marker-for-test)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (should (eq 'quit (org-drill--route-rating-result session m nil))))
    (should (eq :quit (oref session end-pos)))))

(ert-deftest test-route-rating-result-edit-stashes-marker-end-pos ()
  "When the entry returned `edit', end-pos becomes a marker and result is `edit'."
  (let ((session (org-drill-session))
        (m (make-marker-for-test)))
    (with-temp-buffer
      (insert "x")
      (goto-char (point-min))
      (should (eq 'edit (org-drill--route-rating-result session m 'edit))))
    (should (markerp (oref session end-pos)))))

(ert-deftest test-route-rating-result-skip-clears-current-item ()
  "Skip result drops the current-item slot to nil and returns `skip'."
  (let ((session (org-drill-session))
        (m (make-marker-for-test)))
    (oset session current-item m)
    (should (eq 'skip (org-drill--route-rating-result session m 'skip)))
    (should (null (oref session current-item)))))

(ert-deftest test-route-rating-result-failure-pushes-onto-again-entries ()
  "A failed-quality result puts the marker on again-entries and returns `next'."
  (let ((session (org-drill-session))
        (m (make-marker-for-test)))
    (should (eq 'next (org-drill--route-rating-result session m 0)))
    (should (member m (oref session again-entries)))))

(ert-deftest test-route-rating-result-success-pushes-onto-done-entries ()
  "A passing-quality result puts the marker on done-entries and returns `next'."
  (let ((session (org-drill-session))
        (m (make-marker-for-test)))
    (should (eq 'next (org-drill--route-rating-result session m 5)))
    (should (member m (oref session done-entries)))))

(ert-deftest test-route-rating-result-failure-with-existing-again-shuffles-it ()
  "When again-entries already has items, the failure path re-shuffles the list."
  (let* ((session (org-drill-session))
         (m1 (let ((mk (make-marker))) (set-marker mk 1) mk))
         (m2 (let ((mk (make-marker))) (set-marker mk 2) mk))
         (shuffle-called nil))
    (oset session again-entries (list m1 m2))
    (cl-letf (((symbol-function 'org-drill-shuffle)
               (lambda (lst) (setq shuffle-called t) lst)))
      (org-drill--route-rating-result session m1 0))
    (should shuffle-called)))

(provide 'test-org-drill-route-rating-result)
;;; test-org-drill-route-rating-result.el ends here
