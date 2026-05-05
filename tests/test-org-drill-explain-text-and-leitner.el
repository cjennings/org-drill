;;; test-org-drill-explain-text-and-leitner.el --- Tests for explain helpers and leitner dispatch  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for:
;;
;; - `org-drill-get-explain-text': walks up the outline collecting body
;;   text from any parent heading tagged `:explain:'.
;; - `org-drill-explain-answer-presenter': appends an explanation
;;   overlay below the entry.
;; - `org-drill-explain-cleaner': removes the explanation overlay.
;; - `org-drill-sm-or-leitner': chooses between SM and Leitner
;;   learning based on the pending entry count.

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

;;;; org-drill-get-explain-text

(ert-deftest test-get-explain-text-no-explain-parent-returns-existing-text ()
  "When no parent has :explain:, returns the existing-text arg unchanged."
  (with-org-buffer "* Parent\n** Child :drill:\nbody\n"
    (re-search-forward "^\\*\\* Child")
    (org-back-to-heading t)
    (should (equal '("preserved")
                   (org-drill-get-explain-text '("preserved"))))))

(ert-deftest test-get-explain-text-with-explain-parent-collects-body ()
  "Parent tagged :explain: contributes its body text to the result."
  (with-org-buffer "* Parent :explain:
explain-body
** Child :drill:
drill-body
"
    (re-search-forward "^\\*\\* Child")
    (org-back-to-heading t)
    (let ((result (org-drill-get-explain-text)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (string-match-p "explain-body" (car result))))))

(ert-deftest test-get-explain-text-stops-at-top-level ()
  "Recursion stops when outline-level reaches 1 (no further parents)."
  (with-org-buffer "* Top\nbody\n"
    (org-back-to-heading t)
    ;; outline-level is 1 → returns existing-text immediately.
    (should (null (org-drill-get-explain-text)))))

;;;; org-drill-explain-answer-presenter

(ert-deftest test-explain-answer-presenter-creates-overlay-with-explanation ()
  "Creates an overlay below the entry whose after-string contains the
collected explanation text."
  (with-org-buffer "* Parent :explain:
the-explanation
** Child :drill:
the-question
"
    (re-search-forward "^\\*\\* Child")
    (org-back-to-heading t)
    (setq org-drill-explain-overlay nil)
    (org-drill-explain-answer-presenter)
    (should org-drill-explain-overlay)
    (let ((after-text (overlay-get org-drill-explain-overlay 'after-string)))
      (should (string-match-p "Explanation:" after-text))
      (should (string-match-p "the-explanation" after-text)))
    (delete-overlay org-drill-explain-overlay)
    (setq org-drill-explain-overlay nil)))

(ert-deftest test-explain-answer-presenter-replaces-existing-overlay ()
  "Calling twice doesn't accumulate overlays — the prior one is cleared."
  (with-org-buffer "* Parent :explain:\nA\n** Child :drill:\nQ\n"
    (re-search-forward "^\\*\\* Child")
    (org-back-to-heading t)
    (setq org-drill-explain-overlay nil)
    (org-drill-explain-answer-presenter)
    (let ((first-overlay org-drill-explain-overlay))
      (org-drill-explain-answer-presenter)
      ;; First overlay is no longer live in the buffer.
      (should-not (memq first-overlay (overlays-in (point-min) (point-max)))))
    (delete-overlay org-drill-explain-overlay)
    (setq org-drill-explain-overlay nil)))

;;;; org-drill-explain-cleaner

(ert-deftest test-explain-cleaner-removes-the-overlay ()
  (with-org-buffer "* Parent :explain:\nA\n** Child :drill:\nQ\n"
    (re-search-forward "^\\*\\* Child")
    (org-back-to-heading t)
    (setq org-drill-explain-overlay nil)
    (org-drill-explain-answer-presenter)
    (should org-drill-explain-overlay)
    (org-drill-explain-cleaner)
    (should-not (memq org-drill-explain-overlay
                      (overlays-in (point-min) (point-max))))
    (setq org-drill-explain-overlay nil)))

(ert-deftest test-explain-cleaner-no-overlay-no-error ()
  "Cleaner is safe to call when no overlay exists."
  (setq org-drill-explain-overlay nil)
  ;; Should not error.
  (org-drill-explain-cleaner))

;;;; org-drill-sm-or-leitner

(ert-deftest test-sm-or-leitner-with-pending-entries-runs-again ()
  "When the prior session has pending entries beyond leitner-completed,
sm-or-leitner runs the SM (org-drill-again) flow."
  (let ((again-called nil)
        (leitner-called nil)
        (session (org-drill-session)))
    (oset session new-entries
          (list (let ((m (make-marker))) (set-marker m 1) m)
                (let ((m (make-marker))) (set-marker m 1) m)))
    (oset session start-time (float-time (current-time)))
    (let ((org-drill-last-session session)
          (org-drill-leitner-completed 0))
      (cl-letf (((symbol-function 'org-drill-again)
                 (lambda () (setq again-called t)))
                ((symbol-function 'org-drill-leitner)
                 (lambda (&rest _) (setq leitner-called t)))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-map-entries) #'ignore))
        (org-drill-sm-or-leitner)
        (should again-called)
        (should-not leitner-called)))))

(ert-deftest test-sm-or-leitner-no-pending-entries-runs-leitner ()
  "When there's nothing pending in SM-style, fall through to Leitner."
  (let ((again-called nil)
        (leitner-called nil)
        (session (org-drill-session)))
    (let ((org-drill-last-session session)
          (org-drill-leitner-completed 0))
      (cl-letf (((symbol-function 'org-drill-again)
                 (lambda () (setq again-called t)))
                ((symbol-function 'org-drill-leitner)
                 (lambda (&rest _) (setq leitner-called t)))
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'org-drill-map-entries) #'ignore))
        (org-drill-sm-or-leitner)
        (should-not again-called)
        (should leitner-called)))))

(provide 'test-org-drill-explain-text-and-leitner)

;;; test-org-drill-explain-text-and-leitner.el ends here
