;;; test-org-drill-multicloze-dispatch.el --- Tests for multicloze hide-firstmore / show1-lastmore / show1-firstless dispatch  -*- lexical-binding: t; -*-

;;; Commentary:
;; The multicloze "weighted" presenters — hide1-firstmore,
;; show1-lastmore, show1-firstless — wrap a cond that picks between
;; "common" and "uncommon" modes based on `org-drill-cloze-text-weight'
;; and the entry's repeat counter.
;;
;; The presenters they delegate to are interactive (they call
;; `org-drill-presentation-prompt'), but the branch-selection logic
;; above the delegation is a pure cond that's testable in isolation by
;; mocking the underlying presenter functions to record which one was
;; chosen.
;;
;; The user-facing contract: with weight = N, every Nth repetition
;; uses the "uncommon" path, the rest use the "common" path.  When
;; weight is nil, the entire weighting is bypassed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-fresh-drill-entry (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert "* Question :drill:\n")
       (org-mode)
       (goto-char (point-min))
       ,@body)))

(defmacro with-mocked-presenters (&rest body)
  "Replace the multicloze presenters with no-op stubs that record which
function was called by pushing onto `multicloze-calls'."
  `(let ((multicloze-calls nil))
     (cl-letf (((symbol-function 'org-drill-present-multicloze-hide1)
                (lambda (_session) (push 'hide1 multicloze-calls) t))
               ((symbol-function 'org-drill-present-multicloze-hide-first)
                (lambda (_session) (push 'hide-first multicloze-calls) t))
               ((symbol-function 'org-drill-present-multicloze-hide-n)
                (lambda (&rest args) (push (cons 'hide-n args) multicloze-calls) t))
               ((symbol-function 'org-drill-present-multicloze-show1)
                (lambda (_session) (push 'show1 multicloze-calls) t)))
       ,@body)))

;;;; hide1-firstmore

(ert-deftest test-multicloze-hide1-firstmore-nil-weight-falls-back-to-hide1 ()
  "When `org-drill-cloze-text-weight' is nil, behave like plain hide1cloze."
  (with-fresh-drill-entry
    (let ((org-drill-cloze-text-weight nil))
      (with-mocked-presenters
        (org-drill-present-multicloze-hide1-firstmore (org-drill-session))
        (should (memq 'hide1 multicloze-calls))))))

(ert-deftest test-multicloze-hide1-firstmore-invalid-weight-errors ()
  "A non-positive-integer weight is rejected with a user-visible error."
  (with-fresh-drill-entry
    (let ((org-drill-cloze-text-weight -1))
      (should-error (org-drill-present-multicloze-hide1-firstmore (org-drill-session))))
    (let ((org-drill-cloze-text-weight 'not-a-number))
      (should-error (org-drill-present-multicloze-hide1-firstmore (org-drill-session))))))

(ert-deftest test-multicloze-hide1-firstmore-non-trigger-rep-uses-hide-first ()
  "When (1+ total-repeats) is NOT divisible by weight, take the common path
(hide-first).  weight=3, total-repeats=0 → 1 mod 3 = 1 → common."
  (with-fresh-drill-entry
    (org-set-property "DRILL_TOTAL_REPEATS" "0")
    (let ((org-drill-cloze-text-weight 3))
      (with-mocked-presenters
        (org-drill-present-multicloze-hide1-firstmore (org-drill-session))
        (should (memq 'hide-first multicloze-calls))))))

(ert-deftest test-multicloze-hide1-firstmore-trigger-rep-uses-hide-n ()
  "When (1+ total-repeats) IS divisible by weight, take the uncommon
path (hide-n with force-show-first).  weight=3, total-repeats=2 → 3 mod 3 = 0."
  (with-fresh-drill-entry
    (org-set-property "DRILL_TOTAL_REPEATS" "2")
    (let ((org-drill-cloze-text-weight 3))
      (with-mocked-presenters
        (org-drill-present-multicloze-hide1-firstmore (org-drill-session))
        (let ((call (cl-find-if (lambda (c) (and (consp c) (eq 'hide-n (car c))))
                                multicloze-calls)))
          (should call)
          ;; hide-n was called with force-show-first = t (4th arg, after session, n=1).
          (let* ((args (cdr call))
                 ;; args = (session 1 force-show-first)
                 (force-show-first (nth 2 args)))
            (should (eq t force-show-first))))))))

;;;; show1-lastmore

(ert-deftest test-multicloze-show1-lastmore-nil-weight-falls-back-to-show1 ()
  (with-fresh-drill-entry
    (let ((org-drill-cloze-text-weight nil))
      (with-mocked-presenters
        (org-drill-present-multicloze-show1-lastmore (org-drill-session))
        (should (memq 'show1 multicloze-calls))))))

(ert-deftest test-multicloze-show1-lastmore-non-trigger-shows-last ()
  "Common path: hide-n with -1 (show one) and force-show-last = t."
  (with-fresh-drill-entry
    (org-set-property "DRILL_TOTAL_REPEATS" "0")  ; (1+0) mod 3 = 1 → common
    (let ((org-drill-cloze-text-weight 3))
      (with-mocked-presenters
        (org-drill-present-multicloze-show1-lastmore (org-drill-session))
        (let* ((call (cl-find-if (lambda (c) (and (consp c) (eq 'hide-n (car c))))
                                 multicloze-calls))
               ;; args after `hide-n: (session -1 force-show-first force-show-last)
               (args (cdr call)))
          (should (eq -1 (nth 1 args)))
          (should (eq t (nth 3 args))))))))   ; force-show-last

;;;; show1-firstless

(ert-deftest test-multicloze-show1-firstless-nil-weight-falls-back-to-show1 ()
  (with-fresh-drill-entry
    (let ((org-drill-cloze-text-weight nil))
      (with-mocked-presenters
        (org-drill-present-multicloze-show1-firstless (org-drill-session))
        (should (memq 'show1 multicloze-calls))))))

(ert-deftest test-multicloze-show1-firstless-non-trigger-skips-first ()
  "Common path: hide-n with -1 and force-show-first omitted (the show
piece is guaranteed not to be the first)."
  (with-fresh-drill-entry
    (org-set-property "DRILL_TOTAL_REPEATS" "0")
    (let ((org-drill-cloze-text-weight 3))
      (with-mocked-presenters
        (org-drill-present-multicloze-show1-firstless (org-drill-session))
        (let* ((call (cl-find-if (lambda (c) (and (consp c) (eq 'hide-n (car c))))
                                 multicloze-calls))
               (args (cdr call)))
          (should (eq -1 (nth 1 args))))))))

(provide 'test-org-drill-multicloze-dispatch)

;;; test-org-drill-multicloze-dispatch.el ends here
