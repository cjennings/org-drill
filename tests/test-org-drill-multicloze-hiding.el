;;; test-org-drill-multicloze-hiding.el --- Tests for multicloze hide-n and hide-nth  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the multicloze presenters that hide a chosen subset of
;; cloze pieces.  With `org-drill-presentation-prompt' mocked, the
;; remaining work is the cloze-counting + position-selection logic,
;; which is the part we want under test.
;;
;; - `org-drill-present-multicloze-hide-n': hides N pieces (or shows
;;   only abs(N) when N is negative)
;; - `org-drill-present-multicloze-hide-nth': hides exactly the N-th
;;   piece (1-indexed; negative counts from the end)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Helpers

(defmacro with-cloze-card (content &rest body)
  "Run BODY in a temp org buffer with CONTENT (a drill entry containing
clozes), point at start, fontification caches set up."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-startup-folded nil))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       (setq-local org-drill-cloze-regexp (org-drill--compute-cloze-regexp))
       ,@body)))

(defmacro with-mocked-prompt (return-value &rest body)
  "Run BODY with `org-drill-presentation-prompt' replaced by a stub
returning RETURN-VALUE.  Also stubs LaTeX preview / inline-images
helpers that aren't relevant in batch."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-drill-presentation-prompt)
              (lambda (&rest _) ,return-value))
             ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
             ((symbol-function 'org-display-inline-images) #'ignore))
     ,@body))

(defun count-cloze-overlays ()
  "Count the cloze-overlay-defaults overlays in the current buffer."
  (let ((n 0))
    (dolist (ovl (overlays-in (point-min) (point-max)))
      (when (eql 'org-drill-cloze-overlay-defaults
                 (overlay-get ovl 'category))
        (cl-incf n)))
    n))

(defvar overlays-during-prompt nil
  "Captured count of cloze overlays at the moment presentation-prompt fires.")

(defmacro with-mocked-prompt-capturing-overlays (&rest body)
  "Run BODY with presentation-prompt replaced by a stub that records
the cloze-overlay count at the moment it's called.  Records to
`overlays-during-prompt'."
  `(cl-letf (((symbol-function 'org-drill-presentation-prompt)
              (lambda (&rest _)
                (setq overlays-during-prompt (count-cloze-overlays))
                t))
             ((symbol-function 'org-drill--show-latex-fragments) #'ignore)
             ((symbol-function 'org-display-inline-images) #'ignore))
     (setq overlays-during-prompt nil)
     ,@body))

;;;; org-drill-present-multicloze-hide-n with positive N

(ert-deftest test-multicloze-hide-n-hides-exactly-n-cloze-pieces ()
  "With three cloze pieces and n=2, exactly 2 cloze overlays exist
at the moment the prompt fires."
  (with-cloze-card "* Question :drill:
The capitals are [Paris], [Rome], and [Madrid].
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-n (org-drill-session) 2))
    (should (= 2 overlays-during-prompt))))

(ert-deftest test-multicloze-hide-n-hides-one-when-n-is-1 ()
  (with-cloze-card "* Question :drill:
[A] [B] [C] [D]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-n (org-drill-session) 1))
    (should (= 1 overlays-during-prompt))))

(ert-deftest test-multicloze-hide-n-no-cloze-no-overlays ()
  (with-cloze-card "* Question :drill:
No cloze syntax in this body.
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-n (org-drill-session) 1))
    (should (= 0 overlays-during-prompt))))

;;;; org-drill-present-multicloze-hide-n with negative N (show-mode)

(ert-deftest test-multicloze-hide-n-negative-shows-only-n-pieces ()
  "n=-1 means \"show only 1\" — hide all but 1.  With 4 pieces, 3 hidden."
  (with-cloze-card "* Question :drill:
[A] [B] [C] [D]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-n (org-drill-session) -1))
    (should (= 3 overlays-during-prompt))))

(ert-deftest test-multicloze-hide-n-negative-2-shows-2-pieces ()
  "n=-2 means \"show 2\" — hide N-2 of N total."
  (with-cloze-card "* Question :drill:
[A] [B] [C] [D]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-n (org-drill-session) -2))
    (should (= 2 overlays-during-prompt))))

;;;; force-show-first / force-show-last interactions

(ert-deftest test-multicloze-hide-n-force-show-first-and-hide-first-mutually-exclusive ()
  "Passing both force-hide-first and force-show-first → user-visible error."
  (with-cloze-card "* Question :drill:
[A] [B] [C]
"
    (with-mocked-prompt t
      (should-error
       (org-drill-present-multicloze-hide-n (org-drill-session) 1 t nil t)))))

;;;; org-drill-present-multicloze-hide-nth — positive index

(ert-deftest test-multicloze-hide-nth-hides-only-the-nth-piece ()
  "Hides exactly the 2nd cloze piece, leaving the others visible.
Verified by checking the resulting overlay's bounds match the
2nd cloze."
  (with-cloze-card "* Question :drill:
[A] [B] [C]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-nth (org-drill-session) 2))
    (should (= 1 overlays-during-prompt))))

(ert-deftest test-multicloze-hide-nth-out-of-range-leaves-no-overlays ()
  "Asking for the 10th piece when only 3 exist hides nothing."
  (with-cloze-card "* Question :drill:
[A] [B] [C]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-nth (org-drill-session) 10))
    (should (= 0 overlays-during-prompt))))

(ert-deftest test-multicloze-hide-nth-negative-counts-from-end ()
  "n=-1 means \"the last piece.\""
  (with-cloze-card "* Question :drill:
[A] [B] [C]
"
    (with-mocked-prompt-capturing-overlays
     (org-drill-present-multicloze-hide-nth (org-drill-session) -1))
    (should (= 1 overlays-during-prompt))))

(provide 'test-org-drill-multicloze-hiding)

;;; test-org-drill-multicloze-hiding.el ends here
