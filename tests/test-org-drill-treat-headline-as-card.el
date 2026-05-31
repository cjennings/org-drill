;;; test-org-drill-treat-headline-as-card.el --- Tests for headline-as-card  -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-drill-treat-headline-as-card-p' controls whether a drill entry with an
;; empty body is skipped (the default) or presented as a card with the heading
;; itself as the question (upstream issues #30 and #41).
;;
;; The gate lives in `org-drill--entry-empty-and-not-empty-friendly-p', whose
;; non-nil result makes `org-drill--classify-status' return nil (the entry is
;; not a drillable card).  These tests pin both the predicate and the
;; classification outcome, on and off, and confirm the per-card-type
;; DRILL-EMPTY-P mechanism still works independently of the new global switch.

;;; Code:

(require 'ert)
(require 'org-drill)

;;;; Defcustom default

(ert-deftest test-org-drill-treat-headline-as-card-defaults-off ()
  "The defcustom ships nil so existing decks keep skipping empty entries."
  (should (eq nil (default-value 'org-drill-treat-headline-as-card-p))))

;;;; Predicate — org-drill--entry-empty-and-not-empty-friendly-p

(ert-deftest test-org-drill-empty-entry-skipped-when-headline-card-off ()
  "With the switch off, an empty-bodied drill entry is treated as
empty-and-skippable (predicate returns non-nil)."
  (let ((org-drill-treat-headline-as-card-p nil))
    (with-temp-buffer
      (insert "* A headline-only card :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (should (org-drill--entry-empty-and-not-empty-friendly-p)))))

(ert-deftest test-org-drill-empty-entry-not-skipped-when-headline-card-on ()
  "With the switch on, an empty-bodied drill entry is NOT skippable
\(predicate returns nil) — the heading becomes the card."
  (let ((org-drill-treat-headline-as-card-p t))
    (with-temp-buffer
      (insert "* A headline-only card :drill:\n")
      (org-mode)
      (goto-char (point-min))
      (should-not (org-drill--entry-empty-and-not-empty-friendly-p)))))

(ert-deftest test-org-drill-non-empty-entry-not-skipped-regardless ()
  "An entry with a body is never empty-and-skippable, switch on or off."
  (dolist (flag '(nil t))
    (let ((org-drill-treat-headline-as-card-p flag))
      (with-temp-buffer
        (insert "* A normal card :drill:\nThe answer body.\n")
        (org-mode)
        (goto-char (point-min))
        (should-not (org-drill--entry-empty-and-not-empty-friendly-p))))))

(ert-deftest test-org-drill-empty-friendly-card-type-unaffected-by-switch ()
  "A card type that opts into empty bodies via DRILL-EMPTY-P (e.g. twosided)
is never skippable, independent of the global switch."
  (dolist (flag '(nil t))
    (let ((org-drill-treat-headline-as-card-p flag))
      (with-temp-buffer
        (insert "* A two-sided card :drill:\n"
                ":PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n")
        (org-mode)
        (goto-char (point-min))
        (should-not (org-drill--entry-empty-and-not-empty-friendly-p))))))

;;;; Classification outcome — org-drill--classify-status

(ert-deftest test-org-drill-classify-empty-entry-flips-with-switch ()
  "An empty drill entry classifies as nil (skipped) with the switch off, and
as a real status with the switch on — the user-facing effect of the feature."
  (with-temp-buffer
    (insert "* A headline-only card :drill:\n")
    (org-mode)
    (goto-char (point-min))
    ;; due=nil reaches :unscheduled only once the empty gate is passed; session
    ;; is unused on that path, so nil is safe here.
    (let ((org-drill-treat-headline-as-card-p nil))
      (should (null (org-drill--classify-status nil nil 1))))
    (let ((org-drill-treat-headline-as-card-p t))
      (should (org-drill--classify-status nil nil 1)))))

(provide 'test-org-drill-treat-headline-as-card)

;;; test-org-drill-treat-headline-as-card.el ends here
