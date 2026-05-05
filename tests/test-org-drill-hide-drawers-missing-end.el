;;; test-org-drill-hide-drawers-missing-end.el --- Regression for missing :END:  -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-drill-hide-drawers' captured drawer-end as
;;   (save-excursion (re-search-forward ":END:" end t) (point))
;; which always returns a number — `(point)' is always defined.  The
;; subsequent `(when drawer-end ...)' guard was dead code, so a
;; malformed drawer without `:END:' would either no-op silently or
;; create a zero/negative-width overlay (depending on point state).
;;
;; The user-visible failure mode: a drawer with a typo or
;; mid-edit truncation gets the body kept visible during the drill
;; (which is intended), but the function does junk overlay work to
;; get there.  Captured the search result and gate on that.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression

(ert-deftest test-hide-drawers-malformed-drawer-without-end-creates-no-overlay ()
  "A drawer that opens but never closes should not produce a hidden-text
overlay (and should not error)."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      ;; Note: no :END:.  Just :NOTANEND: as a near-miss.
      (insert "* Question :drill:\n:PROPERTIES:\n:ID: x\n:NOTANEND:\nbody\n")
      (org-mode)
      (goto-char (point-min))
      (org-drill-hide-drawers)
      ;; Count overlays — there should be none from the malformed drawer.
      (let ((n 0))
        (dolist (ovl (overlays-in (point-min) (point-max)))
          (when (eql 'org-drill-hidden-text-overlay (overlay-get ovl 'category))
            (cl-incf n)))
        ;; Either no overlays at all (drawer skipped entirely), or only
        ;; well-formed drawers were hidden.  No malformed-drawer overlay.
        (should (= 0 n))))))

(ert-deftest test-hide-drawers-malformed-followed-by-wellformed-still-hides-good ()
  "Even after encountering a malformed drawer, well-formed drawers still hide."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :drill:
:LOGBOOK:
note one
:END:
body line
")
      (org-mode)
      (goto-char (point-min))
      (org-drill-hide-drawers)
      ;; The LOGBOOK drawer is well-formed → 1 overlay expected.
      (let ((n 0))
        (dolist (ovl (overlays-in (point-min) (point-max)))
          (when (eql 'org-drill-hidden-text-overlay (overlay-get ovl 'category))
            (cl-incf n)))
        (should (>= n 1))))))

(provide 'test-org-drill-hide-drawers-missing-end)

;;; test-org-drill-hide-drawers-missing-end.el ends here
