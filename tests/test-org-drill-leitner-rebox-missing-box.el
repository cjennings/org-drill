;;; test-org-drill-leitner-rebox-missing-box.el --- Regression for missing DRILL_LEITNER_BOX  -*- lexical-binding: t; -*-

;;; Commentary:
;; `org-drill-leitner-rebox' read DRILL_LEITNER_BOX via `org-entry-get'
;; and passed the result straight into `string-to-number'.  When the
;; property is absent, org-entry-get returns nil and string-to-number
;; errors with "Wrong type argument: char-or-string-p, nil".  Reachable
;; if a user removes the property mid-session, or if a Leitner-tagged
;; entry is rebox'd before its DRILL_LEITNER_BOX is set.
;;
;; Fix wraps the value with `(or ... "0")' so a missing property is
;; treated as box 0 (which makes the rating semantics still sensible —
;; a "downgrade by one" stays at 0, and a promotion goes to 1).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-drill)

;;;; Regression

(ert-deftest test-leitner-rebox-survives-missing-leitner-box-property ()
  "On a leitner entry without DRILL_LEITNER_BOX, rebox should not crash."
  (with-temp-buffer
    (let ((org-startup-folded nil))
      (insert "* Question :leitner:\nbody\n")    ; no DRILL_LEITNER_BOX
      (org-mode)
      (goto-char (point-min))
      (let ((session (org-drill-session)))
        (cl-letf (((symbol-function 'read-key-sequence)
                   (lambda (_prompt) "5"))
                  ((symbol-function 'sit-for) #'ignore))
          ;; Should not error.
          (org-drill-leitner-rebox session)
          ;; After a quality-5 promotion from missing (treated as box 0):
          ;; new box = 0 + 1 = 1.  Either way, the property should now exist.
          (should (org-entry-get (point) "DRILL_LEITNER_BOX")))))))

(provide 'test-org-drill-leitner-rebox-missing-box)

;;; test-org-drill-leitner-rebox-missing-box.el ends here
