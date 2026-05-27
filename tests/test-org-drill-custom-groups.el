;;; test-org-drill-custom-groups.el --- Customize sub-group structure  -*- lexical-binding: t; -*-

;;; Commentary:
;; The defcustoms are split across four sub-groups under the top-level
;; org-drill group so M-x customize-group org-drill is navigable instead of
;; dumping 37 options in one flat list.  (There is no leitner sub-group: the
;; Leitner settings are defvars, not defcustoms.)

;;; Code:

(require 'ert)
(require 'org-drill)

(defconst test-org-drill-subgroups
  '(org-drill-display org-drill-algorithm org-drill-session org-drill-leech)
  "The customize sub-groups org-drill defines.")

(ert-deftest test-org-drill-subgroups-exist-and-nest-under-org-drill ()
  "Each sub-group is defined and is a child of the org-drill group."
  (dolist (g test-org-drill-subgroups)
    (should (get g 'group-documentation))
    (should (assq g (get 'org-drill 'custom-group)))))

(ert-deftest test-org-drill-defcustoms-land-in-expected-subgroups ()
  "A representative defcustom from each sub-group is a member of it."
  (should (assq 'org-drill-spaced-repetition-algorithm
                (get 'org-drill-algorithm 'custom-group)))
  (should (assq 'org-drill-leech-method
                (get 'org-drill-leech 'custom-group)))
  (should (assq 'org-drill-scope
                (get 'org-drill-session 'custom-group)))
  (should (assq 'org-drill-use-visible-cloze-face-p
                (get 'org-drill-display 'custom-group))))

(provide 'test-org-drill-custom-groups)

;;; test-org-drill-custom-groups.el ends here
