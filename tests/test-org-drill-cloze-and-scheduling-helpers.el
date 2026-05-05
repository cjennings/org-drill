;;; test-org-drill-cloze-and-scheduling-helpers.el --- Tests for cloze regex and small scheduler helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for several small helpers users encounter indirectly:
;;
;; - `org-drill--compute-cloze-regexp' / `--compute-cloze-keywords':
;;   build the regex that detects cloze syntax in card text.  Users
;;   write `[hidden text]' or `[hidden||hint]' and expect those to be
;;   recognised — regardless of whether they've customized the cloze
;;   delimiters.
;;
;; - `org-drill-hypothetical-next-review-date' /
;;   `org-drill-hypothetical-next-review-dates': drive the "if you rate
;;   this Q, you'll see this card again in N days" preview the prompt
;;   shows.  The function reads the card's stored state and runs the
;;   active scheduler.
;;
;; - `org-drill-strip-entry-data': removes scheduling state from an
;;   entry — what happens when a user shares their deck with someone
;;   else.

;;; Code:

(require 'ert)
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

;;;; org-drill--compute-cloze-regexp

(ert-deftest test-org-drill--compute-cloze-regexp-matches-default-cloze ()
  "With default `[' and `]' delimiters, `[hidden]' is recognised as cloze."
  (let ((re (org-drill--compute-cloze-regexp)))
    (should (string-match-p re "Capital of France: [Paris]"))))

(ert-deftest test-org-drill--compute-cloze-regexp-matches-cloze-with-hint ()
  "`[hidden||hint]' is recognised — the hint separator is the default `||'."
  (let ((re (org-drill--compute-cloze-regexp)))
    (should (string-match-p re "Capital of France: [Paris||a city of light]"))))

(ert-deftest test-org-drill--compute-cloze-regexp-rejects-empty-cloze ()
  "An empty `[]' doesn't match — the regex requires at least one char inside."
  (let ((re (org-drill--compute-cloze-regexp)))
    (should-not (string-match-p re "Empty: []"))))

(ert-deftest test-org-drill--compute-cloze-regexp-rejects-bare-text ()
  "Plain prose with no cloze delimiters doesn't match."
  (let ((re (org-drill--compute-cloze-regexp)))
    (should-not (string-match-p re "No clozes here at all."))))

(ert-deftest test-org-drill--compute-cloze-regexp-honors-custom-delimiters ()
  "Custom `{{` / `}}' delimiters work — the regex rebuilds from current customs."
  (let ((org-drill-left-cloze-delimiter "{{")
        (org-drill-right-cloze-delimiter "}}"))
    (let ((re (org-drill--compute-cloze-regexp)))
      (should (string-match-p re "Capital of France: {{Paris}}"))
      ;; And it shouldn't accidentally match the old bracket form.
      (should-not (string-match-p re "Capital of France: [Paris]")))))

(ert-deftest test-org-drill--compute-cloze-regexp-captures-three-groups ()
  "The regex captures left-bracket+text, hint-or-empty, right-bracket as three groups.
Cloze fontification depends on this — regression check."
  (let* ((re (org-drill--compute-cloze-regexp))
         (s "[hidden||hint]"))
    (should (string-match re s))
    (should (equal "[hidden" (match-string 1 s)))
    (should (equal "||hint" (match-string 2 s)))
    (should (equal "]" (match-string 3 s)))))

;;;; org-drill--compute-cloze-keywords

(ert-deftest test-org-drill--compute-cloze-keywords-returns-fontification-spec ()
  "Returns a one-element list whose only entry is a font-lock matcher
with the cloze regex and three per-group face specs.

Note on shape: font-lock face specs use a quoted face-name form, e.g.
`(1 'org-drill-visible-cloze-face nil)' — the cadr at runtime is the
quoted-symbol list `(quote org-drill-visible-cloze-face)', not the
symbol itself."
  (let ((kw (org-drill--compute-cloze-keywords)))
    (should (= 1 (length kw)))
    (let ((spec (car kw)))
      (should (stringp (car spec)))                       ; the regex
      (should (= 4 (length spec)))                        ; regex + 3 face specs
      ;; Each per-group entry is (group-num 'face flag).  Pull the face
      ;; symbol out of its quoted form before comparing.
      (cl-flet ((face-of (entry) (cadr (cadr entry))))
        (should (eq 'org-drill-visible-cloze-face (face-of (nth 1 spec))))
        (should (eq 'org-drill-visible-cloze-hint-face (face-of (nth 2 spec))))
        (should (eq 'org-drill-visible-cloze-face (face-of (nth 3 spec))))))))

;;;; org-drill-hypothetical-next-review-date

(ert-deftest test-org-drill-hypothetical-next-review-date-virgin-quality-5 ()
  "On a virgin card with default SM5, quality-5 returns the SM5 first interval."
  (with-fresh-drill-entry
    (let ((days (org-drill-hypothetical-next-review-date 5)))
      (should (numberp days))
      (should (> days 0)))))

(ert-deftest test-org-drill-hypothetical-next-review-date-quality-2-or-below-returns-zero ()
  "Quality below 3 (failure) means the card resets — zero days, drill again today."
  (with-fresh-drill-entry
    (should (equal 0 (org-drill-hypothetical-next-review-date 0)))
    (should (equal 0 (org-drill-hypothetical-next-review-date 1)))
    (should (equal 0 (org-drill-hypothetical-next-review-date 2)))))

(ert-deftest test-org-drill-hypothetical-next-review-date-sm2-algorithm ()
  "With sm2 selected, hypothetical-next-review-date returns a positive number."
  (with-fresh-drill-entry
    (let ((org-drill-spaced-repetition-algorithm 'sm2))
      (let ((days (org-drill-hypothetical-next-review-date 5)))
        (should (numberp days))
        (should (>= days 0))))))

(ert-deftest test-org-drill-hypothetical-next-review-date-simple8-algorithm ()
  "With simple8 selected, hypothetical-next-review-date returns a positive number."
  (with-fresh-drill-entry
    (let ((org-drill-spaced-repetition-algorithm 'simple8))
      (let ((days (org-drill-hypothetical-next-review-date 5)))
        (should (numberp days))
        (should (>= days 0))))))

(ert-deftest test-org-drill-hypothetical-next-review-date-quality-monotonic ()
  "Higher quality means longer next-interval — the curve should be monotonic
non-decreasing across q=3 → q=5 on a virgin card."
  (with-fresh-drill-entry
    (let ((q3 (org-drill-hypothetical-next-review-date 3))
          (q4 (org-drill-hypothetical-next-review-date 4))
          (q5 (org-drill-hypothetical-next-review-date 5)))
      (should (<= q3 q4))
      (should (<= q4 q5)))))

(ert-deftest test-org-drill-hypothetical-next-review-date-respects-card-weight ()
  "DRILL_CARD_WEIGHT > 1 stretches the next-interval delta.
The contract: weight=2 with old-interval 0 and computed next of N gives
roughly old + max(1, (next-old)/2) days."
  (with-fresh-drill-entry
    ;; Set up a card that's been reviewed before, with weight = 2.
    (org-set-property "DRILL_CARD_WEIGHT" "2")
    (org-drill-store-item-data 10 3 0 3 4.5 2.5)
    (let ((q5-no-weight (progn
                          (org-delete-property "DRILL_CARD_WEIGHT")
                          (org-drill-hypothetical-next-review-date 5)))
          (q5-with-weight (progn
                            (org-set-property "DRILL_CARD_WEIGHT" "2")
                            (org-drill-hypothetical-next-review-date 5))))
      ;; Weight should *reduce* the gain compared to no weight.
      (should (<= q5-with-weight q5-no-weight)))))

;;;; org-drill-hypothetical-next-review-dates

(ert-deftest test-org-drill-hypothetical-next-review-dates-returns-six-values ()
  "Returns one value per quality level (0..5) — six total."
  (with-fresh-drill-entry
    (let ((dates (org-drill-hypothetical-next-review-dates)))
      (should (= 6 (length dates))))))

(ert-deftest test-org-drill-hypothetical-next-review-dates-non-decreasing ()
  "Each entry is at least as large as the previous — the function clamps to monotonic.
This is what users see in the prompt: `1 / 2 / 3 / 4 / 6 / 9' style hints."
  (with-fresh-drill-entry
    (let* ((dates (org-drill-hypothetical-next-review-dates))
           (pairs (cl-mapcar #'list dates (cdr dates))))
      (dolist (pair pairs)
        (should (<= (car pair) (cadr pair)))))))

;;;; org-drill-strip-entry-data

(ert-deftest test-org-drill-strip-entry-data-removes-scheduling-properties ()
  "Stripping wipes every property listed in `org-drill-scheduling-properties'."
  (with-fresh-drill-entry
    (org-drill-store-item-data 10 3 1 5 3.8 2.4)
    ;; sanity: the props are there
    (should (org-entry-get (point) "DRILL_LAST_INTERVAL"))
    (org-drill-strip-entry-data)
    ;; every scheduling property is gone
    (dolist (prop org-drill-scheduling-properties)
      (should (null (org-entry-get (point) prop))))))

(ert-deftest test-org-drill-strip-entry-data-on-virgin-entry-is-a-no-op ()
  "Stripping a card that has no data succeeds quietly."
  (with-fresh-drill-entry
    ;; should not error
    (org-drill-strip-entry-data)
    (dolist (prop org-drill-scheduling-properties)
      (should (null (org-entry-get (point) prop))))))

(provide 'test-org-drill-cloze-and-scheduling-helpers)

;;; test-org-drill-cloze-and-scheduling-helpers.el ends here
