;;; test-org-drill-statistics-render-attention.el --- Tests for render-attention statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard render-attention block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; tests/test-org-drill-statistics-render-attention.el -*- lexical-binding: t; -*-

(defun test-org-drill-stats--attn-fixture ()
  "Insert a fixture buffer of drill cards and return today's day number.
Twelve leech candidates so the cap and footer are exercised, plus one
long-overdue card and one forgotten-new card.  Dates derive from the
current time so the fixture never hardcodes today."
  (let* ((today (org-drill-statistics--today-day))
         (old-review (org-drill-time-to-inactive-org-timestamp
                      (org-time-string-to-time
                       (format-time-string
                        "%Y-%m-%d"
                        (time-subtract (current-time)
                                       (days-to-time 400))))))
         (old-added (format-time-string
                     "%Y-%m-%d"
                     (time-subtract (current-time) (days-to-time 30)))))
    (insert "* Drill cards\n")
    (dotimes (i 12)
      (insert (format "** Leech %02d :drill:\n" i))
      (insert ":PROPERTIES:\n")
      (insert (format ":DRILL_FAILURE_COUNT: %d\n"
                      (+ org-drill-leech-failure-threshold 2)))
      (insert (format ":DRILL_AVERAGE_QUALITY: %s\n"
                      (number-to-string (+ 1.0 (* 0.1 i)))))
      (insert ":END:\n"))
    (insert "** Overdue card :drill:\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":DRILL_LAST_REVIEWED: %s\n" old-review))
    (insert ":DRILL_FAILURE_COUNT: 0\n")
    (insert ":END:\n")
    (insert "** Forgotten card :drill:\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":DATE_ADDED: %s\n" old-added))
    (insert ":DRILL_TOTAL_REPEATS: 0\n")
    (insert ":END:\n")
    (org-mode)
    today))

(ert-deftest test-org-drill-statistics-attention-section-heading ()
  "The rendered section opens with the Needs attention heading."
  (with-temp-buffer
    (test-org-drill-stats--attn-fixture)
    (let ((out (org-drill-statistics--render-attention 'file)))
      (should (string-match-p "^\\*\\* Needs attention$" out))
      (should (string-match-p "^\\*\\*\\* Leech candidates$" out))
      (should (string-match-p "^\\*\\*\\* Long overdue$" out))
      (should (string-match-p "^\\*\\*\\* Forgotten new$" out)))))

(ert-deftest test-org-drill-statistics-attention-leech-rows-as-links ()
  "Leech candidates render as org links carrying card headings."
  (with-temp-buffer
    (test-org-drill-stats--attn-fixture)
    (let ((out (org-drill-statistics--render-attention 'file)))
      (should (string-match-p "| Card |" out))
      (should (string-match-p "\\[\\[org-drill-card:[0-9]+\\]\\[Leech 00\\]\\]"
                              out)))))

(ert-deftest test-org-drill-statistics-attention-cap-and-footer ()
  "Twelve leeches over a 10 cap show 10 rows and a +2 more footer."
  (with-temp-buffer
    (test-org-drill-stats--attn-fixture)
    (let* ((org-drill-statistics-attention-row-limit 10)
           (out (org-drill-statistics--render-attention 'file))
           (link-count
            (cl-count ?\n
                      (mapconcat #'identity
                                 (seq-filter
                                  (lambda (l) (string-match-p "org-drill-card:" l))
                                  (split-string out "\n"))
                                 "\n"))))
      (should (string-match-p "+2 more" out))
      ;; 10 leech (capped) + 1 overdue + 1 forgotten = 12 link rows.
      (should (= 12 (1+ link-count))))))

(ert-deftest test-org-drill-statistics-attention-leech-sort-worst-first ()
  "Leech rows are ordered by ascending average quality, worst first."
  (with-temp-buffer
    (test-org-drill-stats--attn-fixture)
    (let* ((out (org-drill-statistics--render-attention 'file))
           (leech-00 (string-match "Leech 00" out))
           (leech-01 (string-match "Leech 01" out)))
      (should leech-00)
      (should leech-01)
      ;; Leech 00 has avg 1.0, Leech 01 has 1.1, so 00 sorts first.
      (should (< leech-00 leech-01)))))

(ert-deftest test-org-drill-statistics-attention-empty-category-note ()
  "A category with no matches renders a note rather than a table."
  (with-temp-buffer
    (insert "* Cards\n** Healthy :drill:\n:PROPERTIES:\n")
    (insert ":DRILL_FAILURE_COUNT: 0\n:DRILL_TOTAL_REPEATS: 5\n:END:\n")
    (org-mode)
    (let ((out (org-drill-statistics--render-attention 'file)))
      (should (string-match-p "No leech candidates\\." out))
      (should (string-match-p "No long-overdue cards\\." out))
      (should (string-match-p "No forgotten-new cards\\." out)))))

(ert-deftest test-org-drill-statistics-attention-no-footer-under-cap ()
  "With matches at or under the cap, no +N more footer appears."
  (with-temp-buffer
    (insert "* Cards\n")
    (dotimes (i 3)
      (insert (format "** Leech %d :drill:\n" i))
      (insert ":PROPERTIES:\n")
      (insert (format ":DRILL_FAILURE_COUNT: %d\n"
                      (+ org-drill-leech-failure-threshold 1)))
      (insert ":DRILL_AVERAGE_QUALITY: 1.0\n:END:\n"))
    (org-mode)
    (let* ((org-drill-statistics-attention-row-limit 10)
           (out (org-drill-statistics--render-attention 'file)))
      (should-not (string-match-p "more" out)))))

(ert-deftest test-org-drill-statistics-card-link-sanitizes-brackets ()
  "Closing brackets in a heading cannot terminate the link early."
  (let ((link (org-drill-statistics--card-link "a]] b" 42)))
    (should (string-prefix-p "[[org-drill-card:42][" link))
    (should (string-suffix-p "]]" link))
    (should-not (string-match-p "a]] b" link))))

(ert-deftest test-org-drill-statistics-card-link-empty-heading-fallback ()
  "An empty heading falls back to a position-based description."
  (let ((link (org-drill-statistics--card-link "" 99)))
    (should (string-match-p "\\[\\[org-drill-card:99\\]\\[card at 99\\]\\]"
                            link))))

(provide 'test-org-drill-statistics-render-attention)

;;; test-org-drill-statistics-render-attention.el ends here
