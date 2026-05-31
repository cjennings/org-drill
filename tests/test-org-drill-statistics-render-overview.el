;;; test-org-drill-statistics-render-overview.el --- Tests for render-overview statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard render-overview block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

(defun test-org-drill-statistics--overview-record (start end qualities pass)
  "Build a session record fixture for overview renderer tests.
START and END are floats; QUALITIES a vector of ints; PASS an int."
  (make-org-drill-session-record
   :start-time start
   :end-time end
   :scope 'file
   :algorithm 'sm5
   :qualities qualities
   :pass-percent pass
   :new-count 0
   :mature-count 0
   :failed-count 0
   :cram-mode nil))

(ert-deftest test-org-drill-statistics-overview-table-row ()
  "Overview renders the header and a data row from the scope counts."
  (cl-letf (((symbol-function 'org-drill-statistics--overview-counts)
             (lambda (&optional _scope)
               (list :total 42 :new 7 :mature 30 :lapsed 5))))
    (let ((out (org-drill-statistics--render-overview nil nil)))
      (should (string-match-p "\\*\\* Overview" out))
      (should (string-match-p
               "| Total cards | New | Mature | Lapsed |" out))
      (should (string-match-p "| 42 | 7 | 30 | 5 |" out)))))

(ert-deftest test-org-drill-statistics-overview-last-session-recap ()
  "The recap line reports date, duration, card count, and pass percent."
  (cl-letf (((symbol-function 'org-drill-statistics--overview-counts)
             (lambda (&optional _scope)
               (list :total 1 :new 1 :mature 0 :lapsed 0))))
    ;; 2026-05-15 12:00:00 local, 15 minutes long, 3 cards, 67% pass.
    (let* ((start (float-time (encode-time 0 0 12 15 5 2026)))
           (end (+ start (* 15 60)))
           (record (test-org-drill-statistics--overview-record
                    start end (vector 4 4 1) 67))
           (out (org-drill-statistics--render-overview nil (list record))))
      (should (string-match-p "Last session: 2026-05-15" out))
      (should (string-match-p "15 min" out))
      (should (string-match-p "3 cards reviewed" out))
      (should (string-match-p "67% pass" out)))))

(ert-deftest test-org-drill-statistics-overview-singular-card ()
  "A one-card session uses the singular \"card\" in the recap."
  (cl-letf (((symbol-function 'org-drill-statistics--overview-counts)
             (lambda (&optional _scope)
               (list :total 1 :new 0 :mature 1 :lapsed 0))))
    (let* ((start (float-time (encode-time 0 0 9 1 1 2026)))
           (end (+ start 60.0))
           (record (test-org-drill-statistics--overview-record
                    start end (vector 5) 100))
           (out (org-drill-statistics--render-overview nil (list record))))
      (should (string-match-p "1 card reviewed" out))
      (should-not (string-match-p "1 cards reviewed" out)))))

(ert-deftest test-org-drill-statistics-overview-empty-log ()
  "With no logged sessions the recap states none recorded."
  (cl-letf (((symbol-function 'org-drill-statistics--overview-counts)
             (lambda (&optional _scope)
               (list :total 0 :new 0 :mature 0 :lapsed 0))))
    ;; Bind the persistent log to empty so a nil LOG arg resolves to an
    ;; empty log rather than falling back to whatever sessions the
    ;; running Emacs has persisted.
    (let* ((org-drill-session-log nil)
           (out (org-drill-statistics--render-overview nil nil)))
      (should (string-match-p "Last session: none recorded yet" out))
      (should (string-match-p "| 0 | 0 | 0 | 0 |" out)))))

(ert-deftest test-org-drill-statistics-overview-scope-traversal ()
  "Counts come from the org buffer in scope via the real aggregator.
Components integrated:
- org-drill-statistics--render-overview (entry point, real)
- org-drill-statistics--overview-counts (real, traverses the buffer)
- org-drill-entry-status / org-drill-session (real)
Validates the renderer threads SCOPE through to a live org traversal
rather than relying on a stub."
  (with-temp-buffer
    ;; The card must carry the drill question tag, otherwise
    ;; `org-drill-map-entries' skips it and the population is zero.
    (insert "* Cards\n"
            "** Card one :drill:\n"
            ":PROPERTIES:\n:DRILL_CARD_TYPE: simple\n:END:\n"
            "Front\n")
    (org-mode)
    (let* ((org-drill-scope 'file)
           (org-drill-question-tag "drill")
           (org-drill-match nil)
           (out (org-drill-statistics--render-overview 'file nil)))
      ;; One genuine drill card, never reviewed, so total and new are 1.
      (should (string-match-p "| 1 | 1 | 0 | 0 |" out)))))

(provide 'test-org-drill-statistics-render-overview)

;;; test-org-drill-statistics-render-overview.el ends here
