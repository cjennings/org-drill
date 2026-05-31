;;; test-org-drill-statistics-export.el --- Tests for stats CSV export  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 3 of the statistics dashboard: `org-drill-statistics-export-csv'
;; writes sessions.csv, cards.csv, and daily.csv.  The row builders are pure
;; and tested with deterministic fixtures here; the command itself is exercised
;; against a temp directory.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)

;;;; Fixtures

(defun test-org-drill-stats-export--record (day-offset qualities duration-min
                                                       &optional algorithm)
  "Build a session record DAY-OFFSET days before now.
QUALITIES is a list of ints, DURATION-MIN the session length in minutes,
ALGORITHM a symbol (default simple8)."
  (let* ((start (- (float-time) (* day-offset 86400.0)))
         (end (+ start (* duration-min 60.0)))
         (qv (vconcat qualities))
         (passes (cl-count-if (lambda (q) (> q org-drill-failure-quality)) qv)))
    (make-org-drill-session-record
     :start-time start :end-time end :scope 'directory
     :algorithm (or algorithm 'simple8)
     :qualities qv
     :pass-percent (if (> (length qv) 0)
                       (round (* 100.0 (/ (float passes) (length qv)))) 0)
     :new-count 0 :mature-count 0 :failed-count 0 :cram-mode nil)))

;;;; csv-quote

(ert-deftest test-org-drill-statistics-export-csv-quote-plain ()
  "A field with no special characters is returned unchanged."
  (should (equal (org-drill-statistics--csv-quote "hello") "hello")))

(ert-deftest test-org-drill-statistics-export-csv-quote-comma ()
  "A field containing a comma is wrapped in double quotes."
  (should (equal (org-drill-statistics--csv-quote "a,b") "\"a,b\"")))

(ert-deftest test-org-drill-statistics-export-csv-quote-doubles-quotes ()
  "Embedded double quotes are doubled and the field is wrapped."
  (should (equal (org-drill-statistics--csv-quote "say \"hi\"")
                 "\"say \"\"hi\"\"\"")))

(ert-deftest test-org-drill-statistics-export-csv-quote-newline ()
  "A field containing a newline is wrapped in double quotes."
  (should (equal (org-drill-statistics--csv-quote "a\nb") "\"a\nb\"")))

(ert-deftest test-org-drill-statistics-export-csv-quote-coerces-non-string ()
  "A non-string field is coerced to its printed form."
  (should (equal (org-drill-statistics--csv-quote 42) "42")))

;;;; csv-row

(ert-deftest test-org-drill-statistics-export-csv-row-joins-and-quotes ()
  "A row joins fields with commas, quoting only those that need it."
  (should (equal (org-drill-statistics--csv-row '("a" "b,c" 7))
                 "a,\"b,c\",7")))

;;;; sessions rows

(ert-deftest test-org-drill-statistics-export-session-row-fields ()
  "A session row carries the counts, the space-joined qualities, and cram."
  (let* ((rec (test-org-drill-stats-export--record 0 '(5 4 1) 6 'sm5))
         (row (org-drill-statistics--session-row rec)))
    ;; qualities column is space-joined
    (should (member "5 4 1" row))
    ;; algorithm and cram render as printed symbols
    (should (member "sm5" row))
    (should (member "nil" row))
    ;; pass-percent for 2 of 3 passing is 67
    (should (member "67" row))))

(ert-deftest test-org-drill-statistics-export-sessions-rows-one-per-record ()
  "One row per record in the log."
  (let ((log (list (test-org-drill-stats-export--record 0 '(5) 1)
                   (test-org-drill-stats-export--record 1 '(2) 1))))
    (should (= (length (org-drill-statistics--sessions-rows log)) 2))))

;;;; daily rows

(ert-deftest test-org-drill-statistics-export-daily-rows-length ()
  "Daily rows have one entry per day in the window."
  (let ((log (list (test-org-drill-stats-export--record 0 '(5) 1))))
    (should (= (length (org-drill-statistics--daily-rows log 7)) 7))))

(ert-deftest test-org-drill-statistics-export-daily-rows-today-aggregates ()
  "Today's row sums reviews, passes, fails, and duration across its records."
  ;; failure-quality default 2: 5 and 4 pass, 1 fails.
  (let* ((log (list (test-org-drill-stats-export--record 0 '(5 4 1) 6)))
         (rows (org-drill-statistics--daily-rows log 7))
         (today (car (last rows))))  ; oldest-first, so today is last
    ;; row shape: (date reviews passes fails pass% duration)
    (should (equal (nth 1 today) "3"))    ; reviews
    (should (equal (nth 2 today) "2"))    ; passes
    (should (equal (nth 3 today) "1"))    ; fails
    (should (equal (nth 4 today) "67"))   ; pass percent
    (should (equal (nth 5 today) "6"))))  ; duration minutes

(ert-deftest test-org-drill-statistics-export-daily-rows-empty-day-zeros ()
  "A day with no records reports zero reviews and a zero pass percent."
  (let* ((log (list (test-org-drill-stats-export--record 0 '(5) 1)))
         (rows (org-drill-statistics--daily-rows log 7))
         (yesterday (nth (- (length rows) 2) rows)))
    (should (equal (nth 1 yesterday) "0"))
    (should (equal (nth 2 yesterday) "0"))))

;;;; cards rows

(ert-deftest test-org-drill-statistics-export-cards-rows-from-scope ()
  "Cards rows carry the heading, scheduling props, and computed status."
  (with-temp-buffer
    (insert "* Capital of France :drill:\n"
            ":PROPERTIES:\n"
            ":DRILL_TOTAL_REPEATS: 4\n"
            ":DRILL_LAST_QUALITY: 5\n"
            ":END:\n"
            "Paris\n")
    (org-mode)
    (let* ((org-drill-question-tag "drill")
           (rows (org-drill-statistics--cards-rows 'file)))
      (should (= (length rows) 1))
      (let ((row (car rows)))
        (should (member "Capital of France" row))
        (should (member "4" row))     ; DRILL_TOTAL_REPEATS
        (should (member "5" row))))))  ; DRILL_LAST_QUALITY

;;;; export command

(ert-deftest test-org-drill-statistics-export-writes-three-files ()
  "The export command writes sessions.csv, cards.csv, and daily.csv."
  (let ((dir (make-temp-file "org-drill-stats-export" t)))
    (unwind-protect
        (with-temp-buffer
          (insert "* A card :drill:\nQ [answer] A\n")
          (org-mode)
          (let ((org-drill-question-tag "drill")
                (org-drill-scope 'file)
                (org-drill-session-log
                 (list (test-org-drill-stats-export--record 0 '(5 4) 3)
                       (test-org-drill-stats-export--record 1 '(1) 2))))
            (org-drill-statistics-export-csv dir)))
      nil)
    (unwind-protect
        (dolist (name '("sessions.csv" "cards.csv" "daily.csv"))
          (let ((path (expand-file-name name dir)))
            (should (file-exists-p path))
            ;; non-empty: at least a header line
            (should (> (nth 7 (file-attributes path)) 0))))
      (delete-directory dir t))))

(ert-deftest test-org-drill-statistics-export-sessions-file-has-header-and-rows ()
  "sessions.csv starts with the header and has one line per record."
  (let ((dir (make-temp-file "org-drill-stats-export" t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "* A card :drill:\nQ [answer] A\n")
            (org-mode)
            (let ((org-drill-question-tag "drill")
                  (org-drill-scope 'file)
                  (org-drill-session-log
                   (list (test-org-drill-stats-export--record 0 '(5 4) 3)
                         (test-org-drill-stats-export--record 1 '(1) 2))))
              (org-drill-statistics-export-csv dir)))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "sessions.csv" dir))
            (let ((lines (split-string (buffer-string) "\n" t)))
              ;; header + 2 data rows
              (should (= (length lines) 3))
              (should (string-match-p "pass_percent" (car lines))))))
      (delete-directory dir t))))

(provide 'test-org-drill-statistics-export)

;;; test-org-drill-statistics-export.el ends here
