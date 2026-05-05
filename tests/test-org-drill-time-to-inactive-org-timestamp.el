;;; test-org-drill-time-to-inactive-org-timestamp.el --- Tests for inactive org-timestamp formatting  -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for `org-drill-time-to-inactive-org-timestamp'.
;;
;; Upstream issue #59 (chipschap, 2025-08-28) reported broken timestamps
;; like `[Y-08-27 Wed 16:%]' written into DRILL_LAST_REVIEWED.  Root
;; cause: in Org 9.6+, `(cdr org-time-stamp-formats)' returns the format
;; string without the angle brackets it had pre-9.6.  The original
;; `(substring ... 1 -1)' assumed brackets and stripped the leading `%'
;; of `%Y' and the trailing `M' of `%M', producing `Y-%m-%d %a %H:%' as
;; the format directive.  format-time-string then interpolated `Y' as a
;; literal and left a trailing `%]' in the output.
;;
;; Cherry-pick 4c6e62a switched the Org 9.6+ path to
;; `(org-time-stamp-format t 'no-bracket)' (which despite the arg name
;; *does* return a bracketed format).  These tests lock that behavior
;; in.

;;; Code:

(require 'ert)
(require 'org-drill)

(defconst test-tts-fixed-time
  ;; Saturday 2024-06-15 12:30:00 local time.
  (encode-time 0 30 12 15 6 2024)
  "Fixed reference time used by these tests.")

(defconst test-tts-timestamp-regex
  "\\`\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z][a-z] [0-9]\\{2\\}:[0-9]\\{2\\}\\]\\'"
  "Anchored regex for a well-formed inactive org-timestamp like `[2024-06-15 Sat 12:30]'.")

;;;; Normal cases

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-normal-fixed-time-matches-shape ()
  "A normal time produces a bracketed YYYY-MM-DD Day HH:MM string."
  (let ((result (org-drill-time-to-inactive-org-timestamp test-tts-fixed-time)))
    (should (string-match-p test-tts-timestamp-regex result))))

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-normal-fixed-time-exact-string ()
  "The fixed reference time produces the expected literal timestamp."
  (should (equal (org-drill-time-to-inactive-org-timestamp test-tts-fixed-time)
                 "[2024-06-15 Sat 12:30]")))

;;;; Boundary cases

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-boundary-midnight ()
  "Midnight (00:00) formats correctly with leading zeros."
  (let* ((midnight (encode-time 0 0 0 1 1 2024))
         (result (org-drill-time-to-inactive-org-timestamp midnight)))
    (should (string-match-p test-tts-timestamp-regex result))
    (should (string-match-p "00:00\\]\\'" result))))

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-boundary-end-of-year ()
  "Last minute of the year formats correctly."
  (let* ((eoy (encode-time 0 59 23 31 12 2024))
         (result (org-drill-time-to-inactive-org-timestamp eoy)))
    (should (string-match-p test-tts-timestamp-regex result))
    (should (string-match-p "2024-12-31" result))
    (should (string-match-p "23:59\\]\\'" result))))

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-boundary-leap-day ()
  "Feb 29 on a leap year formats correctly."
  (let* ((leap (encode-time 0 0 12 29 2 2024))
         (result (org-drill-time-to-inactive-org-timestamp leap)))
    (should (string-match-p test-tts-timestamp-regex result))
    (should (string-match-p "2024-02-29" result))))

;;;; Error / regression cases

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-error-no-stray-percent-directives ()
  "Issue #59 regression: output must not contain stray `%' characters.

Pre-fix, Org 9.6+ produced timestamps like `[Y-08-27 Wed 16:%]' because the
Org 9.5-shaped substring slicing dropped the leading `%' of `%Y' and the
trailing `M' of `%M'.  Any literal `%' in the output indicates the format
string lost a directive."
  (let ((result (org-drill-time-to-inactive-org-timestamp test-tts-fixed-time)))
    (should-not (string-match-p "%" result))))

(ert-deftest test-org-drill-time-to-inactive-org-timestamp-error-year-not-literal-Y ()
  "Issue #59 regression: the year must be four digits, not the literal char `Y'."
  (let ((result (org-drill-time-to-inactive-org-timestamp test-tts-fixed-time)))
    (should-not (string-match-p "\\[Y-" result))
    (should (string-match-p "\\[2024-" result))))

(provide 'test-org-drill-time-to-inactive-org-timestamp)

;;; test-org-drill-time-to-inactive-org-timestamp.el ends here
