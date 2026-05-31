;;; test-org-drill-statistics-shell.el --- Tests for shell statistics  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the org-drill statistics dashboard shell block.

;;; Code:

(require 'ert)
(require 'org-drill)
(require 'cl-lib)
(require 'org)

;;; Tests for the statistics dashboard shell (step 2).


(defun org-drill-statistics-test--record (start-offset-days algorithm qualities)
  "Build a session record START-OFFSET-DAYS before now.
ALGORITHM is the algorithm symbol.  QUALITIES is a vector of int.  The
session lasts ten minutes.  Offsets are relative to `current-time' so the
fixture never hardcodes a date."
  (let* ((start (- (float-time) (* start-offset-days 86400.0)))
         (end (+ start 600.0)))
    (make-org-drill-session-record
     :start-time start
     :end-time end
     :scope 'file
     :algorithm algorithm
     :qualities qualities
     :pass-percent 50
     :new-count 1
     :mature-count 2
     :failed-count 0
     :cram-mode nil)))

(ert-deftest test-org-drill-statistics-shell-range-cutoff-known-label ()
  "A range preset with a day count yields a cutoff that many days back."
  (let ((now (float-time)))
    (let ((cutoff (org-drill-statistics--range-cutoff-float "last 7d")))
      (should cutoff)
      ;; Cutoff is roughly seven days before now, within a generous slop.
      (should (< (abs (- cutoff (- now (* 7 86400.0)))) 5.0)))))

(ert-deftest test-org-drill-statistics-shell-range-cutoff-all-time-nil ()
  "The all-time preset (nil days) yields no cutoff."
  (should (null (org-drill-statistics--range-cutoff-float "all time")))
  (should (null (org-drill-statistics--range-cutoff-float "no such label"))))

(ert-deftest test-org-drill-statistics-shell-filtered-log-by-algorithm ()
  "Filtering the log by algorithm keeps only matching records."
  (let ((org-drill-session-log
         (list (org-drill-statistics-test--record 1 'simple8 [4 5])
               (org-drill-statistics-test--record 2 'sm5 [3 2]))))
    (let ((only-sm5 (org-drill-statistics--filtered-log "all time" 'sm5)))
      (should (= 1 (length only-sm5)))
      (should (eq 'sm5 (org-drill-session-record-algorithm
                        (car only-sm5)))))
    (should (= 2 (length (org-drill-statistics--filtered-log
                          "all time" nil))))))

(ert-deftest test-org-drill-statistics-shell-filtered-log-by-range ()
  "An old record falls outside a short range window."
  (let ((org-drill-session-log
         (list (org-drill-statistics-test--record 1 'simple8 [4])
               (org-drill-statistics-test--record 40 'simple8 [3]))))
    (should (= 1 (length (org-drill-statistics--filtered-log
                          "last 7d" nil))))
    (should (= 2 (length (org-drill-statistics--filtered-log
                          "last 90d" nil))))))

(ert-deftest test-org-drill-statistics-shell-header-line-format ()
  "The header line names all three active filters."
  (let ((line (org-drill-statistics--header-line 'file "last 90d" 'simple8)))
    (should (string-match-p "Scope: file" line))
    (should (string-match-p "Range: last 90d" line))
    (should (string-match-p "Algorithm: simple8" line)))
  ;; nil scope falls back to org-drill-scope, nil algorithm reads "all".
  (let* ((org-drill-scope 'directory)
         (line (org-drill-statistics--header-line nil "last 7d" nil)))
    (should (string-match-p "Scope: directory" line))
    (should (string-match-p "Algorithm: all" line))))

(ert-deftest test-org-drill-statistics-shell-cycle-range-wraps ()
  "Cycling range advances through presets and wraps to the first."
  (with-temp-buffer
    (let ((org-drill-statistics-range-presets
           '(("last 90d" . 90) ("last 30d" . 30) ("all time" . nil)))
          ;; Stub the in-place re-render so the cycle command stays pure
          ;; with respect to buffer contents and the renderers.
          (org-drill-session-log nil))
      (cl-letf (((symbol-function 'org-drill-statistics-refresh)
                 (lambda () nil)))
        (setq org-drill-statistics--range "last 90d")
        (org-drill-statistics-cycle-range)
        (should (equal "last 30d" org-drill-statistics--range))
        (org-drill-statistics-cycle-range)
        (should (equal "all time" org-drill-statistics--range))
        (org-drill-statistics-cycle-range)
        (should (equal "last 90d" org-drill-statistics--range))))))

(ert-deftest test-org-drill-statistics-shell-cycle-algorithm-from-log ()
  "Cycling algorithm walks nil then each algorithm seen in the log."
  (with-temp-buffer
    (let ((org-drill-session-log
           (list (org-drill-statistics-test--record 1 'simple8 [4])
                 (org-drill-statistics-test--record 2 'sm5 [3]))))
      (cl-letf (((symbol-function 'org-drill-statistics-refresh)
                 (lambda () nil)))
        (setq org-drill-statistics--algorithm nil)
        (org-drill-statistics-cycle-algorithm)
        (should (memq org-drill-statistics--algorithm '(simple8 sm5)))
        (org-drill-statistics-cycle-algorithm)
        (should (memq org-drill-statistics--algorithm '(simple8 sm5)))
        ;; Third cycle wraps back to all-algorithms (nil).
        (org-drill-statistics-cycle-algorithm)
        (should (null org-drill-statistics--algorithm))))))

(ert-deftest test-org-drill-statistics-shell-integration-assembles-sections ()
  "The assembled dashboard body contains every section's output.
Components integrated:
- org-drill-statistics--render-all (entry point, real)
- the five org-drill-statistics--render-* helpers (real)
- org-drill-session-log fixture (real, let-bound)
The card-scanning helpers are exercised against an empty current buffer,
so the card population is zero, but every section header must still
appear in the assembled string."
  (with-temp-buffer
    (org-mode)
    (let ((org-drill-session-log
           (list (org-drill-statistics-test--record 1 'simple8 [4 5 2])
                 (org-drill-statistics-test--record 3 'simple8 [3 4])
                 (org-drill-statistics-test--record 8 'sm5 [5 5 1]))))
      ;; Use 'file scope: the buffer has no headline, and 'tree errors
      ;; when point is before the first headline.  'file scans the
      ;; whole (empty) buffer and yields a zero card population.
      (let ((body (org-drill-statistics--render-all
                   'file (caar org-drill-statistics-range-presets) nil)))
        (should (stringp body))
        ;; The header line is always present.
        (should (string-match-p "Scope:" body))
        (should (string-match-p "Range:" body))
        (should (string-match-p "Algorithm:" body))
        ;; Each render section contributes recognizable text.  The exact
        ;; header wording lives in the render helpers; assert on the
        ;; section keywords the spec fixes rather than full prose.
        (should (string-match-p "[Oo]verview" body))
        (should (string-match-p "[Tt]rend" body))
        (should (string-match-p "[Dd]istribution" body))
        (should (string-match-p "[Aa]ttention" body))
        (should (string-match-p "[Ff]orecast" body))))))

(provide 'test-org-drill-statistics-shell)

;;; test-org-drill-statistics-shell.el ends here
