;;; test-org-drill-session-record.el --- Tests for the stats session log  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the persist + recording layer that powers the stats
;; dashboard (see docs/design/stats-dashboard.org).
;;
;; The contract:
;;
;; - Every completed (non-suspended) drill session contributes one
;;   `org-drill-session-record' to `org-drill-session-log'.
;; - The log persists via `persist-defvar' between Emacs runs.
;; - A corrupt persist file is renamed to a dated `.corrupt-...'
;;   sibling and the log starts fresh, matching the SM5-matrix
;;   recovery pattern in `test-org-drill-persist-recovery.el'.
;; - Suspended sessions (end-pos set) do NOT record — the abort path
;;   discards, mirroring the `org-drill-on-timeout-action'
;;   `discard-current' semantics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'persist)
(require 'org-drill)

;;;; Helpers

(defun test-session-record--marker-at (pos)
  "Return a marker pointing at POS (an integer)."
  (let ((m (make-marker)))
    (set-marker m pos)
    m))

(defmacro test-session-record--with-empty-log (&rest body)
  "Run BODY with a fresh, empty `org-drill-session-log' and a stub
`persist-save' so tests never touch the real persist file."
  (declare (indent 0))
  `(let ((org-drill-session-log nil))
     (cl-letf (((symbol-function 'persist-save) #'ignore))
       ,@body)))

(cl-defun test-session-record--populated-session (&key qualities new mature failed
                                                       cram-mode
                                                       (scope-at-start 'file)
                                                       (algorithm-at-start 'simple8))
  "Return an `org-drill-session' with QUALITIES (a list of 0-5 integers)
and the requested entry counts.  NEW / MATURE / FAILED are integers.
SCOPE-AT-START and ALGORITHM-AT-START populate the session slots that
`org-drill--prepare-fresh-session' would set in production; pass nil to
exercise the not-prepared path."
  (let ((session (org-drill-session)))
    (oset session qualities qualities)
    (oset session new-entries
          (cl-loop for i from 1 to (or new 0)
                   collect (test-session-record--marker-at i)))
    ;; Mature = young + old; we put all on `old-mature-entries' for
    ;; the count assertion — sum is what the record stores.
    (oset session old-mature-entries
          (cl-loop for i from 1 to (or mature 0)
                   collect (test-session-record--marker-at i)))
    (oset session failed-entries
          (cl-loop for i from 1 to (or failed 0)
                   collect (test-session-record--marker-at i)))
    (oset session cram-mode cram-mode)
    (oset session scope-at-start scope-at-start)
    (oset session algorithm-at-start algorithm-at-start)
    session))

;;;; A. Struct construction (Normal)

(ert-deftest test-session-record-struct-construction-all-slots ()
  "`make-org-drill-session-record' accepts every documented slot and
the accessors read them back unchanged."
  (let ((rec (make-org-drill-session-record
              :start-time 1700000000.0
              :end-time   1700001800.0
              :scope 'file
              :algorithm 'simple8
              :qualities [5 4 3 2 1 0]
              :pass-percent 50
              :new-count 4
              :mature-count 10
              :failed-count 2
              :cram-mode nil)))
    (should (= 1700000000.0 (org-drill-session-record-start-time rec)))
    (should (= 1700001800.0 (org-drill-session-record-end-time rec)))
    (should (eq 'file (org-drill-session-record-scope rec)))
    (should (eq 'simple8 (org-drill-session-record-algorithm rec)))
    (should (equal [5 4 3 2 1 0] (org-drill-session-record-qualities rec)))
    (should (= 50 (org-drill-session-record-pass-percent rec)))
    (should (= 4 (org-drill-session-record-new-count rec)))
    (should (= 10 (org-drill-session-record-mature-count rec)))
    (should (= 2 (org-drill-session-record-failed-count rec)))
    (should-not (org-drill-session-record-cram-mode rec))))

;;;; B. Pass-percent (Normal / Boundary / Error)

(ert-deftest test-compute-pass-percent-shared-helper-handles-empty ()
  "`org-drill--compute-pass-percent' is the single source of truth shared
by `org-drill-final-report' and the dashboard record.  Empty qualities
must yield 0 (no div-by-zero).  Pinned here so future drift between
the two consumers gets caught."
  (let ((org-drill-failure-quality 2))
    (should (= 0 (org-drill--compute-pass-percent nil)))
    (should (= 0 (org-drill--compute-pass-percent '())))))

(ert-deftest test-compute-pass-percent-shared-helper-rounds-mixed ()
  "Helper rounds (count above failure-quality) / total * 100."
  (let ((org-drill-failure-quality 2))
    (should (= 60 (org-drill--compute-pass-percent '(5 4 3 2 1))))
    (should (= 100 (org-drill--compute-pass-percent '(5 5 5))))
    (should (= 0 (org-drill--compute-pass-percent '(0 1 2))))))



(ert-deftest test-session-record-pass-percent-mixed-qualities ()
  "Pass percent rounds (count of qualities > failure-quality) / total.

With the default failure-quality of 2 and qualities (5 4 3 2 1), 3 of
5 are above threshold => 60%."
  (test-session-record--with-empty-log
    (let* ((org-drill-failure-quality 2)
           (session (test-session-record--populated-session
                     :qualities '(5 4 3 2 1)))
           (rec (org-drill-session-record-from-session
                 session 0.0 1.0)))
      (should (= 60 (org-drill-session-record-pass-percent rec))))))

(ert-deftest test-session-record-pass-percent-all-pass ()
  "Every quality above failure-quality => 100%."
  (test-session-record--with-empty-log
    (let* ((org-drill-failure-quality 2)
           (session (test-session-record--populated-session
                     :qualities '(5 5 5 4 3)))
           (rec (org-drill-session-record-from-session
                 session 0.0 1.0)))
      (should (= 100 (org-drill-session-record-pass-percent rec))))))

(ert-deftest test-session-record-pass-percent-all-fail ()
  "Every quality at-or-below failure-quality => 0%."
  (test-session-record--with-empty-log
    (let* ((org-drill-failure-quality 2)
           (session (test-session-record--populated-session
                     :qualities '(2 1 0 2 1)))
           (rec (org-drill-session-record-from-session
                 session 0.0 1.0)))
      (should (= 0 (org-drill-session-record-pass-percent rec))))))

(ert-deftest test-session-record-pass-percent-empty-qualities-is-zero ()
  "An empty qualities list must not divide by zero — returns 0%."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session :qualities nil))
           (rec (org-drill-session-record-from-session
                 session 0.0 1.0)))
      (should (= 0 (org-drill-session-record-pass-percent rec))))))

;;;; C. Builder from session (Normal / Boundary)

(ert-deftest test-session-record-from-session-copies-counts ()
  "The builder reads new / mature / failed counts directly off the session."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session
                     :qualities '(4 4 4)
                     :new 7
                     :mature 12
                     :failed 1))
           (rec (org-drill-session-record-from-session
                 session 100.0 200.0)))
      (should (= 7 (org-drill-session-record-new-count rec)))
      (should (= 12 (org-drill-session-record-mature-count rec)))
      (should (= 1 (org-drill-session-record-failed-count rec))))))

(ert-deftest test-session-record-mature-count-sums-young-and-old ()
  "Mature count = young-mature-entries + old-mature-entries."
  (test-session-record--with-empty-log
    (let ((session (org-drill-session)))
      (oset session qualities '(3))
      (oset session young-mature-entries
            (list (test-session-record--marker-at 1)
                  (test-session-record--marker-at 2)))
      (oset session old-mature-entries
            (list (test-session-record--marker-at 3)
                  (test-session-record--marker-at 4)
                  (test-session-record--marker-at 5)))
      (let ((rec (org-drill-session-record-from-session session 0.0 1.0)))
        (should (= 5 (org-drill-session-record-mature-count rec)))))))

(ert-deftest test-session-record-records-cram-mode-flag ()
  "The session's cram-mode value lands on the record."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session
                     :qualities '(4) :cram-mode t))
           (rec (org-drill-session-record-from-session session 0.0 1.0)))
      (should (org-drill-session-record-cram-mode rec)))))

(ert-deftest test-session-record-stores-algorithm-captured-at-start ()
  "The record carries the algorithm captured at session start
(via the session's `algorithm-at-start' slot), not the global at
record-build time.  This protects against a mid-session defcustom flip
misrepresenting what was actually drilled."
  (test-session-record--with-empty-log
    (let* ((org-drill-spaced-repetition-algorithm 'simple8)  ; mid-session value
           (session (test-session-record--populated-session
                     :qualities '(4)
                     :algorithm-at-start 'sm5))               ; captured at start
           (rec (org-drill-session-record-from-session session 0.0 1.0)))
      (should (eq 'sm5 (org-drill-session-record-algorithm rec))))))

(ert-deftest test-session-record-stores-scope-captured-at-start ()
  "The record carries the scope captured at session start, not the
global at record-build time.  Same rationale as the algorithm capture."
  (test-session-record--with-empty-log
    (let* ((org-drill-scope 'directory)                       ; mid-session value
           (session (test-session-record--populated-session
                     :qualities '(3)
                     :scope-at-start 'file))                  ; captured at start
           (rec (org-drill-session-record-from-session session 0.0 1.0)))
      (should (eq 'file (org-drill-session-record-scope rec))))))

(ert-deftest test-session-record-stores-timestamps ()
  "Start and end timestamps land on the record verbatim."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session :qualities '(3)))
           (rec (org-drill-session-record-from-session
                 session 1234567890.0 1234571490.0)))
      (should (= 1234567890.0 (org-drill-session-record-start-time rec)))
      (should (= 1234571490.0 (org-drill-session-record-end-time rec))))))

(ert-deftest test-session-record-qualities-stored-as-vector ()
  "Qualities are stored as a vector (the spec's chosen shape), not a list."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session
                     :qualities '(5 4 3 2 1)))
           (rec (org-drill-session-record-from-session session 0.0 1.0)))
      (should (vectorp (org-drill-session-record-qualities rec)))
      (should (equal [5 4 3 2 1]
                     (org-drill-session-record-qualities rec))))))

;;;; D. Log append (Normal / Boundary)

(ert-deftest test-record-session-prepends-newest-first ()
  "`org-drill-record-session' adds the new record at the head of the log."
  (test-session-record--with-empty-log
    (let ((session (test-session-record--populated-session :qualities '(4))))
      (org-drill-record-session session 0.0 1.0)
      (org-drill-record-session session 10.0 11.0)
      (should (= 2 (length org-drill-session-log)))
      ;; Most-recent at head.
      (should (= 10.0 (org-drill-session-record-start-time
                       (car org-drill-session-log)))))))

(ert-deftest test-record-session-handles-empty-log ()
  "First record on an empty log produces a single-element list."
  (test-session-record--with-empty-log
    (let ((session (test-session-record--populated-session :qualities '(4))))
      (org-drill-record-session session 0.0 1.0)
      (should (= 1 (length org-drill-session-log)))
      (should (org-drill-session-record-p (car org-drill-session-log))))))

(ert-deftest test-record-session-calls-persist-save ()
  "`org-drill-record-session' calls `persist-save' on the log symbol."
  (let ((org-drill-session-log nil)
        (saved nil))
    (cl-letf (((symbol-function 'persist-save)
               (lambda (sym) (setq saved sym))))
      (let ((session (test-session-record--populated-session :qualities '(4))))
        (org-drill-record-session session 0.0 1.0))
      (should (eq 'org-drill-session-log saved)))))

;;;; E. Persist round-trip smoke check

(ert-deftest test-session-log-symbol-is-bound ()
  "After org-drill loads, `org-drill-session-log' is bound (either to the
loaded value or, on persist failure, to the fallback nil — same recovery
pattern as `org-drill-sm5-optimal-factor-matrix')."
  (should (boundp 'org-drill-session-log)))

;;;; F. Corrupt-load recovery (Error)

(ert-deftest test-session-log-quarantine-renames-corrupt-file ()
  "`org-drill--session-log-quarantine' renames the live persist file to a
timestamped `.corrupt-...' sibling so the next save doesn't overwrite it.
The suffix uses seconds granularity (YYYY-MM-DDTHHMMSS) so a same-day
re-quarantine doesn't clobber the earlier one."
  (let* ((tmp (make-temp-file "org-drill-session-log-test"))
         (renamed-to nil))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "garbage"))
          (cl-letf (((symbol-function 'persist--file-location)
                     (lambda (_sym) tmp))
                    ;; Capture the destination path so the cleanup branch
                    ;; can remove it regardless of the exact timestamp.
                    ((symbol-function 'rename-file)
                     (lambda (from to &rest _)
                       (setq renamed-to to)
                       ;; Honor the call so the side effect actually happens.
                       (copy-file from to t)
                       (delete-file from))))
            (org-drill--session-log-quarantine))
          (should-not (file-exists-p tmp))
          (should renamed-to)
          (should (file-exists-p renamed-to))
          ;; Match the seconds-granularity suffix shape.
          (should (string-match-p
                   "\\.corrupt-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{6\\}\\'"
                   renamed-to)))
      (ignore-errors (delete-file tmp))
      (when renamed-to (ignore-errors (delete-file renamed-to))))))

(ert-deftest test-session-log-quarantine-uses-seconds-in-suffix ()
  "The quarantine uses `format-time-string' with a seconds-granularity
format, so a same-day second corruption gets a distinct suffix.  This
test pins the format string itself so a regression to date-only would
fail loudly."
  (let* ((tmp (make-temp-file "org-drill-session-log-test"))
         (format-arg nil)
         (renamed-to nil))
    (unwind-protect
        (cl-letf (((symbol-function 'persist--file-location)
                   (lambda (_sym) tmp))
                  ((symbol-function 'format-time-string)
                   (lambda (fmt &rest _)
                     (setq format-arg fmt)
                     "STAMP"))
                  ((symbol-function 'rename-file)
                   (lambda (_from to &rest _)
                     (setq renamed-to to))))
          (with-temp-file tmp (insert "garbage"))
          (org-drill--session-log-quarantine)
          (should format-arg)
          ;; Format string must include hours/minutes/seconds, not just date.
          (should (string-match-p "%H" format-arg))
          (should (string-match-p "%M" format-arg))
          (should (string-match-p "%S" format-arg))
          (should (string-suffix-p ".corrupt-STAMP" renamed-to)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest test-session-log-quarantine-no-file-is-noop ()
  "Quarantine on a missing persist file is a quiet no-op (no error)."
  (let ((tmp (concat (make-temp-name "org-drill-no-such-file-") ".never")))
    (should-not (file-exists-p tmp))
    (cl-letf (((symbol-function 'persist--file-location)
               (lambda (_sym) tmp)))
      ;; Must not raise.
      (org-drill--session-log-quarantine))))

;;;; G. Hook integration

(ert-deftest test-show-end-message-records-on-normal-completion ()
  "A non-suspended session triggers `org-drill-record-session'."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session :qualities '(4 3 5)))
           (recorded nil))
      (oset session end-pos nil)
      (cl-letf (((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'save-some-buffers) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'org-drill-record-session)
                 (lambda (&rest _) (setq recorded t))))
        (org-drill--show-end-message session))
      (should recorded))))

(ert-deftest test-show-end-message-logs-when-recorder-errors ()
  "A recorder failure (struct shape, persist-save IO, etc.) must surface
via `message' rather than silently disappearing — no silent data loss."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session :qualities '(4)))
           (logged nil))
      (oset session end-pos nil)
      (cl-letf (((symbol-function 'org-drill-final-report) #'ignore)
                ((symbol-function 'save-some-buffers) #'ignore)
                ((symbol-function 'sit-for) #'ignore)
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   ;; Accumulate (newest first).  The flow after the recorder
                   ;; error also calls `(message "Drill session finished!")'
                   ;; and `(message nil)', so a single-binding capture would
                   ;; lose the failure message.  Nil FMT is the minibuffer-
                   ;; clear call and is ignored.
                   (when fmt (push (apply #'format fmt args) logged))))
                ((symbol-function 'org-drill-record-session)
                 (lambda (&rest _) (error "boom"))))
        (org-drill--show-end-message session))
      (should logged)
      (should (cl-some (lambda (m) (string-match-p "failed to record session" m))
                       logged)))))

(ert-deftest test-show-end-message-skips-record-on-suspend ()
  "A suspended session (end-pos set) must NOT record — discard semantics."
  (test-session-record--with-empty-log
    (let* ((session (test-session-record--populated-session :qualities '(4)))
           (recorded nil))
      (oset session end-pos (test-session-record--marker-at 1))
      (cl-letf (((symbol-function 'org-reveal) #'ignore)
                ((symbol-function 'org-fold-show-entry) #'ignore)
                ((symbol-function 'org-drill-goto-entry) #'ignore)
                ((symbol-function 'org-drill--show-resume-hint) #'ignore)
                ((symbol-function 'org-drill-record-session)
                 (lambda (&rest _) (setq recorded t))))
        (org-drill--show-end-message session))
      (should-not recorded))))

(provide 'test-org-drill-session-record)

;;; test-org-drill-session-record.el ends here
