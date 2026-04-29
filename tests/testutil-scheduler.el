;;; testutil-scheduler.el --- Shared extractors for scheduler tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Common result-list element extractors used by the SM2, SM5, and Simple8
;; scheduler test files.  Each algorithm's result list shares the same layout
;; for INTERVAL / REPEATS / FAILURES / MEAN / TOTAL-REPEATS.  Position 2 holds
;; either the EF (SM2, SM5) or the EASE (Simple8); both names are exposed as
;; aliases pointing at the same `nth' position so each call site reads
;; accurately.

;;; Code:

(defsubst test-scheduler--extract-interval (result)
  "Extract the next-interval (position 0) from a scheduler RESULT list."
  (nth 0 result))

(defsubst test-scheduler--extract-repeats (result)
  "Extract the repeats count (position 1) from a scheduler RESULT list."
  (nth 1 result))

(defsubst test-scheduler--extract-ef (result)
  "Extract the easiness factor (position 2) from an SM2 or SM5 RESULT list."
  (nth 2 result))

(defsubst test-scheduler--extract-ease (result)
  "Alias for `test-scheduler--extract-ef' (same `nth' position).
Use this name in Simple8 tests where the field is called `ease' not `ef'."
  (nth 2 result))

(defsubst test-scheduler--extract-failures (result)
  "Extract the failure count (position 3) from a scheduler RESULT list."
  (nth 3 result))

(defsubst test-scheduler--extract-meanq (result)
  "Extract the mean quality (position 4) from a scheduler RESULT list."
  (nth 4 result))

(defsubst test-scheduler--extract-total-repeats (result)
  "Extract the total repeats count (position 5) from a scheduler RESULT list."
  (nth 5 result))

(defsubst test-scheduler--extract-of-matrix (result)
  "Extract the optimal-factor matrix (position 6) from an SM5 RESULT list."
  (nth 6 result))

(provide 'testutil-scheduler)
;;; testutil-scheduler.el ends here
