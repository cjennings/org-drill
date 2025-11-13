;;; test-integration-drill-session-simple-workflow-integration-test.el --- Integration test for basic drill workflow

;;; Commentary:
;; Integration test for the basic org-drill workflow, testing how
;; components work together:
;;
;; 1. Entry detection (org-drill-entry-p)
;; 2. Entry enumeration (org-drill-map-entries)
;; 3. Data retrieval (org-drill-get-item-data)
;; 4. Scheduling algorithm (org-drill-determine-next-interval-sm2)
;; 5. Data storage (org-drill-store-item-data)
;;
;; This test uses actual org-mode buffers with drill entries to verify
;; the complete workflow from entry detection through scheduling.

;;; Code:

(require 'ert)
(require 'assess)
(require 'org-drill)

;;; Test Data

(defconst test-integration-simple-workflow-basic-entries
  "#+TITLE: Basic Drill Session Test

* First Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 4
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 5
:DRILL_FAILURE_COUNT: 1
:DRILL_AVERAGE_QUALITY: 3.8
:DRILL_EASE: 2.5
:END:

Question: What is 2+2?

Answer: 4

* Second Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 10
:DRILL_REPEATS_SINCE_FAIL: 4
:DRILL_TOTAL_REPEATS: 4
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.5
:DRILL_EASE: 2.6
:END:

Question: What is the capital of France?

Answer: Paris

* Not a drill card

This heading has no drill tag, so it should be ignored.

* Third Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 1
:DRILL_REPEATS_SINCE_FAIL: 1
:DRILL_TOTAL_REPEATS: 3
:DRILL_FAILURE_COUNT: 2
:DRILL_AVERAGE_QUALITY: 2.3
:DRILL_EASE: 2.0
:END:

Question: What is the largest planet?

Answer: Jupiter
"
  "Basic drill entries with varying scheduling data.")

(defconst test-integration-simple-workflow-new-entries
  "#+TITLE: New Drill Entries Test

* Brand New Card :drill:

Question: What is Emacs?

Answer: A powerful text editor.

* Another New Card :drill:

Question: What is org-mode?

Answer: An Emacs mode for note-taking and organization.
"
  "New drill entries with no scheduling data.")

;;; Helper Functions

(defun test-integration-simple-workflow--with-drill-buffer (content callback)
  "Execute CALLBACK in temporary org-mode buffer with drill CONTENT."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (goto-char (point-min))
    (funcall callback)))

(defun test-integration-simple-workflow--count-drill-entries (content)
  "Count number of drill entries in CONTENT."
  (test-integration-simple-workflow--with-drill-buffer
   content
   (lambda ()
     (length (org-drill-map-entries (lambda () (point)) 'file nil)))))

;;; Normal Cases - Entry Detection

(ert-deftest test-integration-simple-workflow-normal-detect-multiple-entries ()
  "Test that multiple drill entries are detected in a file.
Should find exactly 3 drill entries, ignoring non-drill headings."
  (let ((count (test-integration-simple-workflow--count-drill-entries
                test-integration-simple-workflow-basic-entries)))
    (should (= count 3))))

(ert-deftest test-integration-simple-workflow-normal-detect-new-entries ()
  "Test detection of new drill entries without scheduling data."
  (let ((count (test-integration-simple-workflow--count-drill-entries
                test-integration-simple-workflow-new-entries)))
    (should (= count 2))))

;;; Normal Cases - Entry Data Retrieval

(ert-deftest test-integration-simple-workflow-normal-retrieve-entry-data ()
  "Test retrieving scheduling data from drill entry.
Should correctly parse all DRILL_* properties."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Find first drill entry
     (re-search-forward "^\\* First Card :drill:")
     (beginning-of-line)
     (let ((data (org-drill-get-item-data)))
       ;; Verify structure: (last-interval repeats failures total-repeats meanq ease)
       (should (listp data))
       (should (= (length data) 6))
       ;; Verify values match properties
       (should (= (nth 0 data) 4))   ; DRILL_LAST_INTERVAL
       (should (= (nth 1 data) 2))   ; DRILL_REPEATS_SINCE_FAIL
       (should (= (nth 2 data) 1))   ; DRILL_FAILURE_COUNT
       (should (= (nth 3 data) 5))   ; DRILL_TOTAL_REPEATS
       (should (= (nth 4 data) 3.8)) ; DRILL_AVERAGE_QUALITY
       (should (= (nth 5 data) 2.5)) ; DRILL_EASE
       ))))

(ert-deftest test-integration-simple-workflow-normal-retrieve-new-entry-data ()
  "Test retrieving data from new entry with no properties.
Should return default values for new items."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-new-entries
   (lambda ()
     ;; Find first drill entry
     (re-search-forward "^\\* Brand New Card :drill:")
     (beginning-of-line)
     (let ((data (org-drill-get-item-data)))
       ;; Verify structure exists
       (should (listp data))
       (should (= (length data) 6))
       ;; New items should have sensible defaults (or nil)
       (should (or (null (nth 0 data)) (numberp (nth 0 data)))) ; last-interval
       (should (numberp (nth 1 data))) ; repeats
       (should (numberp (nth 2 data))) ; failures
       (should (numberp (nth 3 data))) ; total-repeats
       ;; meanq (nth 4) can be nil for new items
       ;; ease (nth 5) can be nil for new items
       ))))

;;; Normal Cases - Scheduling Algorithm Integration

(ert-deftest test-integration-simple-workflow-normal-schedule-from-entry-data ()
  "Test that entry data can be fed to scheduling algorithm.
Verifies integration between data retrieval and SM2 algorithm."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Find second drill entry (good performance history)
     (re-search-forward "^\\* Second Card :drill:")
     (beginning-of-line)
     (cl-destructuring-bind (last-interval repeats failures total-repeats meanq ease)
         (org-drill-get-item-data)
       ;; Simulate quality rating of 4 (good recall)
       (let* ((quality 4)
              (result (org-drill-determine-next-interval-sm2
                       last-interval repeats ease quality
                       failures meanq total-repeats))
              (next-interval (nth 0 result))
              (new-repeats (nth 1 result))
              (new-ease (nth 2 result)))
         ;; Verify scheduling result makes sense
         (should (> next-interval last-interval)) ; Interval should increase
         (should (= new-repeats (1+ repeats)))    ; Repeats incremented
         (should (numberp new-ease))              ; EF calculated
         )))))

(ert-deftest test-integration-simple-workflow-normal-schedule-failed-recall ()
  "Test scheduling when card is failed.
Verifies that failure handling works correctly in integrated workflow."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Find third drill entry (struggling item)
     (re-search-forward "^\\* Third Card :drill:")
     (beginning-of-line)
     (cl-destructuring-bind (last-interval repeats failures total-repeats meanq ease)
         (org-drill-get-item-data)
       ;; Simulate complete failure (quality 0)
       (let* ((quality 0)
              (result (org-drill-determine-next-interval-sm2
                       last-interval repeats ease quality
                       failures meanq total-repeats))
              (next-interval (nth 0 result))
              (new-repeats (nth 1 result))
              (new-failures (nth 3 result)))
         ;; Verify failure handling
         (should (= next-interval -1))              ; Failed cards get -1
         (should (= new-repeats 1))                 ; Repeats reset to 1
         (should (= new-failures (1+ failures)))))))) ; Failure count incremented

;;; Normal Cases - Data Storage Integration

(ert-deftest test-integration-simple-workflow-normal-store-item-data ()
  "Test that scheduling results can be stored back to entry.
Verifies org-drill-store-item-data updates properties correctly."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Find first drill entry
     (re-search-forward "^\\* First Card :drill:")
     (beginning-of-line)

     ;; Get original data
     (cl-destructuring-bind (last-interval repeats failures total-repeats meanq ease)
         (org-drill-get-item-data)

       ;; Calculate new scheduling data
       (let* ((quality 5) ; Perfect recall
              (result (org-drill-determine-next-interval-sm2
                       last-interval repeats ease quality
                       failures meanq total-repeats))
              (next-interval (nth 0 result))
              (new-repeats (nth 1 result))
              (new-ease (nth 2 result))
              (new-failures (nth 3 result))
              (new-meanq (nth 4 result))
              (new-total (nth 5 result)))

         ;; Store new data
         (org-drill-store-item-data next-interval new-repeats new-failures
                                    new-total new-meanq new-ease)

         ;; Verify data was stored (properties exist and are valid)
         (should (org-entry-get (point) "DRILL_LAST_INTERVAL"))
         (should (org-entry-get (point) "DRILL_REPEATS_SINCE_FAIL"))
         (should (org-entry-get (point) "DRILL_FAILURE_COUNT"))
         (should (org-entry-get (point) "DRILL_TOTAL_REPEATS"))
         (should (org-entry-get (point) "DRILL_AVERAGE_QUALITY"))
         (should (org-entry-get (point) "DRILL_EASE"))

         ;; Verify values are numeric and match expectations
         (should (= (string-to-number (org-entry-get (point) "DRILL_REPEATS_SINCE_FAIL"))
                    new-repeats))
         (should (= (string-to-number (org-entry-get (point) "DRILL_FAILURE_COUNT"))
                    new-failures))
         (should (= (string-to-number (org-entry-get (point) "DRILL_TOTAL_REPEATS"))
                    new-total))
         )))))

;;; Boundary Cases - Edge Conditions

(ert-deftest test-integration-simple-workflow-boundary-empty-file ()
  "Test drill entry detection in empty file.
Should handle empty files gracefully."
  (let ((count (test-integration-simple-workflow--count-drill-entries "")))
    (should (= count 0))))

(ert-deftest test-integration-simple-workflow-boundary-no-drill-entries ()
  "Test file with headings but no drill tags."
  (let* ((content "* Heading One\n\nContent.\n\n* Heading Two\n\nMore content.\n")
         (count (test-integration-simple-workflow--count-drill-entries content)))
    (should (= count 0))))

(ert-deftest test-integration-simple-workflow-boundary-mixed-drill-and-non-drill ()
  "Test that drill and non-drill entries are correctly distinguished."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     (let ((drill-count 0)
           (non-drill-count 0))
       ;; Count drill entries
       (goto-char (point-min))
       (while (re-search-forward "^\\* " nil t)
         (beginning-of-line)
         (if (org-drill-entry-p)
             (cl-incf drill-count)
           (cl-incf non-drill-count))
         (forward-line))
       (should (= drill-count 3))
       (should (= non-drill-count 1))))))

;;; Integration - Complete Workflow Simulation

(ert-deftest test-integration-simple-workflow-integration-complete-review-cycle ()
  "Test complete review cycle: detect -> retrieve -> schedule -> store.
Simulates reviewing a card and verifies all components work together."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Step 1: Detect drill entries
     (let ((entries (org-drill-map-entries (lambda () (point)) 'file nil)))
       (should (= (length entries) 3))

       ;; Step 2: Navigate to first entry
       (goto-char (car entries))
       (should (org-drill-entry-p))

       ;; Step 3: Retrieve current data
       (cl-destructuring-bind (last-interval repeats failures total-repeats meanq ease)
           (org-drill-get-item-data)
         (should (numberp last-interval))
         (should (numberp repeats))

         ;; Step 4: Simulate review with quality 4
         (let* ((quality 4)
                (result (org-drill-determine-next-interval-sm2
                         last-interval repeats ease quality
                         failures meanq total-repeats))
                (next-interval (nth 0 result))
                (new-repeats (nth 1 result))
                (new-ease (nth 2 result))
                (new-failures (nth 3 result))
                (new-meanq (nth 4 result))
                (new-total (nth 5 result)))

           ;; Step 5: Store results
           (org-drill-store-item-data next-interval new-repeats new-failures
                                      new-total new-meanq new-ease)

           ;; Step 6: Verify data persisted
           (let ((retrieved-data (org-drill-get-item-data)))
             (should (= (nth 0 retrieved-data) (floor next-interval)))
             (should (= (nth 1 retrieved-data) new-repeats))
             (should (= (nth 2 retrieved-data) new-failures))
             (should (= (nth 3 retrieved-data) new-total)))))))))

;;; Error Cases - Data Corruption and Malformation

(ert-deftest test-integration-simple-workflow-error-invalid-property-values ()
  "Test behavior with invalid (non-numeric) property values.
Should handle gracefully or provide sensible defaults."
  (let ((content "* Corrupted Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: invalid_number
:DRILL_REPEATS_SINCE_FAIL: abc
:DRILL_TOTAL_REPEATS: xyz
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: not_a_float
:DRILL_EASE: 2.5
:END:

Question content.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Corrupted Card :drill:")
       (beginning-of-line)
       ;; Should not error when retrieving data with invalid values
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         ;; Verify function handles corruption gracefully
         (should (= (length data) 6)))))))

(ert-deftest test-integration-simple-workflow-error-missing-properties ()
  "Test behavior with completely missing drill properties.
Should provide sensible defaults for new/uninitialized cards."
  (let ((content "* Card Without Properties :drill:

Question: Test question?
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Card Without Properties :drill:")
       (beginning-of-line)
       ;; Should not error, provide defaults
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         (should (= (length data) 6)))))))

(ert-deftest test-integration-simple-workflow-error-negative-property-values ()
  "Test behavior with negative values in properties.
Negative values should be handled gracefully or sanitized."
  (let ((content "* Negative Values Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: -5
:DRILL_REPEATS_SINCE_FAIL: -1
:DRILL_TOTAL_REPEATS: -10
:DRILL_FAILURE_COUNT: -3
:DRILL_AVERAGE_QUALITY: -2.5
:DRILL_EASE: -1.0
:END:

Question content.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Negative Values Card :drill:")
       (beginning-of-line)
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         ;; Verify system doesn't crash with negative values
         (should (= (length data) 6)))))))

(ert-deftest test-integration-simple-workflow-error-extremely-large-values ()
  "Test behavior with extremely large property values.
Should handle large numbers without overflow or crash."
  (let ((content "* Large Values Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 999999999
:DRILL_REPEATS_SINCE_FAIL: 1000000
:DRILL_TOTAL_REPEATS: 9999999
:DRILL_FAILURE_COUNT: 500000
:DRILL_AVERAGE_QUALITY: 5.0
:DRILL_EASE: 10.0
:END:

Question content.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Large Values Card :drill:")
       (beginning-of-line)
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         (should (= (length data) 6))
         ;; Verify scheduling can handle extreme values
         (cl-destructuring-bind (last-interval repeats failures total-repeats meanq ease)
             data
           (let ((result (org-drill-determine-next-interval-sm2
                          last-interval repeats ease 4
                          failures meanq total-repeats)))
             (should (listp result)))))))))

;;; Error Cases - Workflow Error Handling

(ert-deftest test-integration-simple-workflow-error-retrieve-data-not-at-heading ()
  "Test retrieving data when point is not at a heading.
Should handle gracefully without crashing."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Move to body text, not a heading
     (re-search-forward "Question: What is 2\\+2\\?")
     (beginning-of-line)
     ;; Should not crash, may return nil or defaults
     (let ((data (org-drill-get-item-data)))
       ;; Just verify it doesn't crash
       (should (or (null data) (listp data)))))))

(ert-deftest test-integration-simple-workflow-error-retrieve-data-at-non-drill-heading ()
  "Test retrieving data from heading without drill tag.
Should handle non-drill headings gracefully."
  (test-integration-simple-workflow--with-drill-buffer
   test-integration-simple-workflow-basic-entries
   (lambda ()
     ;; Find non-drill heading
     (re-search-forward "^\\* Not a drill card")
     (beginning-of-line)
     (should-not (org-drill-entry-p))
     ;; Retrieving data from non-drill entry
     (let ((data (org-drill-get-item-data)))
       ;; Should not crash
       (should (or (null data) (listp data)))))))

(ert-deftest test-integration-simple-workflow-error-nested-drill-entries ()
  "Test handling of nested drill entries (parent and child both tagged).
Should detect both as separate drill entries."
  (let ((content "* Parent Drill Entry :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 5
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 3
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:END:

Parent question.

** Child Drill Entry :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 3
:DRILL_REPEATS_SINCE_FAIL: 1
:DRILL_TOTAL_REPEATS: 2
:DRILL_FAILURE_COUNT: 1
:DRILL_AVERAGE_QUALITY: 3.5
:DRILL_EASE: 2.3
:END:

Child question.
"))
    (let ((count (test-integration-simple-workflow--count-drill-entries content)))
      ;; Should find both parent and child as separate entries
      (should (= count 2)))))

(ert-deftest test-integration-simple-workflow-error-very-deep-nesting ()
  "Test drill entries at very deep nesting levels.
Should handle deep heading structures without issues."
  (let ((content "* Level 1
** Level 2
*** Level 3
**** Level 4
***** Level 5
****** Level 6 :drill:

Deep question.
"))
    (let ((count (test-integration-simple-workflow--count-drill-entries content)))
      (should (= count 1)))))

;;; Error Cases - Robustness

(ert-deftest test-integration-simple-workflow-error-unicode-in-properties ()
  "Test handling of Unicode characters in drill properties.
Should handle or reject Unicode in numeric properties gracefully."
  (let ((content "* Unicode Property Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 5
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 3
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:END:

Question with Unicode: 日本語 Café.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Unicode Property Card :drill:")
       (beginning-of-line)
       ;; Should handle Unicode in content without issues
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         (should (= (length data) 6)))))))

(ert-deftest test-integration-simple-workflow-error-special-chars-in-heading ()
  "Test drill entries with special characters in heading.
Special chars should not interfere with entry detection or data handling."
  (let ((content "* Card @#$%^&*() Special :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 5
:DRILL_REPEATS_SINCE_FAIL: 2
:DRILL_TOTAL_REPEATS: 3
:DRILL_FAILURE_COUNT: 0
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:END:

Question.
"))
    (let ((count (test-integration-simple-workflow--count-drill-entries content)))
      (should (= count 1)))))

(ert-deftest test-integration-simple-workflow-error-empty-properties-drawer ()
  "Test drill entry with empty PROPERTIES drawer.
Should handle empty drawer gracefully."
  (let ((content "* Empty Drawer Card :drill:
:PROPERTIES:
:END:

Question.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Empty Drawer Card :drill:")
       (beginning-of-line)
       (let ((data (org-drill-get-item-data)))
         ;; Should provide defaults for missing properties
         (should (listp data))
         (should (= (length data) 6)))))))

(ert-deftest test-integration-simple-workflow-error-float-in-integer-property ()
  "Test behavior with float values in expected-integer properties.
System should handle type coercion or rounding."
  (let ((content "* Float Values Card :drill:
:PROPERTIES:
:DRILL_LAST_INTERVAL: 5.7
:DRILL_REPEATS_SINCE_FAIL: 2.3
:DRILL_TOTAL_REPEATS: 3.9
:DRILL_FAILURE_COUNT: 0.5
:DRILL_AVERAGE_QUALITY: 4.0
:DRILL_EASE: 2.5
:END:

Question.
"))
    (test-integration-simple-workflow--with-drill-buffer
     content
     (lambda ()
       (re-search-forward "^\\* Float Values Card :drill:")
       (beginning-of-line)
       (let ((data (org-drill-get-item-data)))
         (should (listp data))
         ;; Verify system handles float->int conversion
         (should (numberp (nth 0 data)))
         (should (numberp (nth 1 data))))))))

(provide 'test-integration-drill-session-simple-workflow-integration-test)
;;; test-integration-drill-session-simple-workflow-integration-test.el ends here
