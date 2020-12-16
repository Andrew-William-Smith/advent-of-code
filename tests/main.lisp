(uiop:define-package #:advent-of-code/tests/main
    (:use #:cl #:fiveam)
  (:export #:all-tests #:test-all))

(in-package #:advent-of-code/tests/main)

(def-suite all-tests
  :description "Test suite for all Advent of Code problems.")

(in-suite all-tests)

(defun test-all ()
  (run! 'all-tests))
