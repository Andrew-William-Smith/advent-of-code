(in-package #:advent-of-code-tests)

(def-suite all-tests
  :description "Test suite for all Advent of Code problems.")

(in-suite all-tests)

(defun test-all ()
  (run! 'all-tests))
