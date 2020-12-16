(uiop:define-package #:advent-of-code/tests/day15
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day15/tests))

(in-package #:advent-of-code/tests/day15)

(def-suite day15/tests :in all-tests)
(in-suite day15/tests)

(defparameter *inputs*
  (loop for i from 1 to 7
        for path = (concatenate 'string "tests/input/day15-" (write-to-string i) ".txt")
        collect (advent-of-code/day15:parse path)))

(defparameter *results* '(436 1 10 27 78 438 1836))

(test part1-acceptance
      (loop for input in *inputs*
            for result in *results*
            do (is (= result (advent-of-code/day15:part1 input)))))
