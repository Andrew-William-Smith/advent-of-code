(uiop:define-package #:advent-of-code/tests/day5
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day5/tests))

(in-package #:advent-of-code/tests/day5)

(def-suite day5/tests :in all-tests)
(in-suite day5/tests)

(defparameter *input* (advent-of-code/day5:parse #p"tests/input/day5.txt"))

(test part1-acceptance
      (is (= 820 (advent-of-code/day5:part1 *input*))))
