(uiop:define-package #:advent-of-code/tests/day8
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day8/tests))

(in-package #:advent-of-code/tests/day8)

(def-suite day8/tests :in all-tests)
(in-suite day8/tests)

(defparameter *input* (advent-of-code/day8:parse #p"tests/input/day8.txt"))

(test part1-acceptance
      (is (= 5 (advent-of-code/day8:part1 *input*))))

(test part2-acceptance
      (is (= 8 (advent-of-code/day8:part2 *input*))))
