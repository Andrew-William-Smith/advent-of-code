(uiop:define-package #:advent-of-code/tests/day9
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day9/tests))

(in-package #:advent-of-code/tests/day9)

(def-suite day9/tests :in all-tests)
(in-suite day9/tests)

(defparameter *input* (advent-of-code/day9:parse #p"tests/input/day9.txt"))

(test part1-acceptance
      (is (= 127 (advent-of-code/day9:part1 *input* 5))))

(test part2-acceptance
      (is (= 62 (advent-of-code/day9:part2 *input* 5))))
