(uiop:define-package #:advent-of-code/tests/day11
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day11/tests))

(in-package #:advent-of-code/tests/day11)

(def-suite day11/tests :in all-tests)
(in-suite day11/tests)

(defparameter *input* (advent-of-code/day11:parse #p"tests/input/day11.txt"))

(test part1-acceptance
      (is (= 37 (advent-of-code/day11:part1 *input*))))

(test part2-acceptance
      (is (= 26 (advent-of-code/day11:part2 *input*))))
