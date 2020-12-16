(uiop:define-package #:advent-of-code/tests/day3
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day3/tests))

(in-package #:advent-of-code/tests/day3)

(def-suite day3/tests :in all-tests)
(in-suite day3/tests)

(defparameter *input* (advent-of-code/day3:parse #p"tests/input/day3.txt"))

(test part1-acceptance
      (is (= 7 (advent-of-code/day3:part1 *input*))))

(test part2-acceptance
      (is (= 336 (advent-of-code/day3:part2 *input*))))
