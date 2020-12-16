(uiop:define-package #:advent-of-code/tests/day6
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day6/tests))

(in-package #:advent-of-code/tests/day6)

(def-suite day6/tests :in all-tests)
(in-suite day6/tests)

(defparameter *input* (advent-of-code/day6:parse #p"tests/input/day6.txt"))

(test part1-acceptance
      (is (= 11 (advent-of-code/day6:part1 *input*))))

(test part2-acceptance
      (is (= 6 (advent-of-code/day6:part2 *input*))))
