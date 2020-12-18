(uiop:define-package #:advent-of-code/tests/day18
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day18/tests))

(in-package #:advent-of-code/tests/day18)

(def-suite day18/tests :in all-tests)
(in-suite day18/tests)

(defparameter *input* (advent-of-code/day18:parse #p"tests/input/day18.txt"))

(test part1-acceptance
      (is (= 26457 (advent-of-code/day18:part1 *input*))))

(test part2-acceptance
      (is (= 694173 (advent-of-code/day18:part2 *input*))))
