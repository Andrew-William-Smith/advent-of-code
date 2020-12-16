(uiop:define-package #:advent-of-code/tests/day1
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day1/tests))

(in-package #:advent-of-code/tests/day1)

(def-suite day1/tests :in all-tests)
(in-suite day1/tests)

(defparameter *input* (advent-of-code/day1:parse #p"tests/input/day1.txt"))

(test part1-acceptance
      (is (= 514579 (advent-of-code/day1:part1 *input*))))

(test part2-acceptance
      (is (= 241861950 (advent-of-code/day1:part2 *input*))))
