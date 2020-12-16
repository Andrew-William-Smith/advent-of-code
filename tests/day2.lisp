(uiop:define-package #:advent-of-code/tests/day2
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day2/tests))

(in-package #:advent-of-code/tests/day2)

(def-suite day2/tests :in all-tests)
(in-suite day2/tests)

(defparameter *input* (advent-of-code/day2:parse #p"tests/input/day2.txt"))

(test part1-acceptance
      (is (= 2 (advent-of-code/day2:part1 *input*))))

(test part2-acceptance
      (is (= 1 (advent-of-code/day2:part2 *input*))))
