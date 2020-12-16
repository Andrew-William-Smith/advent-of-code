(uiop:define-package #:advent-of-code/tests/day12
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day12/tests))

(in-package #:advent-of-code/tests/day12)

(def-suite day12/tests :in all-tests)
(in-suite day12/tests)

(defparameter *input* (advent-of-code/day12:parse #p"tests/input/day12.txt"))

(test part1-acceptance
      (is (= 25 (advent-of-code/day12:part1 *input*))))

(test part2-acceptance
      (is (= 286 (advent-of-code/day12:part2 *input*))))
