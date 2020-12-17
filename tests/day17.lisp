(uiop:define-package #:advent-of-code/tests/day17
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day17/tests))

(in-package #:advent-of-code/tests/day17)

(def-suite day17/tests :in all-tests)
(in-suite day17/tests)

(defparameter *input* (advent-of-code/day17:parse #p"tests/input/day17.txt"))

(test part1-acceptance
      (is (= 112 (advent-of-code/day17:part1 *input*))))

(test part2-acceptance
      (is (= 848 (advent-of-code/day17:part2 *input*))))
