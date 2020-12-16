(uiop:define-package #:advent-of-code/tests/day10
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day10/tests))

(in-package #:advent-of-code/tests/day10)

(def-suite day10/tests :in all-tests)
(in-suite day10/tests)

(defparameter *input1* (advent-of-code/day10:parse #p"tests/input/day10-1.txt"))
(defparameter *input2* (advent-of-code/day10:parse #p"tests/input/day10-2.txt"))

(test part1-acceptance
      (is (= 35 (advent-of-code/day10:part1 *input1*)))
      (is (= 220 (advent-of-code/day10:part1 *input2*))))

(test part2-acceptance
      (is (= 8 (advent-of-code/day10:part2 *input1*)))
      (is (= 19208 (advent-of-code/day10:part2 *input2*))))
