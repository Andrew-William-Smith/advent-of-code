(uiop:define-package #:advent-of-code/tests/day4
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day4/tests))

(in-package #:advent-of-code/tests/day4)

(def-suite day4/tests :in all-tests)
(in-suite day4/tests)

(defparameter *input1* (advent-of-code/day4:parse #p"tests/input/day4-1.txt"))
(defparameter *input2* (advent-of-code/day4:parse #p"tests/input/day4-2.txt"))

(test part1-acceptance
      (is (= 2 (advent-of-code/day4:part1 *input1*)))
      (is (= 8 (advent-of-code/day4:part1 *input2*))))

(test part2-acceptance
      (is (= 2 (advent-of-code/day4:part2 *input1*)))
      (is (= 4 (advent-of-code/day4:part2 *input2*))))
