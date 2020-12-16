(uiop:define-package #:advent-of-code/tests/day14
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day14/tests))

(in-package #:advent-of-code/tests/day14)

(def-suite day14/tests :in all-tests)
(in-suite day14/tests)

(defparameter *input1* (advent-of-code/day14:parse #p"tests/input/day14-1.txt"))
(defparameter *input2* (advent-of-code/day14:parse #p"tests/input/day14-2.txt"))

(test part1-acceptance
      (is (= 165 (advent-of-code/day14:part1 *input1*)))
      (is (= 51 (advent-of-code/day14:part1 *input2*))))

(test part2-acceptance
      (is (= 208 (advent-of-code/day14:part2 *input2*))))
