(uiop:define-package #:advent-of-code/tests/day7
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day7/tests))

(in-package #:advent-of-code/tests/day7)

(def-suite day7/tests :in all-tests)
(in-suite day7/tests)

(defparameter *input1* (advent-of-code/day7:parse #p"tests/input/day7-1.txt"))
(defparameter *input2* (advent-of-code/day7:parse #p"tests/input/day7-2.txt"))

(test part1-acceptance
      (is (= 4 (advent-of-code/day7:part1 *input1*)))
      (is (= 0 (advent-of-code/day7:part1 *input2*))))

(test part2-acceptance
      (is (= 32 (advent-of-code/day7:part2 *input1*)))
      (is (= 126 (advent-of-code/day7:part2 *input2*))))
