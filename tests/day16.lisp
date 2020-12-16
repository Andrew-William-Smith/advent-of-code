(uiop:define-package #:advent-of-code/tests/day16
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day16/tests))

(in-package #:advent-of-code/tests/day16)

(def-suite day16/tests :in all-tests)
(in-suite day16/tests)

(defparameter *input1* (advent-of-code/day16:parse #p"tests/input/day16-1.txt"))
(defparameter *input2* (advent-of-code/day16:parse #p"tests/input/day16-2.txt"))

(test part1-acceptance
      (is (= 71 (advent-of-code/day16:part1 *input1*)))
      (is (= 23044 (advent-of-code/day16:part1 *input2*))))

(test part2-acceptance
      (is (= 3765150732757 (advent-of-code/day16:part2 *input2*))))
