(uiop:define-package #:advent-of-code/tests/day13
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day13/tests))

(in-package #:advent-of-code/tests/day13)

(def-suite day13/tests :in all-tests)
(in-suite day13/tests)

(defparameter *input1* (advent-of-code/day13:parse #p"tests/input/day13-1.txt"))
(defparameter *input2* (advent-of-code/day13:parse #p"tests/input/day13-2.txt"))
(defparameter *input3* (advent-of-code/day13:parse #p"tests/input/day13-3.txt"))
(defparameter *input4* (advent-of-code/day13:parse #p"tests/input/day13-4.txt"))
(defparameter *input5* (advent-of-code/day13:parse #p"tests/input/day13-5.txt"))
(defparameter *input6* (advent-of-code/day13:parse #p"tests/input/day13-6.txt"))

(test part1-acceptance
      (is (= 295 (advent-of-code/day13:part1 *input1*))))

(test part2-acceptance
      (is (= 1068781 (advent-of-code/day13:part2 *input1*)))
      (is (= 3417 (advent-of-code/day13:part2 *input2*)))
      (is (= 754018 (advent-of-code/day13:part2 *input3*)))
      (is (= 779210 (advent-of-code/day13:part2 *input4*)))
      (is (= 1261476 (advent-of-code/day13:part2 *input5*)))
      (is (= 1202161486 (advent-of-code/day13:part2 *input6*))))
