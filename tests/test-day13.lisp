(in-package #:advent-of-code-tests)

(def-suite day13-tests :in all-tests)
(in-suite day13-tests)

(defparameter *day13/input1* (day13/parse #p"tests/input/day13-1.txt"))
(defparameter *day13/input2* (day13/parse #p"tests/input/day13-2.txt"))
(defparameter *day13/input3* (day13/parse #p"tests/input/day13-3.txt"))
(defparameter *day13/input4* (day13/parse #p"tests/input/day13-4.txt"))
(defparameter *day13/input5* (day13/parse #p"tests/input/day13-5.txt"))
(defparameter *day13/input6* (day13/parse #p"tests/input/day13-6.txt"))

(test day13/part1-acceptance
      (is (= 295 (day13/part1 *day13/input1*))))

(test day13/part2-acceptance
      (is (= 1068781 (day13/part2 *day13/input1*)))
      (is (= 3417 (day13/part2 *day13/input2*)))
      (is (= 754018 (day13/part2 *day13/input3*)))
      (is (= 779210 (day13/part2 *day13/input4*)))
      (is (= 1261476 (day13/part2 *day13/input5*)))
      (is (= 1202161486 (day13/part2 *day13/input6*))))
