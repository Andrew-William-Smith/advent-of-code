(in-package #:advent-of-code-tests)

(def-suite day10-tests :in all-tests)
(in-suite day10-tests)

(defparameter *day10/input1* (day10/parse #p"tests/input/day10-1.txt"))
(defparameter *day10/input2* (day10/parse #p"tests/input/day10-2.txt"))

(test day10/part1-acceptance
      (is (= 35 (day10/part1 *day10/input1*)))
      (is (= 220 (day10/part1 *day10/input2*))))

(test day10/part2-acceptance
      (is (= 8 (day10/part2 *day10/input1*)))
      (is (= 19208 (day10/part2 *day10/input2*))))
