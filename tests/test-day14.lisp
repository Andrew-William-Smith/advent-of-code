(in-package #:advent-of-code-tests)

(def-suite day14-tests :in all-tests)
(in-suite day14-tests)

(defparameter *day14/input1* (day14/parse #p"tests/input/day14-1.txt"))
(defparameter *day14/input2* (day14/parse #p"tests/input/day14-2.txt"))

(test day14/part1-acceptance
      (is (= 165 (day14/part1 *day14/input1*)))
      (is (= 51 (day14/part1 *day14/input2*))))

(test day14/part2-acceptance
      (is (= 205 (day14/part2 *day14/input2*))))
