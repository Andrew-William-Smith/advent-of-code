(in-package #:advent-of-code-tests)

(def-suite day4-tests :in all-tests)
(in-suite day4-tests)

(defparameter *day4/input1* (day4/parse #p"tests/input/day4-1.txt"))
(defparameter *day4/input2* (day4/parse #p"tests/input/day4-2.txt"))

(test day4/part1-acceptance
      (is (= 2 (day4/part1 *day4/input1*)))
      (is (= 8 (day4/part1 *day4/input2*))))

(test day4/part2-acceptance
      (is (= 2 (day4/part2 *day4/input1*)))
      (is (= 4 (day4/part2 *day4/input2*))))
