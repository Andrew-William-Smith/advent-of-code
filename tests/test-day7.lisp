(in-package #:advent-of-code-tests)

(def-suite day7-tests :in all-tests)
(in-suite day7-tests)

(defparameter *day7/input1* (day7/parse #p"tests/input/day7-1.txt"))
(defparameter *day7/input2* (day7/parse #p"tests/input/day7-2.txt"))

(test day7/part1-acceptance
      (is (= 4 (day7/part1 *day7/input1*)))
      (is (= 0 (day7/part1 *day7/input2*))))

(test day7/part2-acceptance
      (is (= 32 (day7/part2 *day7/input1*)))
      (is (= 126 (day7/part2 *day7/input2*))))
