(in-package #:advent-of-code-tests)

(def-suite day8-tests :in all-tests)
(in-suite day8-tests)

(defparameter *day8/input* (day8/parse #p"tests/input/day8.txt"))

(test day8/part1-acceptance
      (is (= 5 (day8/part1 *day8/input*))))

(test day8/part2-acceptance
      (is (= 8 (day8/part2 *day8/input*))))
