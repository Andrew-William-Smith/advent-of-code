(in-package #:advent-of-code-tests)

(def-suite day2-tests :in all-tests)
(in-suite day2-tests)

(defparameter *day2/input* (day2/parse #p"tests/input/day2.txt"))

(test day2/part1-acceptance
      (is (= 2 (day2/part1 *day2/input*))))

(test day2/part2-acceptance
      (is (= 1 (day2/part2 *day2/input*))))
