(in-package #:advent-of-code-tests)

(def-suite day6-tests :in all-tests)
(in-suite day6-tests)

(defparameter *day6/input* (day6/parse #p"tests/input/day6.txt"))

(test day6/part1-acceptance
      (is (= 11 (day6/part1 *day6/input*))))

(test day6/part2-acceptance
      (is (= 6 (day6/part2 *day6/input*))))
