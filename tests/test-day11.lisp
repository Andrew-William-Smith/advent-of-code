(in-package #:advent-of-code-tests)

(def-suite day11-tests :in all-tests)
(in-suite day11-tests)

(defparameter *day11/input* (day11/parse #p"tests/input/day11.txt"))

(test day11/part1-acceptance
      (is (= 37 (day11/part1 *day11/input*))))

(test day11/part2-acceptance
      (is (= 26 (day11/part2 *day11/input*))))
