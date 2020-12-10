(in-package #:advent-of-code-tests)

(def-suite day3-tests :in all-tests)
(in-suite day3-tests)

(defparameter *day3/input* (day3/parse #p"tests/input/day3.txt"))

(test day3/part1-acceptance
      (is (= 7 (day3/part1 *day3/input*))))

(test day3/part2-acceptance
      (is (= 336 (day3/part2 *day3/input*))))
