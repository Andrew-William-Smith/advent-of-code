(in-package #:advent-of-code-tests)

(def-suite day12-tests :in all-tests)
(in-suite day12-tests)

(defparameter *day12/input* (day12/parse #p"tests/input/day12.txt"))

(test day12/part1-acceptance
      (is (= 25 (day12/part1 *day12/input*))))

(test day12/part2-acceptance
      (is (= 286 (day12/part2 *day12/input*))))
