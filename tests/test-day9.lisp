(in-package #:advent-of-code-tests)

(def-suite day9-tests :in all-tests)
(in-suite day9-tests)

(defparameter *day9/input* (day9/parse #p"tests/input/day9.txt"))

(test day9/part1-acceptance
      (is (= 127 (day9/part1 *day9/input* 5))))

(test day9/part2-acceptance
      (is (= 62 (day9/part2 *day9/input* 5))))
