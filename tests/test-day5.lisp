(in-package #:advent-of-code-tests)

(def-suite day5-tests :in all-tests)
(in-suite day5-tests)

(defparameter *day5/input* (day5/parse #p"tests/input/day5.txt"))

(test day5/part1-acceptance
      (is (= 820 (day5/part1 *day5/input*))))
