(in-package #:advent-of-code-tests)

(def-suite day1-tests :in all-tests)
(in-suite day1-tests)

(defparameter *day1/input* (day1/parse #p"tests/input/day1.txt"))

(test day1/part1-acceptance
      (is (= 514579 (day1/part1 *day1/input*))))

(test day1/part2-acceptance
      (is (= 241861950 (day1/part2 *day1/input*))))
