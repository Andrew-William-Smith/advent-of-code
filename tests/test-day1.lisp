(in-package #:advent-of-code-tests)

(def-suite day1-tests :in all-tests)
(in-suite day1-tests)

(defparameter *input* (day1/parse #p"tests/input/day1.txt"))

(test part1-test
      (is (= 514579 (day1/part1 *input*))))

(test part2-test
      (is (= 241861950 (day1/part2 *input*))))
