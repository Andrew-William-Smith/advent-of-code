(uiop:define-package #:advent-of-code/tests/day19
    (:use #:cl #:fiveam #:advent-of-code/tests/main)
  (:export #:day19/tests))

(in-package #:advent-of-code/tests/day19)

(def-suite day19/tests :in all-tests)
(in-suite day19/tests)

(test part1-acceptance
      (is (= 2 (advent-of-code/day19:part1 #p"tests/input/day19-1.txt")))
      (is (= 3 (advent-of-code/day19:part1 #p"tests/input/day19-2.txt"))))

(test part2-acceptance
      (is (= 12 (advent-of-code/day19:part2 #p"tests/input/day19-2.txt"))))
