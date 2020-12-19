(uiop:define-package #:advent-of-code/tests/package
    (:use #:cl #:advent-of-code #:fiveam)
  (:nicknames #:advent-of-code/tests)
  (:use-reexport #:advent-of-code/tests/main)
  (:reexport #:advent-of-code/tests/day1
             #:advent-of-code/tests/day2
             #:advent-of-code/tests/day3
             #:advent-of-code/tests/day4
             #:advent-of-code/tests/day5
             #:advent-of-code/tests/day6
             #:advent-of-code/tests/day7
             #:advent-of-code/tests/day8
             #:advent-of-code/tests/day9
             #:advent-of-code/tests/day10
             #:advent-of-code/tests/day11
             #:advent-of-code/tests/day12
             #:advent-of-code/tests/day13
             #:advent-of-code/tests/day14
             #:advent-of-code/tests/day15
             #:advent-of-code/tests/day16
             #:advent-of-code/tests/day17
             #:advent-of-code/tests/day18
             #:advent-of-code/tests/day19))

(in-package #:advent-of-code/tests)
