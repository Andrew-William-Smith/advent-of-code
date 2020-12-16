(uiop:define-package #:advent-of-code/day1
    (:use #:cl #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day1)

(defun parse (filename)
  (let ((input (make-hash-table)))
    (map-file filename
              [let ((int% (nth-value 0 (parse-integer %))))
                (setf (gethash int% input) int%)])
    input))

(defun two-sum (hashtbl target)
  "Find the two numbers in HASHTBL whose sum is equal to TARGET."
  (loop for num being the hash-keys in hashtbl
        for complement = (- target num)
        when (gethash complement hashtbl)
          return (list num complement)))

(define-solution 1 (input) ((parse #p"input/day1.txt"))
  "Find the product of two numbers whose sum is equal to 2020."
  (reduce #'* (two-sum input 2020)))

(defun three-sum (hashtbl target)
  "Find the three numbers in HASHTBL whose sum is equal to TARGET."
  (loop for num being the hash-keys in hashtbl
        for remainder = (two-sum hashtbl (- target num))
        when remainder
          return (cons num remainder)))

(define-solution 2 (input) ((parse #p"input/day1.txt"))
  "Find the product of three numbers whose sum is equal to 2020."
  (reduce #'* (three-sum input 2020)))
