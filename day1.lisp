(in-package #:advent-of-code)

(defun two-sum (hashtbl target)
  "Find the two numbers in HASHTBL whose sum is equal to TARGET."
  (loop for num being the hash-keys in hashtbl
        for complement = (- target num)
        when (gethash complement hashtbl)
          return (list num complement)))

(defun three-sum (hashtbl target)
  "Find the three numbers in HASHTBL whose sum is equal to TARGET."
  (loop for num being the hash-keys in hashtbl
        for remainder = (two-sum hashtbl (- target num))
        when remainder
          return (cons num remainder)))

(defun day1/parse (filename)
  (let ((input (make-hash-table)))
    (map-file filename
              [let ((int% (nth-value 0 (parse-integer %))))
                (setf (gethash int% input) int%)])
    input))

(defparameter *input* (day1/parse #p"input/day1.txt"))

(define-solution 1 1 (input) (*input*)
  "Find the product of two numbers whose sum is equal to 2020."
  (reduce #'* (two-sum input 2020)))

(define-solution 1 2 (input) (*input*)
  "Find the product of three numbers whose sum is equal to 2020."
  (reduce #'* (three-sum input 2020)))
