(load "common.lisp")

(defparameter *input* (make-hash-table))
(map-file #p"input/day1.txt"
          #'(lambda (v)
              (let ((int-v (nth-value 0 (parse-integer v))))
                (setf (gethash int-v *input*) int-v))))

(defun two-sum (target)
  "Find the two numbers in *INPUT* whose sum is equal to TARGET."
  (loop for num being the hash-keys in *input*
        for complement = (- target num)
        when (gethash complement *input*)
          return (list num complement)))

(defun three-sum (target)
  "Find the three numbers in *INPUT* whose sum is equal to TARGET."
  (loop for num being the hash-keys in *input*
        for remainder = (two-sum (- target num))
        when remainder
          return (cons num remainder)))

;; Part 1: Find the product of two numbers whose sum is equal to 2020.
(let* ((answer (two-sum 2020))
       (n1 (first answer))
       (n2 (second answer)))
  (format t "Part 1: ~d * ~d = ~d~%" n1 n2 (* n1 n2)))

;; Part 2: Find the product of three numbers whose sum is equal to 2020.
(let ((answer (three-sum 2020)))
  (format t "Part 2: ~{~d~^ * ~} = ~d~%" answer (reduce #'* answer)))
