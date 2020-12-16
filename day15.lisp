(uiop:define-package #:advent-of-code/day15
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day15)

(defun parse (filename)
  (with-open-file (in (file-in-system filename))
    (mapcar #'parse-integer (str:split "," (read-line in)))))

(defun elf-game (starting iterations)
  "Return the value said after the specified number of ITERATIONS of the elves'
   memory game, beginning with the specified STARTING sequence."
  (let* ((last-start (last-elt starting))
         (last-occurrences (make-array (max (1+ last-start) iterations))))
    ;; Pre-populate the array of occurrences with the starting values.
    (loop for val in starting
          for i from 1
          do (setf (aref last-occurrences val) i))
    ;; Run the sequence for the requested number of iterations after the start.
    (loop for turn from (1+ (length starting)) to iterations
          for last = last-start then current
          for last-turn = 0 then (aref last-occurrences last)
          for current = (if (zerop last-turn) 0 (- turn last-turn 1))
          do (setf (aref last-occurrences last) (1- turn))
          finally (return current))))

(define-solution 1 (input) ((parse #p"input/day15.txt"))
  "Return the number spoken in the 2020th iteration of the elves' memory game."
  (elf-game input 2020))

(define-solution 2 (input) ((parse #p"input/day15.txt"))
  "Return the number spoken in the 30,000,000th iteration of the elves' memory
   game."
  (elf-game input 30000000))
