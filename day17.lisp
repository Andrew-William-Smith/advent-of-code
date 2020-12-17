(uiop:define-package #:advent-of-code/day17
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day17)

(defun parse (filename)
  (map-file filename [map 'list [char= #\# %] %]))

(defun make-conway-cube (dimensions cells steps)
  "Create a Conway cube of the specified number of DIMENSIONS beginning with the
   specified central 2-dimensional grid of CELLS and able to expand outward to
   accommodate the specified number of STEPS."
  (loop with offset = (1+ steps)
        with padding = (* 2 offset)
        with pad-dim = (make-list (- dimensions 2) :initial-element (1+ padding))
        with pad-idx = (make-list (- dimensions 2) :initial-element offset)
        with cube = (make-array `(,@pad-dim
                                  ,(+ (length cells) padding)
                                  ,(+ (length (car cells)) padding))
                                :initial-element nil)
        for row in cells and y from offset
        do (loop for cell in row and x from offset
                 do (setf (apply #'aref cube `(,@pad-idx ,y ,x)) cell))
        finally (return cube)))

(defun neighbour-offsets (cube)
  "Return a list of the offsets of the neighbours of a cell in the specified
   Conway CUBE."
  (apply #'map-product 'list (make-list (array-rank cube)
                                        :initial-element '(-1 0 1))))

(defun next-state (cube position offsets)
  "Return the state of the cell at the specified POSITION in the specified CUBE
   in the next iteration of the Conway cube automaton."
  (let ((old-cell (apply #'aref cube position))
        (neighbours (loop for offset in offsets
                          for indices = (mapcar #'+ position offset)
                          when (not (every #'zerop offset))
                            count (apply #'aref cube indices))))
    (cond
      ((and old-cell (/= neighbours 2) (/= neighbours 3)) nil)
      ((and (not old-cell) (= neighbours 3)) t)
      (t old-cell))))

(defun next-dimension (cube next dimensions coordinate offsets)
  "Update the state of the NEXT iteration of the specified Conway CUBE for the
   remaining DIMENSIONS at the specified COORDINATE.  Note that the list of
   DIMENSIONS must be reversed from the proper axes of the cube."
  (if (emptyp dimensions)
      (setf (apply #'aref next coordinate) (next-state cube coordinate offsets))
      (loop for i from 1 below (1- (car dimensions))
            do (next-dimension cube next (cdr dimensions) (cons i coordinate)
                               offsets))))

(defun next-cube (cube)
  "Return the state of the specified Conway CUBE in the next iteration of the
   automaton."
  (let ((next (make-array (array-dimensions cube) :initial-element nil))
        (offsets (neighbour-offsets cube)))
    (next-dimension cube next (reverse (array-dimensions cube)) nil offsets)
    next))

(defun count-active (cube &optional (axis 0) coordinate)
  "Count the number of active cells in the specified Conway CUBE."
  (if (= axis (array-rank cube))
      (boolean-to-bit (apply #'aref cube coordinate))
      (loop for i below (array-dimension cube axis)
            sum (count-active cube (1+ axis) `(,@coordinate ,i)))))

(define-solution 1 (input) ((parse #p"input/day17.txt"))
  (loop repeat 7
        for cube = (make-conway-cube 3 input 6) then (next-cube cube)
        finally (return (count-active cube))))

(define-solution 2 (input) ((parse #p"input/day17.txt"))
  (loop repeat 7
        for cube = (make-conway-cube 4 input 6) then (next-cube cube)
        finally (return (count-active cube))))
