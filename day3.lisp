(in-package #:advent-of-code)

;; Convert each line of the input to a bit vector describing whether each
;; terrain item is a tree.  Made incredibly concise courtesy of my new nested
;; shorthand lambdas!
(defun day3/parse (filename)
  (map-file filename [map-bit-vector [char= #\# %] %]))

(defparameter *input* (day3/parse #p"input/day3.txt"))

(defun 🎄 (line index)
  "Determine whether the terrain item at the specified INDEX on the specified
   LINE (as in *INPUT*) is a tree.  Lines are treated as circular buffers, in
   which navigating off of the right side will result in wrapping around to the
   left.  Output is returned as a bit."
  (bit line (mod index (length line))))

(defun count-trees (lines x Δx Δy)
  "Count the trees encountered at a (ΔX, ΔY) path in the current list of LINES,
   with a current horizontal position X."
  (if lines
      ;; There are lines remaining, so traverse downward.
      (+ (🎄 (car lines) x)
         (count-trees (nthcdr Δy lines) (+ x Δx) Δx Δy))
      ;; No lines remaining, so there can be no more trees.
      0))

(define-solution 3 1 (input) (*input*)
  "Determine how many trees will be encountered on a (+3, +1) path from the
   upper left-hand corner of the map, assuming that each line repeats
   indefinitely to the right."
  (count-trees input 0 3 1))

(define-solution 3 2 (input) (*input*)
  "Determine the product of the numbers of trees encountered on the specified
   paths from the upper left-hand corner of the map."
  (loop for (Δx Δy) in '((1 1) (3 1) (5 1) (7 1) (1 2))
        collect (count-trees input 0 Δx Δy) into counts
        finally (return (reduce #'* counts))))
