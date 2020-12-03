(load "common.lisp")

(defun find-trees (line)
  "Convert the specified LINE to a boolean list describing whether each space is
   a tree (#)."
  (map 'list #'(lambda (c) (char= c #\#)) line))

(defparameter *input* (map-file #p"input/day3.txt" #'find-trees))

(defun treep (pos line)
  "Determine whether the terrain item at the specified INDEX on the specified
   LINE (as in *INPUT*) is a tree.  Lines are treated as circular buffers, in
   which navigating off of the right side will result in wrapping around to the
   left."
  (nth (mod pos (length line)) line))

(defun count-trees (lines x Δx Δy)
  "Count the trees encountered at a (ΔX, ΔY) path in the current list of LINES,
   with a current horizontal position X."
  (if lines
      ;; There are lines remaining, so traverse downward.
      (let ((trees-below (count-trees (nthcdr Δy lines) (+ x Δx) Δx Δy)))
        (if (treep x (car lines))
            (1+ trees-below)
            trees-below))
      ;; No lines remaining, so there can be no more trees.
      0))

;; Part 1: Determine how many trees will be encountered on a (+3, +1) path from
;;         the upper left-hand corner of the map, assuming that each line
;;         repeats infinitely to the right.
(format t "Part 1: ~d trees encountered~%" (count-trees *input* 0 3 1))

;; Part 2: Determine the product of the numbers of trees encountered on the
;;         specified paths from the upper left-hand corner of the map.
(defparameter *paths* '((1 1) (3 1) (5 1) (7 1) (1 2)))

(let ((tree-counts (loop for (Δx Δy) in *paths*
                         collect (count-trees *input* 0 Δx Δy))))
  (format t "Part 2: ~{~d~^ * ~} = ~d trees encountered~%"
          tree-counts
          (reduce #'* tree-counts)))
