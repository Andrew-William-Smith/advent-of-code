(load "common.lisp")

(defun parse-binary (spec one)
  "Parse the specified seat assignment SPEC to an integer, with the specified
   ONE character mapping to a bit value 1 and all others mapping to 0."
  (parse-integer (map 'string [if (char= % one) #\1 #\0] spec) :radix 2))

(defun parse-seat (spec)
  "Parse a seat assignment from the specified string SPEC.  Returns the seat as
   a (ROW . COLUMN) cons cell."
  (cons
   (parse-binary (subseq spec 0 7) #\B)
   (parse-binary (subseq spec 7 10) #\R)))

;; Parse each line of the input to a (ROW . COLUMN) pair.
(defparameter *input* (map-file #p"input/day5.txt" #'parse-seat))

(destructuring-bind (min-id max-id id-total)
    (loop for seat in *input*
          for id = (+ (* 8 (car seat)) (cdr seat))
          minimize id into min
          maximize id into max
          sum id into total
          finally (return (list min max total)))

  ;; Part 1: Determine the highest seat ID on any boarding pass
  (format t "Part 1: ~d~%" max-id)

  ;; Part 2: Determine my seat, the only unoccupied seat on the plane knowing
  ;;         that some seats at the front or back of the plane may be missing.
  ;;         We use the property that the difference between the sum of all
  ;;         occupied seat ID's and the sum of the sequence from the minimum to
  ;;         the maximum seat ID should be the ID of my seat.  The sum of the
  ;;         sequence is computed using the standard arithmetic series summation
  ;;         n * (a1 + a2) / 2.
  (let ((series-sum (/ (* (1+ (- max-id min-id)) (+ min-id max-id)) 2)))
    (format t "Part 2: ~d~%" (- series-sum id-total))))
