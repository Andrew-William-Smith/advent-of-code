(uiop:define-package #:advent-of-code/day9
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day9)

(defun parse (filename)
  (map-file filename #'parse-integer))

(defun find-two-sum (seq target)
  "Determine whether the specified sequence SEQ contains two values whose sum is
   equal to the TARGET."
  (member target (apply #'map-product #'+ (list seq seq))))

(defun xmas-advance (window length)
  "Determine whether the next input value is a valid item in the XMAS sequence
   specified in the WINDOW, which is LENGTH elements long.  The window is a
   structural subset of the input and can thus be traversed in step with the
   input list.  If the next item is valid, return the updated window; otherwise,
   return NIL."
  (when-let* ((target (cadr window))
              (win (subseq (car window) 0 length))
              (found (find-two-sum win target)))
             ;; Advance both head and tail to form the new window.
             (cons (cdar window) (cddr window))))

(define-solution 1 (input preamble-length) ((parse #p"input/day9.txt") 25)
  "Determine the first number in the INPUT list that does not satisfy the XMAS
   encoding invariant, which is that two of the past PREAMBLE-LENGTH numbers add
   up to the number.  Note that the sliding windows in this problem are
   represented as cons cells in which the car is the window from the front and
   the cdr is the window from the tail onward.  This allows constant-time
   traversal through the input list with no additional memory usage."
  (loop for window = (cons input (nthcdr preamble-length input)) then new-window
        for new-window = (xmas-advance window preamble-length)
        while new-window
        finally (return (cadr window))))

(defun subseq-sum (seq end length target)
  "Return the contiguous subsequence of SEQ whose values add up to TARGET.  The
   subsequence ends at the sequence END and is of the specified LENGTH."
  (cond
    ((zerop target) (subseq seq 0 length))
    ((> target 0) (subseq-sum seq (cdr end) (1+ length) (- target (car end))))
    ((< target 0) (subseq-sum (cdr seq) end (1- length) (+ target (car seq))))))

(define-solution 2 (input preamble-length) ((parse #p"input/day9.txt") 25)
  "Determine the encryption weakness of this XMAS cipher, the sum of the
   smallest and largest values in the subset of the input that adds up to the
   invalid value computed in Part 1."
  (let ((target-seq (subseq-sum input input 0 (part1 input preamble-length))))
    (+ (apply #'min target-seq) (apply #'max target-seq))))
