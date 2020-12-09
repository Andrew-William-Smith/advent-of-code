(load "common.lisp")
(ql:quickload :alexandria :silent t)
(use-package :alexandria)

(defparameter *input* (map-file #p"input/day9.txt" #'parse-integer))

(defun find-two-sum (seq target)
  "Determine whether the specified sequence SEQ contains two values whose sum is
   equal to the TARGET."
  (member target (apply #'map-product #'+ (list seq seq))))

(defun xmas-advance (window length)
  "Determine whether the next *INPUT* value is a valid item in the XMAS sequence
   specified in the WINDOW, which is LENGTH elements long.  The window is a
   structural subset of the *INPUT* and can thus be traversed in step with the
   input list.  If the next item is valid, return the updated window; otherwise,
   return NIL."
  (when-let* ((target (cadr window))
              (win (subseq (car window) 0 length))
              (found (find-two-sum win target)))
    ;; Advance both head and tail to form the new window.
    (cons (cdar window) (cddr window))))

;; Part 1: Determine the first number in the input list that does not satisfy
;;         the XMAS encoding variant, which is that two of the past 25 numbers
;;         add up to the number.  Note that sliding windows in this problem are
;;         represented as cons cells in which the car is the window from the
;;         front and the cdr is the window from the tail onward.  This allows
;;         constant-time traversal through the input list with no additional
;;         memory usage.
(defparameter *xmas-invalid* 0)
(solution "Part 1: ~d is invalid~%"
          (loop for window = (cons *input* (nthcdr 25 *input*)) then new-window
                for new-window = (xmas-advance window 25)
                while new-window
                finally (return (setf *xmas-invalid* (cadr window)))))

(defun subseq-sum (seq end length target)
  "Return the contiguous subsequence of SEQ whose values add up to TARGET.  The
   subsequence ends at the sequence END and is of the specified LENGTH."
  (cond
    ((zerop target) (subseq seq 0 length))
    ((> target 0) (subseq-sum seq (cdr end) (1+ length) (- target (car end))))
    ((< target 0) (subseq-sum (cdr seq) end (1- length) (+ target (car seq))))))

;; Part 2: Determine the encryption weakness of this XMAS cipher, the sum of the
;;         smallest and largest values in the subset of the input that adds up
;;         to the invalid value computed in Part 1.
(solution "Part 2: Encryption weakness is ~d~%"
          (let ((target-seq (subseq-sum *input* *input* 0 *xmas-invalid*)))
            (+ (apply #'min target-seq) (apply #'max target-seq))))
