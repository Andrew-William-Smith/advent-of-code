(uiop:define-package #:advent-of-code/day10
    (:use #:cl #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day10)

(defun parse (filename)
  (cons 0 (sort (map-file filename #'parse-integer) #'<)))

(defun count-differences (adapters)
  "Count the number of 1-jolt and 3-jolt differences in the specified list of
   ADAPTERS.  It is assumed that each adapter can connect to each subsequent
   adapter, and that the phone's adapter always has a rating 3 jolts greater
   than the highest-rated adapter."
  (loop for (a b) on adapters while b
        for diff = (- b a)
        count (= diff 1) into ones
        count (= diff 3) into threes
        finally (return (values ones (1+ threes)))))

(define-solution 1 (input) ((parse #p"input/day10.txt"))
  "Determine the product of the number of 1-jolt- and 3-jolt-difference adapters
   in the input list, including the outlet and phone."
  (multiple-value-bind (ones threes)
      (count-differences input)
    (* ones threes)))

(defun adapter-arrangements (adapters target cache)
  "Determine the number of arrangements of the specified ADAPTERS that can reach
   the TARGET joltage.  For efficiency, the number of arrangements for each
   joltage will be stored in the specified CACHE hash table."
  (let* ((current (car adapters))
         (cached (gethash current cache)))
    (cond
      ;; We have reached the last adapter: there is one way to reach the phone.
      ((= 3 (- target current)) 1)
      ;; The number of arrangements is cached, so stop computation.
      (cached cached)
      ;; We need to compute the number of arrangements.
      (t (loop for candidates on (cdr adapters)
               while (<= (- (car candidates) current) 3)
               sum (adapter-arrangements candidates target cache) into s
               finally (return (setf (gethash current cache) s)))))))

(define-solution 2 (input) ((parse #p"input/day10.txt"))
  "Determine the number of adapter arrangements that can reach from the outlet
   to the phone."
  (let ((phone-joltage (+ 3 (apply #'max input))))
    (adapter-arrangements input phone-joltage (make-hash-table))))
