(in-package #:advent-of-code)

(defun parse-group (in)
  "Parse a single group definition from the input file handled by the stream IN.
   Group definitions are separated by a blank line.  Each group is returned as a
   list of lists, in which each list represents the questions responded to in
   the affirmative by each person."
  (let ((next-line (read-line in nil "")))
    (when (> (length next-line) 0)
      (cons (coerce next-line 'list) (parse-group in)))))

(defun day6/parse (filename)
  (with-open-file (in (file-in-system filename))
    (loop for group = (parse-group in)
          while group
          collect group)))

(define-solution 6 1 (input) ((day6/parse #p"input/day6.txt"))
  "Determine the sum of the total number of questions responded to in the
   affirmative amongst all groups."
  (loop for group in input
        for total-questions = (reduce #'union group)
        sum (length total-questions)))

(define-solution 6 2 (input) ((day6/parse #p"input/day6.txt"))
  "Determine the sum of the number of questions for which everyone responded in
   the affirmative in each group."
  (loop for group in input
        for unanimous-questions = (reduce #'intersection group)
        sum (length unanimous-questions)))
