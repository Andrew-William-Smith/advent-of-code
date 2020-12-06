(load "common.lisp")

(defun parse-group (in)
  "Parse a single group definition from the input file handled by the stream IN.
   Group definitions are separated by a blank line.  Each group is returned as a
   list of lists, in which each list represents the questions responded to in
   the affirmative by each person."
  (let ((next-line (read-line in nil "")))
    (when (> (length next-line) 0)
      (cons (coerce next-line 'list) (parse-group in)))))

(defparameter *input*
  (with-open-file (in #p"input/day6.txt")
    (loop for group = (parse-group in)
          while group
          collect group)))

(destructuring-bind (total unanimous)
    (loop for group in *input*
          for total-questions = (reduce #'union group)
          for unanimous-questions = (reduce #'intersection group)
          sum (length total-questions) into total
          sum (length unanimous-questions) into unanimous
          finally (return (list total unanimous)))

  ;; Part 1: Determine the sum of the total number of questions responded to in
  ;;         the affirmative amongst all groups.
  (format t "Part 1: ~d total responses~%" total)

  ;; Part 2: Determine the sum of the number of questions for which everyone
  ;;         responded in the affirmative in each group.
  (format t "Part 2: ~d unanimous responses~%" unanimous))
