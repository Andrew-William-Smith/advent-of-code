;;; READER MACROS

(defun extract-positional-number (argument)
  "Extract the positional argument number (%{number}) from the specified ARGUMENT atom."
  (let ((sarg (string argument)))
    (if (and (char= #\% (char sarg 0))
             (> (length sarg) 1))
        (nth-value 0 (parse-integer sarg :start 1))
        0)))

(defun count-positional-args (sexp)
  "Find the highest-numbered positional argument (%{number}) in the specified SEXP."
  (cond
    ((symbolp sexp) (extract-positional-number sexp))
    ((consp sexp) (max (count-positional-args (car sexp))
                       (count-positional-args (cdr sexp))))
    (t 0)))

(defun |[-reader| (stream char)
  "Parse a shorthand lambda.  The body of the lambda is dispatched by [], and
   positional arguments may be referred to therein with %1, %2, etc.  A special
   form %& is used to refer to the &REST arguments of the function, which will
   be automatically assigned as those that follow the last positional argument.
   If only a % appears, then it will be assumed to refer to a single positional
   argument.  If the body consists of multiple sexps, they will be surrounded by
   an implicit PROGN.  Due to parameter name mangling, shorthand lambdas may be
   nested, and each lambda's arguments will apply only within its immeidate
   scope; please use this feature sparingly."
  (declare (ignore char))
  (let* ((sexp (read-delimited-list #\] stream t))
         (arg-count (count-positional-args sexp))
         (args (if (zerop arg-count)
                   (list (cons '% (gensym)))
                   (loop for i from 1 to arg-count
                         collect (cons (intern (format nil "%~d" i)) (gensym)))))
         (arg-names (mapcar #'cdr args))
         (rest-sym (gensym))
         (mangled (sublis (cons (cons '%& rest-sym) args) sexp)))
    `(lambda (&optional ,@arg-names &rest ,rest-sym)
       (declare (ignorable ,@arg-names ,rest-sym))
       ,(if (and (listp mangled) (listp (car mangled)))
            (cons 'progn mangled)
            mangled))))

(set-macro-character #\[ #'|[-reader|)
(set-macro-character #\] (get-macro-character #\)))


;;; UTILITY FUNCTIONS

(defun map-file (filename transform)
  "Apply the specified TRANSFORM function to all lines in the specified FILENAME."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect (funcall transform line))))

(defun string-to-char (string)
  "Convert the specified single-character STRING to a CHARACTER."
  (coerce string 'character))

(defun boolean-to-bit (boolean)
  "Convert the specified BOOLEAN to a BIT (0 or 1)."
  (if boolean 1 0))

(defun map-bit-vector (predicate seq)
  "Map the specified sequence SEQ to a BIT-VECTOR using the specified PREDICATE to determine the value of each bit."
  (map 'bit-vector [boolean-to-bit (funcall predicate %)] seq))

(defun neq (i1 i2)
  "Return whether I1 and I2 are not equal according to EQ."
  (not (eq i1 i2)))
