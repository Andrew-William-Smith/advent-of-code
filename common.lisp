(defun map-file (filename transform)
  "Apply the specified TRANSFORM function to all lines in the specified FILENAME."
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect (funcall transform line))))

(defun string-to-char (string)
  "Convert the specified single-character STRING to a CHARACTER."
  (coerce string 'character))

(defun neq (i1 i2)
  "Return whether I1 and I2 are not equal according to EQ."
  (not (eq i1 i2)))
