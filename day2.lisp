(load "common.lisp")
(ql:quickload :cl-ppcre)

(defstruct password
  "A representation of a password as specified in the input file."
  (min-index 0 :type integer)
  (max-index 0 :type integer)
  (char #\0 :type character)
  (data "" :type string))

(defun parse-password (spec)
  "Return a PASSWORD instance for the password defined in the SPEC."
  (cl-ppcre::register-groups-bind
   ((#'parse-integer min max) (#'string-to-char char) password)
   ("(\\d+)-(\\d+) ([a-z]): ([a-z]+)" spec)
   (when password
     (make-password :min-index min
                    :max-index max
                    :char char
                    :data password))))

(defparameter *input* (map-file #p"input/day2.txt" #'parse-password))

(defun valid-count-occurrences (password)
  "Determine if the specified PASSWORD is valid according to Part 1."
  (<= (password-min-index password)
      (count (password-char password) (password-data password))
      (password-max-index password)))

;; Part 1: Determine how many passwords have occurrences of the character within
;;         the specified limit.
(format t "Part 1: ~d passwords are valid~%"
        (count-if #'valid-count-occurrences *input*))

(defun char-at-index= (string index target)
  "Determine whether the character at the specified 1-based INDEX in the specified STRING is equal to the TARGET character."
  (char= (char string (1- index)) target))

(defun valid-index-occurrences (password)
  "Determine if the specified PASSWORD is valid according to Part 2."
  (let ((search-char (password-char password))
        (data (password-data password)))
    (neq (char-at-index= data (password-min-index password) search-char)
         (char-at-index= data (password-max-index password) search-char))))

;; Part 2: Determine how many passwords have occurrences of the character only
;;         at one of the specified indices.
(format t "Part 2: ~d passwords are valid~%"
        (count-if #'valid-index-occurrences *input*))
