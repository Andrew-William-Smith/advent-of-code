(in-package #:advent-of-code)

(defstruct password
  "A representation of a password as specified in the input file."
  (min-index 0 :type integer)
  (max-index 0 :type integer)
  (char #\0 :type character)
  (data "" :type string))

(defun parse-password (spec)
  "Return a PASSWORD instance for the password defined in the SPEC."
  (cl-ppcre:register-groups-bind
   ((#'parse-integer min max) (#'string-to-char char) password)
   ("(\\d+)-(\\d+) ([a-z]): ([a-z]+)" spec)
   (when password
     (make-password :min-index min
                    :max-index max
                    :char char
                    :data password))))

(defun day2/parse (filename)
  (map-file filename #'parse-password))

(defparameter *input* (day2/parse #p"input/day2.txt"))

(defun valid-count-occurrences (password)
  "Determine if the specified PASSWORD is valid according to Part 1."
  (with-slots (min-index max-index char data) password
    (<= min-index (count char data) max-index)))

(define-solution 2 1 (input) (*input*)
  "Determine how many passwords have occurrences of the character within the
   specified limit."
  (count-if #'valid-count-occurrences input))

(defun char-at-index= (string index target)
  "Determine whether the character at the specified 1-based INDEX in the specified STRING is equal to the TARGET character."
  (char= (char string (1- index)) target))

(defun valid-index-occurrences (password)
  "Determine if the specified PASSWORD is valid according to Part 2."
  (with-slots (min-index max-index char data) password
    (neq (char-at-index= data min-index char)
         (char-at-index= data max-index char))))

;; Part 2: Determine how many passwords have occurrences of the character only
;;         at one of the specified indices.
(define-solution 2 2 (input) (*input*)
  "Determine how many passwords have occurrences of the character only at one of
   the specified indices."
  (count-if #'valid-index-occurrences input))
