(load "common.lisp")
(ql:quickload :alexandria :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :str :silent t)

(use-package :alexandria)

(defstruct passport
  "A representation of a passport as per the problem description."
  byr iyr eyr hgt hcl ecl pid cid)

(defun read-passport (in)
  "Read a passport entry from the input stream IN."
  (let ((next-line (read-line in nil nil)))
    (unless (str:emptyp next-line)
      (append (str:words next-line) (read-passport in)))))

(defun parse-passport (in)
  "Parse an entire passport entry from the input stream IN."
  (when-let* ((entries (read-passport in))
              (parsed (make-passport)))
    (loop for entry in entries
          for (raw-field value) = (str:split ":" entry)
          for field = (intern (str:upcase raw-field))
          do (setf (slot-value parsed field) value))
    parsed))

(defparameter *input*
  (with-open-file (in #p"input/day4.txt")
    (loop for passport = (parse-passport in)
          while passport
          collect passport)))

(defun passport-validp (passport fields)
  "Return whether the specified PASSPORT is valid, meaning that all of its
   fields other than CID are defined and the constraints on the FIELDS are met.
   The constraints are an alist with entries of the form (ACCESSOR . VALIDATOR),
   where VALIDATOR is a monadic lambda that returns whether the value returned
   from the ACCESSOR is valid."
  (every #'identity
         (map 'list [when-let ((field-value (funcall (car %) passport)))
                      (funcall (cdr %) field-value)] fields)))

(defun tolerant-in-range (min value max)
  "Return whether the specified string VALUE is within the range [MIN, MAX].
   Non-digit characters may appear in the VALUE and will be ignored."
  (<= min (parse-integer value :junk-allowed t) max))

;; Part 1: Determine the number of valid passports in the input.  A passport is
;;         considered valid if all of its fields other than CID are defined.
(defparameter *identity-fields*
  (map 'list [cons % #'identity]
       '(passport-byr passport-iyr passport-eyr passport-hgt passport-hcl
         passport-ecl passport-pid)))

(format t "Part 1: ~d valid passports~%"
        (count-if [passport-validp % *identity-fields*] *input*))

;; Part 2: Determine the number of valid passports in the input.  A passport is
;;         considered valid if its validator returns true.
(defparameter *validate-fields*
  (list (cons #'passport-byr [tolerant-in-range 1920 % 2002])
        (cons #'passport-iyr [tolerant-in-range 2010 % 2020])
        (cons #'passport-eyr [tolerant-in-range 2020 % 2030])
        (cons #'passport-hgt
              [cond
                ((str:ends-with-p "cm" %) (tolerant-in-range 150 % 193))
                ((str:ends-with-p "in" %) (tolerant-in-range 59 % 76))
                (t nil)])
        (cons #'passport-hcl [cl-ppcre:scan "^\\#[0-9a-f]{6}$" %])
        (cons #'passport-ecl [cl-ppcre:scan "^(amb|blu|brn|gry|grn|hzl|oth)$" %])
        (cons #'passport-pid [cl-ppcre:scan "^\\d{9}$" %])))

(format t "Part 2: ~d valid passports~%"
        (count-if [passport-validp % *validate-fields*] *input*))
