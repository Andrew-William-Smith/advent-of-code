(uiop:define-package #:advent-of-code/day4
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day4)

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
          for field = (intern (str:upcase raw-field) :advent-of-code/day4)
          do (setf (slot-value parsed field) value))
    parsed))

(defun parse (filename)
  (with-open-file (in (file-in-system filename))
    (loop for passport = (parse-passport in)
          while passport
          collect passport)))

(defun passport-validp (passport fields)
  "Return whether the specified PASSPORT is valid, meaning that all of its
   fields other than CID are defined and the constraints on the FIELDS are met.
   The constraints are an alist with entries of the form (ACCESSOR . VALIDATOR),
   where VALIDATOR is a monadic lambda that returns whether the value returned
   from the ACCESSOR is valid."
  (loop for (accessor . validator) in fields
        for field-value = (funcall accessor passport)
        always (and field-value (funcall validator field-value))))

(defun tolerant-in-range (min value max &key length)
  "Return whether the specified string VALUE is within the range [MIN, MAX].
   Non-digit characters may appear in the VALUE and will be ignored.  If
   specified,also validates that the VALUE is of the specified LENGTH."
  (and (or (not length)
           (= length (length value)))
       (<= min (parse-integer value :junk-allowed t) max)))

(defparameter *fields*
  `((passport-byr . ,[tolerant-in-range 1920 % 2002 :length 4])
    (passport-iyr . ,[tolerant-in-range 2010 % 2020 :length 4])
    (passport-eyr . ,[tolerant-in-range 2020 % 2030 :length 4])
    (passport-hgt
     . ,[cond
          ((str:ends-with-p "cm" %) (tolerant-in-range 150 % 193 :length 5))
          ((str:ends-with-p "in" %) (tolerant-in-range 59 % 76 :length 4))
          (t nil)])
    (passport-hcl . ,[cl-ppcre:scan "^\\#[0-9a-f]{6}$" %])
    (passport-ecl . ,[cl-ppcre:scan "^(amb|blu|brn|gry|grn|hzl|oth)$" %])
    (passport-pid . ,[cl-ppcre:scan "^\\d{9}$" %])))

(define-solution 1 (input) ((parse #p"input/day4.txt"))
  "Determine the number of valid passports in the input.  A passport is
   considered valid if all of its fields other than CID are defined."
  (let ((identity-fields (mapcar [cons (car %) #'identity] *fields*)))
    (count-if [passport-validp % identity-fields] input)))

(define-solution 2 (input) ((parse #p"input/day4.txt"))
  "Determine the number of valid passports in the input.  A passport is
   considered valid if all of its field validators return true."
  (count-if [passport-validp % *fields*] input))
