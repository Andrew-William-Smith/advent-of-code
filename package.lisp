(defpackage #:advent-of-code
  (:use #:cl #:alexandria)
  (:export ()))

(in-package :advent-of-code)

(defparameter *max-day* 10)

(defun daysym (day suffix)
  "Create a symbol of the form DAY{DAY}/{SUFFIX}."
  (intern (concatenate 'string
                       "DAY" (write-to-string day)
                       "/" suffix)))

(loop for i from 1 to *max-day*
      do (export (map 'list
                      (lambda (s) (daysym i s))
                      '("PARSE" "PART1" "PART2" "RUN1" "RUN2"))))
