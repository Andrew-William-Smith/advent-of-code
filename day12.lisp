(uiop:define-package #:advent-of-code/day12
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day12)

(defun parse-motion (command)
  "Parse the specified motion COMMAND to a cons cell containing a motion keyword
   and an argument."
  (let ((argument (parse-integer command :start 1)))
    (eswitch ((string-to-char (str:s-first command)))
      (#\N (cons :move (complex 0 argument)))
      (#\S (cons :move (complex 0 (- argument))))
      (#\E (cons :move (complex argument)))
      (#\W (cons :move (complex (- argument))))
      ;; Multiplying by i rotates a complex coordinate 90 degrees to the left.
      (#\L (cons :rotate (expt #C(0 1) (/ argument 90))))
      (#\R (cons :rotate (expt #C(0 -1) (/ argument 90))))
      (#\F (cons :forward argument)))))

(defun parse (filename)
  (map-file filename #'parse-motion))

(defun move-agents (ship waypoint move motion argument)
  "Move the WAYPOINT and SHIP coordinates according to the specified MOTION with
   the specified ARGUMENT.  When a :MOVE command is executed, the result of the
   MOVE function is returned.  Returns a cons cell containing the new positions
   of both agents."
  (eswitch (motion)
    (:move (funcall move ship waypoint argument))
    (:rotate (cons ship (* waypoint argument)))
    (:forward (cons (+ ship (* waypoint argument)) waypoint))))

(defun manhattan-distance (position)
  "Determine the Manhattan distance between the specified POSITION, a complex
   number of the form x + yi, and the origin."
  (nth-value 0 (round (+ (abs (realpart position))
                         (abs (imagpart position))))))

(defun evade-storm (instructions move waypoint)
  "Execute the specified list of INSTRUCTIONS to evade the storm, moving the
   agents according to the specified MOVE function with the specified initial
   WAYPOINT value.  Return the Manhattan distance that the ship has travelled
   from its initial waypoint."
  (loop for (motion . arg) in instructions
        for (ship . way) = (move-agents 0 waypoint move motion arg)
          then (move-agents ship way move motion arg)
        finally (return (manhattan-distance ship))))

(define-solution 1 (input) ((parse #p"input/day12.txt"))
  "Determine the position of the ship after executing all of the INPUT commands,
   assuming that the commands act upon the ship directly and that the ship
   starts at the origin facing eastward.  Coordinates are represented using
   complex numbers throughout."
  (evade-storm input [cons (+ %1 %3) %2] 1))

(define-solution 2 (input) ((parse #p"input/day12.txt"))
  "Determine the position of the ship after executing all of the INPUT commands.
   In this part of the problem, the commands act upon a waypoint positioned
   relative to the ship, save for :FORWARD, which moves the ship in the
   direction of the waypoint.  The ship starts at the origin and the waypoint
   starts 1 unit north and 10 units east of the ship."
  (evade-storm input [cons %1 (+ %2 %3)] #C(10 1)))
