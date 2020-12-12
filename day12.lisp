(in-package #:advent-of-code)

(defun parse-motion (command)
  "Parse the specified motion COMMAND to a cons cell containing a motion keyword
   and an argument."
  (let ((argument (parse-integer command :start 1)))
    (eswitch ((string-to-char (str:s-first command)))
      (#\N (cons :move (complex 0 argument)))
      (#\S (cons :move (complex 0 (- argument))))
      (#\E (cons :move (complex argument)))
      (#\W (cons :move (complex (- argument))))
      (#\L (cons :rotate (- argument)))
      (#\R (cons :rotate argument))
      (#\F (cons :forward argument)))))

(defun day12/parse (filename)
  (map-file filename #'parse-motion))

(defun rotate-position (position θ)
  "Rotate the specified POSITION by Θ degrees, returning the new position."
  (let ((x (realpart position))
        (y (imagpart position))
        (rad (* pi (/ (mod θ 360) 180))))
    (complex (+ (* x (cos rad)) (* y (sin rad)))
             (- (* y (cos rad)) (* x (sin rad))))))

(defun move-agents (ship waypoint move motion argument)
  "Move the WAYPOINT and SHIP coordinates according to the specified MOTION with
   the specified ARGUMENT.  When a :MOVE command is executed, the result of the
   MOVE function is returned.  Returns a cons cell containing the new positions
   of both agents."
  (switch (motion)
    (:move (funcall move ship waypoint argument))
    (:rotate (cons ship (rotate-position waypoint argument)))
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

(define-solution 12 1 (input) ((day12/parse #p"input/day12.txt"))
  "Determine the position of the ship after executing all of the INPUT commands,
   assuming that the commands act upon the ship directly and that the ship
   starts at the origin facing eastward.  Coordinates are represented using
   complex numbers throughout."
  (evade-storm input [cons (+ %1 %3) %2] 1))

(define-solution 12 2 (input) ((day12/parse #p"input/day12.txt"))
  "Determine the position of the ship after executing all of the INPUT commands.
   In this part of the problem, the commands act upon a waypoint positioned
   relative to the ship, save for :FORWARD, which moves the ship in the
   direction of the waypoint.  The ship starts at the origin and the waypoint
   starts 1 unit north and 10 units east of the ship."
  (evade-storm input [cons %1 (+ %2 %3)] #C(10 1)))
