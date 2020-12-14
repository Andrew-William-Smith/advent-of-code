(in-package #:advent-of-code)

(defun day13/parse (filename)
  (with-open-file (in (file-in-system filename))
    (let ((departure (parse-integer (read-line in)))
          (schedule (map 'list [when (str:digitp %) (parse-integer %)]
                         (str:split "," (read-line in)))))
      (cons departure schedule))))

(defun wait-time (departure interval)
  "Determine the amount of time that would be necessary to wait past the
   specified DEPARTURE time for the next occurrence of the bus running at the
   scheduled INTERVAL."
  (- interval (mod departure interval)))

(defun next-bus (departure schedule
                 &optional (min-wait most-positive-fixnum) (best-bus 0))
  "Determine the next bus in the specified SCHEDULE of intervals to arrive after
   the specified DEPARTURE time."
  (let* ((this-bus (car schedule))
         (this-wait (when this-bus (wait-time departure this-bus))))
    (cond
      ((null this-bus) best-bus)
      ((< this-wait min-wait) (next-bus departure (cdr schedule) this-wait this-bus))
      (t (next-bus departure (cdr schedule) min-wait best-bus)))))

(define-solution 13 1 (input) ((day13/parse #p"input/day13.txt"))
  "Determine the product of the ID and wait time for the next departure of the
   bus with the shortest wait time."
  (let* ((departure (car input))
         (buses (remove nil (cdr input)))
         (next-bus (next-bus departure buses)))
    (* next-bus (wait-time departure next-bus))))

(defun find-coincidence (schedule)
  "Find the time when all of the buses in the specified SCHEDULE, which are cons
   cells of the form (BUS-ID . INDEX), leave in consecutive times when arranged
   by their indices.  This is an implementation of the sieve form of the Chinese
   Remainder Theorem, which I definitely had to Google in order to solve this
   problem."
  (loop with coincidence = 0
        with step = (caar schedule)
        for bus in (cdr schedule)
        for (id . offset) = bus
        do (loop for c from coincidence by step
                 when (zerop (mod (+ c offset) id))
                   return (setf coincidence c))
        do (setf step (* step id))
        finally (return coincidence)))

(define-solution 13 2 (input) ((day13/parse #p"input/day13.txt"))
  "Determine the first time at which the buses at consecutive indices in the
   schedule will leave at consecutive times."
  (find-coincidence (loop for bus in (cdr input)
                          for idx from 0
                          when bus
                            collect (cons bus idx))))
