(in-package #:advent-of-code)

(defun parse-seats (line)
  "Parse the specified string LINE of seats into a list of grid cells, with an
   additional floor cell added at each edge."
  (concatenate 'list '(:floor)
               (map 'list [eswitch (% :test #'char=)
                            (#\. :floor)
                            (#\L :empty)
                            (#\# :taken)] line)
               '(:floor)))

(defun day11/parse (filename)
  (let* ((seats (map-file filename #'parse-seats))
         (rows (+ 2 (length seats)))
         (cols (length (car seats)))
         (padding (list (loop repeat cols collect :floor)))
         (padded (concatenate 'list padding seats padding)))
    (make-array (list rows cols) :initial-contents padded)))

(defparameter *adjacent-offsets*
  '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1))
  "(Δrow, Δcolumn) offsets for the cells adjacent to a cell.")

(defun count-adjacent (seats row col leave-limit)
  "Count the number of seats that are directly adjacent to the seat at position
   (ROW, COL) in the specified grid of SEATS, up to the specified LEAVE-LIMIT at
   which the person sitting in that seat will leave."
  (loop for (Δr . Δc) in *adjacent-offsets*
        while (< adjacent leave-limit)
        for new-row = (+ row Δr)
        for new-col = (+ col Δc)
        count (eq :taken (aref seats new-row new-col)) into adjacent
        finally (return adjacent)))

(defun next-seat (seats row col counter leave-limit)
  "Determine the next value for the seat at position (ROW, COL) in the specified
   grid of SEATS, using the specified COUNTER function to count the number of
   seats adjacent to the current seat and assuming that people will leave
   occupied seats when their neighbour count reaches the LEAVE-LIMIT."
  (if (eq :floor (aref seats row col))
      ;; Floor cells will never change, so don't count their neighbours.
      :floor
      ;; All other cells can change dependent on their taken neighbours.
      (let ((old-seat (aref seats row col))
            (adjacent (funcall counter seats row col leave-limit)))
        (cond
          ((and (eq :empty old-seat) (zerop adjacent)) :taken)
          ((and (eq :taken old-seat) (>= adjacent leave-limit)) :empty)
          (t old-seat)))))

(defun next-grid (seats counter leave-limit)
  "Determine the resultant passenger grid on the next iteration of the automaton
   running on the specified grid of SEATS, returning both the grid itself and
   the number of cells changed between the current and next states.  The
   specified COUNTER and LEAVE-LIMIT define the rules for the automaton in the
   current run."
  (loop with (rows cols) = (array-dimensions seats)
        with next = (make-array (list rows cols) :initial-element :floor)
        for r from 1 to (- rows 2)
        sum (loop for c from 1 to (- cols 2)
                  for changed = (next-seat seats r c counter leave-limit)
                  do (setf (aref next r c) changed)
                  count (neq (aref seats r c) changed))
          into changed
        finally (return (values next changed))))

(defun seats-fixpoint (seats counter leave-limit)
  "Return the state of the passenger automaton beginning with the specified grid
   of SEATS once iterating the automaton ceases to change any cells.  The
   specified COUNTER and LEAVE-LIMIT define the rules for the automaton in the
   current run."
  (loop for (next changed) = (list seats (apply #'* (array-dimensions seats)))
          then (multiple-value-list (next-grid next counter leave-limit))
        until (zerop changed)
        finally (return next)))

(defun count-taken (seats)
  "Count the number of seats that are occupied in the specified grid of SEATS."
  (loop for r below (array-dimension seats 0)
        sum (loop for c below (array-dimension seats 1)
                  count (eq :taken (aref seats r c)))))

(define-solution 11 1 (input) ((day11/parse #p"input/day11.txt"))
  "Count the number of occupied seats once the passenger automaton reaches its
   fixpoint beginning with the specified INPUT grid."
  (count-taken (seats-fixpoint input #'count-adjacent 4)))

(defun search-taken (seats row col Δr Δc)
  "Search for an occupied seat in the specified grid of SEATS as visible from
   the seat at position (ROW, COL) along a path with a slope (ΔR, ΔC)."
  ;; Note: Explicit destructuring required because SBCL can't interpret ranks.
  (loop with num-rows = (array-dimension seats 0)
        with num-cols = (array-dimension seats 1)
        for r = (+ row Δr) then (+ r Δr)
        for c = (+ col Δc) then (+ c Δc)
        while (and (< 0 r num-rows) (< 0 c num-cols))
        for cell = (aref seats r c)
        when (neq :floor cell) return (eq :taken cell)))

(defun count-visible (seats row col leave-limit)
  "Count the number of seats that are visible to the seat at position (ROW, COL)
   in the specified grid of SEATS according to the raycasting algorithm
   described in Part 2, up to the specified LEAVE-LIMIT at which the person
   sitting in the seat will leave."
  (loop for (Δr . Δc) in *adjacent-offsets*
        while (< visible leave-limit)
        count (search-taken seats row col Δr Δc) into visible
        finally (return visible)))

(define-solution 11 2 (input) ((day11/parse #p"input/day11.txt"))
  "Count the number of occupied seats once the passenger automaton reaches its
   fixpoint beginning with the specified INPUT grid using the raycasting
   neighbour counting algorithm."
  (count-taken (seats-fixpoint input #'count-visible 5)))
