(uiop:define-package #:advent-of-code/day7
    (:use #:cl #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day7)

(defstruct node
  "A single node in the graph of bags."
  name visited auxiliary adjacent)

(defstruct edge
  "A weighted edge connecting two nodes in the graph of bags."
  weight destination)

(defun parse-bag (graph spec)
  "Parse a bag definition of the form given in each line of the problem input
   contained in SPEC and add it to the specified GRAPH."
  (let ((source-colour (subseq spec 0 (search " bags" spec)))
        (contents nil))
    (cl-ppcre:do-register-groups (quantity colour)
      ("(?: (\\d+) ([a-z ]+?) bags?)" spec)
      (when (and quantity colour)
        (setf contents (cons (make-edge :weight (parse-integer quantity)
                                        :destination colour)
                             contents))))
    (setf (gethash source-colour graph)
          (make-node :name source-colour :adjacent contents))))

(defun parse (filename)
  (let ((graph (make-hash-table :test #'equal)))
    (map-file filename [parse-bag graph %])
    graph))

(defun reset-bags (graph)
  "Reset the specified GRAPH of bags to its original state, with both the
   VISITED and AUXILIARY slots of every node set to NIL."
  (loop for node being the hash-values of graph
        do (setf (node-visited node) nil)
        do (setf (node-auxiliary node) nil)))

(defun visit-bag (bag value)
  "Mark the specified BAG as visited, and set its auxiliary value to the
   specified VALUE.  Returns the VALUE for convenience."
  (setf (node-visited bag) t)
  (setf (node-auxiliary bag) value)
  value)

(defun contains-shiny-gold (bag graph)
  "Determine whether the specified BAG can contain a shiny gold bag, with any
   number of degrees of indirection.  Modifies the bag GRAPH in order to
   accelerate future queries."
  (if (node-visited bag)
      ;; If we have already visited this node, its status cannot change.
      (node-auxiliary bag)
      ;; Otherwise, let's determine if we can contain a shiny gold bag.
      (progn
        (setf (node-visited bag) t)
        (loop for edge in (node-adjacent bag)
              for destination = (edge-destination edge)
              when (or (equal "shiny gold" destination)
                       (contains-shiny-gold (gethash destination graph) graph))
                return (visit-bag bag t)))))

(define-solution 1 (input) ((parse #p"input/day7.txt"))
  "Determine the number of colours that can be the outer layer of a series of
   bags with a shiny gold bag as the innermost layer."
  (reset-bags input)
  (loop for bag being the hash-values of input
        count (contains-shiny-gold bag input)))

(defun num-bags-contained (bag graph)
  "Determine the total number of bags in the specified GRAPH that the specified
   BAG can contain, assuming that bags are nested non-recursively."
  (cond
    ;; If we have already visited this bag, return its containment count.
    ((node-visited bag) (node-auxiliary bag))
    ;; If the bag has no neighbours, it cannot contain any bags.
    ((null (node-adjacent bag)) (visit-bag bag 0))
    ;; This bag has neighbours, so add up all of their containment counts.
    ;; We include the current bag in the count as well to simplify computations.
    (t (loop for edge in (node-adjacent bag)
             for weight = (edge-weight edge)
             for destination = (gethash (edge-destination edge) graph)
             sum (* weight (1+ (num-bags-contained destination graph))) into total
             finally (return (visit-bag bag total))))))

(define-solution 2 (input) ((parse #p"input/day7.txt"))
  "Determine the number of bags that a shiny gold bag can contain."
  (reset-bags input)
  (num-bags-contained (gethash "shiny gold" input) input))
