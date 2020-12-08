(load "common.lisp")
(ql:quickload :cl-ppcre :silent t)

(defstruct node
  "A single node in the graph of bags."
  name visited auxiliary adjacent)

(defstruct edge
  "A weighted edge connecting two nodes in the graph of bags."
  weight destination)

(defparameter *input* (make-hash-table :test #'equal))

(defun parse-bag (spec)
  "Parse a bag definition of the form given in each line of the problem input
   contained in SPEC and add it to the *INPUT* graph."
  (let ((source-colour (subseq spec 0 (search " bags" spec)))
        (contents nil))
    (cl-ppcre:do-register-groups (quantity colour)
        ("(?: (\\d+) ([a-z ]+?) bags?)" spec)
      (when (and quantity colour)
        (setf contents (cons (make-edge :weight (parse-integer quantity)
                                        :destination colour)
                             contents))))
    (setf (gethash source-colour *input*)
          (make-node :name source-colour :adjacent contents))))

(map-file #p"input/day7.txt" #'parse-bag)

(defun reset-bags ()
  "Reset the graph of bags, *INPUT*, to its original state, with both the
   VISITED and AUXILIARY slots of every node set to NIL."
  (loop for node being the hash-values of *input*
        do (setf (node-visited node) nil)
        do (setf (node-auxiliary node) nil)))

(defun visit-bag (bag value)
  "Mark the specified BAG as visited, and set its auxiliary value to the
   specified VALUE.  Returns the VALUE for convenience."
  (setf (node-visited bag) t)
  (setf (node-auxiliary bag) value)
  value)

(defun contains-shiny-gold (bag)
  "Determine whether the specified BAG can contain a shiny gold bag, with any
   number of degrees of indirection.  Modifies the bag graph in order to
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
                       (contains-shiny-gold (gethash destination *input*)))
                return (visit-bag bag t)))))

;; Part 1: Determine the number of colours that can be the outer layer of a
;;         series of bags with a shiny gold bag as the innermost layer.
(format t "Part 1: ~d outer layers~%"
        (loop for bag being the hash-values of *input*
              count (contains-shiny-gold bag)))

(defun num-bags-contained (bag)
  "Determine the total number of bags that the specified BAG can contain,
   assuming that bags are nested non-recursively."
  (cond
    ;; If we have already visited this bag, return its containment count.
    ((node-visited bag) (node-auxiliary bag))
    ;; If the bag has no neighbours, it cannot contain any bags.
    ((null (node-adjacent bag)) (visit-bag bag 1))
    ;; This bag has neighbours, so add up all of their containment counts.
    ;; We include the current bag in the count as well to simplify computations.
    (t (loop for edge in (node-adjacent bag)
             for weight = (edge-weight edge)
             for destination = (gethash (edge-destination edge) *input*)
             sum (* weight (num-bags-contained destination)) into total
             finally (return (visit-bag bag (1+ total)))))))

;; Part 2: Determine the number of bags that a shiny gold bag can contain.
(reset-bags)
(format t "Part 2: ~d total bags~%"
        (1- (num-bags-contained (gethash "shiny gold" *input*))))
