(uiop:define-package #:advent-of-code/day16
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day16)

(defstruct field
  "A single field on a train ticket, with its name and valid range of values."
  name start-low end-low start-high end-high)

(defun parse-field (line)
  "Attempt to parse a field described in the specified LINE of input.  Return
   the corresponding FIELD struct if parsing is successful, and NIL otherwise."
  (cl-ppcre:register-groups-bind (name (#'parse-integer sl el sh eh))
      ("^([a-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" line)
    (make-field :name name :start-low sl :end-low el
                :start-high sh :end-high eh)))

(defun parse-ticket (line)
  "Attempt to parse the ticket described in the specified LINE of input.  Return
   a list of integer field values if parsing is successful, and NIL otherwise."
  (when-let* ((fields (str:split "," line))
              (is-num (str:digitp (car fields))))
    (map 'vector #'parse-integer fields)))

(defstruct notes
  "The notes collected by inspecting nearby passengers' tickets, consisting of
   field descriptions and a series of tickets."
  (fields (make-hash-table :test #'equal))
  your-ticket
  nearby-tickets)

(defun parse-line (line notes)
  "Parse a LINE of the input file, storing the result in the appropriate field
   of the specified NOTES instance."
  (let ((field (parse-field line))
        (ticket (parse-ticket line)))
    (with-slots (fields your-ticket nearby-tickets) notes
      (cond
        (field (setf (gethash (field-name field) fields) field))
        ;; The first ticket is yours; all thereafter are nearby.
        ((and ticket (null your-ticket)) (setf your-ticket ticket))
        (ticket (setf nearby-tickets (cons ticket nearby-tickets)))))))

(defun parse (filename)
  (let ((notes (make-notes)))
    (with-open-file (in (file-in-system filename))
      (loop for line = (read-line in nil nil)
            while line
            do (parse-line line notes)
            finally (return notes)))))

(defun field-valid-p (value field)
  "Determine whether the specified VALUE is valid for the specified FIELD,
   meaning that it is within either of the field's lower or higher ranges."
  (with-slots (start-low end-low start-high end-high) field
    (or (<= start-low value end-low)
        (<= start-high value end-high))))

(defun ticket-error-rate (ticket fields)
  "Determine the error rate for the specified TICKET, or the sum of the values
   on the ticket that are not valid for any of the specified FIELDS.  Also
   return whether any error was found in the ticket."
  (loop with found-error = nil
        for value across ticket
        when (loop for field in fields never (field-valid-p value field))
          sum value into Σ
          and do (setf found-error t)
        finally (return (values Σ found-error))))

(define-solution 1 (input) ((parse #p"input/day16.txt"))
  "Determine the sum of the error rates of all nearby tickets."
  (let ((fields (hash-table-values (notes-fields input)))
        (tickets (notes-nearby-tickets input)))
    (reduce #'+ (mapcar [ticket-error-rate % fields] tickets))))

(defun field-is-candidate (tickets index field)
  "Determine whether the specified FIELD is valid in all of the TICKETS at the
   specified INDEX."
  (loop for tkt in tickets
        always (field-valid-p (aref tkt index) field)))

(defun candidate-fields (tickets fields)
  "Determine which of the specified FIELDS are valid for the values in the
   specified TICKETS.  Returns a list of cons cells containing the index of each
   ticket and that ticket's valid fields."
  (loop for i below (length (car tickets))
        collect (cons i (loop for fld in fields
                              when (field-is-candidate tickets i fld)
                                collect fld))))

(defun remove-candidate (target candidates)
  "Remove the specified TARGET candidate from the specified list of CANDIDATES."
  (loop for c in candidates
        for (index . fields) = c
        collect (cons index (remove target fields))))

(defun map-fields (candidates &optional mappings)
  "Determine a unique list of field MAPPINGS amongst the specified list of
   CANDIDATES.  The mapping indices are parallel with the candidates list."
  (let* ((current (car candidates))
         (index (car current))
         (fields (cdr current)))
    (cond
      ;; We have mapped all fields, so reverse the constructed list.
      ((emptyp candidates) (reverse mappings))
      ;; There are no candidates for this index, so a previous choice was wrong.
      ((emptyp fields) nil)
      ;; Evaluate all remaining candidates.
      (t (loop for f in fields
               for map = (map-fields (remove-candidate f (cdr candidates))
                                     (cons (cons index f) mappings))
               when map
                 return map)))))

(define-solution 2 (input) ((parse #p"input/day16.txt"))
  "Determine the product of the values of all fields on your ticket whose names
   begin with the word 'departure'."
  (with-slots (fields your-ticket nearby-tickets) input
    (let* ((raw-fields (hash-table-values fields))
           (valid-tickets (remove-if
                           [nth-value 1 (ticket-error-rate % raw-fields)]
                           nearby-tickets))
           (candidates (candidate-fields valid-tickets raw-fields))
           (sorted (sort candidates #'< :key #'length))
           (mappings (map-fields sorted)))
      (reduce #'* (loop for m in mappings
                        for (index . field) = m
                        when (str:starts-with-p "departure" (field-name field))
                          collect (aref your-ticket index))))))
