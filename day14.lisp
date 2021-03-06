(uiop:define-package #:advent-of-code/day14
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day14)

(defun parse-command (line)
  "Parse the command described in the specified LINE of code to a directive of
   the form :MASK or :ASSIGN."
  (if (str:starts-with-p "mask" line)
      ;; This is a mask assignment: construct the proper masks.
      (cl-ppcre:register-groups-bind (mask)
          ("mask = ([01X]{36})" line)
        (list :mask mask))
      ;; This is a memory assignment: simply extract the address and value.
      (cl-ppcre:register-groups-bind ((#'parse-integer address value))
          ("mem\\[(\\d+)\\] = (\\d+)" line)
        (list :assign address value))))

(defun parse (filename)
  (map-file filename #'parse-command))

(defun execute-command (command mask memory parse-mask assign)
  "Execute the specified COMMAND upon the specified MEMORY module, with the
   current MASK active for memory writes, upon which the specified ASSIGN
   function will be called and which are generated by the PARSE-MASK function.
   Returns the mask after the command is executed, and may modify (write values
   to) the memory module."
  (eswitch ((car command))
    (:mask (funcall parse-mask (second command)))
    (:assign (progn
               (funcall assign (second command) (third command) mask memory)
               mask))))

(defun execute-program (program parse-mask assign)
  "Execute the specified PROGRAM, returning the state of memory once the program
   has finished executing.  The specified ASSIGN function is executed when an
   :ASSIGN command is encountered, and PARSE-MASK is used to determine how to
   construct mask values from strings."
  (loop with memory = (make-hash-table)
        for command in program
        for mask = (execute-command command '(0 0) memory parse-mask assign)
          then (execute-command command mask memory parse-mask assign)
        finally (return memory)))

(defun value-mask (mask)
  "Generate cancellation and application masks for the specified MASK string,
   which is comprised of 0 or 1 to set a bit in the output integer, and X to
   preserve the existing bit."
  (list (parse-integer (str:replace-using '("1" "0" "X" "1") mask) :radix 2)
        (parse-integer (str:replace-all "X" "0" mask) :radix 2)))

(defun assign-value (address value mask memory)
  "Assign the specified VALUE masked with the specified value-type MASK to the
   specified ADDRESS in MEMORY."
  (destructuring-bind (cancel apply) mask
    (setf (gethash address memory)
          (logior (logand value cancel) apply))))

(define-solution 1 (input) ((parse #p"input/day14.txt"))
  (reduce #'+ (hash-table-values
               (execute-program input #'value-mask #'assign-value))))

(defun address-mask (mask)
  "Generate a mask for a floating address from the specified MASK string.  In
   this type of mask, a 0 indicates that the original value should be preserved,
   a 1 indicates that a bit should be overwritten with a 1, and an X indicates a
   floating bit, which may have a value of either 0 or 1.  Returns a list in
   which the car is the cancellation mask and the cdr is all application masks."
  (cons (parse-integer (str:replace-using '("0" "?" "1" "0" "X" "0" "?" "1") mask)
                       :radix 2)
        (labels ((mask-float (source mask index)
                   (if (= index (length source))
                       ;; All bits have been assigned, so the mask is built.
                       (list mask)
                       ;; Need to construct the remaining bits.
                       (eswitch ((char source index) :test #'char=)
                         (#\0 (append-0 source mask index))
                         (#\1 (append-1 source mask index))
                         (#\X (concatenate 'list
                                           (append-0 source mask index)
                                           (append-1 source mask index))))))
                 (append-0 (source mask index)
                   (mask-float source (ash mask 1) (1+ index)))
                 (append-1 (source mask index)
                   (mask-float source (1+ (ash mask 1)) (1+ index))))
          (mask-float mask 0 0))))

(defun assign-floating (address value mask memory)
  "Assign the specified VALUE to the specified ADDRESS in MEMORY, masked with
   the specified floating-type MASK."
  (loop with cancelled = (logand address (car mask))
        for apply in (cdr mask)
        do (setf (gethash (logior cancelled apply) memory) value)))

(define-solution 2 (input) ((parse #p"input/day14.txt"))
  (reduce #'+ (hash-table-values
               (execute-program input #'address-mask #'assign-floating))))
