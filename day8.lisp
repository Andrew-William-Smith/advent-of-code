(load "common.lisp")
(ql:quickload :alexandria :silent t)
(ql:quickload :str :silent t)
(use-package :alexandria)

(defclass instruction ()
  ((opcode :accessor ins-op    :initform :nop :type keyword :initarg :op)
   (arg    :accessor ins-arg   :initform 0    :type integer :initarg :arg)
   (count  :accessor ins-count :initform 0    :type integer))
  (:documentation "An instruction executable by the virtual machine."))

(defun make-instruction (asm)
  "Create an INSTRUCTION corresponding to the specified assembly-language
   instruction ASM."
  (destructuring-bind (op-name arg-str)
      (str:split " " asm)
    (make-instance 'instruction
                   :op (make-keyword (str:upcase op-name))
                   :arg (parse-integer arg-str))))

(defmethod print-object ((this instruction) out)
  "Override default printer for INSTRUCTION instances."
  (print-unreadable-object (this out :type t)
    (format out "~a ~d (×~d)" (ins-op this) (ins-arg this) (ins-count this))))

(defclass vm ()
  ((accumulator :accessor vm-ra      :initform 0   :type integer)
   (ip          :accessor vm-rip     :initform 0   :type integer)
   (program     :reader   vm-program :initform #() :type vector :initarg :program))
  (:documentation "The accumulator virtual machine modelled in this problem."))

(defun make-vm (program)
  "Create a VM to execute the specified PROGRAM, a list of assembly-language
   instructions."
  (make-instance 'vm :program (map 'vector #'make-instruction program)))

(defmethod print-object ((this vm) out)
  "Override default printer for VM instances."
  (print-unreadable-object (this out :type t)
    (format out "A:~d I:~d" (vm-ra this) (vm-rip this))))

(defmethod reset-vm ((this vm))
  "Reset THIS virtual machine to its initial state, with registers with values of
   0 and no instructions having been run."
  (setf (vm-ra this) 0)
  (setf (vm-rip this) 0)
  (loop for ins across (vm-program this)
        do (setf (ins-count ins) 0)))

(defmethod next-instruction ((this vm))
  "Return the instruction currently referenced by the instruction pointer of THIS
   virtual machine."
  (aref (vm-program this) (vm-rip this)))

(defmethod execute-next ((this vm))
  "Execute the instruction at the instruction pointer of THIS virtual machine."
  (let* ((ins (next-instruction this))
         (arg (ins-arg ins)))
    ;; By default, assume instructions increment the IP by 1.
    (incf (vm-rip this))
    (incf (ins-count ins))
    ;; As is tradition for emulators/VM's, here's the giant switch-case.
    (switch ((ins-op ins))
            (:nop nil)
            (:jmp (incf (vm-rip this) (1- arg)))
            (:acc (incf (vm-ra this) arg)))))

(defmethod execute-program ((this vm) &key cycles)
  "Execute the program in THIS virtual machine until termination.  Returns the
   value of the accumulator upon termination.  Optionally terminate execution
   once an instruction has been run CYCLES times."
  (reset-vm this)
  (loop while (and (< (vm-rip this) (length (vm-program this)))
                   (and cycles
                        (< (ins-count (next-instruction this)) cycles)))
        do (execute-next this)
        finally (return (vm-ra this))))

;; Initialise the VM from the assembly file.
(defparameter *vm* (make-vm (map-file #p"input/day8.txt" #'identity)))

;; Part 1: Determine the value of the accumulator immediately before any
;;         instruction is run for a second time.
(solution "Part 1: A = ~d~%" (execute-program *vm* :cycles 1))

(defun swap-instruction (ins)
  "Swap the NOP or JMP instruction INS to the opposite instruction, then run the
   VM on the loaded program for a single loop cycle.  If the program terminates,
   return the value in the accumulator; otherwise return NIL."
  (let ((original-op (ins-op ins)))
    ;; Swap the instruction for the opposite.
    (when (eq :nop original-op) (setf (ins-op ins) :jmp))
    (when (eq :jmp original-op) (setf (ins-op ins) :nop))
    ;; Run the program and determine if it reached the end.
    (execute-program *vm* :cycles 1)
    (setf (ins-op ins) original-op)
    (when (= (vm-rip *vm*) (length (vm-program *vm*)))
      (vm-ra *vm*))))

;; Part 2: Determine the value of the accumulator when the errant instruction is
;;         repaired.  This is a NOP or JMP instruction that, when changed to the
;;         other instruction with the same argument, will allow the program to
;;         terminate.  I've stuck with a simple brute-force approach to this
;;         problem, since the small size of the input means that it's not really
;;         worth attempting too many optimisations.
(solution "Part 2: A = ~d~%"
          (loop for ins across (vm-program *vm*)
             for op = (ins-op ins)
             when (member op '(:nop :jmp))
             do (when-let ((new-ra (swap-instruction ins)))
                  (return new-ra))))
