(uiop:define-package #:advent-of-code/day18
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day18)

(defun tokenise (expr &optional (index 0) tokens (int-start 0))
  "Tokenise the expression EXPR to a form that can be interpreted by the
   expression evaluator."
  (flet ((tokens-with-int ()
           (if (/= index int-start)
               (cons (parse-integer expr :start int-start :end index) tokens)
               tokens))
         (next-char (new-tokens)
           (tokenise expr (1+ index) new-tokens (1+ index))))
    (if (= index (length expr))
        (reverse (tokens-with-int))
        (switch ((char expr index) :test #'char=)
          (#\Space (next-char (tokens-with-int)))
          (#\+ (next-char (cons :plus tokens)))
          (#\* (next-char (cons :times tokens)))
          (#\🎄 (next-char (cons :tree tokens)))
          (#\( (next-char (cons :lparen tokens)))
          (#\) (next-char (cons :rparen (tokens-with-int))))
          (t (tokenise expr (1+ index) tokens int-start))))))

(defun parse (filename)
  (map-file filename #'tokenise))

(defun eval-operator (operator values)
  "Return the value pushed onto the stack of VALUES when the specified dyadic
   OPERATOR is affected upon it."
  (let ((op1 (first values))
        (op2 (second values)))
    (eswitch (operator)
      (:plus (+ op1 op2))
      (:times (* op1 op2))
      (:tree (floor (+ op1 op2) 2)))))

(defun eval-expression (expr operators &optional output op-stack)
  "Return the integer value to which the expression EXPR evaluates when computed
   using the specified OPERATORS, an alist of the precedences of the operators
   that may occur in the expression.  This function is an implementation of the
   standard Shunting-Yard Algorithm as described by Dijkstra, although we
   maintain an output stack instead of a queue."
  (flet ((pop-operator ()
           (eval-expression expr operators
                            (cons (eval-operator (car op-stack) output)
                                  (cddr output))
                            (cdr op-stack))))
    (if-let ((token (car expr)))
      (cond
        ;; If the token is a number, push it to the output stack.
        ((numberp token)
         (eval-expression (cdr expr) operators (cons token output) op-stack))
        ;; If the token is a left parenthesis, push it to the operator stack.
        ((eq :lparen token)
         (eval-expression (cdr expr) operators output (cons token op-stack)))
        ;; If the token is a right parenthesis, pop any pending operators.
        ((and (eq :rparen token) (eq :lparen (car op-stack)))
         (eval-expression (cdr expr) operators output (cdr op-stack)))
        ((eq :rparen token) (pop-operator))
        ;; If the token is an operator, pop operators with higher/same precedence.
        ((and (car op-stack)
              (neq :lparen (car op-stack))
              (>= (cdr (assoc (car op-stack) operators))
                  (cdr (assoc token operators))))
         (pop-operator))
        (t (eval-expression (cdr expr) operators output (cons token op-stack))))
      ;; If there are no more tokens, evaluate any remaining operators.
      (if (emptyp op-stack)
          (car output)
          (pop-operator)))))

(define-solution 1 (input) ((parse #p"input/day18.txt"))
  "Determine the sum of the values of all expressions in the input when
   evaluated with both operators having equal precedence."
  (reduce #'+ (mapcar
               [eval-expression % '((:plus . 1) (:times . 1) (:tree . 3))]
               input)))

(define-solution 2 (input) ((parse #p"input/day18.txt"))
  "Determine the sum of the values of all expressions in the input when
   evaluated with addition having a higher precedence than multiplication."
  (reduce #'+ (mapcar
               [eval-expression % '((:plus . 2) (:times . 1) (:tree . 3))]
               input)))
