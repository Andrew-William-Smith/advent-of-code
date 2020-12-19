(uiop:define-package #:advent-of-code/day19
    (:use #:cl #:alexandria #:advent-of-code/common)
  (:export #:parse #:part1 #:run1 #:part2 #:run2))

(in-package #:advent-of-code/day19)

(defun parse-rule (rule)
  "Parse the specified RULE definition, returning the rule number and a list of
   options that the rule may match."
  (let ((separator-idx (position #\: rule)))
    (values
     (parse-integer rule :end separator-idx)
     (mapcar [mapcar [if (str:digitp %) (parse-integer %) (str:substring 1 -1 %)]
                     (str:words %)]
             (str:split " | " rule :start (1+ separator-idx))))))

(defun parse (filename)
  (let ((rules (make-hash-table)))
    (with-open-file (in (file-in-system filename))
      ;; Begin by reading the rules until a blank line is encountered.
      (loop for line = (read-line in nil "")
            until (str:emptyp line)
            do (multiple-value-bind (rule-num options)
                   (parse-rule line)
                 (setf (gethash rule-num rules) options)))
      ;; Next, read the messages, which are just simple strings.
      (cons rules
            (loop for line = (read-line in nil nil)
                  while line
                  collect line)))))

(defun match (rules string &optional (rule 0) (index 0))
  "Determine the lengths of all prefixes of the specified STRING that match the
   specified set of RULES."
  (cond
    ;; We reached the end of a rule, so it matched in full.
    ((null rule) (list index))
    ;; We have reached the end of the string but not the rule, so no match.
    ((> index (length string)) nil)
    ;; Integers must reference other rules.
    ((numberp rule) (match rules string (gethash rule rules) index))
    ;; Strings must match in full to succeed.
    ((stringp rule)
     (when (equal rule (str:substring index (+ index (length rule)) string))
       (list (+ index (length rule)))))
    ;; Conjunctive rules must successfully match all terms.
    ((and (listp rule) (> (+ index (length rule)) (length string))) nil)
    ((and (listp rule) (atom (car rule)))
     (loop for next in (match rules string (car rule) index)
           append (match rules string (cdr rule) next)))
    ;; Disjunctive rules may match any term, but must match it in full.
    (t (loop for option in rule
             append (match rules string option index)))))

(defun full-match-p (rules string)
  "Determine whether the specified STRING can be fully matched by the specified
   set of RULES, meaning that one of the substring matches is the string itself."
  (let ((matches (match rules string)))
    (and matches
         (= (length string) (apply #'max matches)))))

(defun count-matches (rules messages)
  "Count the number of MESSAGES that completely match the specified ruleset
   RULES.  In order for a message to completely match a rule, every character
   must be matched by a terminal (string) rule beginning at rule 0."
  (loop for msg in messages
        count (full-match-p rules msg)))

(define-solution 1 (input) (#p"input/day19.txt")
  "Determine the number of messages that match the specified ruleset."
  (destructuring-bind (rules . messages) (parse input)
    (count-matches rules messages)))

(define-solution 2 (input) (#p"input/day19.txt")
  "Determine the number of messages that match the specified ruleset, with the
   addition of recursive rules for 8 and 11."
  (destructuring-bind (rules . messages) (parse input)
    (setf (gethash 8 rules) '((42) (42 8)))
    (setf (gethash 11 rules) '((42 31) (42 11 31)))
    (count-matches rules messages)))
