(asdf:defsystem #:advent-of-code
  :description "Solutions to Advent of Code problems from various years."
  :author "Andrew Smith (Andrew-William-Smith)"
  :license  "GNU GPLv3"
  :serial t
  :class :package-inferred-system
  :defsystem-depends-on (#:asdf-package-system)
  :depends-on (#:alexandria
               #:cl-ppcre
               #:str
               #:advent-of-code/package)
  :in-order-to ((test-op (test-op "advent-of-code/tests"))))

(asdf:defsystem #:advent-of-code/tests
  :class :package-inferred-system
  :defsystem-depends-on (#:asdf-package-system)
  :depends-on (#:advent-of-code
               #:fiveam
               #:advent-of-code/tests/package)
  :perform (test-op (o s) (uiop:symbol-call
                           :advent-of-code/tests :test-all)))
