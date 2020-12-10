(asdf:defsystem #:advent-of-code
  :description "Solutions to Advent of Code problems from various years."
  :author "Andrew Smith (Andrew-William-Smith)"
  :license  "GNU GPLv3"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:str)
  :components ((:file "package")
               (:file "common")
               (:file "day1")
               (:file "day2")
               (:file "day3"))
  :in-order-to ((test-op (test-op "advent-of-code/tests"))))

(asdf:defsystem #:advent-of-code/tests
  :depends-on (#:advent-of-code
               #:fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "main")
                             (:file "test-day1")
                             (:file "test-day2")
                             (:file "test-day3"))))
  :perform (test-op (o s) (uiop:symbol-call
                           :advent-of-code-tests :test-all)))
