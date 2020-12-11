;;; Runner script for Advent of Code acceptance tests, run as a GitHub Action.
;;; This script is designed to be directly loaded along with Quicklisp by SBCL,
;;; and relies on the SBCL debugger being disabled so that test failures will
;;; cause the process to terminate with a non-zero exit code.

(ql:quickload :advent-of-code)
(ql:quickload :fiveam)
(setf fiveam:*on-error* :debug)
(setf fiveam:*on-failure* :debug)
(asdf:test-system 'advent-of-code)
