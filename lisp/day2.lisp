;;; day2 --- My solution to day2 -*-

;;; Commentary:
;; My solution to advent of code: day2

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day2
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day2)

;; # PART 1:

(defun day2-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for (x y z) in input-elements)
    (sum (+ (* 2 x y)
            (* 2 y z)
            (* 2 z x)
            (min (* x y)
                 (* y z)
                 (* z x))))))

;; # PART 2:

(defun day2-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for (x y z) in input-elements)
    (for sorted = (sort (list x y z) #'<))
    (for (a b)  = sorted)
    (sum (+ (* 2 a)
            (* 2 b)
            (* x y z)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day2-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day2-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day2-part-1"))
        (input-2 (file-lines-numbers "day2-part-1")))
    (format t "
Part 1: ~s
" (day2-part-1 input-1))
    (format t "
Part 2: ~s
" (day2-part-2 input-2))))

