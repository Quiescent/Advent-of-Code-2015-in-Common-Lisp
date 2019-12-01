;;; day1 --- My solution to day1 -*-

;;; Commentary:
;; My solution to advent of code: day1

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day1
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day1)

;; # PART 1:

(defun day1-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for char in-string (car input-elements))
    (count (eq char #\() into up)
    (count (eq char #\)) into down)
    (finally (return (- up down)))))



;; # PART 2:

(defun day1-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with floor = 0)
    (with count = 1)
    (for char in-string (car input-elements))
    (when (eq char #\()
      (incf floor))
    (when (eq char #\))
      (decf floor))
    (when (eq floor -1)
      (return count))
    (incf count)))

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
" expected-1 (day1-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day1-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day1-part-1"))
        (input-2 (file-lines "day1-part-1")))
    (format t "
Part 1: ~s
" (day1-part-1 input-1))
    (format t "
Part 2: ~s
" (day1-part-2 input-2))))

