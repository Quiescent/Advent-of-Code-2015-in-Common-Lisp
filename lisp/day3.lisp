;;; day3 --- My solution to day3 -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day3
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day3)

;; # PART 1:

(defun day3-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (1+ (iter
        (with x = 0)
        (with y = 0)
        (with seen)
        (for move in-string (car input-elements))
        (when (eq move #\>) (incf x))
        (when (eq move #\v) (incf y))
        (when (eq move #\<) (decf x))
        (when (eq move #\^) (decf y))
        (when (not (member (cons x y) seen :test #'equal))
          (push (cons x y) seen))
        (finally (return (length seen))))))

;; # PART 2:

(defun day3-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with santa-x = 0)
    (with santa-y = 0)
    (with robot-x = 0)
    (with robot-y = 0)
    (with turn    = 'santa)
    (with seen)
    (for move in-string (car input-elements))
    (when (eq turn 'santa)
      (when (eq move #\>) (incf santa-x))
      (when (eq move #\v) (incf santa-y))
      (when (eq move #\<) (decf santa-x))
      (when (eq move #\^) (decf santa-y))
      (when (not (member (cons santa-x santa-y) seen :test #'equal))
        (push (cons santa-x santa-y) seen)))
    (when (eq turn 'robot)
      (when (eq move #\>) (incf robot-x))
      (when (eq move #\v) (incf robot-y))
      (when (eq move #\<) (decf robot-x))
      (when (eq move #\^) (decf robot-y))
      (when (not (member (cons robot-x robot-y) seen :test #'equal))
        (push (cons robot-x robot-y) seen)))
    (setf turn (if (eq turn 'santa) 'robot 'santa))
    (finally (return (length seen)))))

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
" expected-1 (day3-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day3-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day3-part-1"))
        (input-2 (file-lines "day3-part-1")))
    (format t "
Part 1: ~s
" (day3-part-1 input-1))
    (format t "
Part 2: ~s
" (day3-part-2 input-2))))

