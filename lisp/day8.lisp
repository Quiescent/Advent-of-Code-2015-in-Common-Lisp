;;; day8 --- My solution to day8 -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day8
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day8)

;; # PART 1:

(defun day8-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (summing (length line) into originals)
    (summing (length (read-from-string (ppcre:regex-replace-all "\\\\x[0-9a-f][0-9a-f]" line "-")))
             into parsed)
    (finally (return (- originals parsed)))))

;; # PART 2:

(defun day8-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (summing (length line) into originals)
    (summing (length (format nil "~s" line)) into formatted)
    (finally (return (- formatted originals)))))

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
" expected-1 (day8-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day8-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day8-part-1"))
        (input-2 (file-lines "day8-part-1")))
    (format t "
Part 1: ~s
" (day8-part-1 input-1))
    (format t "
Part 2: ~s
" (day8-part-2 input-2))))

