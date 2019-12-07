;;; day5 --- My solution to day5 -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day5
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day5)

;; # PART 1:

(defun day5-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (count (is-nice line))))

(defun is-nice (str)
  (and (iter
         (for char in-string str)
         (count (member char '(#\a #\e #\i #\o #\u)) into vowel-count)
         (finally (return (>= vowel-count 3))))
       (iter
         (for char in-string str)
         (for p-char previous char initially nil)
         (when (eq char p-char)
           (return t))
         (finally (return nil)))
       (iter
         (for char in-string str)
         (for p-char previous char)
         (when (or (and (eq char #\b)
                        (eq p-char #\a))
                   (and (eq char #\d)
                        (eq p-char #\c))
                   (and (eq char #\q)
                        (eq p-char #\p))
                   (and (eq char #\y)
                        (eq p-char #\x)))
           (return nil))
         (finally (return t)))))

;; # PART 2:

(defun day5-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for line in input-elements)
    (count (is-nice-2 line))))

(defun is-nice-2 (str)
  (and (iter
         (with pairs = (make-hash-table :test #'equal))
         (with pair-positions = (make-hash-table :test #'equal))
         (for i from 0)
         (for char in-string str)
         (for p-char previous char)
         (when (not (eq (gethash (cons char p-char) pair-positions) (1- i)))
           (setf (gethash (cons char p-char) pair-positions) i)
           (incf (gethash (cons char p-char) pairs 0)))
         (finally (return (iter (for (key value) in-hashtable pairs)
                                (when (>= value 2)
                                  (return t))))))
       (iter
         (for char in-string str)
         (for p-char previous char)
         (for pp-char previous p-char)
         (when (eq char pp-char)
           (return t)))))

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
" expected-1 (day5-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day5-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day5-part-1"))
        (input-2 (file-lines "day5-part-1")))
    (format t "
Part 1: ~s
" (day5-part-1 input-1))
    (format t "
Part 2: ~s
" (day5-part-2 input-2))))

