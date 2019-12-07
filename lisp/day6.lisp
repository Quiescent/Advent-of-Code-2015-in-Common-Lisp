;;; day6 --- My solution to day6 -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day6
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day6)

;; # PART 1:

(defun day6-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with matrix = (make-array '(1000 1000) :initial-element nil))
    (for (command x1 y1 x2 y2) in (mapcar #'parse-line input-elements))
    ;(format t "~a ~a ~a ~a ~a~%" command x1 y1 x2 y2)
    (iter
      (for x from x1 to x2)
      (iter
        (for y from y1 to y2)
        (cond
          ((eq command 'toggle) (setf (aref matrix x y) (not (aref matrix x y))))
          ((eq command 'off)    (setf (aref matrix x y) nil))
          ((eq command 'on)     (setf (aref matrix x y) t)))))
    (finally (return (iter (for x from 0 below 1000)
                           (sum (iter (for y from 0 below 1000)
                                      (count (aref matrix x y)))))))))

(defun parse-line (line)
  (cons (cond
          ((search "toggle" line) 'toggle)
          ((search "off" line)    'off)
          ((search "on" line)     'on))
        (mapcar #'read-from-string
                (str:words (ppcre:regex-replace-all "[^0-9]" line " ")))))

;; # PART 2:

(defun day6-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with matrix = (make-array '(1000 1000) :initial-element 0))
    (for (command x1 y1 x2 y2) in (mapcar #'parse-line input-elements))
    (iter
      (for x from x1 to x2)
      (iter
        (for y from y1 to y2)
        (cond
          ((eq command 'toggle) (incf (aref matrix x y) 2))
          ((eq command 'off)    (decf (aref matrix x y) (if (eq (aref matrix x y) 0) 0 1)))
          ((eq command 'on)     (incf (aref matrix x y) 1)))))
    (finally (return (iter (for x from 0 below 1000)
                           (sum (iter (for y from 0 below 1000)
                                      (sum (aref matrix x y)))))))))

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
" expected-1 (day6-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day6-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day6-part-1"))
        (input-2 (file-lines "day6-part-1")))
    (format t "
Part 1: ~s
" (day6-part-1 input-1))
    (format t "
Part 2: ~s
" (day6-part-2 input-2))))

