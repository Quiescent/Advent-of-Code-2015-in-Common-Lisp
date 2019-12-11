;;; day10 --- My solution to day10 -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day10
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day10)

;; # PART 1:

(defun day10-part-1 (input)
  "Run my solution to part one of the problem on the input in INPUT."
  (let ((digits (map 'list (lambda (c) (read-from-string (string c))) (format nil "~a" input))))
    (iter
      (for i from 0 below 40)
      (setf digits
            (iter
              (while digits)
              (for digit = (pop digits))
              (for count = 1)
              (iter
                (while (eq (car digits) digit))
                (incf count)
                (pop digits))
              (appending (list count digit))))
      (finally (return (length digits))))))

;; # PART 2:

(defun day10-part-2 (input)
  "Run my solution to part two of the problem on the input in INPUT."
  (let ((digits (map 'list (lambda (c) (read-from-string (string c))) (format nil "~a" input))))
    (iter
      (for i from 0 below 50)
      (setf digits
            (iter
              (while digits)
              (for digit = (pop digits))
              (for count = 1)
              (iter
                (while (eq (car digits) digit))
                (incf count)
                (pop digits))
              (appending (list count digit))))
      (finally (return (length digits))))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day10-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day10-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 "1321131112")
        (input-2 "1321131112"))
    (format t "
Part 1: ~s
" (day10-part-1 input-1))
    (format t "
Part 2: ~s
" (day10-part-2 input-2))))

