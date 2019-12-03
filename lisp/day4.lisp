;;; day4 --- My solution to day4 -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day4
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day4)

;; # PART 1:

(defun day4-part-1 (key)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for i from 0)
    (for res = (md5 (concatenate 'string key (format nil "~a" i))))
    (when (eq 5 (count-if (lambda (c) (eq c #\0)) res :end 5))
      (return i))))

;; # PART 2:

(defun day4-part-2 (key)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for i from 0)
    (for res = (md5 (concatenate 'string key (format nil "~a" i))))
    (when (eq 6 (count-if (lambda (c) (eq c #\0)) res :end 6))
      (return i))))

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
;; " expected-1 (day4-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day4-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 "iwrupvqb")
        (input-2 "iwrupvqb"))
    (format t "
Part 1: ~s
" (day4-part-1 input-1))
    (format t "
Part 2: ~s
" (day4-part-2 input-2))))

