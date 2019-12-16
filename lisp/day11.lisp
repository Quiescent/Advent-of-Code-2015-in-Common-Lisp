;;; day11 --- My solution to day11 -*-

;;; Commentary:
;; My solution to advent of code: day11

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day11
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day11)

;; # PART 1:

(defun day11-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((chars (chars-to-numbers input-elements)))
    (iter
      (for i from 0 below 1000000)
      (while (not (valid-password chars)))
      ;(format t "Current password: ~a~%" (normalised-to-string chars))
      (setf chars (increment chars)))
    (normalised-to-string chars)))

;; Wrong: cqkaabcc

(defun valid-password (chars)
  (iter
    (with two-runs = (make-hash-table :test #'equal))
    (with ascends)
    (for char in-vector chars)
    (for p-char previous char)
    (for pp-char previous p-char)
    (when (eq char p-char)
      (setf (gethash char two-runs) t))
    (when (and p-char pp-char
               (eq 1 (- char p-char))
               (eq 1 (- p-char pp-char)))
      (setf ascends t))
    (when (member char (list (to-normalised-code #\i)
                             (to-normalised-code #\o)
                             (to-normalised-code #\l)))
      (return nil))
    (finally
     (return (and ascends
                  (>= (hash-table-count two-runs) 2))))))

(defun increment (chars)
  (let* ((len (length chars))
         (current-value (aref chars (1- len)))
         (carry (floor (1+ current-value) 26)))
    (setf (aref chars (1- len)) (mod (1+ current-value) 26))
    (iter
      (for j from (- len 2) downto 0)
      (while (> carry 0))
      (for current-value = (aref chars j))
      (setf (aref chars j) (mod (1+ current-value) 26))
      (setf carry (floor (1+ current-value) 26)))
    chars))

(defun chars-to-numbers (str)
  (iter
    (for char in-string str)
    (collect (to-normalised-code char)
      :result-type 'vector)))

(defun normalised-to-string (chars)
  (iter
    (for char in-vector chars)
    (collect (code-char (+ char (char-code #\a)))
      :result-type 'string)))

(defun to-normalised-code (char)
  (- (char-code char) (char-code #\a)))

;; # PART 2:

(defun day11-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (day11-part-1
   (normalised-to-string
    (increment
     (chars-to-numbers
      (day11-part-1 input-elements))))))

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
;; " expected-1 (day11-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day11-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (format t "
Part 1: ~s
" (day11-part-1 "cqjxjnds"))
  (format t "
Part 2: ~s
" (day11-part-2 "cqjxjnds")))

