;;; day9 --- My solution to day9 -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day9
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day9)

;; # PART 1:

(defun day9-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((segments  (mapcar #'parse-line input-elements))
         (destinations (iter (for (start end distance) in segments)
                             (unioning (list start end))))
         (distances (iter (with distances = (make-hash-table :test #'equal))
                          (for (start end distance) in segments)
                          (setf (gethash (cons start end) distances) distance)
                          (setf (gethash (cons end start) distances) distance)
                          (finally (return distances))))
         (orderings (permutations destinations)))
    (iter
      (for ordering in orderings)
      (minimizing (iter
                    (for node in ordering)
                    (for p-node previous node)
                    (if-first-time (next-iteration))
                    (for distance = (gethash (cons p-node node) distances))
                    (when (not distance)
                      (return most-positive-fixnum))
                    (sum distance))))))

;; Wrong: 719 (too high!)

(defun permutations (xs)
  (labels ((iter (ys)
                 (if (null ys)
                     (list nil)
                     (apply #'append (mapcar (lambda (y) (mapcar (lambda (rest) (cons y rest)) 
                                                            (iter (remove y ys))))
                                             ys)))))
    (iter xs)))

(defun parse-line (line)
  (let ((words (ppcre:split " " line)))
    (list (read-from-string (car words))
          (read-from-string (caddr words))
          (read-from-string (car (last words))))))

;; # PART 2:

(defun day9-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((segments  (mapcar #'parse-line input-elements))
         (destinations (iter (for (start end distance) in segments)
                             (unioning (list start end))))
         (distances (iter (with distances = (make-hash-table :test #'equal))
                          (for (start end distance) in segments)
                          (setf (gethash (cons start end) distances) distance)
                          (setf (gethash (cons end start) distances) distance)
                          (finally (return distances))))
         (orderings (permutations destinations)))
    (iter
      (for ordering in orderings)
      (maximizing (iter
                    (for node in ordering)
                    (for p-node previous node)
                    (if-first-time (next-iteration))
                    (for distance = (gethash (cons p-node node) distances))
                    (when (not distance)
                      (return most-positive-fixnum))
                    (sum distance))))))

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
;; " expected-1 (day9-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day9-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day9-part-1"))
        (input-2 (file-lines "day9-part-1")))
    (format t "
Part 1: ~s
" (day9-part-1 input-1))
    (format t "
Part 2: ~s
" (day9-part-2 input-2))))

