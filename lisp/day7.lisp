;;; day7 --- My solution to day7 -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day7
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day7)

;; # PART 1:

(defun day7-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((wires          (mapcar #'parse-line input-elements))
         (start          (find-wire wires "a"))
         (*wire-results* (make-hash-table :test #'equal)))
    (resolve wires start)))

(defun find-wire (wires name)
  (find name wires :key #'cadr :test #'equal))

(defun is-number (str)
  (numberp (read-from-string str)))

(defun resolve-or-number (wires name)
  (if (is-number name)
      (read-from-string name)
      (resolve wires (find-wire wires name))))

(defvar *wire-results* nil)

(defun resolve (wires from)
  (progn
    (format t "~a~%" from)
    (or
     (gethash from *wire-results*)
     (setf (gethash from *wire-results*)
           (case (car from)
             (and    (logand (resolve-or-number wires (caddr from))
                             (resolve-or-number wires (cadddr from))))
             (or     (logior (resolve-or-number wires (caddr from))
                             (resolve-or-number wires (cadddr from))))
             (lshift (ash (resolve-or-number wires (caddr from))
                          (resolve-or-number wires (cadddr from))))
             (rshift (ash (resolve-or-number wires (caddr from))
                          (- 0 (resolve-or-number wires (cadddr from)))))
             (not    (lognot (resolve-or-number wires (caddr from))))
             (assign (if (is-number (caddr from))
                         (read-from-string (caddr from))
                         (resolve-or-number wires (caddr from)))))))))

(defun parse-line (line)
  (let ((type (cond
                ((position "AND"    line :test #'equal) 'and)
                ((position "OR"     line :test #'equal) 'or)
                ((position "LSHIFT" line :test #'equal) 'lshift)
                ((position "RSHIFT" line :test #'equal) 'rshift)
                ((position "NOT"    line :test #'equal) 'not)
                (t                                      'assign)))
        (dest (car (last line))))
    (case (length line)
      (3 (list type dest (car line)))
      (4 (list type dest (cadr line)))
      (5 (list type dest (car line) (caddr line))))))

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
" expected-1 (day7-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day7-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-words "day7-part-1"))
        (input-2 (file-lines-words "day7-part-2")))
    (format t "
Part 1: ~s
" (day7-part-1 input-1))
    (format t "
Part 2: ~s
" (day7-part-1 input-2))))

