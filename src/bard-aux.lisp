;;;; ***********************************************************************
;;;;
;;;; Name:          bard-types.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       auxiliary functions from Norvig
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;                based on code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; =====================================================================
;;; auxiliary functions
;;; =====================================================================

(defun length=1 (x)
  (equal 1 (length x)))

(defun rest2 (x)
  (rest (rest x)))

(defun rest3 (x)
  (rest (rest (rest x))))

(defun starts-with (list x)
  (and (consp list)
       (eql (first list) x)))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))
(defun display (x) (princ x))
(defun newline () (terpri))

