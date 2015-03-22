;;;; ***********************************************************************
;;;;
;;;; Name:          bard-primitives.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       representation of vm primitives
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;                based on code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; primitives
;;; ---------------------------------------------------------------------

(defparameter *primitive-fns*
  '((+ 2 + t) (- 2 - t) (* 2 * t) (/ 2 / t)
    (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
    (|eq?| 2 eq) (|equal?| 2 equal) (|eqv?| 2 eql)
    (|not| 1 not) (|null?| 1 not)
    (|car| 1 car) (|cdr| 1 cdr)  (|cadr| 1 cadr) (|cons| 2 cons t)
    (|list| 1 list1 t) (|list| 2 list2 t) (|list| 3 list3 t)
    (|read| 0 bard-read nil t) (|end?| 1 end?) ;***
    (|write| 1 write nil t) (|display| 1 display nil t)
    (|newline| 0 newline nil t) (|compiler| 1 compiler t) 
    (|name!| 2 name! t t) (|random| 1 random t nil)))

(defstruct (prim (:type list)) 
  symbol n-args opcode always side-effects)

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))
