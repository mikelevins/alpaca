;;;; ***********************************************************************
;;;;
;;;; Name:          bard-macros.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       representation and definition of bard macros
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;                based on code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; representing macros
;;; ---------------------------------------------------------------------

(defun bard-macro (symbol)
  (and (symbolp symbol) (get symbol 'bard-macro)))

(defmacro def-bard-macro (name parmlist &body body)
  `(setf (get ',name 'bard-macro)
         #'(lambda ,parmlist .,body)))

(defun bard-macro-expand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro (first x)))
      (bard-macro-expand
       (apply (bard-macro (first x)) (rest x)))
      x))

;;; ---------------------------------------------------------------------
;;; built-in bard macro definitions
;;; ---------------------------------------------------------------------

(def-bard-macro define (name &rest body)
  (if (atom name)
      `(|name!| (|set!| ,name . ,body) (|quote| ,name))
      (bard-macro-expand
       `(|define| ,(first name) 
                  (^ ,(rest name) . ,body)))))

(def-bard-macro |%let| (bindings &rest body)
  `((^ ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-bard-macro |let| (bindings &rest body)
  (if (null bindings)
      `(|begin| .,body)
      `(|%let| (,(first bindings))
               (|let| ,(rest bindings) . ,body))))

(def-bard-macro |and| (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(|if| ,(first args)
                  (|and| . ,(rest args))))))

(def-bard-macro |or| (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(|let| ((,var ,(first args)))
                     (|if| ,var ,var (|or| . ,(rest args))))))))

(def-bard-macro |cond| (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(|or| ,(first clauses) (|cond| .,(rest clauses))))
        ((starts-with (first clauses) '|else|)
         `(|begin| .,(rest (first clauses))))
        (t `(|if| ,(first (first clauses))
                  (|begin| .,(rest (first clauses)))
                  (|cond| .,(rest clauses))))))

(def-bard-macro |define| (name &rest body)
  (if (atom name)
      `(|begin| (|set!| ,name . ,body) (|quote| ,name))
      `(|define| ,(first name) 
                 (^ ,(rest name) . ,body))))

(def-bard-macro |when| (&rest args)
  `(|if| ,(first args)
         (|begin| ,@(rest args))
         ,(nothing)))

(def-bard-macro |unless| (&rest args)
  `(|if| (|not| ,(first args))
         (|begin| ,@(rest args))
         ,(nothing)))
