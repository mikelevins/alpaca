;;;; ***********************************************************************
;;;;
;;;; Name:          macros.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of bard macros
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defclass bard-macros ()
  ((entries :reader entries :initform (make-hash-table :test #'eq))))

(defun bard-macros ()(make-instance 'bard-macros))

(defun bard-macro (x)
  (gethash x (entries (bard-macros)) nil))

(defun def-bard-macro (x expander)
  (setf (gethash x (entries (bard-macros)))
        expander))

(defun bard-macroexpand (x)
  (let ((expander (bard-macro (first x))))
    (if expander
        ($ expander x)
        (error "bard: no such macro ~S" (first x)))))

