;;;; ***********************************************************************
;;;;
;;;; Name:          bard-methods.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       representation bard methods
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; representing methods
;;; ---------------------------------------------------------------------

(defclass method ()
  ((code :accessor method-code :initform nil :initarg :code)
   (env :accessor method-env :initform nil :initarg :env)
   (name :accessor method-name :initform nil :initarg :name)
   (args :accessor method-args :initform nil :initarg :args)))

(defmethod method? (x) nil)
(defmethod method? ((x method)) t)

(defun make-method (&key env name args code)
  (make-instance 'method :env env :name name :args args
                 :code (optimize code)))

(defun show-method (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (method? fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (method-code fn)))
          (let ((instr (elt (method-code fn) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-method arg stream (+ indent 8)))
                  (fresh-line))))))))
