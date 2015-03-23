;;;; ***********************************************************************
;;;;
;;;; Name:          globals.lisp
;;;; Project:       Bard
;;;; Purpose:       support for global variables
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defclass bard-globals ()
  ((entries :reader entries :initform (make-hash-table :test #'eq)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun bard-globals ()(make-instance 'bard-globals))

(defun global-ref (varname)
  (let ((found (gethash varname (entries (bard-globals)) (undefined))))
    (if (defined? found)
        found
        (error "Undefined global variable ~S" varname))))

(defun global-set! (varname val)
  (setf (gethash varname (entries (bard-globals)))
        val))
