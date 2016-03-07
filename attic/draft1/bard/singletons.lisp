;;;; ***********************************************************************
;;;;
;;;; Name:          singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       management of singleton types
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; the-singleton-table
;;; =====================================================================
;;; stores all existing singletons

(defclass the-singleton-table ()
  ((instances :accessor instances :initform (make-hash-table :test #'equal)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun the-singleton-table ()
  (make-instance 'the-singleton-table))

(defun register-singleton (singleton-table value singleton-object)
  (setf (gethash value (instances singleton-table))
        singleton-object))

(defun find-singleton (x)
  (gethash x (instances (the-singleton-table)) nil))

