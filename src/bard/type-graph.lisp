;;;; ***********************************************************************
;;;;
;;;; Name:          type-graph.lisp
;;;; Project:       Bard
;;;; Purpose:       type taxonomy
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; the-type-graph
;;; =====================================================================
;;; stores taxonomic relationships among bard types

(defclass the-type-graph ()
  ((modified? :accessor modified? :initform t)
   (direct-supers :accessor direct-supers :initform (make-hash-table)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun the-type-graph ()
  (make-instance 'the-type-graph))

(defun register-type (type-graph type-object direct-supers)
  (setf (gethash type-object (direct-supers type-graph))
        direct-supers)
  (setf (modified? type-graph) t))

;;; currently no caching is being done, so this is an no-op
(defun update-precedence-lists! (type-graph) nil)

(defmethod direct-supertypes (x)
  (when (modified? (the-type-graph))
    (update-precedence-lists! (the-type-graph)))
  (gethash x (direct-supers (the-type-graph)) nil))

