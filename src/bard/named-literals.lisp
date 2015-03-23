;;;; ***********************************************************************
;;;;
;;;; Name:          named-literals.lisp
;;;; Project:       Bard
;;;; Purpose:       unique primitive values
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)


;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun undefined () (make-instance 'undefined))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defun nothing () nil)


;;; ---------------------------------------------------------------------
;;; true
;;; ---------------------------------------------------------------------

(defclass true ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun true () (make-instance 'true))

;;; ---------------------------------------------------------------------
;;; false
;;; ---------------------------------------------------------------------

(defclass false ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun false () (make-instance 'false))

;;; ---------------------------------------------------------------------
;;; end
;;; ---------------------------------------------------------------------

(defclass end ()()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun end () (make-instance 'end))

;;; ---------------------------------------------------------------------
;;; predicates
;;; ---------------------------------------------------------------------

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x undefined))(declare (ignore x)) (true))

(defmethod defined? (x)(declare (ignore x)) (true))
(defmethod defined? ((x undefined))(declare (ignore x)) nil)

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x null))(declare (ignore x)) (true))
(defmethod nothing? ((x undefined))(declare (ignore x)) (undefined))

(defmethod something? (x)(declare (ignore x)) (true))
(defmethod something? ((x null))(declare (ignore x)) nil)
(defmethod something? ((x undefined))(declare (ignore x)) (undefined))

(defmethod end? (x)(declare (ignore x)) nil)
(defmethod end? ((x end)) (true))
(defmethod end? ((x undefined))(declare (ignore x)) (undefined))

(defmethod true? (x)(declare (ignore x)) (true))
(defmethod true? ((x false))(declare (ignore x)) nil)
(defmethod true? ((x null))(declare (ignore x)) nil)
(defmethod true? ((x undefined))(declare (ignore x)) (undefined))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) (true))
(defmethod false? ((x null))(declare (ignore x)) (true))
(defmethod false? ((x undefined))(declare (ignore x)) (undefined))
