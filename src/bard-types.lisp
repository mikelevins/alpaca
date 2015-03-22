;;;; ***********************************************************************
;;;;
;;;; Name:          bard-types.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       representation of bard datatypes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; =====================================================================
;;; named literals
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; end
;;; ---------------------------------------------------------------------

(defclass end ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod print-object ((end end)(out stream))
  (princ "end" out))

(defun end ()(make-instance 'end))
(defmethod end? (x) nil)
(defmethod end? ((x end)) t)

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod print-object ((un undefined)(out stream))
  (princ "undefined" out))

(defun undefined ()(make-instance 'undefined))
(defmethod undefined? (x) nil)
(defmethod undefined? ((x undefined)) t)
(defmethod defined? (x) t)
(defmethod defined? ((x undefined)) nil)

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defclass nothing ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod print-object ((un nothing)(out stream))
  (princ "nothing" out))

(defun nothing ()(make-instance 'nothing))
(defmethod nothing? (x) nil)
(defmethod nothing? ((x nothing)) t)
(defmethod something? (x) t)
(defmethod something? ((x nothing)) nil)


;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(defclass true ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defclass false ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod print-object ((true true)(out stream))
  (princ "true" out))

(defmethod print-object ((false false)(out stream))
  (princ "false" out))

(defun true ()(make-instance 'true))
(defun false ()(make-instance 'false))
(defmethod true? (x) t)
(defmethod true? ((x false)) nil)
(defmethod true? ((x null)) nil)
(defmethod true? ((x nothing)) nil)
(defmethod true? ((x undefined)) (error "undefined is neither true nor false"))
(defmethod false? (x) nil)
(defmethod false? ((x false)) t)
(defmethod false? ((x nothing)) t)
(defmethod false? ((x null)) t)
(defmethod false? ((x undefined)) (error "undefined is neither true nor false"))

;;; ---------------------------------------------------------------------
;;; all the named literals
;;; ---------------------------------------------------------------------

(defparameter +named-literals+
  (list (end)
        (undefined)
        (nothing)
        (true)
        (false)))
