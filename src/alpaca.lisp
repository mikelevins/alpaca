;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       the singleton application-state object
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:alpaca)

(defclass alpaca ()
  ((bard :accessor get-bard :initform (bard-internal::bard)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod initialize-instance :after ((obj alpaca) &rest initargs &key &allow-other-keys)
  ())

(defun alpaca ()(make-instance 'alpaca))
