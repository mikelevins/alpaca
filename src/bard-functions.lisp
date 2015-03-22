;;;; ***********************************************************************
;;;;
;;;; Name:          bard-functions.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       representation bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; representing functions
;;; ---------------------------------------------------------------------

(defclass function ()
  ((name :accessor method-name :initform nil :initarg :name)
   (args :accessor method-args :initform nil :initarg :args)
   (methods :accessor methods :initform nil :initarg :methods)))
