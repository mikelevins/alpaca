;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Alpaca
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:bard
  (:use #:cl)
  (:shadow #:compile #:debug #:function #:make-method #:method #:optimize #:symbol))

(defpackage #:alpaca
  (:use #:cl))
