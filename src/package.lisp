;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       alpaca main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage #:bard
  (:use #:cl)
  (:shadow #:debug #:display #:function #:make-method #:method #:optimize #:symbol))

(defpackage #:alpaca
  (:use #:cl #:capi #:bard))

