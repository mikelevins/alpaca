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
  (:shadow #:display #:function #:method #:optimize))

(defpackage #:alpaca
  (:use #:cl #:capi #:bard))

