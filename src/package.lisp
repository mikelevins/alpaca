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
  (:shadow #:display #:optimize))

(defpackage #:alpaca
  (:use #:cl #:capi #:bard))

