;;;; *********************************************************************** 
;;;; 
;;;; Name:          package.lisp 
;;;; Project:       alpaca: a programmable editor 
;;;; Purpose:       package definitions 
;;;; Author:        mikel evins 
;;;; Copyright:     2016 by mikel evins 
;;;; 
;;;; *********************************************************************** 
 
(in-package :cl-user) 
 
;;; --------------------------------------------------------------------- 
;;; the alpaca package 
;;; ---------------------------------------------------------------------
(defpackage #:alpaca
  (:use #:cl+qt #:trivial-gray-streams)
  (:export #:main))
