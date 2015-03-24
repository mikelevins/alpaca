;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       Alpaca
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(defpackage #:bard-internal
  (:use #:cl)
  (:shadow #:compile #:function #:make-method #:method #:no-applicable-method #:structure)
  (:import-from :NET.BARDCODE.FOLIO2.FUNCTIONS #:$ #:^))

(defpackage #:bard
  (:use #:cl)
  (:export
   ;; special forms
   #:^
   #:$
   #:->
   #:|begin|
   #:|case|
   #:|cond|
   #:|define|
   #:|if|
   #:|quasiquote|
   #:|quote|
   #:|unquote|
   #:|unquote-splicing|
   ))

