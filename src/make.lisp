;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          make.lisp
;;;; Project:       Alpaca - a near-minimal Cocoa application
;;;; Purpose:       Build the app's executable image
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(defun build-image (path)
  (save-application path :application-class 'alpaca::alpaca-application :prepend-kernel t))

