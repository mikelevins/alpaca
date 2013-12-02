;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delegate.lisp
;;;; Project:       Alpaca
;;;; Purpose:       application delegate
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

(defclass alpaca-app-delegate (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/applicationDidFinishLaunching: :void) ((self alpaca-app-delegate) notification)
  )

(objc:defmethod (#/newDocument: :void) ((self alpaca-app-delegate) notification)
  )
