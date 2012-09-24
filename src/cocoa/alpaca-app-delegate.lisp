;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          alpaca-app-delegate.lisp
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
  (let ((doc-controller (#/sharedDocumentController ns:ns-document-controller)))
    ))
