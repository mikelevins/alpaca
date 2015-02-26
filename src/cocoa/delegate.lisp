;;;; ***********************************************************************
;;;;
;;;; Name:          delegate.lisp
;;;; Project:       Alpaca
;;;; Purpose:       application delegate
;;;; Author:        mikel evins
;;;; Copyright:     2011-2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

(defclass alpaca-app-delegate (ns:ns-object)
  ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/applicationDidFinishLaunching: :void) ((self alpaca-app-delegate) notification)
  (init-alpaca-keymaps)
  (load-alpaca-init-file))

(objc:defmethod (#/newDocument: :void) ((self alpaca-app-delegate) notification)
  (let ((doc-controller (#/sharedDocumentController (@class ns:ns-document-controller))))
    (:openUntitledDocumentOfType:display:
     doc-controller (%make-nsstring "NSStringPboardType")
     t)))
