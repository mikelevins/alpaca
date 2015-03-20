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
  (load-alpaca-init-file)
  (#/newDocument: self nil))

(objc:defmethod (#/newDocument: :void) ((self alpaca-app-delegate) notification)
  (let ((w (make-editor-window))
        (app (ccl::nsapp)))
    (#/makeKeyAndOrderFront: w app)))

(objc:defmethod (#/applicationOpenUntitledFile :void) ((self alpaca-app-delegate))
  (#/newDocument: self nil))
