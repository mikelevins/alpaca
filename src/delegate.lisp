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
  (objc:with-autorelease-pool
    (let* ((windowmask (logior #$NSTitledWindowMask
                               #$NSClosableWindowMask
                               #$NSResizableWindowMask
                               #$NSMiniaturizableWindowMask)))
      (ns:with-ns-rect (rect 100 100 800 600)
        (let* ((w (#/autorelease
                   (#/initWithContentRect:styleMask:backing:defer:
                    (#/alloc (objc:@class ns-window)) rect windowmask #$NSBackingStoreBuffered #$NO)))
               (controller (#/autorelease (#/initWithWindow: (#/alloc (objc:@class ns-window-controller)) w)))
               (app (ccl::nsapp)))
          (#/makeKeyAndOrderFront: w app))))))

(objc:defmethod (#/applicationOpenUntitledFile :void) ((self alpaca-app-delegate))
  (#/newDocument: self nil))
