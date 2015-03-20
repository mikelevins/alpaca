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


;;; ---------------------------------------------------------------------
;;; the plain text view
;;; ---------------------------------------------------------------------

(defclass alpaca-text-view (ns:ns-text-view)
  ((document :foreign-type :id :accessor document))
  (:metaclass ns:+ns-object))

(objc:defmethod #/initWithFrame:textContainer:
    ((self alpaca-text-view) (frame ns:ns-rect) (container :id))
  (let ((new (call-next-method frame container)))
    (unless (%null-ptr-p new)
      (#/setAllowsUndo: new t))
    new))

;;; ---------------------------------------------------------------------
;;; the plain text document
;;; ---------------------------------------------------------------------

(defclass alpaca-document (ns:ns-document)
  ()
  (:metaclass ns:+ns-object))

;;; ---------------------------------------------------------------------
;;; editor window
;;; ---------------------------------------------------------------------

(defun editor-window-mask ()
  (logior #$NSTitledWindowMask
          #$NSClosableWindowMask
          #$NSResizableWindowMask
          #$NSMiniaturizableWindowMask))

(defun make-editor-window ()
  (objc:with-autorelease-pool
      (let* ((windowmask (editor-window-mask)))
        (ns:with-ns-rect (rect 100 100 800 600)
          (let* ((textview (#/initWithFrame: (#/alloc (objc:@class alpaca-text-view)) rect))
                 (w (#/initWithContentRect:styleMask:backing:defer:
                     (#/alloc (objc:@class ns-window)) rect windowmask #$NSBackingStoreBuffered #$NO))
                 (scrollview (#/initWithFrame: (#/alloc (objc:@class ns-scroll-view))
                                               (#/frame (#/contentView w))))
                 (controller (#/initWithWindow: (#/alloc (objc:@class ns-window-controller)) w)))
            (#/setHasHorizontalScroller: scrollview #$YES)
            (#/setHasVerticalScroller: scrollview #$YES)
            (#/setBorderType: scrollview #$NSNoBorder)
            (#/setAutoresizingMask: scrollview (logior #$NSViewWidthSizable #$NSViewHeightSizable))
            (#/setDocumentView: scrollview textview)
            (#/setContentView: w scrollview)
            (ns:with-ns-point (p 20 20)
              (#/cascadeTopLeftFromPoint: w p))
            w)))))
