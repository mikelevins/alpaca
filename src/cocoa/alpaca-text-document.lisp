;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          alpaca-text-document.lisp
;;;; Project:       Alpaca
;;;; Purpose:       plain-text document
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

(defclass alpaca-text-document (ns:ns-document)
  ((text :foreign-type :id))
  (:metaclass ns:+ns-object))

(objc:defmethod (#/setText: :void) 
    ((self alpaca-text-document)(new-text :id))
  (let ((old-text (slot-value self 'text))
        (new-text (#/retain new-text)))
    (unless (%null-ptr-p old-text)
      (#/release old-text))
    (setf (slot-value self 'text) new-text)
    (let ((str (#/string new-text)))
      (#_NSLog str))))

(objc:defmethod (#/readFromData:ofType:error: :<BOOL>) ((self alpaca-text-document)(data :id)(type-name :id)(err :id))
  (let* ((read-success nil)
         (file-contents (#/initWithData:options:documentAttributes:error: 
                         (#/alloc (objc:@class ns:ns-attributed-string))
                         data +null-ptr+ +null-ptr+ err)))
    (unless (%null-ptr-p file-contents)
      (setf read-success t)
      (#/setText: self file-contents)
      (#/release file-contents))
    read-success))

(objc:defmethod (#/dataOfType:error: :id) ((self alpaca-text-document)(type-name :id)(err :id))
  (#/breakUndoCoalescing text-view)
  (#/dataFromRange:documentAttributes:error: 
   text-view (ns-make-range 0 (#/length (#/text-storage text-view)))
   +null-ptr+ err))

