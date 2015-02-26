;;;; ***********************************************************************
;;;;
;;;; Name:          document.lisp
;;;; Project:       Alpaca
;;;; Purpose:       alpaca documents
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

(defclass alpaca-document (ns:ns-document)
  (tag
   scrollview
   filedata)
  (:metaclass ns:+ns-object))

(defclass alpaca-text-document (alpaca-document)
  (textview)
  (:metaclass ns:+ns-object))

(objc:defmethod (#/windowNibName :id) ((self alpaca-text-document))
  #@"AlpacaTextEditor")

