;;;; ***********************************************************************
;;;;
;;;; Name:          app.lisp
;;;; Project:       alpaca: a programmable editor
;;;; Purpose:       application main
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)
(named-readtables:in-readtable :qtools)

;;; ---------------------------------------------------------------------
;;; editor-plain
;;; ---------------------------------------------------------------------
;;; the editing UI for plaintext

(define-widget editor-plain (QTextEdit)())

(define-initializer (editor-plain setup)
  (let ((font (q+:make-qfont "Monospace" 14)))
    (setf (q+:style-hint font) (q+:qfont.type-writer))
    (setf (q+:font editor-plain) font)))

(defun editor-plain-text (editor-plain)
  (q+:to-plain-text editor-plain))

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------

(define-widget frame (QWidget)())

(define-subwidget (frame editor-plain) (make-instance 'editor-plain))

(define-subwidget (frame layout) (q+:make-qvboxlayout frame)
  (setf (q+:window-title frame) "Untitled")
  (q+:add-widget layout editor-plain))

(define-initializer (frame setup)
  nil)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defparameter *main-window* nil)

(defun main ()
  (with-main-window (window (make-instance 'frame))
    (setf *main-window* window)))
