;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          application.lisp
;;;; Project:       Alpaca: a programmable word processor
;;;; Purpose:       the basic editor pane
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

;;; ---------------------------------------------------------------------
;;; <editor>
;;; ---------------------------------------------------------------------

(defparameter +alpaca-default-font+
  (gp:make-font-description :family "Menlo" :size 14))

(defparameter +alpaca-default-face+
  (editor:make-face 'alpaca-default-face
                    :foreground :black
                    :background :white
                    :if-exists t
                    :font +alpaca-default-font+))

(define-interface <editor> ()
  ;; -- slots ---------------------------------------------
  ()

  ;; -- panes ---------------------------------------------
  (:panes
   (editor-pane rich-text-pane :reader editor-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout simple-layout '(editor-pane)))

  ;; -- defaults ---------------------------------------------
  (:default-initargs :create-callback 
      (lambda (intf))))

;;; (setf $ed (contain (make-instance '<editor>)))
;;; (setf $rtf "/Volumes/ymra/Archives/mikel_2012-7-10/Attic/Writing/FoL/cvs/FoL/draft4/Book\ II/1-5/01.rtf")
;;; (setf $ed (contain (make-instance 'rich-text-pane :filename $rtf)))
