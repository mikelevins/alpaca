;;;; ***********************************************************************
;;;;
;;;; Name:          document-window.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       windows for editor documents
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:alpaca)

(defparameter *bard-editor-font*
  (gp:make-font-description
   :family "Menlo" 
   :size 16
   :weight :medium                         
   :slant :roman))

(define-interface document-window (interface)
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes (document-pane editor-pane :reader document-pane
                         :buffer-modes '("Lisp")
                         :background (htmlcolor "181818")
                         :foreground (htmlcolor "dedede")))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(document-pane)))
  ;; -- defaults ---------------------------------------------
  (:default-initargs
      :title "Alpaca"
    :width 600 :height 600
    :create-callback (lambda (intf)
                       (let ((font (gp:find-best-font (document-pane intf)
                                                      *bard-editor-font*)))
                         (setf (simple-pane-font (document-pane intf)) font)))))

