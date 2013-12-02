;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          application.lisp
;;;; Project:       Alpaca: a programmable word processor
;;;; Purpose:       the main application and menu
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

;;; ---------------------------------------------------------------------
;;; <alpaca-application>
;;; ---------------------------------------------------------------------

(define-interface <alpaca-application> (capi:cocoa-default-application-interface)
  ;; -- slots ---------------------------------------------
  ((windows :accessor %alpaca-windows :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes)

  ;; -- menus ---------------------------------------------
  (:menus
   (application-menu "Alpaca"
                     ((:component (("About Alpaca" :callback 'about-alpaca2
                                                   :callback-type :none)))
                      (:component (("Preferences...")))
                      (:component () :name :application-services) ; Cocoa Services menu
                      (:component (("Hide Alpaca" :accelerator "accelerator-h"
                                                  :callback-data :hidden)
                                   ("Hide Others" :accelerator "accelerator-meta-h"
                                                  :callback-data :others-hidden)
                                   ("Show All" :callback-data :all-normal))
                                  :callback #'(setf capi:top-level-interface-display-state)
                                  :callback-type :data-interface)
                      (:component (("Quit Alpaca" :accelerator "accelerator-q"
                                                  :callback 'capi:destroy
                                                  :callback-type :interface)))))
   (file-menu "File" ((:component (("New" :enabled-function (constantly nil)) 
                                   ("Open..." :enabled-function (constantly nil))
                                   ("Open Recent" :enabled-function (constantly nil))))
                      (:component (("Close" :enabled-function (constantly nil))
                                   ("Save" :enabled-function (constantly nil)) 
                                   ("Save As..." :enabled-function (constantly nil))
                                   ("Revert to Saved" :enabled-function (constantly nil))))
                      (:component (("Page Setup..." :enabled-function (constantly nil))
                                   ("Print" :enabled-function (constantly nil))))))
   (edit-menu "Edit"
              ((:component (("Undo" :enabled-function (constantly nil))
                            ("Redo" :enabled-function (constantly nil))))
               (:component (("Cut" :enabled-function (constantly nil)) 
                            ("Copy" :enabled-function (constantly nil))
                            ("Paste" :enabled-function (constantly nil))
                            ("Select All" :enabled-function (constantly nil))))))
   (list-menu "List" 
              ((:component (("Add Row" :enabled-function (constantly nil))
                            ("Delete Row" :enabled-function (constantly nil))))
               (:component (("Add Column" :enabled-function (constantly nil))
                            ("Delete Column" :enabled-function (constantly nil))
                            ("Rename Column..." :enabled-function (constantly nil))))
               (:component (("Show Deleted Items" :enabled-function (constantly nil))
                            ("Purge Deleted Items" :enabled-function (constantly nil))))))
   
   (font-menu "Font" 
              ((:component (("Bigger" :enabled-function (constantly nil))
                            ("Smaller" :enabled-function (constantly nil)))))))
  
  (:menu-bar file-menu edit-menu list-menu font-menu)
  ;; -- layouts ---------------------------------------------
  (:layouts)

  ;; -- defaults ---------------------------------------------
  (:default-initargs :application-menu 'application-menu
    :create-callback (lambda (intf?) )))

;;; ---------------------------------------------------------------------
;;; menu callbacks
;;; ---------------------------------------------------------------------

(defun about-alpaca2 ()
  (display-message-on-screen 
   (convert-to-screen nil)
   (format nil "Alpaca 1.0d1")))

;;; (contain (make-instance '<alpaca-application>))
