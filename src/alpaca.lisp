;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       alpaca main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:alpaca)

(capi:define-interface alpaca-application (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    "Alpaca"
    ((:component
      (("About Alpaca"
        :callback 'alpaca-about
        :callback-type :none)))
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      (("Hide Alpaca"
        :accelerator "accelerator-h"
        :callback-data :hidden)
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       ("Show All"
        :callback-data :all-normal))
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      (("Quit Alpaca"
        :accelerator "accelerator-q"
        :callback 'capi:destroy
        :callback-type :interface))))))
  (:menu-bar application-menu)
  (:default-initargs
      :title "Alpaca" :application-menu 'application-menu))

(defun alpaca-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Alpaca 1.0"))


(in-package :cl-user)

(defun alpaca-main ()
  (let ((application (make-instance 'alpaca::alpaca-application)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
