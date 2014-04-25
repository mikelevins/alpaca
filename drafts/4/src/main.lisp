;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Project:       Alpaca - a programmable editor
;;;; Purpose:       Alpaca's main function
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :alpaca)

;;; ------------------------------------------------------------
;;; the Alpaca application class

(defclass alpaca-application (ccl::application) ())

;;; ignore the Finder's -psn argument
(defmethod ccl::parse-application-arguments ((a alpaca-application))
  (values nil nil nil nil))

;;; ------------------------------------------------------------
;;; the Alpaca main function
;;; the toplevel-function runs the Cocoa NSApp object

(defmethod toplevel-function ((app alpaca-application) init-file)
  (declare (ignore init-file))
  (ccl::with-autorelease-pool
      (let* ((app (ccl::nsapp))
             (delegate (#/autorelease (#/init (#/alloc alpaca-app-delegate)))))
        (#/setDelegate: app delegate)
        (setup-menus)
        (ccl::run-event-loop))))
