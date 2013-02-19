;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Alpaca.lisp
;;;; Project:       The Alpaca Text Editor
;;;; Purpose:       build the Alpaca application image
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require "OBJC-SUPPORT")

;;; ---------------------------------------------------------------------
;;; dev-time path utils
;;; ---------------------------------------------------------------------

(let* ((path *load-truename*)
       (project-root (make-pathname :directory (pathname-directory path))))
  ;;; when the app is delivered, we redefine path-base to resolve
  ;;; paths relative to the app bundle
  (defun path-base () project-root))

(defun path (p)(merge-pathnames p (path-base)))

(defun add-to-asdf (path)
  (pushnew (truename (merge-pathnames path (path-base)))
           asdf:*central-registry* :test 'equalp))

;;; ---------------------------------------------------------------------
;;; system definitions and loaders
;;; ---------------------------------------------------------------------

(defpackage #:alpaca-asd
  (:use :cl :asdf))

(in-package :alpaca-asd)

(defsystem alpaca
  :name "alpaca"
  :version "0.6"
  :author "mikel evins"
  :description "Alpaca, the Programmable Editor"
  :serial t
  :components ((:module src :serial t
                        :components
                        ((:file "package")
                         (:module cocoa :serial t
                                  :components
                                  ((:file "cocoa")
                                   (:file "delegate")
                                   (:file "menus")
                                   (:file "main-menu")
                                   (:file "main")))
                         (:file "make")))))

(in-package :cl-user)

(defun load-alpaca ()
  (asdf::oos 'asdf:compile-op :alpaca)
  (asdf::oos 'asdf:load-op :alpaca))

(defun build-alpaca (path)
  (load-alpaca)
  (build-image path))

;;; (load-alpaca)
