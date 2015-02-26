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

(require :asdf)
(require "OBJC-SUPPORT")

;;; ---------------------------------------------------------------------
;;; system definitions and loaders
;;; ---------------------------------------------------------------------

(defparameter *alpaca-root* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *assets-path* (merge-pathnames "assets/" *alpaca-root*))
(defparameter *assets-bundle-path* (merge-pathnames "bundle/" *assets-path*))
(defparameter *bundle-path* (merge-pathnames "Alpaca.app/" *alpaca-root*))
(defparameter *contents-path* (merge-pathnames "Contents/" *bundle-path*))
(defparameter *macos-path* (merge-pathnames "MacOS/" *contents-path*))
(defparameter *resources-path* (merge-pathnames "Resources/" *contents-path*))
(defparameter *en.lproj-path* (merge-pathnames "en.lproj/" *resources-path*))

(defun path (stem-path leaf-path)
  (merge-pathnames leaf-path stem-path))

(asdf:defsystem #:alpaca
  :name "alpaca"
  :version "1.0.0d1"
  :author "mikel evins"
  :license "Apache License v 2.0"
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

(defun build-alpaca ()
  (load-alpaca)
  (ensure-directories-exist *macos-path*)
  (ensure-directories-exist *en.lproj-path*)
  (copy-file (path *assets-bundle-path* "en.lproj/Credits.rtf")
             (path *en.lproj-path* "Credits.rtf"))
  ;;(build-image path)
  )

;;; (load-alpaca)
;;; (build-alpaca)
