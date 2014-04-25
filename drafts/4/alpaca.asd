;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       Alpaca: a programmable word processor
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; system definition
;;; ---------------------------------------------------------------------

(asdf:defsystem #:alpaca
  :serial t
  :description "Alpaca: A programmable word processor"
  :version "0.7"
  :author "mikel evins <mevins@me.com>"
  :license "Apache 2.0"
  :depends-on ()
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "cocoa")
                         (:file "delegate")
                         (:file "menus")
                         (:file "main-menu")
                         (:file "main")
                         (:file "make")))))

;;; ---------------------------------------------------------------------
;;; system-loading
;;; ---------------------------------------------------------------------

(defun load-alpaca ()
  (asdf:oos 'asdf:load-op :alpaca))

;;; (load-alpaca)
;;; (in-package :alpaca)
