;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       Alpaca: a programmable word processor
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)


(defvar $alpaca-delivered? nil)

(require :asdf)

;;; ---------------------------------------------------------------------
;;; system definition
;;; ---------------------------------------------------------------------

(asdf:defsystem #:alpaca
  :serial t
  :description "Alpaca 2: A programmable word processor"
  :author "mikel evins <mevins@me.com>"
  :license "Proprietary"
  :depends-on (:cl-store :cl-fad)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "application")))))

;;; ---------------------------------------------------------------------
;;; system-loading
;;; ---------------------------------------------------------------------

(defun load-alpaca ()
  (asdf:oos 'asdf:load-op :alpaca))

;;; (load-alpaca)
;;; (in-package :alpaca)
