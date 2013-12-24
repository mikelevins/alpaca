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

(require :asdf)

;;; ---------------------------------------------------------------------
;;; system definition
;;; ---------------------------------------------------------------------

(asdf:defsystem #:alpaca
  :version "0.6"
  :serial t
  :description "Alpaca 2: A programmable word processor"
  :author "mikel evins <mevins@me.com>"
  :license "Proprietary"
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "utils")))))

;;; ---------------------------------------------------------------------
;;; system-loading
;;; ---------------------------------------------------------------------

(defun load-alpaca ()
  (asdf:oos 'asdf:load-op :alpaca))

;;; (load-alpaca)
;;; (in-package :alpaca)
