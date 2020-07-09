;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       alpaca: a programmable text editor
;;;; Purpose:       alpaca system definition
;;;; Author:        mikel evins
;;;; Copyright:     2017-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ---------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------

(require :asdf)

(asdf:defsystem #:alpaca
  :version "0.9"
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             ))))

;;; (asdf:load-system :alpaca)
