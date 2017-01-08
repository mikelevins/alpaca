;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       alpaca: a programmable text editor
;;;; Purpose:       alpaca system definition
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
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
            ((:file "utils")
             ;;(:file "main")
             ;;(:file "globals")
             ;;(:file "classes")
             ;;(:file "documents")
             ;;(:file "projects")
             ;;(:file "views")
             ;;(:file "delegate")
             ;;(:file "listener")
             ;;(:file "events")
             ;;(:file "api")
             ))))

;;; (asdf:load-system :alpaca)
