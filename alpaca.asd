;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       alpaca system definition
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require :asdf)

(asdf:defsystem #:alpaca
    :description "a programmable editor"
    :author "mikel evins <mevins@me.com>"
    :license "Apache 2.0"
    :serial t
    :depends-on (:qtools :qtcore :qtgui)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "app")
                                       ))))

(defun load-alpaca ()
  (asdf:load-system :alpaca))

;;; (load-alpaca)
;;; (alpaca:main)
