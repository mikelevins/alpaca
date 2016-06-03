;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       alpaca system definition
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem alpaca
  :name "alpaca"
  :license "Apache 2.0"
  :description "A programmable editor"
  :serial t
  :depends-on (:qtools :qtcore :qtgui
                       :cl-ppcre
                       :trivial-gray-streams)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))

;;; (asdf:load-system :alpaca)
