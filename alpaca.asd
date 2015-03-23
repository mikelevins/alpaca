;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       alpaca system definition
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(asdf:defsystem #:alpaca
  :description "Describe alpaca here"
  :author "mikel evins <mevins@me.com>"
  :serial t
  :depends-on (:singleton-classes :fset)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:module "bard" :serial t
                                              :components ((:file "utils")
                                                           (:file "globals")
                                                           (:file "procedures")
                                                           (:file "named-literals")
                                                           (:file "prims")
                                                           (:file "compiler")
                                                           (:file "types")
                                                           (:file "macros")
                                                           (:file "quasiquote")
                                                           (:file "env")
                                                           (:file "printer")
                                                           (:file "assembler")
                                                           (:file "optimize")
                                                           (:file "vm")
                                                           (:file "reader")
                                                           (:file "bard")))
                                     (:file "alpaca")))))

;;; (asdf:load-system :alpaca)



