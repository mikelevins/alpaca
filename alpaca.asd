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
  :depends-on (:singleton-classes :com.informatimago.common-lisp.lisp-reader)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "bard-types")
                                     (:file "bard-aux")
                                     (:file "bard-env")
                                     (:file "bard-macros")
                                     (:file "bard-methods")
                                     (:file "bard-functions")
                                     (:file "bard-compiler")
                                     (:file "bard-quasiquote")
                                     (:file "bard-instructions")
                                     (:file "bard-assembler")
                                     (:file "bard-reader")
                                     (:file "bard-vm")
                                     (:file "bard-primitives")
                                     (:file "bard-opt")
                                     (:file "bard-repl")
                                     (:file "alpaca")))))

;;; (asdf:load-system :alpaca)
;;; (bard::bard)
