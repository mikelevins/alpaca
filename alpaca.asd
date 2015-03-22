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
  :depends-on (:singleton-classes :fset :com.informatimago.common-lisp.lisp-reader)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "bard-auxfns")
                                     (:file "bard-interp1")
                                     (:file "bard-compile1")
                                     (:file "bard-compile2")
                                     (:file "bard-compile3")
                                     (:file "alpaca")))))

;;; (asdf:load-system :alpaca)
