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

(require :asdf)
(pushnew "/usr/local/src/folio2/" asdf:*central-registry*)

(asdf:defsystem #:alpaca
  :description "Describe alpaca here"
  :author "mikel evins <mevins@me.com>"
  :serial t
  :depends-on (:singleton-classes :quri :folio2)
  :components ((:module "src"
                        :serial t
                        :components ((:module "bard" :serial t
                                              :components ((:file "package")
                                                           (:file "version")
                                                           (:file "named-literals")
                                                           (:file "reader")
                                                           (:file "environments")
                                                           (:file "macros")
                                                           (:file "compiler")
                                                           (:file "type-graph")
                                                           (:file "structures")
                                                           (:file "classes")
                                                           (:file "built-in-structures")
                                                           (:file "types")
                                                           (:file "functions")
                                                           (:file "printer")
                                                           (:file "bard")
                                                           ))
                                     (:file "package")
                                     (:file "alpaca")
                                     (:file "bard-listener")
                                     (:file "app")
                                     ))))

(defun load-alpaca ()
  (asdf:load-system :alpaca))

;;; (load-alpaca)
;;; (bard-internal::repl)
