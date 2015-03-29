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
                        :components ((:file "package")
                                     (:module "bard" :serial t
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
                                                           (:file "singletons")
                                                           (:file "types")
                                                           (:file "functions")
                                                           (:file "printer")
                                                           (:file "bard")
                                                           ))
                                     (:file "alpaca")
                                     (:file "util-editor")
                                     (:file "bard-listener")
                                     (:file "document-window")
                                     (:file "app")
                                     ))))

(defun load-alpaca ()
  (asdf:load-system :alpaca))

;;; (load-alpaca)
;;; (bard-internal::repl)
;;; (setf $win (capi:contain (make-instance 'alpaca::bard-listener)))

;;; TODO: invent an apropos feature for the Bard subsystem
;;; TODO: implement #<type> as a reader macro for (with-intended-type (type) ...)
