;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.asd
;;;; Project:       a programmable editor
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require :asdf)
(require :objc-support)

(asdf:defsystem #:alpaca
  :serial t
  :description "alpaca: the programmable editor"
  :author "mikel evins <mevins@me.com>"
  :license "Apacahe 2.0"
  :depends-on (:cffi :cl-fad :cl-gap-buffer :cl-conspack :colorize)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))

(defun load-alpaca ()
  (asdf:load-system :alpaca))

;;; (load-alpaca)

(defun build-alpaca ()
  (load-alpaca)
  (let* ((project-path (slot-value (asdf:find-system :alpaca) 'asdf/component:absolute-pathname))
         (bundle-path (merge-pathnames "alpaca.app/" project-path))
         (alpaca-path (merge-pathnames "Contents/MacOS/alpaca" bundle-path)))
    (ccl:save-application alpaca-path
                          :toplevel-function (intern "ALPACA-TOPLEVEL" (find-package :alpaca))
                          :application-class (find-class (intern "ALPACA-APPLICATION"
                                                                 (find-package :alpaca)))
                          :prepend-kernel t)))

;;; (load-alpaca)
;;; (build-alpaca)
