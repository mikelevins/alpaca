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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* ((this-file *load-pathname*)
         (this-directory (pathname-directory this-file))
         (parent-directory (merge-pathnames "../" (make-pathname :directory this-directory))))))

(asdf:defsystem #:alpaca
  :serial t
  :description "alpaca: the programmable editor"
  :author "mikel evins <mevins@me.com>"
  :license "Apacahe 2.0"
  :depends-on (:cl-fad :cl-gap-buffer :cl-conspack :colorize)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "alpaca")))))

(defun load-alpaca ()
  (asdf:load-system :alpaca))

;;; (load-alpaca)

