;;;; ***********************************************************************
;;;;
;;;; Name:          alpaca.lisp
;;;; Project:       a programmable editor
;;;; Purpose:       alpaca main
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:alpaca)

(defclass alpaca-application (ccl::application)
  ())

(defparameter *alpaca* nil)

(defun alpaca-toplevel ()
  (let* ((nsbundle (#/mainBundle ns:ns-bundle))
         (nsbundle-path (#/bundlePath nsbundle))
         (bundle-path-string (objc:lisp-string-from-nsstring nsbundle-path))
         (bundle-path (pathname (concatenate 'string bundle-path-string "/")))
         (macos-directory (merge-pathnames "Contents/MacOS/" bundle-path))
         (framework-directory (merge-pathnames "Contents/Frameworks/XUL.framework/" bundle-path))
         (resource-directory (merge-pathnames "Contents/Resources/" bundle-path))
         (xulrunner (namestring (merge-pathnames "xulrunner" framework-directory)))
         (appfile (namestring (merge-pathnames "application.ini" resource-directory)))
         (app-args (list appfile))
         )
    (format t "~%Running ~S ~S ~%" xulrunner app-args)
    (ccl:run-program xulrunner app-args)))


