;;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       bard main entry point
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defparameter $bard-banner (format nil "bard ~a" *bard-version-number*))

(defun display-bard-prompt ()
  (format t "bard> "))

(defun bard-print (obj)
  )

(defun repl ()
  (format t "~%~a" $bard-banner)
  (init-bard-globals)
  (catch 'exit-bard
    (loop
       (display-bard-prompt)
       (let* ((input (bard-read))
              (thunk (compile input (empty-environment))))
         (bard-print thunk)
         (terpri)))))
