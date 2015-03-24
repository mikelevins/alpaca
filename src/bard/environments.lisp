;;;; ***********************************************************************
;;;;
;;;; Name:          environments.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defun empty-environment () nil)

(defun find-binding (env varname)
  (assoc varname env :test #'eq))

(defun set-binding-value! (binding val)
  (setf (cdr binding) val))

(defun env-ref (env varname)
  (let ((binding (find-binding env varname)))
    (if binding
        (cdr binding)
        (error "No such variable ~S in environment" varname))))

(defun env-set! (env varname val)
  (let ((binding (find-binding env varname)))
    (if binding
        (setf (cdr binding) val)
        (error "No such variable ~S in environment" varname))))

(defun add-binding (env varname val)
  (cons (cons varname val)
        env))

(defun add-bindings (env var/val-alist)
  (append var/val-alist env))


