;;;; ***********************************************************************
;;;;
;;;; Name:          types.lisp
;;;; Project:       Bard
;;;; Purpose:       bard type protocol
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defgeneric type? (obj))

(defmethod type? (obj) nil)
(defmethod type? ((obj cl:null)) t)
(defmethod type? ((obj unique-instance)) t)
(defmethod type? ((obj structure)) t)
(defmethod type? ((obj cl:class)) t)
