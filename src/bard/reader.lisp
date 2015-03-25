;;;; ***********************************************************************
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defparameter *bard-readtable*
  (let ((table (copy-readtable)))
    (setf (readtable-case table) :preserve)
    table))

(defmethod input->bard-value (x) x)
(defmethod input->bard-value ((x (eql 'bard::|undefined|)))(undefined))
(defmethod input->bard-value ((x null))(nothing))
(defmethod input->bard-value ((x (eql 'bard::|nothing|)))(nothing))
(defmethod input->bard-value ((x (eql 'bard::|end|)))(end))
(defmethod input->bard-value ((x (eql 'bard::|true|)))(true))
(defmethod input->bard-value ((x (eql 'bard::|false|)))(false))

(defmethod input->bard-value ((x cons))
  (mapcar #'input->bard-value
          x))

(defun bard-read (&optional (stream cl:*standard-input*)(eof-error-p t) eof-value recursive-p)
  (let* ((*readtable* *bard-readtable*)
         (*package* (find-package :bard))
         (input (read stream eof-error-p eof-value recursive-p)))
    (input->bard-value input)))

(defun bard-read-from-string (str)
  (with-input-from-string (in str)
    (bard-read in)))
