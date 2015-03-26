;;;; ***********************************************************************
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard
;;;; Purpose:       printing bard values
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; ---------------------------------------------------------------------
;;; named literals
;;; ---------------------------------------------------------------------

(defmethod bard-print ((obj cl:null) &optional (out cl:*standard-output*))
  (format out "nothing"))

(defmethod bard-print ((obj undefined) &optional (out cl:*standard-output*))
  (format out "undefined"))

(defmethod bard-print ((obj true) &optional (out cl:*standard-output*))
  (format out "true"))

(defmethod bard-print ((obj false) &optional (out cl:*standard-output*))
  (format out "false"))

(defmethod bard-print ((obj end) &optional (out cl:*standard-output*))
  (format out "end"))

;;; ---------------------------------------------------------------------
;;; primitive values
;;; ---------------------------------------------------------------------

(defmethod bard-print (obj &optional (out cl:*standard-output*))
  (format out "~s" obj))

(defmethod bard-print ((obj cl:character) &optional (out cl:*standard-output*))
  (format out "#\\~a" obj))

(defmethod bard-print ((obj cl:string) &optional (out cl:*standard-output*))
  (format out "~s" obj))

(defmethod bard-print ((obj cl:symbol) &optional (out cl:*standard-output*))
  (if (keywordp obj)
      (format out ":~a" (symbol-name obj))
      (let ((pkg (symbol-package obj))
            (sname (symbol-name obj)))
        (if (eq pkg (find-package :bard))
            (format out "~a" (symbol-name obj))
            (format out "~a:~a"
                    (string-downcase (package-name pkg))
                    sname)))))



(defmethod bard-print ((obj cl:function) &optional (out cl:*standard-output*))
  (format out "#<primitive-method ~a>" (sys::function-name obj)))

;;; ---------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------

(defmethod bard-print ((obj structure) &optional (out cl:*standard-output*))
  (format out "~a" (or (name obj) "#<an anonymous structure>")))

(defmethod print-object ((obj structure)(out stream))
  (format out "~a" (or (name obj) "#<an anonymous structure>")))
