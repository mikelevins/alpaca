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

(defmethod bard-print ((obj cl:cons) &optional (out cl:*standard-output*))
  (format out "(")
  (if (null (cdr obj))
      (progn (bard-print (car obj) out)
             (format out ")"))
      (progn (bard-print (car obj) out)
             (dolist (it (cdr obj))
               (format out " ")
               (bard-print it out))
             (format out ")"))))

(defmethod bard-print ((obj cl:vector) &optional (out cl:*standard-output*))
  (format out "#(")
  (let ((len (cl:length obj)))
    (cond
      ((zerop len) nil)
      ((= len 1)(bard-print (elt obj 0) out))
      (t (progn (bard-print (elt obj 0) out)
                (loop for i from 1 below len
                   do (progn (format out " ")
                             (bard-print (elt obj i) out)))))))
  (format out ")"))

(defmethod bard-print ((obj fset:wb-seq) &optional (out cl:*standard-output*))
  (format out "(")
  (let ((len (fset:size obj)))
    (cond
      ((zerop len) nil)
      ((= len 1)(bard-print (fset:@ obj 0) out))
      (t (progn (bard-print (fset:@ obj 0) out)
                (loop for i from 1 below len
                   do (progn (format out " ")
                             (bard-print (fset:@ obj i) out)))))))
  (format out ")"))


(defmethod bard-print ((obj cl:function) &optional (out cl:*standard-output*))
  (format out "#<primitive-method ~a>" (sys::function-name obj)))

;;; ---------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------

(defmethod bard-print ((obj structure) &optional (out cl:*standard-output*))
  (format out "~a" (or (name obj) "#<structure 'anonymous'>")))

(defmethod print-object ((obj structure)(out stream))
  (format out "~a" (or (name obj) "#<structure 'anonymous'>")))


(defmethod print-object ((fn bard-function)(out stream))
  (if (rest-parameter? fn)
      (format out "(-> ~{~a~^ ~} &)" (input-classes fn))
      (format out "(-> ~{~a~^ ~})" (input-classes fn))))

(defmethod bard-print ((fn bard-function) &optional (out cl:*standard-output*))
  (when (name fn)
    (format out "#<function ~a> " (name fn)))
  (if (rest-parameter? fn)
      (format out "(-> ~{~a~^ ~} &)" (input-classes fn))
      (format out "(-> ~{~a~^ ~})" (input-classes fn))))


(defmethod bard-print ((obj bard-method) &optional (out cl:*standard-output*))
  (when (name obj)
    (format out "#<method ~a> " (name obj)))
  (if (method-expression obj)
      (format out "~a" (method-expression obj))
      (format out "#<~a>"
              (if (name obj)
                  (format nil "method ~a" (name obj))
                  (format nil "an anonymous method")))))

(defmethod print-object ((obj text)(out stream))
  (format out "#<text> \"~a\"" (fset:convert 'cl:string (text-data obj))))

(defmethod bard-print ((obj text) &optional (out cl:*standard-output*))
  (format out "#<text> \"~a\"" (fset:convert 'cl:string (text-data obj))))

(defmethod bard-print ((obj rectangular-extent) &optional (out cl:*standard-output*))
  (format out "#<rectangular-extent> {:width ~A :height ~A}"
          (extent-width obj)(extent-height obj)))

(defmethod print-object ((obj rectangular-extent)(out stream))
  (format out "#<rectangular-extent> {:width ~A :height ~A}"
          (extent-width obj)(extent-height obj)))


(defmethod bard-print ((obj rectangular-point) &optional (out cl:*standard-output*))
  (format out "#<rectangular-point> {:x ~A :y ~A}"
          (point-x obj)(point-y obj)))

(defmethod print-object ((obj rectangular-point)(out stream))
  (format out "#<rectangular-point> {:x ~A :y ~A}"
          (point-x obj)(point-y obj)))
