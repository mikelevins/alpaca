;;;; ***********************************************************************
;;;;
;;;; Name:          classes.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of bard's abstract classes
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; bard's classes are purely abstract types representing only a named
;;; role in protocols, and subtype/supertype relations with other
;;; types

;;; ---------------------------------------------------------------------
;;; bard-class
;;; ---------------------------------------------------------------------

;;; bard-class is the class used to represent
;;; bard classes. the bard structure 'class' constructs these.
(defclass bard-class (structure)
  ()
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((class bard-class) &rest initargs &key &allow-other-keys)
  (let ((directs (getf initargs :direct-supers nil)))
    (register-type (the-type-graph)
                   class
                   (remove-duplicates directs))))

(defmethod class? (x) nil)
(defmethod class? ((x bard-class)) t)

(defmethod print-object ((cls bard-class)(out stream))
  (princ (name cls) out))

(defmethod bard-print ((obj bard-class) &optional (out cl:*standard-output*))
  (format out "~a" (name obj)))

;;; ---------------------------------------------------------------------
;;; built-in classes
;;; ---------------------------------------------------------------------

(defparameter |Anything| (%construct-class 'bard::|Anything| nil))
(defparameter |Array| (%construct-class 'bard::|Array| (list |List|)))
(defparameter |Atom| (%construct-class 'bard::|Atom| (list |Anything|)))
(defparameter |Boolean| (%construct-class 'bard::|Boolean| (list |Unique|)))
(defparameter |Byte| (%construct-class 'bard::|Byte| (list |Integer|)))
(defparameter |Character| (%construct-class 'bard::|Character| (list |Atom|)))
(defparameter |Complex| (%construct-class 'bard::|Complex| (list |Number|)))
(defparameter |Condition| (%construct-class 'bard::|Condition| (list |Atom|)))
(defparameter |Container| (%construct-class 'bard::|Container| (list |Anything|)))
(defparameter |Event| (%construct-class 'bard::|Event| (list |Condition|)))
(defparameter |Extent| (%construct-class 'bard::|Extent| (list |Geometry|)))
(defparameter |Float| (%construct-class 'bard::|Float| (list |Real|)))
(defparameter |Geometry| (%construct-class 'bard::|Geometry| (list |Atom|)))
(defparameter |Integer| (%construct-class 'bard::|Integer| (list |Rational|)))
(defparameter |List| (%construct-class 'bard::|List| (list |Container|)))
(defparameter |Map| (%construct-class 'bard::|Map| (list |Container|)))
(defparameter |Mutable| (%construct-class 'bard::|Mutable| (list |Anything|)))
(defparameter |Name| (%construct-class 'bard::|Name| (list |Atom|)))
(defparameter |Number| (%construct-class 'bard::|Number| (list |Atom|)))
(defparameter |Pair| (%construct-class 'bard::|Pair| (list |List|)))
(defparameter |Point| (%construct-class 'bard::|Point| (list |Geometry|)))
(defparameter |Presentation| (%construct-class 'bard::|Presentation| (list |Container|)))
(defparameter |Procedure| (%construct-class 'bard::|Procedure| (list |Atom|)))
(defparameter |Rational| (%construct-class 'bard::|Rational| (list |Real|)))
(defparameter |Real| (%construct-class 'bard::|Real| (list |Number|)))
(defparameter |Stream| (%construct-class 'bard::|Stream| (list |Container|)))
(defparameter |String| (%construct-class 'bard::|String| (list |Vector|)))
(defparameter |Text| (%construct-class 'bard::|Text| (list |Vector|)))
(defparameter |Type| (%construct-class 'bard::|Type| (list |Atom|)))
(defparameter |Unique| (%construct-class 'bard::|Unique| (list |Type|)))
(defparameter |Vector| (%construct-class 'bard::|Vector| (list |Array| |List|)))
(defparameter |Window| (%construct-class 'bard::|Window| (list |Presentation|)))

