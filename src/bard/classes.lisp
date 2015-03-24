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

;;; bard-class is the class used to represent
;;; bard classes. the bard structure 'class' constructs these.
(defclass bard-class (structure)
  ((direct-superclasses :accessor direct-superclasses :initarg :direct-superclasses))
  (:metaclass clos:funcallable-standard-class))

(defmethod print-object ((cls bard-class)(out stream))
  (princ (name cls) out))

(defmethod bard-print ((obj bard-class) &optional (out cl:*standard-output*))
  (format out "~a" (name obj)))

(defparameter |Anything| (%construct-class 'bard::|Anything| nil))
(defparameter |Stream| (%construct-class 'bard::|Stream| (list |Anything|)))
(defparameter |Collection| (%construct-class 'bard::|Collection| (list |Anything|)))
(defparameter |Atom| (%construct-class 'bard::|Atom| (list |Anything|)))
(defparameter |List| (%construct-class 'bard::|List| (list |Collection|)))
(defparameter |Type| (%construct-class 'bard::|Type| (list |Atom|)))
(defparameter |Procedure| (%construct-class 'bard::|Procedure| (list |Atom|)))
(defparameter |Name| (%construct-class 'bard::|Name| (list |Atom|)))
(defparameter |Character| (%construct-class 'bard::|Character| (list |Atom|)))
(defparameter |Condition| (%construct-class 'bard::|Condition| (list |Atom|)))
(defparameter |Number| (%construct-class 'bard::|Number| (list |Atom|)))
(defparameter |Pair| (%construct-class 'bard::|Pair| (list |List|)))
(defparameter |Array| (%construct-class 'bard::|Array| (list |List|)))
(defparameter |Map| (%construct-class 'bard::|Map| (list |List|)))
(defparameter |Unique| (%construct-class 'bard::|Unique| (list |Type|)))
(defparameter |Event| (%construct-class 'bard::|Event| (list |Condition|)))
(defparameter |Real| (%construct-class 'bard::|Real| (list |Number|)))
(defparameter |Complex| (%construct-class 'bard::|Complex| (list |Number|)))
(defparameter |Vector| (%construct-class 'bard::|Vector| (list |Array|)))
(defparameter |Boolean| (%construct-class 'bard::|Boolean| (list |Unique|)))
(defparameter |Rational| (%construct-class 'bard::|Rational| (list |Real|)))
(defparameter |Float| (%construct-class 'bard::|Float| (list |Real|)))
(defparameter |String| (%construct-class 'bard::|String| (list |Vector|)))
(defparameter |Integer| (%construct-class 'bard::|Integer| (list |Rational|)))
(defparameter |Byte| (%construct-class 'bard::|Byte| (list |Integer|)))
