;;;; ***********************************************************************
;;;;
;;;; Name:          structures.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of concrete structures
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; bard's concrete types called structures. a structure is also a
;;; procedure, a constructor for its instances. this implementation
;;; represents structures as instances of an instance of funcallable-standard-class
;;; so that they can be applied like functions

(defclass structure ()
  ((name :accessor name :initform nil :initarg :name)
   (constructor :accessor constructor :initform nil :initarg :constructor))
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((struct structure) &rest initargs &key &allow-other-keys)
  (with-slots (constructor) struct
    (let ((directs (getf initargs :direct-supers nil)))
      (register-type (the-type-graph)
                     struct
                     (remove-duplicates directs)))
    (clos:set-funcallable-instance-function
     struct
     (or constructor
         #'(lambda (&rest initargs)
             (error "~a cannot be applied"
                    (or (name struct)
                        "<an anonymous structure>")))))))

(defclass primitive-structure (structure)
  ((native-type :accessor native-type :initform nil :initarg :native-type))
  (:metaclass clos:funcallable-standard-class))

(defmethod name ((x cl:symbol))
  (symbol-name x))

;;; ---------------------------------------------------------------------
;;; class
;;; ---------------------------------------------------------------------


(defun %construct-class (cname direct-supers &key (constructor nil))
  (make-instance 'bard-class
                 :name cname
                 :constructor constructor
                 :direct-supers direct-supers))

(defparameter |class|
  (make-instance 'structure
                 :name 'bard::|class|
                 :constructor #'%construct-class))

