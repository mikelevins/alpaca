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

(defclass structure ()
  ((name :accessor name :initform nil :initarg :name)
   (constructor :accessor constructor :initform nil :initarg :constructor))
  (:metaclass clos:funcallable-standard-class))

(defmethod initialize-instance :after ((struct structure) &key &allow-other-keys)
  (with-slots (constructor) struct
    (clos:set-funcallable-instance-function
     struct
     (or constructor
         #'(lambda (&rest initargs)
             (error "structure ~a cannot be applied"
                    (or (name struct)
                        "<an anonymous structure>")))))))

(defun make-structure (&key name constructor)
  (make-instance 'structure :name name :constructor constructor))

;;; STRUCTURE cons
;;; ---------------------------------------------------------------------

(defparameter |character|
  (make-structure :name 'bard::|character|
                  :constructor #'(lambda (cname)
                                   (typecase cname
                                     (cl:character cname)
                                     (cl:integer (cl:code-char cname))
                                     (t (error "Can't construct a character from value ~S of type ~S"
                                               cname (type-of cname)))))))

(defparameter |cons|
  (make-structure :name 'bard::|cons|
                  :constructor #'(lambda (left right)(cl:cons left right))))

(defparameter |symbol|
  (make-structure :name 'bard::|symbol|
                  :constructor #'(lambda (sname)
                                   (typecase sname
                                     (cl:symbol sname)
                                     (cl:string (cl:intern sname :bard))
                                     (t (error "Can't construct a symbol from value ~S of type ~S"
                                               sname (type-of sname)))))))
