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

(defmethod subtype? (t1 t2)
  (member t2 (hcl:class-precedence-list t1)))

(defmethod subtype? ((t1 cl:null) t2) t)

(defmethod subtype? (t1 (t2 (eql |Anything|))) t)

(defmethod subtype? ((t1 bard-class) (t2 bard-class))
  (member t2 (all-supertypes t1)))

(defmethod subtype? ((t1 structure) t2)
  (member t2 (all-supertypes t1)))

;;; =====================================================================
;;; bard-type-of
;;; =====================================================================

;;; default (returns a CL class)
;;; ---------------------------------------------------------------------

(defmethod bard-type-of (x)
  (cl:class-of x))

;;; named literals (each named literal is its own type)
;;; ---------------------------------------------------------------------

(defmethod bard-type-of ((x cl:null))
  (nothing))

(defmethod bard-type-of ((x cl:function))
  |primitive-method|)

(defmethod bard-type-of ((x true))
  (true))

(defmethod bard-type-of ((x false))
  (false))

(defmethod bard-type-of ((x undefined))
  (undefined))

(defmethod bard-type-of ((x end))
  (end))

;;; structures
;;; ---------------------------------------------------------------------

(defmethod bard-type-of ((x cl:character)) |character|)
(defmethod bard-type-of ((x bard-class)) |class|)
(defmethod bard-type-of ((x cl:complex)) |complex-number|)
(defmethod bard-type-of ((x cl:cons)) |cons|)
(defmethod bard-type-of ((x cl:float)) |float|)
(defmethod bard-type-of ((x bard-function)) |function|)
(defmethod bard-type-of ((x cl:hash-table)) |hash-table|)
(defmethod bard-type-of ((x cl:integer)) |integer|)
(defmethod bard-type-of ((x bard-method)) |method|)
(defmethod bard-type-of ((x cl:ratio)) |ratio|)
(defmethod bard-type-of ((x cl:string)) |string|)
(defmethod bard-type-of ((x cl:symbol)) |symbol|)
(defmethod bard-type-of ((x fset:wb-seq)) |treelist|)
(defmethod bard-type-of ((x fset:wb-map)) |treemap|)
(defmethod bard-type-of ((x quri.uri:uri)) |uri|)
(defmethod bard-type-of ((x text)) |text|)


(defun order-types (types)
  (assert (every #'type? types)()
          "Found something that is not a type in ~s"
          (folio2:filter (complement #'type?)
                         types))
  (sort types #'subtype?))

(defmethod all-supertypes (x)
  (clos:compute-class-precedence-list (class-of x)))

(defmethod all-supertypes ((x cl:null))
  (let ((directs (direct-supertypes x)))
    (if (null directs)
        nil
        (order-types
         (remove-duplicates
          (apply #'cl:append
                 (list (nothing))
                 directs
                 (mapcar #'all-supertypes directs)))))))

(defmethod all-supertypes ((x bard-class))
  (let ((directs (direct-supertypes x)))
    (if (null directs)
        nil
        (order-types
         (remove-duplicates
          (apply #'cl:append
                 (list x)
                 directs
                 (mapcar #'all-supertypes directs)))))))

(defmethod all-supertypes ((x structure))
  (let ((directs (direct-supertypes x)))
    (if (null directs)
        nil
        (order-types
         (remove-duplicates
          (apply #'cl:append
                 (list x)
                 directs
                 (mapcar #'all-supertypes directs)))))))

(defmethod all-supertypes ((x unique-instance))
  (let ((directs (direct-supertypes x)))
    (if (null directs)
        nil
        (order-types
         (remove-duplicates
          (apply #'cl:append
                 (list x)
                 directs
                 (mapcar #'all-supertypes directs)))))))

