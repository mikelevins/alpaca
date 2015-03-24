;;;; ***********************************************************************
;;;;
;;;; Name:          built-in-structures.lisp
;;;; Project:       Bard
;;;; Purpose:       definitions of concrete structures
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; built-in structures
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; actor
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; adjustable-array
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; adjustable-vector
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; bit-vector
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; character
;;; ---------------------------------------------------------------------

(defun %construct-character (cname)
  (typecase cname
    (cl:character cname)
    (cl:integer (cl:code-char cname))
    (t (error "Can't construct a character from value ~S of type ~S"
              cname (type-of cname)))))

(defparameter |character|
  (make-instance 'primitive-structure
                 :name 'bard::|character|
                 :constructor #'%construct-character
                 :native-type 'cl:character
                 :direct-supers (list |Character|)))


;;; ---------------------------------------------------------------------
;;; complex-number
;;; ---------------------------------------------------------------------

(defun %construct-complex-number (real imaginary)(cl:complex real imaginary))

(defparameter |complex-number|
  (make-instance 'primitive-structure
                 :name 'bard::|complex-number|
                 :constructor #'%construct-complex-number
                 :native-type 'cl:complex
                 :direct-supers (list |Complex|)))

;;; ---------------------------------------------------------------------
;;; cons
;;; ---------------------------------------------------------------------

(defun %construct-cons (left right)(cl:cons left right))

(defparameter |cons|
  (make-instance 'primitive-structure
                 :name 'bard::|cons|
                 :constructor #'%construct-cons
                 :native-type 'cl:cons
                 :direct-supers (list |Pair| |Mutable|)))

;;; ---------------------------------------------------------------------
;;; error
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; event-stream
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; filesystem-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; file-stream
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; float
;;; ---------------------------------------------------------------------

(defun %construct-float (val)
  (typecase val
    (cl:float val)
    (cl:integer (cl:float val))
    (cl:ratio (cl:float val))
    (t (error "Can't construct a float from value ~S of type ~S"
              val (type-of val)))))

(defparameter |float|
  (make-instance 'primitive-structure
                 :name 'bard::|float|
                 :constructor #'%construct-float
                 :native-type 'cl:float
                 :direct-supers (list |Float|)))

;;; ---------------------------------------------------------------------
;;; function
;;; ---------------------------------------------------------------------

(defclass bard-function ()
  ((input-classes :accessor input-classes :initform nil :initarg :input-classes)
   (method-tree :accessor method-tree :initform (make-method-tree) :initarg :method-tree)
   (name :accessor name :initform nil :initarg :name))
  (:metaclass clos:funcallable-standard-class))

(defmethod add-method! ((fn bard-function) (signature cl:list) method)
  (setf (method-tree fn)
        (add-method-entry (method-tree fn)
                          signature
                          method)))

(defmethod remove-method! ((fn bard-function) (signature cl:list))
  (setf (method-tree fn)
        (remove-method-entry (method-tree fn) signature)))

(defmethod print-object ((fn bard-function)(out stream))
  (format out "(-> ~{~a~^ ~})" (input-classes fn)))

(defmethod bard-print ((obj bard-function) &optional (out cl:*standard-output*))
  (format out "(-> ~{~a~^ ~})" (input-classes obj)))

(defmethod no-applicable-method (fn args)
  (error "No applicable method of function ~S for arguments ~S"
         fn args))

(defmethod initialize-instance :after ((fn bard-function) &key &allow-other-keys)
  (clos:set-funcallable-instance-function
   fn #'(lambda (&rest args)
          (let ((best-method (most-specific-applicable-method (method-tree fn) args)))
            (if best-method
                (cl:apply best-method args)
                (no-applicable-method fn args))))))

(defun %construct-function (&rest input-classes)
  (assert (every #'class? input-classes)() "All arguments to function must be defined classes")
  (make-instance 'bard-function :input-classes input-classes :method-tree (make-method-tree)))

(defparameter |function|
  (make-instance 'structure
                 :name 'bard::|function|
                 :constructor #'%construct-function
                 :direct-supers (list |Procedure| |Mutable|)))

;;; ---------------------------------------------------------------------
;;; generator
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; hash-table
;;; ---------------------------------------------------------------------

(defun %construct-hash-table (&optional (comparator 'bard::|equal|))
  (case comparator
    (bard::|identical| (make-hash-table :test #'eq))
    (bard::|equivalent| (make-hash-table :test #'eql))
    (bard::|equal| (make-hash-table :test #'equal))
    (t (error "Can't construct a hash-table with comparator ~s" comparator))))

(defparameter |hash-table|
  (make-instance 'primitive-structure
                 :name 'bard::|hash-table|
                 :constructor #'%construct-hash-table
                 :native-type 'cl:hash-table
                 :direct-supers (list |Map| |Mutable|)))

;;; ---------------------------------------------------------------------
;;; integer
;;; ---------------------------------------------------------------------

(defun %construct-integer (val)
  (typecase val
    (cl:integer val)
    (cl:float (cl:truncate val))
    (cl:ratio (cl:round val))
    (t (error "Can't construct an integer from value ~S of type ~S"
              val (type-of val)))))

(defparameter |integer|
  (make-instance 'primitive-structure
                 :name 'bard::|integer|
                 :constructor #'%construct-integer
                 :native-type 'cl:integer
                 :direct-supers (list |Integer|)))

;;; ---------------------------------------------------------------------
;;; key-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; method
;;; ---------------------------------------------------------------------

(defclass bard-method ()
  ((name :accessor name :initform nil :initarg :name)
   (required-parameters :accessor required-parameters :initform nil :initarg :required-parameters)
   (rest-parameter :accessor rest-parameter :initform nil :initarg :rest-parameter)
   (call-environment :accessor call-environment :initform nil :initarg :call-environment)
   (expression :accessor method-expression :initform nil :initarg :expression)
   (body :accessor method-body :initform nil :initarg :body))
  (:metaclass clos:funcallable-standard-class))

(defmethod bard-print ((obj bard-method) &optional (out cl:*standard-output*))
  (if (method-expression obj)
      (format out "~a" (method-expression obj))
      (format out "#<~a>"
              (if (name obj)
                  (format nil "method ~a" (name obj))
                  (format nil "an anonymous method")))))

(defun %parse-method-params (params)
  (let* ((ampersand-pos (position-if (lambda (p)(equal "&" (symbol-name p)))
                                     params))
         (required-params (if ampersand-pos
                              (folio2:take ampersand-pos params)
                              params))
         (rest-param (if ampersand-pos
                         (elt params (1+ ampersand-pos))
                         nil)))
    (values required-params rest-param)))

;;; %construct-method
;;; ---------------------------------------------------------------------
;;; this is a little hairy:
;;; 1. parse out the method parameters, identifying the rest arg if any
;;; 2. create dummy bindings for them and then use those dummy bindings
;;;    to construct a call environment for the method. when the method is actually
;;;    applied, the actual arguments will be assigned to replace the dummy values
;;;    of the parameters in the call environment.
;;; 3. compile the method body in the call environment so that any
;;;    references to the parameters will resolve correctly when the
;;;    body thunk is executed
;;; 4. make an instance of bard-method and save the required parameters,
;;;    the rest parameter if any, the body thunk, and the call environment
;;;    in its slots
;;; 5. construct a lambda that does the business with the call environment,
;;;    executes the body thunk, and then bashes nils into all of the
;;;    required-parameter bindings in the call environment, so that the
;;;    environment doesn't create memory leaks by holding onto bound values,
;;;    before finally returning the values computed by the body (if any)
;;;    whew!

(defun %construct-method (params body env &key (name nil))
  (let ((expression `(bard::^ ,params ,@(copy-tree body))))
    ;; parse the params
    (multiple-value-bind (required-params rest-param)(%parse-method-params params)
      ;; build the dummy bindings
      (let* ((param-bindings (if rest-param
                                 (mapcar (lambda (par)(cons par nil))
                                         (append required-params (list rest-param)))
                                 (mapcar (lambda (par)(cons par nil))
                                         required-params)))
             ;; assemble the call environment
             (call-env (add-bindings env param-bindings))
             ;; compile the body in the call environment
             (meth-body (compile-begin body call-env))
             ;; construct the method object
             (meth (make-instance 'bard-method
                                  :required-parameters required-params
                                  :rest-parameter rest-param
                                  :call-environment call-env
                                  :expression expression
                                  :body meth-body)))
        ;; create the method proc and stuff it into the funcallable instance
        (clos:set-funcallable-instance-function
         meth
         (lambda (&rest args)
           ;; the code in the body of this lambda runs when the method is called
           ;; collect the formal parameters and the passed args
           (let* ((required-params (required-parameters meth))
                  (required-count (length required-params))
                  (rest-param (rest-parameter meth))
                  (arg-count (length args)))
             ;; check for argument errors
             (if (< arg-count required-count)
                 (error "Too few arguments to method ~s; ~a expected" meth required-count))
             (unless rest-param
               (when (> arg-count required-count)
                 (error "Too many arguments to method ~s; ~a expected" meth required-count)))
             ;; replace the dummy values in the call environment with the actual
             ;; arguments passed to the emthod
             (let* ((required-args (folio2:take required-count args))
                    (restargs (folio2:drop required-count args))
                    (call-env (call-environment meth)))
               (dotimes (i required-count)
                 (let ((val (elt required-args i))
                       (binding (elt call-env i)))
                   (set-binding-value! binding val)))
               ;; if there's a rest param, stick any leftover args in its binding
               (when rest-param
                 (env-set! callenv rest-param restargs))
               ;; execute the compiled method body and capture any values it returns
               (let ((vals (multiple-value-list ($ (method-body meth)))))
                 ;; now that we have the results, stuff nil into all the call-env bindings
                 ;; so the environment doesn't hold onto values it shouldn't
                 (dotimes (i required-count)
                   (let ((binding (elt call-env i)))
                     (set-binding-value! binding nil)))
                 ;; if there's a rest param, make sure it's nil as well
                 (when rest-param
                   (env-set! callenv rest-param restargs))
                 ;; now we can return the vals
                 (apply #'cl:values vals))))))
        meth))))


(defparameter |method|
  (make-instance 'structure
                 :name 'bard::|method|
                 :constructor #'%construct-method
                 :direct-supers (list |Procedure|)))

;;; ---------------------------------------------------------------------
;;; mouse-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; network-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; network-stream
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; number-array
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; number-vector
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; object-array
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; object-vector
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; package
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; random-state
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; ratio
;;; ---------------------------------------------------------------------

(defun %construct-ratio (num &optional (denom 1))
  (typecase num
    (cl:integer (cl:/ num denom))
    (cl:float (cl:rationalize (cl:/ num denom)))
    (cl:ratio (cl:/ num denom))
    (t (error "Can't construct a ratio from values ~S and ~S"
              num denom))))

(defparameter |ratio|
  (make-instance 'primitive-structure
                 :name 'bard::|ratio|
                 :constructor #'%construct-ratio
                 :native-type 'cl:ratio
                 :direct-supers (list |Rational|)))

;;; ---------------------------------------------------------------------
;;; read-table
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; record
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; restart
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sequence
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; sequence-stream
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; series
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; singleton
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; string
;;; ---------------------------------------------------------------------

(defun %construct-string (data)
  (typecase data
    (cl:string data)
    (cl:symbol (symbol-name data))
    (cl:character (cl:string data))
    (cl:cons (if (every #'characterp data)
                 (cl:coerce data 'cl:string)
                 (error "Can't construct a string from list ~S; all elements must be characters"
                        data)))
    (t (error "Can't construct a string from value ~S of type ~S"
              data (type-of data)))))

(defparameter |string|
  (make-instance 'primitive-structure
                 :name 'bard::|string|
                 :constructor #'%construct-string
                 :native-type 'cl:string
                 :direct-supers (list |String|)))


;;; ---------------------------------------------------------------------
;;; symbol
;;; ---------------------------------------------------------------------

(defun %construct-symbol (sname)
  (typecase sname
    (cl:symbol sname)
    (cl:string (cl:intern sname :bard))
    (t (error "Can't construct a symbol from value ~S of type ~S"
              sname (type-of sname)))))

(defparameter |symbol|
  (make-instance 'primitive-structure
                 :name 'bard::|symbol|
                 :constructor #'%construct-symbol
                 :native-type 'cl:symbol
                 :direct-supers (list |Name|)))

;;; ---------------------------------------------------------------------
;;; synonym
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; tablet-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; text
;;; ---------------------------------------------------------------------
;;; the text structure uses FSet's wb-seq support for strings
;;; to provide efficient representation of large texts for editing

(defclass text ()
  ((data :accessor text-data :initform "" :initarg :data)))

(defmethod print-object ((obj text)(out stream))
  (format out "#text \"~a\"" (fset:convert 'cl:string (text-data obj))))

(defmethod bard-print ((obj text) &optional (out cl:*standard-output*))
  (format out "#text \"~a\"" (fset:convert 'cl:string (text-data obj))))

(defmethod %construct-text (data)
  (error "Can't construct a text instance from data ~S" data))

(defmethod %construct-text ((data fset:wb-seq))
  (make-instance 'text :data data))

(defmethod %construct-text ((data string))
  (make-instance 'text :data (fset:convert 'fset:wb-seq data)))

(defmethod %construct-text ((data cons))
  (assert (every #'characterp data)()
          "Can't construct a text instance from data ~S" data)
  (make-instance 'text :data (fset:convert 'fset:wb-seq data)))

(defparameter |text|
  (make-instance 'structure
                 :name 'bard::|text|
                 :constructor #'%construct-text
                 :direct-supers (list |Text|)))


;;; ---------------------------------------------------------------------
;;; treelist
;;; ---------------------------------------------------------------------

(defun %construct-treelist (&rest elements)
  (fset:convert 'fset:wb-seq elements))

(defparameter |treelist|
  (make-instance 'primitive-structure
                 :name 'bard::|treelist|
                 :constructor #'%construct-treelist
                 :native-type 'fset:wb-seq
                 :direct-supers (list |List|)))

;;; ---------------------------------------------------------------------
;;; tree-map
;;; ---------------------------------------------------------------------

(defun %construct-tree-map (&rest plist)
  (let ((pairs (loop for kvs on plist by #'cddr
                  collect (cons (first kvs)
                                (second kvs)))))
    (fset:convert 'fset:wb-map pairs)))

(defparameter |treemap|
  (make-instance 'primitive-structure
                 :name 'bard::|treemap|
                 :constructor #'%construct-tree-map
                 :native-type 'fset:wb-map
                 :direct-supers (list |Map|)))

;;; ---------------------------------------------------------------------
;;; touch-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; union
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; uri
;;; ---------------------------------------------------------------------

(defun %construct-uri (data)
  (quri:uri data))

(defparameter |uri|
  (make-instance 'primitive-structure
                 :name 'bard::|uri|
                 :constructor #'%construct-uri
                 :native-type 'quri.uri:uri
                 :direct-supers (list |Name|)))

;;; ---------------------------------------------------------------------
;;; warning
;;; ---------------------------------------------------------------------
