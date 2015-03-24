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

(defmethod initialize-instance :after ((struct structure) &key &allow-other-keys)
  (with-slots (constructor) struct
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
                 :native-type 'cl:character))

;;; ---------------------------------------------------------------------
;;; class
;;; ---------------------------------------------------------------------

(defun %construct-class (cname direct-superclasses &key (constructor nil))
  (make-instance 'bard-class
                 :name cname
                 :constructor constructor
                 :direct-superclasses direct-superclasses))

(defparameter |class|
  (make-instance 'structure
                 :name 'bard::|class|
                 :constructor #'%construct-class))

;;; ---------------------------------------------------------------------
;;; complex-number
;;; ---------------------------------------------------------------------

(defun %construct-complex-number (real imaginary)(cl:complex real imaginary))

(defparameter |complex-number|
  (make-instance 'primitive-structure
                 :name 'bard::|complex-number|
                 :constructor #'%construct-complex-number
                 :native-type 'cl:complex))

;;; ---------------------------------------------------------------------
;;; cons
;;; ---------------------------------------------------------------------

(defun %construct-cons (left right)(cl:cons left right))

(defparameter |cons|
  (make-instance 'primitive-structure
                 :name 'bard::|cons|
                 :constructor #'%construct-cons
                 :native-type 'cl:cons))

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
                 :native-type 'cl:float))

;;; ---------------------------------------------------------------------
;;; function
;;; ---------------------------------------------------------------------

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
                 :native-type 'cl:hash-table))

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
                 :native-type 'cl:integer))

;;; ---------------------------------------------------------------------
;;; key-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; method
;;; ---------------------------------------------------------------------

(defclass bard-method ()
  ((required-parameters :accessor required-parameters :initform nil :initarg :required-parameters)
   (rest-parameter :accessor rest-parameter :initform nil :initarg :rest-parameter)
   (call-environment :accessor call-environment :initform nil :initarg :call-environment)
   (body :accessor method-body :initform nil :initarg :body))
  (:metaclass clos:funcallable-standard-class))

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
      meth)))


(defparameter |method|
  (make-instance 'structure
                 :name 'bard::|method|
                 :constructor #'%construct-method))

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
                 :native-type 'cl:ratio))

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
                 :native-type 'cl:string))


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
                 :native-type 'cl:symbol))

;;; ---------------------------------------------------------------------
;;; synonym
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; tablet-event
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; treelist
;;; ---------------------------------------------------------------------

(defun %construct-treelist (&rest elements)
  (fset:convert 'fset:wb-seq elements))

(defparameter |treelist|
  (make-instance 'primitive-structure
                 :name 'bard::|treelist|
                 :constructor #'%construct-treelist
                 :native-type 'fset:wb-seq))

;;; ---------------------------------------------------------------------
;;; tree-map
;;; ---------------------------------------------------------------------

(defun %construct-tree-map (&rest plist)
  (let ((pairs (loop for kvs on plist by #'cddr
                  collect (cons (first kvs)
                                (second kvs)))))
    (fset:convert 'fset:wb-map pairs)))

(defparameter |tree-map|
  (make-instance 'primitive-structure
                 :name 'bard::|tree-map|
                 :constructor #'%construct-tree-map
                 :native-type 'fset:wb-map))

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
                 :native-type 'quri.uri:uri))

;;; ---------------------------------------------------------------------
;;; warning
;;; ---------------------------------------------------------------------
