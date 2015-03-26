;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       setting up and maintaining the bard runtime environment
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; the bard runtime
;;; =====================================================================

(defclass bard ()
  ((initialized? :accessor initialized? :initform nil)
   (globals :reader globals :initform (make-hash-table :test #'eq)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod initialize-instance :after ((obj bard) &rest initargs &key &allow-other-keys)
  (init-bard-globals obj)
  (setf (initialized? obj) t))

(defun bard ()(make-instance 'bard))

;;; =====================================================================
;;; global variables
;;; =====================================================================

(defmethod global-ref ((bard bard) varname)
  (let ((found (gethash varname (globals bard) (undefined))))
    (if (defined? found)
        found
        (error "Undefined global variable ~S" varname))))

(defmethod global-set! ((bard bard) varname val)
  (setf (gethash varname (globals bard))
        val))

;;; =====================================================================
;;; Lisp interop
;;; =====================================================================

(defun bard-predicate->lisp-predicate (bfn)
  (lambda (&rest args)
    (let* ((vals (multiple-value-list (apply bfn args)))
           (val (first vals)))
      (true? val))))

;;; =====================================================================
;;; init built-in structures
;;; =====================================================================

(defmethod init-bard-structures ((bard bard))
  (global-set! bard 'bard::|character| |character|)
  (global-set! bard 'bard::|class| |class|)
  (global-set! bard 'bard::|complex-number| |complex-number|)
  (global-set! bard 'bard::|cons| |cons|)
  (global-set! bard 'bard::|float| |float|)
  (global-set! bard 'bard::|function| |function|)
  (global-set! bard 'bard::|hash-table| |hash-table|)
  (global-set! bard 'bard::|integer| |integer|)
  (global-set! bard 'bard::|method| |method|)
  (global-set! bard 'bard::|ratio| |ratio|)
  (global-set! bard 'bard::|string| |string|)
  (global-set! bard 'bard::|symbol| |symbol|)
  (global-set! bard 'bard::|treelist| |treelist|)
  (global-set! bard 'bard::|treemap| |treemap|)
  (global-set! bard 'bard::|uri| |uri|))

;;; =====================================================================
;;; init built-in classes
;;; =====================================================================

(defmethod init-bard-classes ((bard bard))
  (global-set! bard 'bard::|Anything| |Anything|)
  (global-set! bard 'bard::|Stream| |Stream|)
  (global-set! bard 'bard::|Collection| |Collection|)
  (global-set! bard 'bard::|Atom| |Atom|)
  (global-set! bard 'bard::|List| |List|)
  (global-set! bard 'bard::|Type| |Type|)
  (global-set! bard 'bard::|Procedure| |Procedure|)
  (global-set! bard 'bard::|Name| |Name|)
  (global-set! bard 'bard::|Character| |Character|)
  (global-set! bard 'bard::|Condition| |Condition|)
  (global-set! bard 'bard::|Number| |Number|) 
  (global-set! bard 'bard::|Pair| |Pair|)
  (global-set! bard 'bard::|Array| |Array|) 
  (global-set! bard 'bard::|Map| |Map|)
  (global-set! bard 'bard::|Unique| |Unique|)
  (global-set! bard 'bard::|Event| |Event|)
  (global-set! bard 'bard::|Real| |Real|)
  (global-set! bard 'bard::|Complex| |Complex|)
  (global-set! bard 'bard::|Vector| |Vector|)
  (global-set! bard 'bard::|Boolean| |Boolean|)
  (global-set! bard 'bard::|Rational| |Rational|)
  (global-set! bard 'bard::|Float| |Float|)
  (global-set! bard 'bard::|String| |String|)
  (global-set! bard 'bard::|Integer| |Integer|)
  (global-set! bard 'bard::|Byte| |Byte|))

;;; =====================================================================
;;; init built-in protocol functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; internal Lisp protocol functions (implementations)
;;; ---------------------------------------------------------------------

;;; Character protocol
;;; ----------------------------------------
(defun |character.alphanumeric?| (c)(if (cl:alpha-char-p c) (true)(false)))

;;; Function protocol
;;; ----------------------------------------
(defun |function.complement| (f)
  (lambda (&rest args)
    (let* ((vals (multiple-value-list (cl:apply f args)))
           (val (first vals)))
      ;; reverse the results
      (if (false? val)
          (true)
          (false)))))

;;; List protocol
;;; ----------------------------------------

;;; add-first
(defun |cons.add-first| (x c)(cons x c))
(defun |string.add-first| (x c)(concatenate 'string (cl:string x) c))
(defun |treelist.add-first| (x c)(fset:insert c 0 x))

;;; add-last
(defun |cons.add-last| (c x)(append c (list x)))
(defun |string.add-last| (c x)(concatenate 'string c (cl:string x)))
(defun |treelist.add-last| (c x)(fset:insert c (fset:size c) x))

;;; any
(defun |cons.any| (ls)(elt ls (random (length ls))))
(defun |string.any| (ls)(elt ls (random (length ls))))
(defun |treelist.any| (ls)(fset:@ ls (random (fset:size ls))))

;;; apportion
(defun |cons.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

(defun |string.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

(defun |treelist.apportion| (ls &rest fns)
  (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns)(let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

;;; append
(defmethod |binary-append| ((x cl:null)(y cl:null)) nil)
(defmethod |binary-append| ((x cl:list)(y cl:list)) (cl:append x y))
(defmethod |binary-append| ((x cl:string)(y cl:string)) (cl:concatenate 'cl:string x y))
(defmethod |binary-append| ((x fset:wb-seq)(y fset:wb-seq))(fset:concat x y))

;;; by
(defun |cons.by| (n ls)(folio2:by n ls))
(defun |string.by| (n ls)(folio2:by n ls))
(defun |treelist.by| (n ls)(folio2:by n ls))

;;; count-if
(defun |cons.count-if| (pred ls)
  (if (null ls)
      0
      (if (atom ls)
          (if (true? (funcall pred ls))
              1
              0)
          (if (true? (funcall pred (car ls)))
              (+ 1 (|cons.count-if| pred (cdr ls)))
              (|cons.count-if| pred (cdr ls))))))

(defun |string.count-if| (pred str)
  (cl:count-if (bard-predicate->lisp-predicate pred) str))

(defun |treelist.count-if| (pred tls)
  (loop for i from 0 below (fset:size tls)
     sum (if (true? (funcall pred (fset:@ tls i)))
             1
             0)))

;;; dispose
(defun |cons.dispose| (x &rest fns)(cl:apply #'folio2:dispose x fns))
(defun |string.dispose| (x &rest fns)(cl:apply #'folio2:dispose x fns))
(defun |treelist.dispose| (x &rest fns)(cl:apply #'folio2:dispose x fns))

;;; first
(defun |cons.first| (x)(car x))
(defun |string.first| (x)(elt x 0))
(defun |treelist.first| (x)(fset:@ x 0))

;;; Pair protocol
;;; ----------------------------------------

;;;  left
(defun |cons.left| (x)(cl:car x))

;;; put-left
(defun |cons.put-left| (x val)(cons val (cdr x)))

;;; put-right
(defun |cons.put-right| (x val)(cons (car x) val))

;;; right
(defun |cons.right| (x)(cl:cdr x))

;;; set-left!
(defun |cons.set-left!| (x val)(setf (car x) val))

;;; set-right!
(defun |cons.set-right!| (x val)(setf (cdr x) val))

;;; Math protocol
;;; ----------------------------------------

;;; +
(defun |Math.+| (&rest nums)(cl:apply #'cl:+ nums))

;;; -
(defun |Math.-| (&rest nums)(cl:apply #'cl:- nums))

;;; *
(defun |Math.*| (&rest nums)(cl:apply #'cl:* nums))

;;; /
(defun |Math./| (&rest nums)(cl:apply #'cl:/ nums))

;;; even?
(defun |Integer.even?| (x)(if (cl:evenp x) (true) (false)))

;;; odd?
(defun |Integer.odd?| (x)(if (cl:oddp x) (true) (false)))

;;; ---------------------------------------------------------------------
;;; init bard functions
;;; ---------------------------------------------------------------------

(defmethod init-bard-functions ((bard bard))
  
  ;; Character protocol
  ;; ----------------------------------------

  ;; add-first
  (global-set! bard 'bard::|alphanumeric?| (%construct-function |Character|))
  (add-method! (global-ref bard 'bard::|alphanumeric?|)(list |Character|) #'|character.alphanumeric?|)
  
  ;; Function protocol
  ;; ----------------------------------------

  ;; complement
  (global-set! bard 'bard::|complement| (%construct-function |Procedure|))
  (add-method! (global-ref bard 'bard::|complement|)(list |Procedure|) #'|function.complement|)
  
  ;; List protocol
  ;; ----------------------------------------

  ;; add-first
  (global-set! bard 'bard::|add-first| (%construct-function |List|))
  (add-method! (global-ref bard 'bard::|add-first|)(list |Anything| |cons|) #'|cons.add-first|)
  (add-method! (global-ref bard 'bard::|add-first|)(list |Character| |string|) #'|string.add-first|)
  (add-method! (global-ref bard 'bard::|add-first|)(list |Anything| |treelist|) #'|treelist.add-first|)

  ;; add-last
  (global-set! bard 'bard::|add-last| (%construct-function |List|))
  (add-method! (global-ref bard 'bard::|add-last|)(list |cons| |Anything|) #'|cons.add-last|)
  (add-method! (global-ref bard 'bard::|add-last|)(list |string| |Character|) #'|string.add-last|)
  (add-method! (global-ref bard 'bard::|add-last|)(list |treelist| |Anything|) #'|treelist.add-last|)

  ;; any
  (global-set! bard 'bard::|any| (%construct-function |List|))
  (add-method! (global-ref bard 'bard::|any|)(list |cons|) #'|cons.any|)
  (add-method! (global-ref bard 'bard::|any|)(list |string|) #'|string.any|)
  (add-method! (global-ref bard 'bard::|any|)(list |treelist|) #'|treelist.any|)

  ;; append
  (global-set! bard 'bard::|append| (%construct-function |List| |List|))
  (add-method! (global-ref bard 'bard::|append|)(list |cons| |cons|) #'|binary-append|)
  (add-method! (global-ref bard 'bard::|append|)(list |string| |string|) #'|binary-append|)
  (add-method! (global-ref bard 'bard::|append|)(list |treelist| |treelist|) #'|binary-append|)

  ;; apportion
  (global-set! bard 'bard::|apportion| (%construct-function |List| (&)))
  (add-method! (global-ref bard 'bard::|apportion|)(list |cons| (&)) #'|cons.apportion|)
  (add-method! (global-ref bard 'bard::|apportion|)(list |string| (&)) #'|string.apportion|)
  (add-method! (global-ref bard 'bard::|apportion|)(list |treelist| (&)) #'|treelist.apportion|)

  ;; by
  (global-set! bard 'bard::|by| (%construct-function |Integer| |List|))
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |cons|) #'|cons.by|)
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |string|) #'|string.by|)
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |treelist|) #'|treelist.by|)

  ;; count-if
  (global-set! bard 'bard::|count-if| (%construct-function |Procedure| |List|))
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |cons|) #'|cons.count-if|)
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |string|) #'|string.count-if|)
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |treelist|) #'|treelist.count-if|)

  ;; dispose
  (global-set! bard 'bard::|dispose| (%construct-function |List| (&)))
  (add-method! (global-ref bard 'bard::|dispose|)(list |cons| (&)) #'|cons.dispose|)
  (add-method! (global-ref bard 'bard::|dispose|)(list |string| (&)) #'|string.dispose|)
  (add-method! (global-ref bard 'bard::|dispose|)(list |treelist| (&)) #'|treelist.dispose|)
  
  ;; first
  (global-set! bard 'bard::|first| (%construct-function |List|))
  (add-method! (global-ref bard 'bard::|first|)(list |cons|) #'|cons.first|)
  (add-method! (global-ref bard 'bard::|first|)(list |string|) #'|string.first|)
  (add-method! (global-ref bard 'bard::|first|)(list |treelist|) #'|treelist.first|)

  ;; Pair protocol
  ;; ----------------------------------------

  ;; left
  (global-set! bard 'bard::|left| (%construct-function |Pair|))
  (add-method! (global-ref bard 'bard::|left|)(list |cons|) #'|cons.left|)

  ;; pair?
  (global-set! bard 'bard::|pair?| (%construct-function |Anything|))
  (add-method! (global-ref bard 'bard::|pair?|)(list |Anything|) (cl:constantly (false)))
  (add-method! (global-ref bard 'bard::|pair?|)(list |cons|) (cl:constantly (true)))

  ;; put-left
  (global-set! bard 'bard::|put-left| (%construct-function |Pair| |Anything|))
  (add-method! (global-ref bard 'bard::|put-left|)(list |Pair| |Anything|) #'|cons.put-left|)

  ;; put-right
  (global-set! bard 'bard::|put-right| (%construct-function |Pair| |Anything|))
  (add-method! (global-ref bard 'bard::|put-right|)(list |Pair| |Anything|) #'|cons.put-right|)

  ;; right
  (global-set! bard 'bard::|right| (%construct-function |Pair|))
  (add-method! (global-ref bard 'bard::|right|)(list |cons|) #'|cons.right|)

  ;; set-left!
  (global-set! bard 'bard::|set-left!| (%construct-function |Pair| |Anything|))
  (add-method! (global-ref bard 'bard::|set-left!|)(list |cons| |Anything|) #'|cons.set-left!|)

  ;; set-right!
  (global-set! bard 'bard::|set-right!| (%construct-function |Pair| |Anything|))
  (add-method! (global-ref bard 'bard::|set-right!|)(list |cons| |Anything|) #'|cons.set-right!|)
  
  ;; Math protocol
  ;; ----------------------------------------

  (global-set! bard 'bard::|+| (%construct-function |Number| (&)))
  (add-method! (global-ref bard 'bard::|+|)(list |Number| (&)) #'|Math.+|)

  (global-set! bard 'bard::|-| (%construct-function |Number| (&)))
  (add-method! (global-ref bard 'bard::|-|)(list |Number| (&)) #'|Math.-|)

  (global-set! bard 'bard::|*| (%construct-function |Number| (&)))
  (add-method! (global-ref bard 'bard::|*|)(list |Number| |Number|) #'|Math.*|)

  (global-set! bard 'bard::|/| (%construct-function |Number| (&)))
  (add-method! (global-ref bard 'bard::|/|)(list |Number| |Number|) #'|Math./|)

  (global-set! bard 'bard::|even?| (%construct-function |Integer|))
  (add-method! (global-ref bard 'bard::|even?|)(list |Integer|) #'|Integer.even?|)

  (global-set! bard 'bard::|odd?| (%construct-function |Integer|))
  (add-method! (global-ref bard 'bard::|odd?|)(list |Integer|) #'|Integer.odd?|)

  )

;;; =====================================================================
;;; init built-in protocol methods
;;; =====================================================================

(defun |exit| ()(throw 'exit-bard :ok))
(defun |list| (&rest elts) elts)
(defun |pair| (left right)(cons left right))

(defmethod init-bard-methods ((bard bard))

  ;; List protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|list| #'|list|)

  ;; Pair protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|pair| #'|pair|)

  ;; System protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|exit| #'|exit|)
  )

;;; =====================================================================
;;; init built-in protocol macros
;;; =====================================================================

(defmethod init-bard-macros ((bard bard))
  )

;;; =====================================================================
;;; init named literals
;;; =====================================================================

(defun init-named-literals ()
  (register-type (the-type-graph) (undefined) (list |Unique|))
  (register-type (the-type-graph) (end) (list |Unique|))
  (register-type (the-type-graph) (nothing) (list |Unique|))
  (register-type (the-type-graph) (true) (list |Boolean|))
  (register-type (the-type-graph) (false) (list |Boolean|)))

;;; =====================================================================
;;; init global variables
;;; =====================================================================

(defun init-global-variables (bard)
  (global-set! bard 'bard::|->| (global-ref bard 'bard::|function|)))

;;; =====================================================================
;;; initialize the global bard environment
;;; =====================================================================

(defmethod init-bard-globals ((bard bard))
  (init-bard-structures bard)
  (init-bard-classes bard)
  (init-bard-functions bard)
  (init-bard-methods bard)
  (init-bard-macros bard)
  (init-named-literals)
  (init-global-variables bard))

;;; =====================================================================
;;; console repl
;;; =====================================================================

(defparameter $bard-banner (format nil "bard ~a" *bard-version-number*))

(defun display-bard-prompt (&optional (stream *standard-output*))
  (format stream "bard> "))

(defun repl ()
  (format t "~%~a~%" $bard-banner)
  (bard)
  (catch 'exit-bard
    (loop
       (display-bard-prompt)
       (let* ((input (bard-read))
              (thunk (compile input (empty-environment)))
              (vals (multiple-value-list ($ thunk))))
         (dolist (val vals)
           (terpri)
           (bard-print val))
         (terpri)))))

