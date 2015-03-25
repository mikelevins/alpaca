;;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       bard main entry point
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; ---------------------------------------------------------------------
;;; the bard runtime
;;; ---------------------------------------------------------------------

(defclass bard ()
  ((initialized? :accessor initialized? :initform nil)
   (globals :reader globals :initform (make-hash-table :test #'eq)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod initialize-instance :after ((obj bard) &rest initargs &key &allow-other-keys)
  (init-bard-globals obj))

(defun bard ()(make-instance 'bard))

;;; ---------------------------------------------------------------------
;;; global variables
;;; ---------------------------------------------------------------------

(defmethod global-ref ((bard bard) varname)
  (let ((found (gethash varname (globals bard) (undefined))))
    (if (defined? found)
        found
        (error "Undefined global variable ~S" varname))))

(defmethod global-set! ((bard bard) varname val)
  (setf (gethash varname (globals bard))
        val))

;;; ---------------------------------------------------------------------
;;; init built-in structures
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; the built-in classes
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; init built-in protocol functions
;;; ---------------------------------------------------------------------

(defun |cons-first| (x)(car x))
(defun |string-first| (x)(elt x 0))
(defun |treelist-first| (x)(fset:@ x 0))

(defmethod init-bard-functions ((bard bard))
  ;; List protocol
  (global-set! bard 'bard::|first| (%construct-function |List|))
  (add-method! (global-ref bard 'bard::|first|)(list |cons|) #'|cons-first|)
  (add-method! (global-ref bard 'bard::|first|)(list |string|) #'|string-first|)
  (add-method! (global-ref bard 'bard::|first|)(list |treelist|) #'|treelist-first|)
  )

;;; ---------------------------------------------------------------------
;;; init built-in protocol methods
;;; ---------------------------------------------------------------------

(defun |exit| ()(throw 'exit-bard :ok))

(defmethod init-bard-methods ((bard bard))
  (global-set! bard 'bard::|exit| #'|exit|)
  )

;;; ---------------------------------------------------------------------
;;; init named literals
;;; ---------------------------------------------------------------------

(defun init-named-literals ()
  (register-type (the-type-graph) (undefined) (list |Unique|))
  (register-type (the-type-graph) (end) (list |Unique|))
  (register-type (the-type-graph) (nothing) (list |Unique|))
  (register-type (the-type-graph) (true) (list |Boolean|))
  (register-type (the-type-graph) (false) (list |Boolean|)))

;;; ---------------------------------------------------------------------
;;; initialize the global bard environment
;;; ---------------------------------------------------------------------

(defmethod init-bard-globals ((bard bard))
  (init-bard-structures bard)
  (init-bard-classes bard)
  (init-bard-functions bard)
  (init-bard-methods bard)
  (init-named-literals))

;;; ---------------------------------------------------------------------
;;; console repl
;;; ---------------------------------------------------------------------

(defparameter $bard-banner (format nil "bard ~a" *bard-version-number*))

(defun display-bard-prompt ()
  (format t "bard> "))

(defun repl ()
  (format t "~%~a~%" $bard-banner)
  (bard)
  (catch 'exit-bard
    (loop
       (display-bard-prompt)
       (let* ((input (bard-read))
              (thunk (compile input (empty-environment))))
         (bard-print ($ thunk))
         (terpri)))))

