;;;; ***********************************************************************
;;;;
;;;; Name:          init.lisp
;;;; Project:       Bard
;;;; Purpose:       initialize the bard environment before starting a repl
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; ---------------------------------------------------------------------
;;; init the built-in structures
;;; ---------------------------------------------------------------------

(defun init-bard-structures ()
  (global-set! 'bard::|character| |character|)
  (global-set! 'bard::|class| |class|)
  (global-set! 'bard::|complex-number| |complex-number|)
  (global-set! 'bard::|cons| |cons|)
  (global-set! 'bard::|float| |float|)
  (global-set! 'bard::|function| |function|)
  (global-set! 'bard::|hash-table| |hash-table|)
  (global-set! 'bard::|integer| |integer|)
  (global-set! 'bard::|method| |method|)
  (global-set! 'bard::|ratio| |ratio|)
  (global-set! 'bard::|string| |string|)
  (global-set! 'bard::|symbol| |symbol|)
  (global-set! 'bard::|treelist| |treelist|)
  (global-set! 'bard::|treemap| |treemap|)
  (global-set! 'bard::|uri| |uri|))

;;; ---------------------------------------------------------------------
;;; init the built-in classes
;;; ---------------------------------------------------------------------

(defun init-bard-classes ()
  (global-set! 'bard::|Anything| |Anything|)
  (global-set! 'bard::|Stream| |Stream|)
  (global-set! 'bard::|Collection| |Collection|)
  (global-set! 'bard::|Atom| |Atom|)
  (global-set! 'bard::|List| |List|)
  (global-set! 'bard::|Type| |Type|)
  (global-set! 'bard::|Procedure| |Procedure|)
  (global-set! 'bard::|Name| |Name|)
  (global-set! 'bard::|Character| |Character|)
  (global-set! 'bard::|Condition| |Condition|)
  (global-set! 'bard::|Number| |Number|) 
  (global-set! 'bard::|Pair| |Pair|)
  (global-set! 'bard::|Array| |Array|) 
  (global-set! 'bard::|Map| |Map|)
  (global-set! 'bard::|Unique| |Unique|)
  (global-set! 'bard::|Event| |Event|)
  (global-set! 'bard::|Real| |Real|)
  (global-set! 'bard::|Complex| |Complex|)
  (global-set! 'bard::|Vector| |Vector|)
  (global-set! 'bard::|Boolean| |Boolean|)
  (global-set! 'bard::|Rational| |Rational|)
  (global-set! 'bard::|Float| |Float|)
  (global-set! 'bard::|String| |String|)
  (global-set! 'bard::|Integer| |Integer|)
  (global-set! 'bard::|Byte| |Byte|))

;;; ---------------------------------------------------------------------
;;; init the built-in protocol functions
;;; ---------------------------------------------------------------------

(defun |cons-first| (x)(car x))
(defun |string-first| (x)(elt x 0))
(defun |treelist-first| (x)(fset:@ x 0))

(defun init-bard-functions ()
  ;; List protocol
  (global-set! 'bard::|first| (%construct-function |List|))
  (add-method! (global-ref 'bard::|first|)(list |cons|) #'|cons-first|)
  (add-method! (global-ref 'bard::|first|)(list |string|) #'|string-first|)
  (add-method! (global-ref 'bard::|first|)(list |treelist|) #'|treelist-first|)
  )

;;; ---------------------------------------------------------------------
;;; init the built-in protocol methods
;;; ---------------------------------------------------------------------

(defun |exit| ()(throw 'exit-bard :ok))

(defun init-bard-methods ()
  (global-set! 'bard::|exit| #'|exit|)
  )

;;; ---------------------------------------------------------------------
;;; init the named literals
;;; ---------------------------------------------------------------------

(defun init-named-literals ()
  (register-type (the-type-graph) (undefined) (list |Unique|))
  (register-type (the-type-graph) (end) (list |Unique|))
  (register-type (the-type-graph) (nothing) (list |Unique|))
  (register-type (the-type-graph) (true) (list |Boolean|))
  (register-type (the-type-graph) (false) (list |Boolean|)))

;;; ---------------------------------------------------------------------
;;; init the global bard environment
;;; ---------------------------------------------------------------------

(defun init-bard-globals ()
  (init-bard-structures)
  (init-bard-classes)
  (init-bard-functions)
  (init-bard-methods)
  (init-named-literals))

