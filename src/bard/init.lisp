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

(defun init-bard-globals ()
  ;; built-in structures
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
  (global-set! 'bard::|uri| |uri|)
  ;; built-in classes
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
  (global-set! 'bard::|Byte| |Byte|)
  ;; built-in functions
  (global-set! 'bard::|first| (%construct-function |List|))
  (add-method! (global-ref 'bard::|first|) (list |string|) #'(lambda (s)(elt s 0)))
  ;; built-in methods
  (global-set! 'bard::|exit| #'(lambda ()(throw 'exit-bard :ok)))
  ;; named literals
  (register-type (the-type-graph) (undefined) (list |Unique|))
  (register-type (the-type-graph) (end) (list |Unique|))
  (register-type (the-type-graph) (nothing) (list |Unique|))
  (register-type (the-type-graph) (true) (list |Boolean|))
  (register-type (the-type-graph) (false) (list |Boolean|)))

