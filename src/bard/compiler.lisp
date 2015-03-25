;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard
;;;; Purpose:       bard 0.5 compiler
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

(defun compile-variable-reference (varname env)
  (if (find-binding env varname)
      (lambda ()(env-ref env varname))
      (lambda ()(global-ref (bard) varname))))

;;; (global-set! (bard) 'x 101)
;;; ($ (compile 'x nil))

(defun check-quote-args (x)
  (if (= 1 (length (cdr x)))
      t
      (error "Wrong numbers of arguments to quote; expected 1, found ~a"
             (length (cdr x)))))

;;; ($ (compile '(bard::|quote| |FooBar|) nil))

(defun compile-constant (x)
  (lambda () x))

(defun check-set!-args (x)
  (let* ((args (cdr x))
         (argcount (length args))
         (place (car args)))
    (cond
      ((not (= argcount 2))(error "Wrong numbers of arguments to set!; expected 2, found ~a" argcount))
      ((not (symbolp place))(error "Invalid argument to set!; first argument must be a symbol, not ~a"
                                   place))
      (t t))))

(defun compile-set! (args env)
  (let* ((varname (first args))
         (valexp (compile (second args) env)))
    (if (find-binding env varname)
        (lambda ()(env-set! env varname ($ valexp)))
        (lambda ()(global-set! (bard) varname ($ valexp))))))

;;; (global-set! (bard) 'x 0)
;;; ($ (compile '(bard::|set!| x 1001) nil))
;;; (global-ref (bard) 'x)

;;; TODO: add handling for (define (foo ...) ...)

(defun check-define-args (x)
  (let* ((args (cdr x))
         (argcount (length args))
         (place (car args)))
    (cond
      ((not (= argcount 2))(error "Wrong numbers of arguments to define; expected 2, found ~a" argcount))
      ((not (symbolp place))(error "Invalid argument to define; first argument must be a symbol, not ~a"
                                   place))
      (t t))))

(defun compile-define (args env)
  (let* ((varname (first args))
         (valexp (compile (second args) env)))
    (lambda ()(global-set! (bard) varname ($ valexp)))))

(defun  compile-begin (x env)
  (let ((vals (mapcar (lambda (e)(compile e env))
                      x)))
    (lambda ()
      (let ((result nil))
        (dolist (val vals)
          (setq result (funcall val)))
        result))))

;;; (global-set! (bard) 'x 0)
;;; ($ (compile '(bard::|begin| (bard::|set!| x 1) x) nil))
;;; (global-ref (bard) 'x)
;;; (defparameter $env (add-binding (empty-environment) 'z 0))
;;; ($ (compile '(bard::|begin| (bard::|set!| z 101) z) $env))

;;; (if test then else)
(defun check-if-args (x)
  (let* ((args (cdr x))
         (argcount (length args)))
    (cond
      ((< argcount 3)(error "bard: missing argument to if: ~s" x))
      ((> argcount 3)(error "bard: extra argument to if: ~s" x))
      (t t))))

(defun compile-if (x env)
  (let ((test (compile (first x) env))
        (then (second x))
        (else (third x)))
    (lambda ()
      (if (true? ($ test))
          ($ (compile then env))
          ($ (compile else env))))))

;;; ($ (compile `(bard::|if| ,(true) 1 0) nil))
;;; ($ (compile `(bard::|if| ,(false) 1 0) nil))

;;; (^ (arg1 arg2 ...) expr1 expr2 ...)
(defun check-method-args (x)
  (let* ((args (cdr x))
         (argcount (length args)))
    (if (< argcount 1)
        (error "Malformed method expression: ~S" x)
        (let ((params (first args)))
          (if (or (symbolp params)
                  (listp params))
              t
              (error "Malformed parameter list in method expression: ~S" params))))))

(defun compile-method (x env)
  (let* ((params (first x))
         (body (rest x))
         (meth (%construct-method params body env)))
    (lambda () meth)))

;;; (defparameter $m ($ (compile '(bard::^ () 3) nil)))
;;; ($ $m)
;;; (defparameter $m ($ (compile '(bard::^ (x y) y) nil)))
;;; ($ $m 10 20)
;;; ($ $m 101 202)

(defun compile-funcall (x env)
  (let ((op ($ (compile (first x) env)))
        (args (mapcar (lambda (e)($ (compile e env)))
                      (rest x))))
    (lambda () (apply op args))))

;;; (global-set! (bard) 'plus #'+)
;;; ($ (compile '(plus 2 3) nil))
;;; (global-set! (bard) 'times #'*)
;;; ($ (compile '(times 2 3 4) nil))

(defmethod compile (x env)
  (lambda () x))

(defmethod compile ((x symbol) env)
  (if (eq x nil)
      (lambda () nil)
      (if (keywordp x)
          (lambda () x)
          (compile-variable-reference x env))))

(defmethod compile ((x cons) env)
  (if (bard-macro (first x))
      (compile (bard-macroexpand x))
      (case (first x)
        (bard::|quote|
               (check-quote-args x)
               (compile-constant (second x)))
        (bard::|QUOTE|
               (check-quote-args x)
               (compile-constant (second x)))
        (bard::|begin|
               (compile-begin (rest x) env))
        (bard::|define|
               (check-define-args x)
               (compile-define (rest x) env))
        (bard::|set!|
               (check-set!-args x)
               (compile-set! (rest x) env))
        (bard::|if|
               (check-if-args x)
               (compile-if (rest x) env))
        (bard::|^|
               (check-method-args x)
               (compile-method (rest x) env))
        (t (compile-funcall x env)))))

(defun compiler (x)
  (compile x (empty-environment)))
