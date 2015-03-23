;;;; ***********************************************************************
;;;;
;;;; Name:          bard-tests.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       tests of the bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; vm tests
;;; ---------------------------------------------------------------------

;;; tests of LVAR LSET GVAR GSET POP CONST
;;; ---------------------------------------------------------------------

(defparameter $halt-test
  (make-method :env (empty-env)
               :code (vector '(HALT))))

;;; (bardrun $halt-test)

(defparameter $lvar-test
  (make-method :env (list (vector 101))
               :code (vector '(LVAR 0 0) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $lvar-test))
;;; should leave 101 on the stack
;;; (vmrun $vm)

(defparameter $lset-test
  (make-method :env (list (vector 101))
               :code (vector '(CONST 2002) '(LSET 0 0) '(POP) '(LVAR 0 0) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $lset-test))
;;; should leave 2002 on the stack and in the env:
;;; (vmrun $vm)

(defparameter $gvar-test
  (make-method :env (empty-env)
               :code (vector '(GVAR xx) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $gvar-test))
;;; should leave 33 on the stack:
;;; (progn (gset! $vm 'xx 33) (vmrun $vm))

(defparameter $gset-test
  (make-method :env (empty-env)
               :code (vector '(CONST 40004) '(GSET yy) '(POP) '(GVAR yy) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $gset-test))
;;; should leave 3003 on the stack:
;;; (progn (setf (get 'xx 'global-val) 3003) (vmrun $vm))


;;; tests of JUMP FJUMP TJUMP
;;; ---------------------------------------------------------------------

(defparameter $jump-test
  (make-method :env (empty-env)
               :code (vector '(JUMP 3) '(CONST 0) '(CONST 1) '(CONST 2) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $jump-test))
;;; should leave only 2 on the stack:
;;; (vmrun $vm)

(defparameter $fjump-test
  (make-method :env (empty-env)
               :code (vector '(CONST nil) '(FJUMP 3) '(CONST 0) '(CONST 1) '(CONST 2) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $fjump-test))
;;; should leave 1 and 2 on the stack:
;;; (vmrun $vm)

(defparameter $tjump-test
  (make-method :env (empty-env)
               :code (vector '(CONST 1) '(TJUMP 3) '(CONST 0) '(CONST 1) '(CONST 2) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $tjump-test))
;;; should leave 1 and 2 on the stack:
;;; (vmrun $vm)

;;; tests of function calls (SAVE, RETURN, CALLJ, ARGS, ARGS.)
;;; ---------------------------------------------------------------------


;;; tests of METHOD and FUNCTION (creating method and function objects)
;;; ---------------------------------------------------------------------


;;; tests of continuations (CC, SETCC)
;;; ---------------------------------------------------------------------


;;; tests of primitives (PRIM and built-in primitive opcodes)
;;; ---------------------------------------------------------------------


