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
;;; (vmrun $vm)

(defparameter $lset-test
  (make-method :env (list (vector 101))
               :code (vector '(CONST 2002) '(LSET 0 0) '(POP) '(LVAR 0 0) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $lset-test))
;;; (vmrun $vm)

(defparameter $gvar-test
  (make-method :env (empty-env)
               :code (vector '(GVAR xx) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $gvar-test))
;;; (progn (gset! $vm 'xx 33) (vmrun $vm))

(defparameter $gset-test
  (make-method :env (empty-env)
               :code (vector '(CONST 40004) '(GSET yy) '(POP) '(GVAR yy) '(HALT))))

;;; (defparameter $vm (make-instance 'bardvm :method $gset-test))
;;; (progn (setf (get 'xx 'global-val) 3003) (vmrun $vm))


;;; tests of JUMP FJUMP TJUMP
;;; ---------------------------------------------------------------------

