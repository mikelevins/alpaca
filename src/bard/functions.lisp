;;;; ***********************************************************************
;;;;
;;;; Name:          functions.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the function structure is defined in structures.lisp.
;;;
;;; a bard function is a funcallable object that examines its
;;; arguments and dispatches on their types to the most specific
;;; applicable method. the function object stores a table of methods
;;; and provides dispatching code to select the most applicable one
;;; at call time.

;;; ---------------------------------------------------------------------
;;; method-tree
;;; ---------------------------------------------------------------------
;;; method storage for functions

(defun make-method-tree () nil)

(defun add-method-entry (method-tree signature method)
  (let ((new-entry (cons signature method)))
    (cons new-entry
          (remove new-entry method-tree
                  :test #'equal :key #'car))))

(defun remove-method-entry (method-tree signature)
  (remove new-entry method-tree
          :test #'equal :key #'car))

(defun signature-subtypes? (candidate signature)
  (let* ((ampersand-pos (position (&) signature))
         (candidate-types (if ampersand-pos
                              (folio2:take ampersand-pos candidate)
                              candidate))
         (signature-types (if ampersand-pos
                              (folio2:take ampersand-pos signature)
                              signature)))
    (every (^ (c s)(subtype? c s))
           candidate-types signature-types)))

(defun find-applicable-method-entries (method-tree signature)
  (folio2:filter (^ (e)(signature-subtypes? signature (car e)))
                 method-tree))

(defun signature (args)
  (mapcar #'bard-type-of args))

;;; ---------------------------------------------------------------------
;;; most-specific-applicable-method
;;; ---------------------------------------------------------------------
;;; method selection for functions

(defun most-specific-applicable-method (method-tree args)
  (let* ((signature (signature args))
         (applicable-entries (find-applicable-method-entries method-tree signature))
         (sorted-entries (sort applicable-entries
                               (lambda (e1 e2)
                                 (signature-subtypes? (car e1)
                                                      (car e2))))))
    (if sorted-entries
        (cdr (first sorted-entries))
        nil)))
