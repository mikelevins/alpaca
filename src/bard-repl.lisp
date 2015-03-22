;;;; ***********************************************************************
;;;;
;;;; Name:          bard-repl.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       the bard toplevel
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;                based on code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; the bard repl
;;; ---------------------------------------------------------------------

(defconstant bard-top-level
  '(|begin| (|define| (|bard|)
             (|newline|)
             (|display| "=> ")
             (|write| ((|compiler| (|read|))))
             (|bard|))
    (|bard|)))

(defun bard ()
  "A compiled Bard read-eval-print loop"
  (init-bard-comp)
  (bardvm (compiler bard-top-level)))
