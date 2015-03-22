;;;; ***********************************************************************
;;;;
;;;; Name:          bard-reader.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;                based on code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; the bard reader
;;; ---------------------------------------------------------------------

(defun bard-reader-macro-quote (stream ch)
  "Standard ' macro reader."
  (declare (ignore ch))
  `(|quote| ,(COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read stream t nil t)))

(defun bard-reader-macro-backquote (stream ch)
  "Standard ` macro reader."
  (declare (ignore ch))
  `(|quasiquote| ,(COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read stream t nil t)))

(defun bard-reader-macro-comma (stream ch)
  "Standard , macro reader."
  (declare (ignore ch))
  `(,(if (char= #\@ (peek-char nil stream t nil t))
         (progn
           (read-char stream t nil t)
           '|unquote-splicing|)
         '|unquote|)
     ,(COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read stream t nil t)))

(defparameter *bard-readtable*
  (let ((tbl (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:copy-readtable)))
    (setf (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:readtable-case tbl) :preserve)
    (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:set-macro-character
     #\' #'bard-reader-macro-quote nil tbl)
    (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:set-macro-character
     #\` #'bard-reader-macro-backquote nil tbl)
    (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:set-macro-character
     #\, #'bard-reader-macro-comma nil tbl)
    
    (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:set-dispatch-macro-character
     #\# #\d
     ;; In both Common Lisp and Bard,
     ;; #x, #o and #b are hexidecimal, octal, and binary,
     ;; e.g. #xff = #o377 = #b11111111 = 255
     ;; In Bard only, #d255 is decimal 255.
     #'(lambda (stream &rest ignore) 
         (let ((*read-base* 10)) (bard-read stream)))
     tbl)

    tbl))

(defun bard-read (&optional (stream *standard-input*))
  (let ((COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:*readtable* *bard-readtable*))
    (input-object->bard-value
     (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read stream nil (end)))))

(defmethod bard-read-from-string ((s string))
  (with-input-from-string (in s)
    (bard-read in)))

(defmethod input-object->bard-value (x) x)

(defmethod input-object->bard-value ((x cons))
  (setf (car x) (input-object->bard-value (car x)))
  (setf (cdr x) (input-object->bard-value (cdr x)))
  x)

(defmethod input-object->bard-value ((x symbol))
  (case x
    (|end| (end))
    (|undefined| (undefined))
    (|nothing| (nothing))
    (|true| (true))
    (|false| (false))
    (t (or (convert-number x) x))))

(defmethod input-object->bard-value ((x vector))
  (dotimes (i (length x))
    (setf (aref x i)
          (input-object->bard-value (aref x i))))
  x)


(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read-from-string
                 str nil nil :start 0 :end pos))
            (im (COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER:read-from-string
                 str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))

