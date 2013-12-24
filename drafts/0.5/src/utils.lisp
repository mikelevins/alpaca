;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: utils.lisp,v 1.2 2003/11/23 19:24:48 mikelevins Exp $
;;; 
;;; utility functions

(in-package "CCL")

(defun copy-macptr (mptr)
  (%int-to-ptr (%ptr-to-int mptr)))

