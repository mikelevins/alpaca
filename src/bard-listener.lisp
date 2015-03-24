 ;;;; ***********************************************************************
;;;;
;;;; Name:          bard-listener.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       a listener window for bard
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:alpaca)

;;; from nick levine on lisphug:

;; Make an interactive stream
(defun make-interactive-stream ()
  (let* ((pane (contain (make-instance 'interactive-pane
                                       :top-level-function 'false)))
         (stream (interactive-pane-stream pane)))
    stream))

;; Spawn a thread to act on everything read on that stream
(defun process-inputs (stream actor)
  (mp:process-run-function 
   (format nil "Processing inputs on ~s" stream) nil
   (lambda ()
     (loop
        (funcall actor (read stream))))))

;; (process-inputs (make-interactive-stream) (lambda (x) (print x #. *standard-output*)))

;; The thread you've just created hangs waiting for input. All other
;; threads keep on going. Try (mp:ps) to confirm this.

;; You'd probably want a destroy-callback on the interactive-pane to
;; close the "processing inputs" thread. You'd probably want some form of
;; error handling so that the loop didn't halt on errors.
