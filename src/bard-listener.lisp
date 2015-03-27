 ;;;; ***********************************************************************
;;;;
;;;; Name:          bard-listener.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       a listener window for bard
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; TODO: add condition-handling for errors that occur in the listener
;;;       so that it doesn;t die

(in-package #:alpaca)

;;; ---------------------------------------------------------------------
;;; for reference
;;; ---------------------------------------------------------------------
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

;;; ---------------------------------------------------------------------
;;; bard listener
;;; ---------------------------------------------------------------------

(defparameter *bard-listener-font*
  (gp:make-font-description
   :family "Menlo" 
   :size 16
   :weight :medium                         
   :slant :roman))

(defun bard-listener-repl (interface pane stream)
  (format stream "~%~a~%~%" bard-internal::$bard-banner)
  (bard-internal::bard) ; make sure the bard runtime is initialized
  (loop
     (bard-internal::display-bard-prompt stream)
     (handler-case (let* ((in-line (read-line stream))
                          (input (bard-internal::bard-read-from-string in-line))
                          (thunk (bard-internal::compile input (bard-internal::empty-environment)))
                          (vals (multiple-value-list ($ thunk))))
                     (dolist (val vals)
                       (terpri stream)
                       (bard-internal::bard-print val stream))
                     (terpri stream))
       (condition (c)
         (format stream "~%bard signaled a condition: ~S~%" c)
         nil))))


(define-interface bard-listener (interface)
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes (repl-pane interactive-pane :reader repl-pane :top-level-function 'bard-listener-repl
                     :buffer-name "bard listener" :buffer-modes '("Lisp")
                     :background (htmlcolor "181818") :foreground (htmlcolor "dedede")))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(repl-pane)))
  ;; -- defaults ---------------------------------------------
  (:default-initargs
      :title "bard listener"
    :width 800 :height 600
    :create-callback (lambda (intf)
                       (let ((font (gp:find-best-font (repl-pane intf)
                                                      *bard-listener-font*)))
                         (setf (simple-pane-font (repl-pane intf)) font)))))

;;; (setf $win (contain (make-instance 'bard-listener)))

(defmethod capi:interface-keys-style ((intf bard-listener))
  :emacs)
