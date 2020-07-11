;;;; alpaca.lisp

(in-package #:alpaca)
(in-readtable :qtools)

(defparameter *version-string* "v0.9.1d1")

(define-widget main (QWidget)
  ())

(define-subwidget (main button) (q+:make-qpushbutton "Alpaca Info" main))

(define-subwidget (main layout) (q+:make-qhboxlayout main)
  (q+:add-widget layout button))

(define-slot (main button-pressed) ()
  (declare (connected button (released)))
  (q+:qmessagebox-information
   main
   (format nil "Alpaca ~A" *version-string*)
   (format NIL
           "Welcome to Alpaca. ~
A work in progress.
Running ~a v~a on ~a."
           (lisp-implementation-type)
           (lisp-implementation-version)
           (machine-type))))

(defun main ()
  (with-main-window (window (make-instance 'main))))

