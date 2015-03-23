;;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Alpaca: a programmable editor
;;;; Purpose:       the bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defclass undefined ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun undefined ()(make-instance 'undefined))

(defun make-stack () nil)
(defun empty-env () nil)

(defclass method ()
  ((code :accessor method-code :initform nil :initarg :code)
   (env :accessor method-env :initform nil :initarg :env)))

(defmethod method? (x) nil)
(defmethod method? ((x method)) t)

(defun make-method (&key code env)
  (make-instance 'method :code code :env env))

(defun label? (x) (atom x))
(defun opcode (instr) (if (label? instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defun top (stack) (first stack))

(defmethod show-method ((method method) &optional (stream *standard-output*) (indent 2))
  (if (not (method? method))
      (format stream "~8a" method)
      (progn
        (fresh-line)
        (dotimes (i (length (method-code fn)))
          (let ((instr (elt (method-code fn) i)))
            (if (label? instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-method arg stream (+ indent 8)))
                  (fresh-line))))))))

(defclass bardvm ()
  ((method :accessor method :initform nil :initarg :method)
   (code :accessor code :initform nil :initarg :code)
   (pc :accessor pc  :initform nil :initarg :pc)
   (env :accessor env  :initform nil :initarg :env)
   (globals :accessor globals  :initform (make-hash-table) :initarg :globals)
   (stack :accessor stack  :initform nil :initarg :stack)
   (nargs :accessor nargs  :initform nil :initarg :nargs)
   (instruction :accessor instruction  :initform nil :initarg :instruction)))

(defmethod gref ((vm bardvm)(vname symbol))
  (gethash vname (globals vm) (undefined)))

(defmethod gset! ((vm bardvm)(vname symbol) val)
  (setf (gethash vname (globals vm))
        val))

(defun vmexec! (vm)
  (case (opcode (instruction vm))
    ;; Variable/stack manipulation instructions:
    (LVAR   (push (elt (elt (env vm)
                            (arg1 (instruction vm)))
                       (arg2 (instruction vm)))
                  (stack vm)))
    (LSET   (setf (elt (elt (env vm)
                            (arg1 (instruction vm)))
                       (arg2 (instruction vm)))
                  (top (stack vm))))
    (GVAR   (push (gref vm (arg1 (instruction vm)))
                  (stack vm)))
    (GSET   (gset! vm (arg1 (instruction vm))
                   (top (stack vm))))
    (POP    (pop (stack vm)))
    (CONST  (push (arg1 (instruction vm))
                  (stack vm)))
    
    ;; Branching instructions:
    (JUMP   (setf (pc vm) (arg1 (instruction vm))))
    (FJUMP  (if (null (pop (stack vm)))
                (setf (pc vm)
                      (arg1 (instruction vm)))))
    (TJUMP  (if (pop (stack vm))
                (setf (pc vm)
                      (arg1 (instruction vm)))))
    
    ;; Function call/return instructions:
    (SAVE   (push (make-ret-addr :pc (arg1 (instruction vm))
                                 :fn (method vm) :env (env vm))
                  (stack vm)))

    (RETURN ;; return value is top of stack; ret-addr is second
      (setf (method vm) (ret-addr-fn (second (stack vm)))
            (code vm) (method-code (method vm))
            (env vm) (ret-addr-env (second (stack vm)))
            (pc vm) (ret-addr-pc (second (stack vm))))
      ;; Get rid of the ret-addr, but keep the value
      (setf (stack vm) (cons (first (stack vm)) (rest2 (stack vm)))))
    (CALLJ  (pop (env vm))                 ; discard the top frame
            (setf (method vm)  (pop (stack vm))
                  (code vm) (method-code (method vm))
                  (env vm) (method-env (method vm))
                  (pc vm) 0
                  (nargs vm) (arg1 (instruction vm))))
    (ARGS   (assert (= (nargs vm) (arg1 (instruction vm))) ()
                    "Wrong number of arguments:~
                         ~d expected, ~d supplied"
                    (arg1 (instruction vm)) (nargs vm))
            (push (make-array (arg1 (instruction vm))) (env vm))
            (loop for i from (- (nargs vm) 1) downto 0 do
                 (setf (elt (first (env vm)) i) (pop (stack vm)))))
    (ARGS.  (assert (>= (nargs vm) (arg1 (instruction vm))) ()
                    "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
                    (arg1 (instruction vm)) (nargs vm))
            (push (make-array (+ 1 (arg1 (instruction vm)))) (env vm))
            (loop repeat (- (nargs vm) (arg1 (instruction vm))) do
                 (push (pop (stack vm)) (elt (first (env vm)) (arg1 (instruction vm)))))
            (loop for i from (- (arg1 (instruction vm)) 1) downto 0 do
                 (setf (elt (first (env vm)) i) (pop (stack vm)))))
    (FN     (push (make-method :code (method-code (arg1 (instruction vm)))
                           :env (env vm)) (stack vm)))
    (PRIM   (push (apply (arg1 (instruction vm))
                         (loop with args = nil repeat (nargs vm)
                            do (push (pop (stack vm)) args)
                            finally (return args)))
                  (stack vm)))
    
    ;; Continuation instructions:
    (SET-CC (setf (stack vm) (top (stack vm))))
    (CC     (push (make-method
                   :env (list (vector (stack vm)))
                   :code '((ARGS 1) (LVAR 1 0 ";" (stack vm)) (SET-CC)
                           (LVAR 0 0) (RETURN)))
                  (stack vm)))
    
    ;; Nullary operations:
    ((SCHEME-READ NEWLINE)
     (push (funcall (opcode (instruction vm))) (stack vm)))
    
    ;; Unary operations:
    ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM) 
     (push (funcall (opcode (instruction vm)) (pop (stack vm))) (stack vm)))
    
    ;; Binary operations:
    ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
     (setf (stack vm) (cons (funcall (opcode (instruction vm)) (second (stack vm))
                                (first (stack vm)))
                       (rest2 (stack vm)))))
    
    ;; Ternary operations:
    (LIST3
     (setf (stack vm) (cons (funcall (opcode (instruction vm)) (third (stack vm))
                               (second (stack vm)) (first (stack vm)))
                       (rest3 (stack vm)))))
    
    ;; Constants:
    ((T NIL -1 0 1 2)
     (push (opcode (instruction vm)) (stack vm)))
    
    ;; Other:
    ((HALT) (throw 'exit-bard (top (stack vm))))
    (otherwise (error "Unknown opcode: ~a" (instruction vm)))))

(defun vmrun (vm)
  (catch 'exit-bard
    (progn
      (assert (method vm)() "No program! (~S)" vm)
      (setf (code vm)(method-code (method vm))
            (pc vm) 0
            (env vm)(method-env (method vm))
            (stack vm)(make-stack)
            (nargs vm) 0)
      (loop
         (setf (instruction vm) (elt (code vm) (pc vm)))
         (incf (pc vm))
         (vmexec! vm))))
  'ok)

(defun bardrun (m)
  (let ((vm (make-instance 'bardvm :method m)))
    (vmrun vm)))
