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


;;; ---------------------------------------------------------------------
;;; auxiliary utilities
;;; ---------------------------------------------------------------------

(defun rest2 (ls)(cddr ls))

;;; ---------------------------------------------------------------------
;;; built-in data structures
;;; ---------------------------------------------------------------------

(defclass undefined ()
  ()
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defun undefined ()(make-instance 'undefined))

(defmethod true? (x) t)
(defmethod true? ((x null)) nil)

(defmethod false? (x) nil)
(defmethod false? ((x null)) t)

;;; ---------------------------------------------------------------------
;;; representation of bard methods
;;; ---------------------------------------------------------------------

(defclass method ()
  ((name :accessor method-name :initform nil :initarg :name)
   (args :accessor method-args :initform nil :initarg :args)
   (code :accessor method-code :initform nil :initarg :code)
   (env :accessor method-env :initform nil :initarg :env)))

(defmethod method? (x) nil)
(defmethod method? ((x method)) t)

(defun make-method (&key args code env)
  (make-instance 'method :args args :code code :env env))

(defun show-method (method  &optional (stream *standard-output*) (indent 2))
  (if (not (method? method))
      (format stream "~8a" method)
      (progn
        (fresh-line)
        (dotimes (i (length (method-code method)))
          (let ((instr (elt (method-code method) i)))
            (if (label? instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-method arg stream (+ indent 8)))
                  (fresh-line))))))))


;;; ---------------------------------------------------------------------
;;; instruction utilities
;;; ---------------------------------------------------------------------

(defun label? (x) (atom x))
(defun opcode (instr) (if (label? instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

;;; ---------------------------------------------------------------------
;;; machine register structures
;;; ---------------------------------------------------------------------

(defun make-stack () nil)
(defun empty-env () nil)

;;; ---------------------------------------------------------------------
;;; vm return records
;;; ---------------------------------------------------------------------

(defclass return-record ()
  ((method :accessor return-method :initform nil :initarg :method)
   (pc :accessor return-pc :initform nil :initarg :pc)
   (env :accessor return-env :initform nil :initarg :env)))

(defun make-return-record (&key method pc env)
  (make-instance 'return-record :method method :pc pc :env env))

;;; ---------------------------------------------------------------------
;;; vm accessors
;;; ---------------------------------------------------------------------

(defun top (stack) (first stack))

(defmethod gref ((vm bardvm)(vname symbol))
  (gethash vname (globals vm) (undefined)))

(defmethod gset! ((vm bardvm)(vname symbol) val)
  (setf (gethash vname (globals vm))
        val))

;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(defclass bardvm ()
  ((method :accessor method :initform nil :initarg :method)
   (code :accessor code :initform nil :initarg :code)
   (pc :accessor pc  :initform nil :initarg :pc)
   (env :accessor env  :initform nil :initarg :env)
   (globals :accessor globals  :initform (make-hash-table) :initarg :globals)
   (stack :accessor stack  :initform nil :initarg :stack)
   (nargs :accessor nargs  :initform nil :initarg :nargs)
   (instruction :accessor instruction  :initform nil :initarg :instruction)))

;;; ---------------------------------------------------------------------
;;; executing an instruction
;;; ---------------------------------------------------------------------

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
    (FJUMP  (if (false? (pop (stack vm)))
                (setf (pc vm)
                      (arg1 (instruction vm)))))
    (TJUMP  (if (true? (pop (stack vm)))
                (setf (pc vm)
                      (arg1 (instruction vm)))))
    
    ;; Function call/return instructions:
    (SAVE   (push (make-return-record :pc (arg1 (instruction vm))
                                      :method (method vm) :env (env vm))
                  (stack vm)))

    (RETURN ;; return value is top of stack; return-record is second
      (setf (method vm) (return-method (second (stack vm)))
            (code vm) (method-code (method vm))
            (env vm) (return-env (second (stack vm)))
            (pc vm) (return-pc (second (stack vm))))
      ;; Get rid of the return-record, but keep the value
      (setf (stack vm)
            (cons (first (stack vm))
                  (rest2 (stack vm)))))
    (CALLJ  (pop (env vm)) ; discard the top frame
            (setf (method vm)(pop (stack vm))
                  (code vm)(method-code (method vm))
                  (env vm)(method-env (method vm))
                  (pc vm) 0
                  (nargs vm)(arg1 (instruction vm))))
    (ARGS   (assert (= (nargs vm) (arg1 (instruction vm))) ()
                    "Wrong number of arguments: ~d expected, ~d supplied"
                    (arg1 (instruction vm)) (nargs vm))
            (push (make-array (arg1 (instruction vm)))
                  (env vm))
            (loop for i from (- (nargs vm) 1) downto 0 do
                 (setf (elt (first (env vm)) i)
                       (pop (stack vm)))))
    (ARGS.  (assert (>= (nargs vm) (arg1 (instruction vm))) ()
                    "Wrong number of arguments: ~d or more expected, ~d supplied"
                    (arg1 (instruction vm)) (nargs vm))
            (push (make-array (+ 1 (arg1 (instruction vm))))
                  (env vm))
            (loop repeat (- (nargs vm) (arg1 (instruction vm))) do
                 (push (pop (stack vm))
                       (elt (first (env vm))
                            (arg1 (instruction vm)))))
            (loop for i from (- (arg1 (instruction vm)) 1) downto 0 do
                 (setf (elt (first (env vm)) i)
                       (pop (stack vm)))))
    (METHOD     (push (make-method :code (method-code (arg1 (instruction vm)))
                                   :env (env vm))
                      (stack vm)))
    (PRIM   (push (apply (arg1 (instruction vm))
                              (loop with args = nil repeat (nargs vm)
                                 do (push (pop (stack vm)) args)
                                 finally (return args)))
                  (stack vm)))
    
    ;; Continuation instructions:
    (SETCC (setf (stack vm)
                 (top (stack vm))))
    (CC (push (make-method
               :env (list (vector (stack vm)))
               :code '((ARGS 1) (LVAR 1 0 ";" (stack vm)) (SETCC)
                       (LVAR 0 0) (RETURN)))
              (stack vm)))
    
    ;; ========================
    ;; built-in ops
    ;; ========================
    
    ;; zero args
    ;; -----------------------
    ((HALT) (throw 'exit-bard (top (stack vm))))

    ((SCHEME-READ NEWLINE)
     (push (funcall (opcode (instruction vm))) (stack vm)))
    
    ;; 1 arg
    ;; -----------------------
    ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM) 
     (push (funcall (opcode (instruction vm)) (pop (stack vm))) (stack vm)))
    
    ;; 2 args
    ;; -----------------------
    ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
     (setf (stack vm) (cons (funcall (opcode (instruction vm)) (second (stack vm))
                                     (first (stack vm)))
                            (rest2 (stack vm)))))
    
    ;; 3 args
    ;; -----------------------
    (LIST3
     (setf (stack vm) (cons (funcall (opcode (instruction vm)) (third (stack vm))
                                     (second (stack vm)) (first (stack vm)))
                            (rest3 (stack vm)))))
    
    ;; Constants:
    ;; -----------------------
    ((T NIL -1 0 1 2)
     (push (opcode (instruction vm)) (stack vm)))
    
    (otherwise (error "Unknown opcode: ~a" (instruction vm)))))


;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

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
