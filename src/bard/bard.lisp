;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       setting up and maintaining the bard runtime environment
;;;; Author:        mikel evins
;;;; Copyright:     2015 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-internal)

;;; =====================================================================
;;; the bard runtime
;;; =====================================================================

(defclass bard ()
  ((initialized? :accessor initialized? :initform nil)
   (globals :reader globals :initform (make-hash-table :test #'eq)))
  (:metaclass org.tfeb.hax.singleton-classes:singleton-class))

(defmethod initialize-instance :after ((obj bard) &rest initargs &key &allow-other-keys)
  (init-bard-globals obj)
  (setf (initialized? obj) t))

(defun bard ()(make-instance 'bard))

;;; =====================================================================
;;; global variables
;;; =====================================================================

(defmethod global-ref ((bard bard) varname)
  (let ((found (gethash varname (globals bard) (undefined))))
    (if (defined? found)
        found
        (error "Undefined global variable ~S" varname))))

(defmethod global-set! ((bard bard) varname val)
  (setf (gethash varname (globals bard))
        val))

;;; =====================================================================
;;; Lisp interop
;;; =====================================================================

(defun bard-predicate->lisp-predicate (bfn)
  (lambda (&rest args)
    (let* ((vals (multiple-value-list (apply bfn args)))
           (val (first vals)))
      (true? val))))

;;; =====================================================================
;;; init built-in structures
;;; =====================================================================

(defmethod init-bard-structures ((bard bard))
  (global-set! bard 'bard::|character| |character|)
  (global-set! bard 'bard::|class| |class|)
  (global-set! bard 'bard::|complex-number| |complex-number|)
  (global-set! bard 'bard::|cons| |cons|)
  (global-set! bard 'bard::|document-window| |document-window|)
  (global-set! bard 'bard::|float| |float|)
  (global-set! bard 'bard::|function| |function|)
  (global-set! bard 'bard::|hash-table| |hash-table|)
  (global-set! bard 'bard::|integer| |integer|)
  (global-set! bard 'bard::|method| |method|)
  (global-set! bard 'bard::|ratio| |ratio|)
  (global-set! bard 'bard::|rectangular-extent| |rectangular-extent|)
  (global-set! bard 'bard::|rectangular-point| |rectangular-point|)
  (global-set! bard 'bard::|string| |string|)
  (global-set! bard 'bard::|symbol| |symbol|)
  (global-set! bard 'bard::|text| |text|)
  (global-set! bard 'bard::|treelist| |treelist|)
  (global-set! bard 'bard::|treemap| |treemap|)
  (global-set! bard 'bard::|uri| |uri|))

;;; =====================================================================
;;; init built-in classes
;;; =====================================================================

(defmethod init-bard-classes ((bard bard))
  (global-set! bard 'bard::|Anything| |Anything|)
  (global-set! bard 'bard::|Array| |Array|) 
  (global-set! bard 'bard::|Atom| |Atom|)
  (global-set! bard 'bard::|Boolean| |Boolean|)
  (global-set! bard 'bard::|Byte| |Byte|)
  (global-set! bard 'bard::|Character| |Character|)
  (global-set! bard 'bard::|Complex| |Complex|)
  (global-set! bard 'bard::|Condition| |Condition|)
  (global-set! bard 'bard::|Container| |Container|)
  (global-set! bard 'bard::|Event| |Event|)
  (global-set! bard 'bard::|Extent| |Extent|)
  (global-set! bard 'bard::|Float| |Float|)
  (global-set! bard 'bard::|Geometry| |Geometry|)
  (global-set! bard 'bard::|Integer| |Integer|)
  (global-set! bard 'bard::|List| |List|)
  (global-set! bard 'bard::|Map| |Map|)
  (global-set! bard 'bard::|Mutable| |Mutable|)
  (global-set! bard 'bard::|Name| |Name|)
  (global-set! bard 'bard::|Number| |Number|) 
  (global-set! bard 'bard::|Pair| |Pair|)
  (global-set! bard 'bard::|Presentation| |Presentation|)
  (global-set! bard 'bard::|Procedure| |Procedure|)
  (global-set! bard 'bard::|Rational| |Rational|)
  (global-set! bard 'bard::|Real| |Real|)
  (global-set! bard 'bard::|Stream| |Stream|)
  (global-set! bard 'bard::|String| |String|)
  (global-set! bard 'bard::|Text| |Text|)
  (global-set! bard 'bard::|Type| |Type|)
  (global-set! bard 'bard::|Unique| |Unique|)
  (global-set! bard 'bard::|Vector| |Vector|)
  (global-set! bard 'bard::|Window| |Window|))

;;; =====================================================================
;;; init built-in protocol functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; internal Lisp protocol functions (implementations)
;;; ---------------------------------------------------------------------

;;; Character protocol
;;; ----------------------------------------
(defun |character.alphanumeric?| (c)(if (cl:alphanumericp c) (true)(false)))

;;; Construction protocol
;;; ----------------------------------------
;;; the function `make`

;;; Conversion protocol
;;; ----------------------------------------
;;; the function `as`

(defun |as.cons.cons|(x y) y)
(defun |as.cons.string|(x y) (coerce y 'cl:list))
(defun |as.cons.treelist|(x y) (fset:convert 'cl:list y))
(defun |as.cons.text|(x y) (fset:convert 'cl:list (text-data y)))

(defun |as.string.cons|(x y)
  (assert (every #'characterp y)()
          "Can't convert a cons to a string unless all elements are characters")
  (coerce y 'cl:string))
(defun |as.string.treelist|(x y)
  (assert (fset::every #'characterp y)()
          "Can't convert a treelist to a string unless all elements are characters")
  (fset:convert 'cl:string y))
(defun |as.string.text|(x y) (fset:convert 'cl:string (text-data y)))

(defun |as.treelist.cons|(x y) (fset:convert 'fset:wb-seq y))

;;; Function protocol
;;; ----------------------------------------
(defun |function.complement| (f)
  (lambda (&rest args)
    (let* ((vals (multiple-value-list (cl:apply f args)))
           (val (first vals)))
      ;; reverse the results
      (if (false? val)
          (true)
          (false)))))

;;; Geometry protocol
;;; ----------------------------------------

(defun |rectangular-point.x| (p)(point-x p))
(defun |rectangular-point.y| (p)(point-y p))
(defun |rectangular-extent.width| (e)(extent-width e))
(defun |rectangular-extent.height|(e)(extent-height e))

;;; List protocol
;;; ----------------------------------------

;;; add-first
(defun |cons.add-first| (x c)(cons x c))
(defun |string.add-first| (x c)(concatenate 'string (cl:string x) c))
(defun |text.add-first| (x c)(%construct-text (fset:insert (text-data c) 0 x)))
(defun |treelist.add-first| (x c)(fset:insert c 0 x))

;;; add-last
(defun |cons.add-last| (c x)(append c (list x)))
(defun |string.add-last| (c x)(concatenate 'string c (cl:string x)))
(defun |text.add-last| (c x)(%construct-text (fset:insert (text-data c)(fset:size (text-data c)) x)))
(defun |treelist.add-last| (c x)(fset:insert c (fset:size c) x))

;;; any
(defun |cons.any| (ls)(elt ls (random (length ls))))
(defun |string.any| (ls)(elt ls (random (length ls))))
(defun |text.any| (ls)(fset:@ (text-data ls) (random (fset:size (text-data ls)))))
(defun |treelist.any| (ls)(fset:@ ls (random (fset:size ls))))

;;; apportion
(defun |cons.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

(defun |string.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

(defun |treelist.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion ls fns*)))

(defun |text.apportion| (ls &rest fns)
  (let ((fns* (mapcar #'bard-predicate->lisp-predicate fns)))
    (cl:apply 'net.bardcode.folio2.sequences:apportion (text-data ls) fns*)))

;;; append
(defmethod |binary-append| ((x cl:null)(y cl:null)) nil)
(defmethod |binary-append| ((x cl:list)(y cl:list)) (cl:append x y))
(defmethod |binary-append| ((x cl:string)(y cl:string)) (cl:concatenate 'cl:string x y))
(defmethod |binary-append| ((x text)(y text))
  (%construct-text (fset:concat (text-data x)(text-data y))))
(defmethod |binary-append| ((x fset:wb-seq)(y fset:wb-seq))(fset:concat x y))

;;; by
(defun |cons.by| (n ls)(folio2:by n ls))
(defun |string.by| (n ls)(folio2:by n ls))
(defun |text.by| (n ls)
  (let ((parts (folio2:by n (text-data ls))))
    (fset:image (^ (p)(%construct-text p)) parts)))
(defun |treelist.by| (n ls)(folio2:by n ls))

;;; count-if
(defun |cons.count-if| (pred ls)
  (if (null ls)
      0
      (if (atom ls)
          (if (true? (funcall pred ls))
              1
              0)
          (if (true? (funcall pred (car ls)))
              (+ 1 (|cons.count-if| pred (cdr ls)))
              (|cons.count-if| pred (cdr ls))))))

(defun |string.count-if| (pred str)
  (cl:count-if (bard-predicate->lisp-predicate pred) str))

(defun |text.count-if| (pred tx)
  (let ((data (text-data tx)))
    (|treelist.count-if| pred data)))

(defun |treelist.count-if| (pred tls)
  (loop for i from 0 below (fset:size tls)
     sum (if (true? (funcall pred (fset:@ tls i)))
             1
             0)))

;;; drop
(defun |cons.drop| (n x)($ #'folio2:drop n x))
(defun |string.drop| (n x)($ #'folio2:drop n x))
(defun |text.drop| (n x)(%construct-text ($ #'folio2:drop n (text-data x))))
(defun |treelist.drop| (n x)($ #'folio2:drop n x))

;;; drop-while
(defun |cons.drop-while| (pred x)($ #'folio2:drop-while (bard-predicate->lisp-predicate pred) x))
(defun |string.drop-while| (pred x)($ #'folio2:drop-while (bard-predicate->lisp-predicate pred) x))
(defun |text.drop-while| (pred x)(%construct-text
                                  ($ #'folio2:drop-while (bard-predicate->lisp-predicate pred)
                                     (text-data x))))
(defun |treelist.drop-while| (pred x)($ #'folio2:drop-while (bard-predicate->lisp-predicate pred) x))

;;; element
(defun |cons.element| (ls i)(elt ls i))
(defun |string.element| (ls i)(elt ls i))
(defun |text.element| (ls i)(fset:@ (text-data ls) i))
(defun |treelist.element| (ls i)(fset:@ ls i))

;;; empty?
(defun |cons.empty?| (ls)(if (folio2:empty? ls)(true)(false)))
(defun |string.empty?| (ls)(if (folio2:empty? ls)(true)(false)))
(defun |text.empty?| (ls)(if (folio2:empty? (text-data ls))(true)(false)))
(defun |treelist.empty?| (ls)(if (folio2:empty? ls)(true)(false)))

;;; filter
(defun |cons.filter| (pred ls)(folio2:filter (bard-predicate->lisp-predicate pred) ls))
(defun |string.filter| (pred ls)(folio2:filter (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.filter| (pred ls)(folio2:filter (bard-predicate->lisp-predicate pred) ls))

;;; find-if
(defun |cons.find-if| (pred ls)(folio2:find-if (bard-predicate->lisp-predicate pred) ls))
(defun |string.find-if| (pred ls)(folio2:find-if (bard-predicate->lisp-predicate pred) ls))
(defun |text.find-if| (pred ls)(folio2:find-if (bard-predicate->lisp-predicate pred) (text-data ls)))
(defun |treelist.find-if| (pred ls)(folio2:find-if (bard-predicate->lisp-predicate pred) ls))

;;; first
(defun |cons.first| (x)(car x))
(defun |string.first| (x)(elt x 0))
(defun |text.first| (x)(fset:@ (text-data x) 0))
(defun |treelist.first| (x)(fset:@ x 0))

;;; head
(defun |cons.head| (x)(car x))
(defun |string.head| (x)(elt x 0))
(defun |text.head| (x)(fset:@ (text-data x) 0))
(defun |treelist.head| (x)(fset:@ x 0))

;;; image
(defun |cons.image| (fn ls)(folio2:image fn ls))
(defun |string.image| (fn ls)(folio2:image fn ls))
(defun |text.image| (fn ls)(folio2:image fn (text-data ls)))
(defun |treelist.image| (fn ls)(folio2:image fn ls))

;;; indexes
(defun |cons.indexes| (x)(folio2:indexes x))
(defun |string.indexes| (x)(folio2:indexes x))
(defun |text.indexes| (x)(folio2:indexes (text-data x)))
(defun |treelist.indexes| (x)(folio2:indexes x))

;;; interleave
(defun |cons.interleave| (x y)(folio2:interleave x y))
(defun |string.interleave| (x y)(folio2:interleave x y))
(defun |text.interleave| (x y)(%construct-text (folio2:interleave (text-data x) (text-data y))))
(defun |treelist.interleave| (x y)(folio2:interleave x y))

;;; interpose
(defun |cons.interpose| (x y)(folio2:interpose x y))
(defun |string.interpose| (x y)(folio2:interpose x y))
(defun |text.interpose| (x y)(%construct-text (folio2:interpose x (text-data y))))
(defun |treelist.interpose| (x y)(folio2:interpose x y))

;;; last
(defun |cons.last| (x)(car (cl:last x)))
(defun |string.last| (x)(elt x (1- (length x))))
(defun |text.last| (x)(fset:@ (text-data x) (1- (fset:size (text-data x)))))
(defun |treelist.last| (x)(fset:@ x (1- (fset:size x))))

;;; leave
(defun |cons.leave| (n x)($ #'folio2:leave n x))
(defun |string.leave| (n x)($ #'folio2:leave n x))
(defun |text.leave| (n x)(%construct-text ($ #'folio2:leave n (text-data x))))
(defun |treelist.leave| (n x)($ #'folio2:leave n x))

;;; length
(defun |cons.length| (x)(cl:length x))
(defun |string.length| (x)(cl:length x))
(defun |text.length| (x)(fset:size (text-data x)))
(defun |treelist.length| (x)(fset:size x))

;;; mismatch
(defun |cons.mismatch| (x y)(folio2:mismatch x y :test #'cl:equal))
(defun |string.mismatch| (x y)(folio2:mismatch x y :test #'cl:equal))
(defun |text.mismatch| (x y)(folio2:mismatch (text-data x) (text-data y) :test #'cl:equal))
(defun |treelist.mismatch| (x y)(folio2:mismatch x y :test #'cl:equal))

;;; partition
(defun |cons.partition| (pred ls)(folio2:partition (bard-predicate->lisp-predicate pred) ls))
(defun |string.partition| (pred ls)(folio2:partition (bard-predicate->lisp-predicate pred) ls))
(defun |text.partition| (pred ls)(folio2:partition (bard-predicate->lisp-predicate pred) (text-data ls)))
(defun |treelist.partition| (pred ls)(folio2:partition (bard-predicate->lisp-predicate pred) ls))

;;; penult
(defun |cons.penult| (x)(folio2:penult x))
(defun |string.penult| (x)(folio2:penult x))
(defun |text.penult| (x)(folio2:penult (text-data x)))
(defun |treelist.penult| (x)(folio2:penult x))

;;; position-if
(defun |cons.position-if|(pred ls)(folio2:position-if (bard-predicate->lisp-predicate pred) ls))
(defun |string.position-if| (pred ls)(folio2:position-if (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.position-if| (pred ls)(folio2:position-if (bard-predicate->lisp-predicate pred) ls))

;;; prefix-match?
(defun |cons.prefix-match?|(pref ls)(if (folio2:prefix-match? pref ls)(true)(false)))
(defun |string.prefix-match?| (pref ls)(if (folio2:prefix-match? pref ls)(true)(false)))
(defun |treelist.prefix-match?| (pref ls)(if (folio2:prefix-match? pref ls)(true)(false)))

;;; reduce
(defun |cons.reduce| (fn init ls)
  (folio2:reduce fn ls :initial-value init))

(defun |string.reduce| (fn init ls)
  (folio2:reduce fn ls :initial-value init))

(defun |treelist.reduce| (fn init ls)
  (folio2:reduce fn ls :initial-value init))

;;; remove-if
(defun |cons.remove-if|(pred ls)(folio2:remove-if (bard-predicate->lisp-predicate pred) ls))
(defun |string.remove-if| (pred ls)(folio2:remove-if (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.remove-if| (pred ls)(folio2:remove-if (bard-predicate->lisp-predicate pred) ls))

;;; remove-duplicates
(defun |cons.remove-duplicates|(ls)(folio2:remove-duplicates ls :test #'equal))
(defun |string.remove-duplicates|(ls)(folio2:remove-duplicates ls :test #'equal))
(defun |treelist.remove-duplicates|(ls)(folio2:remove-duplicates ls :test #'equal))

;;; rest
(defun |cons.rest| (x)(folio2:rest x))
(defun |string.rest| (x)(folio2:rest x))
(defun |treelist.rest| (x)(folio2:rest x))

;;; reverse
(defun |cons.reverse| (x)(folio2:reverse x))
(defun |string.reverse| (x)(folio2:reverse x))
(defun |treelist.reverse| (x)(folio2:reverse x))

;;; search
(defun |cons.search|(pref ls)(folio2:search pref ls :test #'equal))
(defun |string.search| (pref ls)(folio2:search pref ls :test #'equal))
(defun |treelist.search| (pref ls)(folio2:search pref ls :test #'equal))

;;; second
(defun |cons.second| (x)(folio2:second x))
(defun |string.second| (x)(folio2:second x))
(defun |treelist.second| (x)(folio2:second x))

;;; select
(defun |cons.select|(ls indexes)(folio2:select ls indexes))
(defun |string.select| (ls indexes)(folio2:select ls indexes))
(defun |treelist.select| (ls indexes)(folio2:select ls indexes))

;;; shuffle
(defun |cons.shuffle| (x)(folio2:shuffle x))
(defun |string.shuffle| (x)(folio2:shuffle x))
(defun |treelist.shuffle| (x)(folio2:shuffle x))

;;; some?
(defun |cons.some?|(pred ls)(folio2:some? (bard-predicate->lisp-predicate pred) ls))
(defun |string.some?| (pred ls)(folio2:some? (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.some?| (pred ls)(folio2:some? (bard-predicate->lisp-predicate pred) ls))

;;; sort
(defun |cons.sort|(pred ls)(folio2:sort ls (bard-predicate->lisp-predicate pred)))
(defun |string.sort| (pred ls)(folio2:sort ls (bard-predicate->lisp-predicate pred)))
(defun |treelist.sort| (pred ls)(folio2:sort ls (bard-predicate->lisp-predicate pred)))

;;; split
(defun |cons.split|(ls sentinel)(folio2:split ls sentinel :test #'equal))
(defun |string.split| (ls sentinel)(folio2:split ls sentinel :test #'equal))
(defun |treelist.split| (ls sentinel)(folio2:split ls sentinel :test #'equal))

;;; sublist
(defun |cons.sublist|(ls start end)(folio2:subsequence ls start end))
(defun |string.sublist|(ls start end)(folio2:subsequence ls start end))
(defun |treelist.sublist|(ls start end)(folio2:subsequence ls start end))

;;; substitute-if
(defun |cons.substitute-if|(new pred ls)(folio2:substitute-if new (bard-predicate->lisp-predicate pred) ls))
(defun |string.substitute-if|(new pred ls)(folio2:substitute-if new (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.substitute-if|(new pred ls)(folio2:substitute-if new (bard-predicate->lisp-predicate pred) ls))

;;; suffix-match?
(defun |cons.suffix-match?|(ls suff)(if (folio2:suffix-match? ls suff)(true)(false)))
(defun |string.suffix-match?| (ls suff)(if (folio2:suffix-match? ls suff)(true)(false)))
(defun |treelist.suffix-match?| (ls suff)(if (folio2:suffix-match? ls suff)(true)(false)))

;;; tail
(defun |cons.tail| (x)(folio2:tail x))
(defun |string.tail| (x)(folio2:tail x))
(defun |treelist.tail| (x)(folio2:tail x))

;;; tails
(defun |cons.tails| (x)(folio2:tails x))
(defun |string.tails| (x)(folio2:tails x))
(defun |treelist.tails| (x)(folio2:tails x))

;;; take
(defun |cons.take|(n ls)(folio2:take n ls))
(defun |string.take|(n ls)(folio2:take n ls))
(defun |treelist.take|(n ls)(folio2:take n ls))

;;; take-by
(defun |cons.take-by|(m n ls)(folio2:take-by m n ls))
(defun |string.take-by|(m n ls)(folio2:take-by m n ls))
(defun |treelist.take-by|(m n ls)(folio2:take-by m n ls))

;;; take-while
(defun |cons.take-while|(pred ls)(folio2:take-while (bard-predicate->lisp-predicate pred) ls))
(defun |string.take-while|(pred ls)(folio2:take-while (bard-predicate->lisp-predicate pred) ls))
(defun |treelist.take-while|(pred ls)(folio2:take-while (bard-predicate->lisp-predicate pred) ls))

;;; zip
(defun |cons.zip|(ls1 ls2)(folio2:zip  ls1 ls2))
(defun |string.zip|(ls1 ls2)(folio2:zip ls1 ls2))
(defun |treelist.zip|(ls1 ls2)(folio2:zip ls1 ls2))

;;; Math protocol
;;; ----------------------------------------

;;; +
(defun |Math.+| (&rest nums)(cl:apply #'cl:+ nums))

;;; -
(defun |Math.-| (&rest nums)(cl:apply #'cl:- nums))

;;; *
(defun |Math.*| (&rest nums)(cl:apply #'cl:* nums))

;;; /
(defun |Math./| (&rest nums)(cl:apply #'cl:/ nums))

;;; <
(defun |Math.<| (&rest nums)(cl:apply #'cl:< nums))

;;; <=
(defun |Math.<=| (&rest nums)(cl:apply #'cl:<= nums))

;;; >
(defun |Math.>| (&rest nums)(cl:apply #'cl:> nums))

;;; >=
(defun |Math.>=| (&rest nums)(cl:apply #'cl:>= nums))

;;; even?
(defun |Integer.even?| (x)(if (cl:evenp x) (true) (false)))

;;; odd?
(defun |Integer.odd?| (x)(if (cl:oddp x) (true) (false)))


;;; Pair protocol
;;; ----------------------------------------

;;;  left
(defun |cons.left| (x)(cl:car x))

;;; put-left
(defun |cons.put-left| (x val)(cons val (cdr x)))

;;; put-right
(defun |cons.put-right| (x val)(cons (car x) val))

;;; right
(defun |cons.right| (x)(cl:cdr x))

;;; set-left!
(defun |cons.set-left!| (x val)(setf (car x) val))

;;; set-right!
(defun |cons.set-right!| (x val)(setf (cdr x) val))


;;; Type protocol
;;; ----------------------------------------

;;;  type-of
(defmethod |anything.type-of| (x)(bard-type-of x))

;;; ---------------------------------------------------------------------
;;; init bard functions
;;; ---------------------------------------------------------------------

(defmethod init-bard-functions ((bard bard))
  
  ;; Character protocol
  ;; ----------------------------------------

  ;; alphanumeric?
  (global-set! bard 'bard::|alphanumeric?| (%construct-function |Character| :|name| 'bard::|alphanumeric?|))
  (add-method! (global-ref bard 'bard::|alphanumeric?|)(list |Character|) #'|character.alphanumeric?|)

  ;; Construction protocol
  ;; ----------------------------------------
  ;; the function `make`

  ;; Conversion protocol
  ;; ----------------------------------------
  ;; the function `as`

  (global-set! bard 'bard::|as| (%construct-function |Type| |Anything| :|name| 'bard::|as|))
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |cons|) |cons|) #'|as.cons.cons|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |cons|) |string|) #'|as.cons.string|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |cons|) |text|) #'|as.cons.text|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |string|) |treelist|) #'|as.string.treelist|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |string|) |text|) #'|as.string.text|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |cons|) |treelist|) #'|as.cons.treelist|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |string|) |cons|) #'|as.string.cons|)
  (add-method! (global-ref bard 'bard::|as|)(list ($ |singleton| |treelist|) |cons|) #'|as.treelist.cons|)
  
  ;; Function protocol
  ;; ----------------------------------------

  ;; complement
  (global-set! bard 'bard::|complement| (%construct-function |Procedure| :|name| 'bard::|complement|))
  (add-method! (global-ref bard 'bard::|complement|)(list |Procedure|) #'|function.complement|)
  
  ;; Geometry protocol
  ;; ----------------------------------------

  (global-set! bard 'bard::|point-x| (%construct-function |Point| :|name| 'bard::|point-x|))
  (add-method! (global-ref bard 'bard::|point-x|)(list |rectangular-point|) #'|rectangular-point.x|)

  (global-set! bard 'bard::|point-y| (%construct-function |Point| :|name| 'bard::|point-y|))
  (add-method! (global-ref bard 'bard::|point-y|)(list |rectangular-point|) #'|rectangular-point.y|)

  (global-set! bard 'bard::|extent-width| (%construct-function |Extent| :|name| 'bard::|rectangular-extent.width|))
  (add-method! (global-ref bard 'bard::|extent-width|)(list |rectangular-extent|) #'|rectangular-extent.width|)

  (global-set! bard 'bard::|extent-height| (%construct-function |Extent| :|name| 'bard::|rectangular-extent.height|))
  (add-method! (global-ref bard 'bard::|extent-height|)(list |rectangular-extent|) #'|rectangular-extent.height|)
  
  ;; List protocol
  ;; ----------------------------------------

  ;; add-first
  (global-set! bard 'bard::|add-first| (%construct-function |Anything| |List| :|name| 'bard::|add-first|))
  (add-method! (global-ref bard 'bard::|add-first|)(list |Anything| |cons|) #'|cons.add-first|)
  (add-method! (global-ref bard 'bard::|add-first|)(list |Character| |string|) #'|string.add-first|)
  (add-method! (global-ref bard 'bard::|add-first|)(list |Character| |text|) #'|text.add-first|)
  (add-method! (global-ref bard 'bard::|add-first|)(list |Anything| |treelist|) #'|treelist.add-first|)

  ;; add-last
  (global-set! bard 'bard::|add-last| (%construct-function |List| |Anything| :|name| 'bard::|add-last|))
  (add-method! (global-ref bard 'bard::|add-last|)(list |cons| |Anything|) #'|cons.add-last|)
  (add-method! (global-ref bard 'bard::|add-last|)(list |string| |Character|) #'|string.add-last|)
  (add-method! (global-ref bard 'bard::|add-last|)(list |text| |Character|) #'|text.add-last|)
  (add-method! (global-ref bard 'bard::|add-last|)(list |treelist| |Anything|) #'|treelist.add-last|)

  ;; any
  (global-set! bard 'bard::|any| (%construct-function |List| :|name| 'bard::|any|))
  (add-method! (global-ref bard 'bard::|any|)(list |cons|) #'|cons.any|)
  (add-method! (global-ref bard 'bard::|any|)(list |string|) #'|string.any|)
  (add-method! (global-ref bard 'bard::|any|)(list |text|) #'|text.any|)
  (add-method! (global-ref bard 'bard::|any|)(list |treelist|) #'|treelist.any|)

  ;; append
  (global-set! bard 'bard::|append| (%construct-function |List| |List| :|name| 'bard::|append|))
  (add-method! (global-ref bard 'bard::|append|)(list |cons| |cons|) #'|binary-append|)
  (add-method! (global-ref bard 'bard::|append|)(list |string| |string|) #'|binary-append|)
  (add-method! (global-ref bard 'bard::|append|)(list |text| |text|) #'|binary-append|)
  (add-method! (global-ref bard 'bard::|append|)(list |treelist| |treelist|) #'|binary-append|)

  ;; apportion
  (global-set! bard 'bard::|apportion| (%construct-function |List| (&) :|name| 'bard::|apportion|))
  (add-method! (global-ref bard 'bard::|apportion|)(list |cons| (&)) #'|cons.apportion|)
  (add-method! (global-ref bard 'bard::|apportion|)(list |string| (&)) #'|string.apportion|)
  (add-method! (global-ref bard 'bard::|apportion|)(list |text| (&)) #'|text.apportion|)
  (add-method! (global-ref bard 'bard::|apportion|)(list |treelist| (&)) #'|treelist.apportion|)

  ;; by
  (global-set! bard 'bard::|by| (%construct-function |Integer| |List| :|name| 'bard::|by|))
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |cons|) #'|cons.by|)
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |string|) #'|string.by|)
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |text|) #'|text.by|)
  (add-method! (global-ref bard 'bard::|by|)(list |Integer| |treelist|) #'|treelist.by|)

  ;; count-if
  (global-set! bard 'bard::|count-if| (%construct-function |Procedure| |List| :|name| 'bard::|count-if|))
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |cons|) #'|cons.count-if|)
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |string|) #'|string.count-if|)
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |text|) #'|text.count-if|)
  (add-method! (global-ref bard 'bard::|count-if|)(list |Procedure| |treelist|) #'|treelist.count-if|)

  ;; drop
  (global-set! bard 'bard::|drop| (%construct-function |Integer| |List| :|name| 'bard::|drop|))
  (add-method! (global-ref bard 'bard::|drop|)(list |Integer| |cons|) #'|cons.drop|)
  (add-method! (global-ref bard 'bard::|drop|)(list |Integer| |string|) #'|string.drop|)
  (add-method! (global-ref bard 'bard::|drop|)(list |Integer| |text|) #'|text.drop|)
  (add-method! (global-ref bard 'bard::|drop|)(list |Integer| |treelist|) #'|treelist.drop|)

  ;; drop-while
  (global-set! bard 'bard::|drop-while| (%construct-function |Procedure| |List| :|name| 'bard::|drop-while|))
  (add-method! (global-ref bard 'bard::|drop-while|)(list |Procedure| |cons|) #'|cons.drop-while|)
  (add-method! (global-ref bard 'bard::|drop-while|)(list |Procedure| |string|) #'|string.drop-while|)
  (add-method! (global-ref bard 'bard::|drop-while|)(list |Procedure| |text|) #'|text.drop-while|)
  (add-method! (global-ref bard 'bard::|drop-while|)(list |Procedure| |treelist|) #'|treelist.drop-while|)

  ;; element
  (global-set! bard 'bard::|element| (%construct-function |List| |Integer| :|name| 'bard::|element|))
  (add-method! (global-ref bard 'bard::|element|)(list |cons| |Integer|) #'|cons.element|)
  (add-method! (global-ref bard 'bard::|element|)(list |string| |Integer|) #'|string.element|)
  (add-method! (global-ref bard 'bard::|element|)(list |text| |Integer|) #'|text.element|)
  (add-method! (global-ref bard 'bard::|element|)(list |treelist| |Integer|) #'|treelist.element|)

  ;; empty?
  (global-set! bard 'bard::|empty?| (%construct-function |List| :|name| 'bard::|empty?|))
  (add-method! (global-ref bard 'bard::|empty?|)(list |cons|) #'|cons.empty?|)
  (add-method! (global-ref bard 'bard::|empty?|)(list |string|) #'|string.empty?|)
  (add-method! (global-ref bard 'bard::|empty?|)(list |text|) #'|text.empty?|)
  (add-method! (global-ref bard 'bard::|empty?|)(list |treelist|) #'|treelist.empty?|)

  ;; filter
  (global-set! bard 'bard::|filter| (%construct-function |Procedure| |List| :|name| 'bard::|filter|))
  (add-method! (global-ref bard 'bard::|filter|)(list |Procedure| |cons|) #'|cons.filter|)
  (add-method! (global-ref bard 'bard::|filter|)(list |Procedure| |string|) #'|string.filter|)
  (add-method! (global-ref bard 'bard::|filter|)(list |Procedure| |treelist|) #'|treelist.filter|)

  ;; find-if
  (global-set! bard 'bard::|find-if| (%construct-function |Procedure| |List| :|name| 'bard::|find-if|))
  (add-method! (global-ref bard 'bard::|find-if|)(list |Procedure| |cons|) #'|cons.find-if|)
  (add-method! (global-ref bard 'bard::|find-if|)(list |Procedure| |string|) #'|string.find-if|)
  (add-method! (global-ref bard 'bard::|find-if|)(list |Procedure| |text|) #'|text.find-if|)
  (add-method! (global-ref bard 'bard::|find-if|)(list |Procedure| |treelist|) #'|treelist.find-if|)
  
  ;; first
  (global-set! bard 'bard::|first| (%construct-function |List| :|name| 'bard::|first|))
  (add-method! (global-ref bard 'bard::|first|)(list |cons|) #'|cons.first|)
  (add-method! (global-ref bard 'bard::|first|)(list |string|) #'|string.first|)
  (add-method! (global-ref bard 'bard::|first|)(list |text|) #'|text.first|)
  (add-method! (global-ref bard 'bard::|first|)(list |treelist|) #'|treelist.first|)

  ;; head
  (global-set! bard 'bard::|head| (%construct-function |List| :|name| 'bard::|head|))
  (add-method! (global-ref bard 'bard::|head|)(list |cons|) #'|cons.head|)
  (add-method! (global-ref bard 'bard::|head|)(list |string|) #'|string.head|)
  (add-method! (global-ref bard 'bard::|head|)(list |text|) #'|text.head|)
  (add-method! (global-ref bard 'bard::|head|)(list |treelist|) #'|treelist.head|)

  ;; image
  (global-set! bard 'bard::|image| (%construct-function |Procedure| |List| :|name| 'bard::|image|))
  (add-method! (global-ref bard 'bard::|image|)(list |Procedure| |cons|) #'|cons.image|)
  (add-method! (global-ref bard 'bard::|image|)(list |Procedure| |string|) #'|string.image|)
  (add-method! (global-ref bard 'bard::|image|)(list |Procedure| |text|) #'|text.image|)
  (add-method! (global-ref bard 'bard::|image|)(list |Procedure| |treelist|) #'|treelist.image|)
  
  ;; indexes
  (global-set! bard 'bard::|indexes| (%construct-function |List| :|name| 'bard::|indexes|))
  (add-method! (global-ref bard 'bard::|indexes|)(list |cons|) #'|cons.indexes|)
  (add-method! (global-ref bard 'bard::|indexes|)(list |string|) #'|string.indexes|)
  (add-method! (global-ref bard 'bard::|indexes|)(list |text|) #'|text.indexes|)
  (add-method! (global-ref bard 'bard::|indexes|)(list |treelist|) #'|treelist.indexes|)

  ;; interleave
  (global-set! bard 'bard::|interleave| (%construct-function |List| |List| :|name| 'bard::|interleave|))
  (add-method! (global-ref bard 'bard::|interleave|)(list |cons| |cons|) #'|cons.interleave|)
  (add-method! (global-ref bard 'bard::|interleave|)(list |string| |string|) #'|string.interleave|)
  (add-method! (global-ref bard 'bard::|interleave|)(list |text| |text|) #'|text.interleave|)
  (add-method! (global-ref bard 'bard::|interleave|)(list |treelist| |treelist|) #'|treelist.interleave|)

  ;; interpose
  (global-set! bard 'bard::|interpose| (%construct-function |Anything| |List|  :|name| 'bard::|interpose|))
  (add-method! (global-ref bard 'bard::|interpose|)(list |Anything| |cons|) #'|cons.interpose|)
  (add-method! (global-ref bard 'bard::|interpose|)(list |Character| |string| ) #'|string.interpose|)
  (add-method! (global-ref bard 'bard::|interpose|)(list |Character| |text| ) #'|text.interpose|)
  (add-method! (global-ref bard 'bard::|interpose|)(list |Anything| |treelist| ) #'|treelist.interpose|)

  ;; last
  (global-set! bard 'bard::|last| (%construct-function |List| :|name| 'bard::|last|))
  (add-method! (global-ref bard 'bard::|last|)(list |cons|) #'|cons.last|)
  (add-method! (global-ref bard 'bard::|last|)(list |string|) #'|string.last|)
  (add-method! (global-ref bard 'bard::|last|)(list |text|) #'|text.last|)
  (add-method! (global-ref bard 'bard::|last|)(list |treelist|) #'|treelist.last|)

  ;; leave
  (global-set! bard 'bard::|leave| (%construct-function |Integer| |List| :|name| 'bard::|leave|))
  (add-method! (global-ref bard 'bard::|leave|)(list |Integer| |cons|) #'|cons.leave|)
  (add-method! (global-ref bard 'bard::|leave|)(list |Integer| |string|) #'|string.leave|)
  (add-method! (global-ref bard 'bard::|leave|)(list |Integer| |text|) #'|text.leave|)
  (add-method! (global-ref bard 'bard::|leave|)(list |Integer| |treelist|) #'|treelist.leave|)

  ;; length
  (global-set! bard 'bard::|length| (%construct-function |List| :|name| 'bard::|length|))
  (add-method! (global-ref bard 'bard::|length|)(list |cons|) #'|cons.length|)
  (add-method! (global-ref bard 'bard::|length|)(list |string|) #'|string.length|)
  (add-method! (global-ref bard 'bard::|length|)(list |text|) #'|text.length|)
  (add-method! (global-ref bard 'bard::|length|)(list |treelist|) #'|treelist.length|)
  
  ;; list?
  (global-set! bard 'bard::|list?| (%construct-function |Anything| :|name| 'bard::|list?|))
  (add-method! (global-ref bard 'bard::|list?|)(list |Anything|) (constantly (false)))
  (add-method! (global-ref bard 'bard::|list?|)(list |cons|) (constantly (true)))
  (add-method! (global-ref bard 'bard::|list?|)(list |string|) (constantly (true)))
  (add-method! (global-ref bard 'bard::|list?|)(list |text|) (constantly (true)))
  (add-method! (global-ref bard 'bard::|list?|)(list |treelist|) (constantly (true)))

  ;; mismatch
  (global-set! bard 'bard::|mismatch| (%construct-function |List| |List| :|name| 'bard::|mismatch|))
  (add-method! (global-ref bard 'bard::|mismatch|)(list |cons| |cons|) #'|cons.mismatch|)
  (add-method! (global-ref bard 'bard::|mismatch|)(list |string| |string|) #'|string.mismatch|)
  (add-method! (global-ref bard 'bard::|mismatch|)(list |text| |text|) #'|text.mismatch|)
  (add-method! (global-ref bard 'bard::|mismatch|)(list |treelist| |treelist|) #'|treelist.mismatch|)

  ;; partition
  (global-set! bard 'bard::|partition| (%construct-function |Procedure| |List| :|name| 'bard::|partition|))
  (add-method! (global-ref bard 'bard::|partition|)(list |Procedure| |cons|) #'|cons.partition|)
  (add-method! (global-ref bard 'bard::|partition|)(list |Procedure| |string|) #'|string.partition|)
  (add-method! (global-ref bard 'bard::|partition|)(list |Procedure| |text|) #'|text.partition|)
  (add-method! (global-ref bard 'bard::|partition|)(list |Procedure| |treelist|) #'|treelist.partition|)

  ;; penult
  (global-set! bard 'bard::|penult| (%construct-function |List| :|name| 'bard::|penult|))
  (add-method! (global-ref bard 'bard::|penult|)(list |cons|) #'|cons.penult|)
  (add-method! (global-ref bard 'bard::|penult|)(list |string|) #'|string.penult|)
  (add-method! (global-ref bard 'bard::|penult|)(list |text|) #'|text.penult|)
  (add-method! (global-ref bard 'bard::|penult|)(list |treelist|) #'|treelist.penult|)

  ;; position-if
  (global-set! bard 'bard::|position-if| (%construct-function |Procedure| |List| :|name| 'bard::|position-if|))
  (add-method! (global-ref bard 'bard::|position-if|)(list |Procedure| |cons|) #'|cons.position-if|)
  (add-method! (global-ref bard 'bard::|position-if|)(list |Procedure| |string|) #'|string.position-if|)
  (add-method! (global-ref bard 'bard::|position-if|)(list |Procedure| |treelist|) #'|treelist.position-if|)

  ;; prefix-match?
  (global-set! bard 'bard::|prefix-match?| (%construct-function |List| |List| :|name| 'bard::|prefix-match?|))
  (add-method! (global-ref bard 'bard::|prefix-match?|)(list |cons| |cons|) #'|cons.prefix-match?|)
  (add-method! (global-ref bard 'bard::|prefix-match?|)(list |string| |string|) #'|string.prefix-match?|)
  (add-method! (global-ref bard 'bard::|prefix-match?|)(list |treelist| |treelist|) #'|treelist.prefix-match?|)

  ;; reduce
  (global-set! bard 'bard::|reduce| (%construct-function |Procedure| |Anything| |List| :|name| 'bard::|reduce|))
  (add-method! (global-ref bard 'bard::|reduce|)(list |Procedure| |Anything| |cons|) #'|cons.reduce|)
  (add-method! (global-ref bard 'bard::|reduce|)(list |Procedure| |Anything| |string|) #'|string.reduce|)
  (add-method! (global-ref bard 'bard::|reduce|)(list |Procedure| |Anything| |treelist|) #'|treelist.reduce|)

  ;; remove-if
  (global-set! bard 'bard::|remove-if| (%construct-function |Procedure| |List| :|name| 'bard::|remove-if|))
  (add-method! (global-ref bard 'bard::|remove-if|)(list |Procedure| |cons|) #'|cons.remove-if|)
  (add-method! (global-ref bard 'bard::|remove-if|)(list |Procedure| |string|) #'|string.remove-if|)
  (add-method! (global-ref bard 'bard::|remove-if|)(list |Procedure| |treelist|) #'|treelist.remove-if|)

  ;; remove-duplicates
  (global-set! bard 'bard::|remove-duplicates| (%construct-function |List| :|name| 'bard::|remove-duplicates|))
  (add-method! (global-ref bard 'bard::|remove-duplicates|)(list |cons|) #'|cons.remove-duplicates|)
  (add-method! (global-ref bard 'bard::|remove-duplicates|)(list |string|) #'|string.remove-duplicates|)
  (add-method! (global-ref bard 'bard::|remove-duplicates|)(list |treelist|) #'|treelist.remove-duplicates|)

  ;;; rest
  (global-set! bard 'bard::|rest| (%construct-function |List| :|name| 'bard::|rest|))
  (add-method! (global-ref bard 'bard::|rest|)(list |cons|) #'|cons.rest|)
  (add-method! (global-ref bard 'bard::|rest|)(list |string|) #'|string.rest|)
  (add-method! (global-ref bard 'bard::|rest|)(list |treelist|) #'|treelist.rest|)

  ;; reverse
  (global-set! bard 'bard::|reverse| (%construct-function |List| :|name| 'bard::|reverse|))
  (add-method! (global-ref bard 'bard::|reverse|)(list |cons|) #'|cons.reverse|)
  (add-method! (global-ref bard 'bard::|reverse|)(list |string|) #'|string.reverse|)
  (add-method! (global-ref bard 'bard::|reverse|)(list |treelist|) #'|treelist.reverse|)

  ;; search
  (global-set! bard 'bard::|search| (%construct-function |List| |List| :|name| 'bard::|search|))
  (add-method! (global-ref bard 'bard::|search|)(list |cons| |cons|) #'|cons.search|)
  (add-method! (global-ref bard 'bard::|search|)(list |string| |string|) #'|string.search|)
  (add-method! (global-ref bard 'bard::|search|)(list |treelist| |treelist|) #'|treelist.search|)

  ;;; second
  (global-set! bard 'bard::|second| (%construct-function |List| :|name| 'bard::|second|))
  (add-method! (global-ref bard 'bard::|second|)(list |cons|) #'|cons.second|)
  (add-method! (global-ref bard 'bard::|second|)(list |string|) #'|string.second|)
  (add-method! (global-ref bard 'bard::|second|)(list |treelist|) #'|treelist.second|)

  ;; select
  (global-set! bard 'bard::|select| (%construct-function |List| |List| :|name| 'bard::|select|))
  (add-method! (global-ref bard 'bard::|select|)(list |cons| |List|) #'|cons.select|)
  (add-method! (global-ref bard 'bard::|select|)(list |string| |List|) #'|string.select|)
  (add-method! (global-ref bard 'bard::|select|)(list |treelist| |List|) #'|treelist.select|)

  ;;; shuffle
  (global-set! bard 'bard::|shuffle| (%construct-function |List| :|name| 'bard::|shuffle|))
  (add-method! (global-ref bard 'bard::|shuffle|)(list |cons|) #'|cons.shuffle|)
  (add-method! (global-ref bard 'bard::|shuffle|)(list |string|) #'|string.shuffle|)
  (add-method! (global-ref bard 'bard::|shuffle|)(list |treelist|) #'|treelist.shuffle|)

  ;; some?
  (global-set! bard 'bard::|some?| (%construct-function |Procedure| |List| :|name| 'bard::|some?|))
  (add-method! (global-ref bard 'bard::|some?|)(list |Procedure| |cons|) #'|cons.some?|)
  (add-method! (global-ref bard 'bard::|some?|)(list |Procedure| |string|) #'|string.some?|)
  (add-method! (global-ref bard 'bard::|some?|)(list |Procedure| |treelist|) #'|treelist.some?|)

  ;; sort
  (global-set! bard 'bard::|sort| (%construct-function |Procedure| |List| :|name| 'bard::|sort|))
  (add-method! (global-ref bard 'bard::|sort|)(list |Procedure| |cons|) #'|cons.sort|)
  (add-method! (global-ref bard 'bard::|sort|)(list |Procedure| |string|) #'|string.sort|)
  (add-method! (global-ref bard 'bard::|sort|)(list |Procedure| |treelist|) #'|treelist.sort|)

  ;;; split
  (global-set! bard 'bard::|split| (%construct-function |List| |Anything| :|name| 'bard::|split|))
  (add-method! (global-ref bard 'bard::|split|)(list |cons| |cons|) #'|cons.split|)
  (add-method! (global-ref bard 'bard::|split|)(list |string| |string|) #'|string.split|)
  (add-method! (global-ref bard 'bard::|split|)(list |treelist| |treelist|) #'|treelist.split|)

  ;; sublist
  (global-set! bard 'bard::|sublist| (%construct-function |List| |Integer| |Integer| :|name| 'bard::|sublist|))
  (add-method! (global-ref bard 'bard::|sublist|)(list |cons| |Integer| |Integer|) #'|cons.sublist|)
  (add-method! (global-ref bard 'bard::|sublist|)(list |string| |Integer| |Integer|) #'|string.sublist|)
  (add-method! (global-ref bard 'bard::|sublist|)(list |treelist| |Integer| |Integer|) #'|treelist.sublist|)

  ;; substitute-if
  (global-set! bard 'bard::|substitute-if| (%construct-function |Anything| |Procedure| |List| :|name| 'bard::|substitute-if|))
  (add-method! (global-ref bard 'bard::|substitute-if|)(list |Anything| |Procedure| |cons|) #'|cons.substitute-if|)
  (add-method! (global-ref bard 'bard::|substitute-if|)(list |Anything| |Procedure| |string|) #'|string.substitute-if|)
  (add-method! (global-ref bard 'bard::|substitute-if|)(list |Anything| |Procedure| |treelist|) #'|treelist.substitute-if|)

  ;; suffix-match?
  (global-set! bard 'bard::|suffix-match?| (%construct-function |List| |List| :|name| 'bard::|suffix-match?|))
  (add-method! (global-ref bard 'bard::|suffix-match?|)(list |cons| |cons|) #'|cons.suffix-match?|)
  (add-method! (global-ref bard 'bard::|suffix-match?|)(list |string| |string|) #'|string.suffix-match?|)
  (add-method! (global-ref bard 'bard::|suffix-match?|)(list |treelist| |treelist|) #'|treelist.suffix-match?|)

  ;;; tail
  (global-set! bard 'bard::|tail| (%construct-function |List| :|name| 'bard::|tail|))
  (add-method! (global-ref bard 'bard::|tail|)(list |cons|) #'|cons.tail|)
  (add-method! (global-ref bard 'bard::|tail|)(list |string|) #'|string.tail|)
  (add-method! (global-ref bard 'bard::|tail|)(list |treelist|) #'|treelist.tail|)

  ;;; tails
  (global-set! bard 'bard::|tails| (%construct-function |List| :|name| 'bard::|tails|))
  (add-method! (global-ref bard 'bard::|tails|)(list |cons|) #'|cons.tails|)
  (add-method! (global-ref bard 'bard::|tails|)(list |string|) #'|string.tails|)
  (add-method! (global-ref bard 'bard::|tails|)(list |treelist|) #'|treelist.tails|)

  ;;; take
  (global-set! bard 'bard::|take| (%construct-function |Integer| |List| :|name| 'bard::|take|))
  (add-method! (global-ref bard 'bard::|take|)(list |Integer| |cons|) #'|cons.take|)
  (add-method! (global-ref bard 'bard::|take|)(list |Integer| |string|) #'|string.take|)
  (add-method! (global-ref bard 'bard::|take|)(list |Integer| |treelist|) #'|treelist.take|)

  ;;; take-by
  (global-set! bard 'bard::|take-by| (%construct-function |Integer| |Integer| |List| :|name| 'bard::|take-by|))
  (add-method! (global-ref bard 'bard::|take-by|)(list |Integer| |Integer| |cons|) #'|cons.take-by|)
  (add-method! (global-ref bard 'bard::|take-by|)(list |Integer| |Integer| |string|) #'|string.take-by|)
  (add-method! (global-ref bard 'bard::|take-by|)(list |Integer| |Integer| |treelist|) #'|treelist.take-by|)

  ;; take-while
  (global-set! bard 'bard::|take-while| (%construct-function |Procedure| |List| :|name| 'bard::|take-while|))
  (add-method! (global-ref bard 'bard::|take-while|)(list |Procedure| |cons|) #'|cons.take-while|)
  (add-method! (global-ref bard 'bard::|take-while|)(list |Procedure| |string|) #'|string.take-while|)
  (add-method! (global-ref bard 'bard::|take-while|)(list |Procedure| |treelist|) #'|treelist.take-while|)

  ;;; zip
  (global-set! bard 'bard::|zip| (%construct-function |List| |List| :|name| 'bard::|zip|))
  (add-method! (global-ref bard 'bard::|zip|)(list |cons| |cons|) #'|cons.zip|)
  (add-method! (global-ref bard 'bard::|zip|)(list |string| |string|) #'|string.zip|)
  (add-method! (global-ref bard 'bard::|zip|)(list |treelist| |treelist|) #'|treelist.zip|)

  ;; Math protocol
  ;; ----------------------------------------

  (global-set! bard 'bard::|+| (%construct-function |Number| (&) :|name| 'bard::|+|))
  (add-method! (global-ref bard 'bard::|+|)(list |Number| (&)) #'|Math.+|)

  (global-set! bard 'bard::|-| (%construct-function |Number| (&) :|name| 'bard::|-|))
  (add-method! (global-ref bard 'bard::|-|)(list |Number| (&)) #'|Math.-|)

  (global-set! bard 'bard::|*| (%construct-function |Number| (&) :|name| 'bard::|*|))
  (add-method! (global-ref bard 'bard::|*|)(list |Number| |Number|) #'|Math.*|)

  (global-set! bard 'bard::|/| (%construct-function |Number| (&) :|name| 'bard::|/|))
  (add-method! (global-ref bard 'bard::|/|)(list |Number| |Number|) #'|Math./|)

  (global-set! bard 'bard::|even?| (%construct-function |Integer| :|name| 'bard::|even?|))
  (add-method! (global-ref bard 'bard::|even?|)(list |Integer|) #'|Integer.even?|)

  (global-set! bard 'bard::|odd?| (%construct-function |Integer| :|name| 'bard::|odd?|))
  (add-method! (global-ref bard 'bard::|odd?|)(list |Integer|) #'|Integer.odd?|)

  (global-set! bard 'bard::|<| (%construct-function |Number| |Number| (&) :|name| 'bard::|<|))
  (add-method! (global-ref bard 'bard::|<|)(list |Number| |Number| (&)) #'|Math.<|)

  (global-set! bard 'bard::|<=| (%construct-function |Number| |Number| (&) :|name| 'bard::|<=|))
  (add-method! (global-ref bard 'bard::|<=|)(list |Number| |Number| (&)) #'|Math.<=|)

  (global-set! bard 'bard::|>| (%construct-function |Number| |Number| (&) :|name| 'bard::|>|))
  (add-method! (global-ref bard 'bard::|>|)(list |Number| |Number| (&)) #'|Math.>|)

  (global-set! bard 'bard::|>=| (%construct-function |Number| |Number| (&) :|name| 'bard::|>=|))
  (add-method! (global-ref bard 'bard::|>=|)(list |Number| |Number| (&)) #'|Math.>=|)

  ;; Pair protocol
  ;; ----------------------------------------

  ;; left
  (global-set! bard 'bard::|left| (%construct-function |Pair| :|name| 'bard::|left|))
  (add-method! (global-ref bard 'bard::|left|)(list |cons|) #'|cons.left|)

  ;; pair?
  (global-set! bard 'bard::|pair?| (%construct-function |Anything| :|name| 'bard::|pair?|))
  (add-method! (global-ref bard 'bard::|pair?|)(list |Anything|) (cl:constantly (false)))
  (add-method! (global-ref bard 'bard::|pair?|)(list |cons|) (cl:constantly (true)))

  ;; put-left
  (global-set! bard 'bard::|put-left| (%construct-function |Pair| |Anything| :|name| 'bard::|put-left|))
  (add-method! (global-ref bard 'bard::|put-left|)(list |Pair| |Anything|) #'|cons.put-left|)

  ;; put-right
  (global-set! bard 'bard::|put-right| (%construct-function |Pair| |Anything| :|name| 'bard::|put-right|))
  (add-method! (global-ref bard 'bard::|put-right|)(list |Pair| |Anything|) #'|cons.put-right|)

  ;; right
  (global-set! bard 'bard::|right| (%construct-function |Pair| :|name| 'bard::|right|))
  (add-method! (global-ref bard 'bard::|right|)(list |cons|) #'|cons.right|)

  ;; set-left!
  (global-set! bard 'bard::|set-left!| (%construct-function |Pair| |Anything| :|name| 'bard::|set-left!|))
  (add-method! (global-ref bard 'bard::|set-left!|)(list |cons| |Anything|) #'|cons.set-left!|)

  ;; set-right!
  (global-set! bard 'bard::|set-right!| (%construct-function |Pair| |Anything| :|name| 'bard::|set-right!|))
  (add-method! (global-ref bard 'bard::|set-right!|)(list |cons| |Anything|) #'|cons.set-right!|)

  ;; Type protocol
  ;; ----------------------------------------

  ;; left
  (global-set! bard 'bard::|type-of| (%construct-function |Anything| :|name| 'bard::|type-of|))
  (add-method! (global-ref bard 'bard::|type-of|)(list |Anything|) #'|anything.type-of|)

  )

;;; =====================================================================
;;; init built-in protocol methods
;;; =====================================================================

(defun |exit| ()(throw 'exit-bard :ok))
(defun |list| (&rest elts) elts)
(defun |pair| (left right)(cons left right))
(defun |range| (start end &optional (by 1))(folio2:range start end :by by))

(defmethod init-bard-methods ((bard bard))

  ;; List protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|list| #'|list|)
  ;; BUG:
  ;; (range 0 -10 -2) causes an infinite loop
  ;; it should be written
  ;; (range 0 -10 2), but the other way should report an error,
  ;; rather than looping forever
  (global-set! bard 'bard::|range| #'|range|)

  ;; Pair protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|pair| #'|pair|)

  ;; System protocol
  ;; ----------------------------------------
  (global-set! bard 'bard::|exit| #'|exit|)
  )

;;; =====================================================================
;;; init built-in protocol macros
;;; =====================================================================

(defmethod init-bard-macros ((bard bard))
  )

;;; =====================================================================
;;; init named literals
;;; =====================================================================

(defun init-named-literals ()
  (register-type (the-type-graph) (undefined) (list |Unique|))
  (register-type (the-type-graph) (end) (list |Unique|))
  (register-type (the-type-graph) (nothing) (list |Unique|))
  (register-type (the-type-graph) (true) (list |Boolean|))
  (register-type (the-type-graph) (false) (list |Boolean|)))

;;; =====================================================================
;;; init global variables
;;; =====================================================================

(defun init-global-variables (bard)
  (global-set! bard 'bard::|->| (global-ref bard 'bard::|function|)))

;;; =====================================================================
;;; initialize the global bard environment
;;; =====================================================================

(defmethod init-bard-globals ((bard bard))
  (init-bard-structures bard)
  (init-bard-classes bard)
  (init-bard-functions bard)
  (init-bard-methods bard)
  (init-bard-macros bard)
  (init-named-literals)
  (init-global-variables bard))

;;; =====================================================================
;;; console repl
;;; =====================================================================

(defparameter $bard-banner (format nil "bard ~a" *bard-version-number*))

(defun display-bard-prompt (&optional (stream *standard-output*))
  (format stream "bard> "))

(defun repl ()
  (format t "~%~a~%" $bard-banner)
  (bard)
  (catch 'exit-bard
    (loop
       (display-bard-prompt)
       (handler-case (let* ((input (bard-read))
                            (thunk (compile input (empty-environment)))
                            (vals (multiple-value-list ($ thunk))))
                       (dolist (val vals)
                         (terpri)
                         (bard-print val))
                       (terpri))
         (condition (c)
           (format t "~%bard signaled a condition:~%")
           (describe c t)
           (terpri t)
           nil)))))

