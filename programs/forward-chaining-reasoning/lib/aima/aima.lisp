;;; Concat of all files so that we can include only one file
;;; And we can easily compile this whole file as its library

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities.lisp

;;;; Basic utility functions and macros, used throughout the code. 

;;; The utilities are divided into control flow macros, list
;;; utilities, functions for 2-dimensional points, numeric utilities,
;;; some trivial functions, utilities for strings, symbols and
;;; printing, a debugging tool, and a testing tool."

(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+(or Allegro EXCL)
  (dolist (pkg '(excl common-lisp common-lisp-user))
    (setf (excl:package-definition-lock (find-package pkg)) nil)))

;;;; Control Flow Macros

;;; We define iteration macros to match the book's pseudo-code.
;;; This could all be done with LOOP, but some users don't have
;;; the LOOP from the 2nd edition of 'Common Lisp: the Language'.

(defmacro while (test do &body body)
  "Execute body while the test is true."
  (assert (eq do 'do))
  `(do () ((not ,test) nil) ,@body))

(defmacro for-each (var in list do &body body)
  "Execute body for each element of list.  VAR can be a list or tree
  of variables, in which case the elements are destructured."
  (assert (eq in 'in)) (assert (eq do 'do))
  (typecase var
    (symbol `(dolist (,var ,list) ,@body))
    (cons (let ((list-var (gensym)))
	    `(dolist (,list-var ,list)
	       (destructuring-bind ,var ,list-var ,@body))))
    (t (error "~V is an illegal variable in (for each ~V in ~A ...)"
	      var list))))

(defmacro for (var = start to end do &body body)
  "Execute body with var bound to succesive integers."
  (cond ((eq var 'each) ; Allow (for each ...) instead of (for-each ...)
	 `(for-each ,= ,start ,to ,end ,do ,@body))
	(t (assert (eq = '=)) (assert (eq to 'to)) (assert (eq do 'do))
	   (let ((end-var (gensym "END")))
	     `(do ((,var ,start (+ 1 ,var)) (,end-var ,end))
		  ((> ,var ,end-var) nil)
		,@body)))))

(defmacro deletef (item sequence &rest keys &environment env)
  "Destructively delete item from sequence, which must be SETF-able."
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion sequence env)
    (assert (= (length stores) 1))
    (let ((item-var (gensym "ITEM")))
    `(let* ((,item-var ,item)
	    ,@(mapcar #'list temps vals)
	    (,(first stores) (delete ,item-var ,access-form ,@keys)))
      ,store-form))))

(defmacro define-if-undefined (&rest definitions)
  "Use this to conditionally define functions, variables, or macros that
  may or may not be pre-defined in this Lisp.  This can be used to provide
  CLtL2 compatibility for older Lisps."
  `(progn
     ,@(mapcar #'(lambda (def)
		   (let ((name (second def)))
		     `(when (not (or (boundp ',name) (fboundp ',name)
				     (special-form-p ',name)
				     (macro-function ',name)))
		       ,def)))
	       definitions)))

;;;; List Utilities

(defun length>1 (list)
  "Is this a list of 2 or more elements?"
  (and (consp list) (cdr list)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

(defun mappend (fn &rest lists)
  "Apply fn to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun starts-with (list element)
  "Is this a list that starts with the given element?"
  (and (consp list) (eq (first list) element)))

(defun last1 (list)
  "Return the last element of a list."
  (first (last list)))

(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun transpose (list-of-lists)
  "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => ((a d) (b e) (c f))."
  (apply #'mapcar #'list list-of-lists))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

"An expression is a list consisting of a prefix operator followed by args,
Or it can be a symbol, denoting an operator with no arguments.
Expressions are used in Logic, and as actions for agents."

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) "Operator of an expression" (if (listp exp) (first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (rest exp) nil))
(defun arg1 (exp) "First argument" (first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))

(defsetf args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))

(defun prefix->infix (exp)
  "Convert a fully parenthesized prefix expression into infix notation."
  (cond ((atom exp) exp)
	((length=1 (args exp)) exp)
	(t (insert-between (op exp) (mapcar #'prefix->infix (args exp))))))

(defun insert-between (item list)
  "Insert item between every element of list."
  (if (or (null list) (length=1 list))
      list
    (list* (first list) item (insert-between item (rest list)))))

;;;; Functions for manipulating 2-dimensional points 

(defstruct (xy (:type list)) "A two-dimensional (i.e. x and y) point." x y)

(defun xy-p (arg) 
  "Is the argument a 2-D point?"
  (and (consp arg) (= (length arg) 2) (every #'numberp arg)))

(defun @ (x y) "Create a 2-D point" (make-xy :x x :y y))

(defun xy-equal (p q) (equal p q))

(defun xy-add (p q)
  "Add two points, component-wise."
  (@ (+ (xy-x p) (xy-x q)) (+ (xy-y p) (xy-y q))))

(defun xy-distance (p q)
  "The distance between two points."
  (sqrt (+ (square (- (xy-x p) (xy-x q)))
	   (square (- (xy-y p) (xy-y q))))))

(defun x+y-distance (p q)
  "The 'city block distance' between two points."
  (+ (abs (- (xy-x p) (xy-x q)))
     (abs (- (xy-y p) (xy-y q)))))

(defun xy-between (xy1 xy2 xy3)
  "Predicate; return t iff xy1 is between xy2 and xy3. Points are collinear."
  (and (between (xy-x xy1) (xy-x xy2) (xy-x xy3))
       (between (xy-y xy1) (xy-y xy2) (xy-y xy3))))

(defun rotate (o a b c d)
  (let ((x (xy-x o))
	(y (xy-y o)))
    (@ (+ (* a x) (* b y)) (+ (* c x) (* d y)))))

(defun inside (l xmax ymax)
  "Is the point l inside a rectangle from 0,0 to xmax,ymax?"
  (let ((x (xy-x l)) (y (xy-y l)))
    (and (>= x 0) (>= y 0) (< x xmax) (< y ymax))))

;;;; Numeric Utilities

(defconstant infinity most-positive-single-float)
(defconstant minus-infinity most-negative-single-float)

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (/ (sum numbers) (length numbers)))

(defun running-average (avg new n)
  "Calculate new average given previous average over n data points"
  (/ (+ new (* avg n)) (1+ n)))

(defun square (x) (* x x))

(defun sum (numbers &optional (key #'identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
      (+ (funcall key (first numbers)) (sum (rest numbers) key))))

(defun between (x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defun rms-error (predicted target)
  "Compute root mean square error between predicted list and target list"
  (sqrt (ms-error predicted target)))

(defun ms-error (predicted target &aux (sum 0))
  "Compute mean square error between predicted list and target list"
  (mapc #'(lambda (x y) (incf sum (square (- x y)))) predicted target)
  (/ sum (length predicted)))

(defun boolean-error (predicted target)
  (if (equal predicted target) 0 1))

(defun dot-product (l1 l2 &aux (sum 0)) ;;; dot product of two lists
  (mapc #'(lambda (x1 x2) (incf sum (* x1 x2))) l1 l2)
  sum)

(defun iota (n &optional (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(defun random-integer (from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random (+ 1 (- to from)))))

(defun normal (x mu sigma)
  (/ (exp (/ (- (square (- x mu))) (* 2 (square sigma)))) 
     (* (sqrt (* 2 pi)) sigma)))

(defun sample-with-replacement (n population)
  (let ((result nil))
    (dotimes (i n) (push (random-element population) result))
    result))

(defun sample-without-replacement (n population &optional
				     (m (length population)))
  ;; Assumes that m = (length population)
  (cond ((<= n 0) nil)
	((>= n m) population)
	((>= (/ n m) (random 1.0))
	 (cons (first population) (sample-without-replacement
				   (- n 1) (rest population) (- m 1))))
	(t (sample-without-replacement n (rest population) (- m 1)))))

(defun fuzz (quantity &optional (proportion .1) (round-off .01))
  "Add and also subtract a random fuzz-factor to a quantity."
  (round-off (+ quantity
		(* quantity (- (random (float proportion))
			       (random (float proportion)))))
	     round-off))

(defun round-off (number precision)
  "Round off the number to specified precision. E.g. (round-off 1.23 .1) = 1.2"
  (* precision (round number precision)))

;;;; Trivial Functions

(defun nothing (&rest args)
  "Don't do anything, and return nil."
  (declare (ignore args))
  nil)

(defun declare-ignore (&rest args)
  "Ignore the arguments."
  ;; This is used to avoid compiler warnings in defmethod.
  ;; Some compilers warn "Variable unused" if it is bound by a method
  ;; but does not appear in the body.  However, if you put in a
  ;; (declare (ignore var)), then other compilers warn "var declared
  ;; ignored, but is actually used", on the grounds that it is implicitly
  ;; used to do method dispatch.  So its safest to use declare-ignore.
  ;; If you like, you can redefine declare-ignore to be a macro that
  ;; expands to either (declare (ignore args)), or to nothing, depending
  ;; on the implementation.
  (declare (ignore args))
  nil)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(defun true (&rest args) "Always return true." (declare (ignore args)) t)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(defun false (&rest args) "Always return false." (declare (ignore args)) nil)

(defun required (&optional (msg "A required argument is missing.") &rest args)
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (apply #'error msg args))

;;;; Utilities for strings and symbols and printing

(defun stringify (exp)
  "Coerce argument to a string."
  (cond ((stringp exp) exp)
	((symbolp exp) (symbol-name exp))
	(t (format nil "~A" exp))))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))

(defun print-grid (array &key (stream t) (key #'identity) (width 3))
  "Print the contents of a 2-D array, numbering the edges."
  (let ((max-x (- (array-dimension array 0) 1))
	(max-y (- (array-dimension array 1) 1)))
    ;; Print the header
    (format stream "~&") (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream "|") (print-dashes width stream))
    (format stream "|~%")
    ;; Print each row
    (for y1 = 0 to max-y do
	 (let ((y (- max-y y1)))
	   (print-centered y width stream)
	   ;; Print each location
	   (for x = 0 to max-x do
		(format stream "|")
		(print-centered (funcall key (aref array x y)) width stream))
	   (format stream "|~%") 
	   ;; Print a dashed line
	   (print-repeated " " width stream)
	   (for x = 0 to max-x do
		(format stream "|") (print-dashes width stream)))
	 (format stream "|~%"))
    ;; Print the X-coordinates along the bottom
    (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream " ") (print-centered x width stream))
    array))

(defun print-centered (string width &optional (stream t))
  "Print STRING centered in a field WIDTH wide."
  (let ((blanks (- width (length (stringify string)))))
    (print-repeated " " (floor blanks 2) stream)
    (format stream "~A" string)
    (print-repeated " " (ceiling blanks 2) stream)))

(defun print-repeated (string n &optional (stream t))
  "Print the string n times."
  (dotimes (i n)
    (format stream "~A" string)))

(defun print-dashes (width &optional (stream t) separate-line)
  "Print a line of dashes WIDTH wide."
  (when separate-line (format stream "~&"))
  (print-repeated "-" width stream)
  (when separate-line (format stream "~%")))

;;;; Assorted conversion utilities and predicates

(defun copy-array (a &aux (dim (array-dimensions a))
                          (b (make-array dim)))
  "Make a copy of an array."
  (copy-subarray a b nil dim)
  b)

(defun copy-subarray (a b indices dim)
  (if dim
    (dotimes (i (first dim))
      (copy-subarray a b (append indices (list i)) (rest dim)))
    (setf (apply #'aref (cons b indices))
          (apply #'aref (cons a indices)))))

(defun array->vector (array)
  "Convert a multi-dimensional array to a vector with the same elements."
  (make-array (array-total-size array) :displaced-to array))


(defun plot-alist (alist file)
  (with-open-file (stream file :direction :output :if-does-not-exist :create
                     :if-exists :supersede)
    (dolist (xy alist)
      (format stream "~&~A ~A~%" (car xy) (cdr xy)))))

(defun copy-hash-table (H1 &optional (copy-fn #'identity))
  (let ((H2 (make-hash-table :test #'equal)))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
	     H1)
    H2))

(defun hash-table->list (table)
  "Convert a hash table into a list of (key . val) pairs."
  (maphash #'cons table))

(defun hprint (h &optional (stream t)) 
  "prints a hash table line by line"
  (maphash #'(lambda (key val) (format stream "~&~A:~10T ~A" key val)) h)
  h)

(defun compose (f g)
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(defun the-biggest (fn l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
	(when (> val best-val)
	  (setq best-val val)
	  (setq biggest x))))
    biggest))

(defun the-biggest-random-tie (fn l)
  (random-element
   (let ((biggest (list (first l)))
	 (best-val (funcall fn (first l))))
     (dolist (x (rest l))
       (let ((val (funcall fn x)))
	 (cond ((> val best-val)
		(setq best-val val)
		(setq biggest (list x)))
	       ((= val best-val)
		(push x biggest)))))
     biggest)))

(defun the-biggest-that (fn p l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (when (funcall p x)
	(let ((val (funcall fn x)))
	  (when (> val best-val)
	    (setq best-val val)
	    (setq biggest x)))))
    biggest))

(defun the-smallest (fn l)
  (the-biggest (compose #'- fn) l))

(defun the-smallest-random-tie (fn l)
  (the-biggest-random-tie (compose #'- fn) l))

(defun the-smallest-that (fn p l)
  (the-biggest-that (compose #'- fn) p l))

;;;; Debugging tool

(defvar *debugging* nil)

(defun dprint (&rest args)
  "Echo all the args when *debugging* is true.  Return the first one."
  (when *debugging* (format t "~&~{~S ~}~%" args))
  (first args))

;;;; Testing Tool: deftest and test

(defmacro deftest (name &rest examples)
  "Define a set of test examples.  Each example is of the form (exp test)
  or (exp).  Evaluate exp and see if the result passes the test. Within the
  test, the result is bound to *.  The example ((f 2))) has no test to
  fail, so it alweays passes the test.  But ((+ 2 2) (= * 3)) has the test
  (= * 3), which fails because * will be bound to the result 4, so the test
  fails.  Call (TEST name) to count how many tests are failed within the
  named test.  NAME is the name of an aima-system."
  `(add-test ',name ',examples))

(defun add-test (name examples)
  "The functional interface for deftest: adds test examples to a system."
  (let ((system (or (get-aima-system name)
		    (add-aima-system :name name :examples examples))))
    (setf (aima-system-examples system) examples))
  name)

(defun test (&optional (name 'all) (print? 't))
  "Run a test suite and sum the number of errors.  If all is well, this
  should return 0.  The second argument says what to print: nil for
  nothing, t for everything, or FAIL for just those examples that fail.
  If there are no test examples in the named system, put the system has
  other systems as parts, run the tests for all those and sum the result."
  (let ((*print-pretty* t)
	(*standard-output* (if print? *standard-output*
			     (make-broadcast-stream)))
	(system (aima-load-if-unloaded name)))
    (cond ((null system) (warn "No such system as ~A." name))
	  ((and (null (aima-system-examples system))
		(every #'symbolp (aima-system-parts system)))
	   (sum  (aima-system-parts system)
		 #'(lambda (part) (test part print?))))
          (t (when print? (format t "Testing System ~A~%" name))
	     (let ((errors (count-if-not #'(lambda (example) 
					     (test-example example print?))
			   (aima-system-examples system))))
	       (format *debug-io* "~%~2D error~P on system ~A~%"
		       errors errors name)
	       errors)))))

(defun test-example (example &optional (print? t))
  "Does the EXP part of this example pass the TEST?"
  (if (stringp example)
      (progn
        (when (eq print? t)
          (format t "~&;;; ~A~%" example))
        t)
    (let* ((exp (first example))
	   (* nil)
	   (test (cond ((null (second example)) t)
		       ((constantp (second example))
			`(equal * ,(second example)))
		       (t (second example))))
           test-result)
      (when (eq print? t)
        (format t "~&> ~S~%" exp))
      (setf * (eval exp))
      (when (eq print? t)
        (format t "~&~S~%" *))
      (setf test-result (eval test))
      (when (null test-result)
        (case print?
          ((FAIL) (format t "~&;;; FAILURE on ~S; expected ~S, got:~%;;; ~S~%"
                          exp test *))
          ((T) (format t "~&;;; FAILURE: expected ~S" test))
          (otherwise)))
      test-result)))
  
;;; File: binary-tree.lisp -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;;;  The following definitions implement binary search trees.

;;;  They are not balanced as yet.  Currently, they all order their
;;;  elements by #'<, and test for identity of elements by #'eq.


(defstruct search-tree-node
  "node for binary search tree"
  value        ;; list of objects with equal key
  num-elements ;; size of the value set
  key          ;; f-cost of the a-star-nodes
  parent       ;; parent of search-tree-node
  leftson      ;; direction of search-tree-nodes with lesser f-cost
  rightson     ;; direction of search-tree-nodes with greater f-cost
  )


 
(defun make-search-tree (root-elem root-key &aux root)
  "return dummy header for binary search tree, with initial
  element root-elem whose key is root-key."
  (setq root
	(make-search-tree-node
	  :value nil
	  :parent nil
	  :rightson nil
	  :leftson (make-search-tree-node
		     :value (list root-elem)
		     :num-elements 1
		     :key root-key
		     :leftson nil :rightson nil)))
  (setf (search-tree-node-parent
	  (search-tree-node-leftson root)) root)
  root)



(defun create-sorted-tree (list-of-elems key-fun &aux root-elem root)
  "return binary search tree containing list-of-elems ordered according
  tp key-fun"
  (if (null list-of-elems)
      nil
      (progn
	(setq root-elem (nth (random (length list-of-elems)) list-of-elems))
	(setq list-of-elems (remove root-elem list-of-elems :test #'eq))
	(setq root (make-search-tree root-elem
				     (funcall key-fun root-elem)))
	(dolist (elem list-of-elems)
	  (insert-element elem root (funcall key-fun elem)))
	root)))



(defun empty-tree (root)
  "Predicate of search trees; return t iff empty."
  (null (search-tree-node-leftson root)))



(defun leftmost (tree-node &aux next)
  "return leftmost descendant of tree-node"
  ;; used by pop-least-element and inorder-successor
  (loop (if (null (setq next (search-tree-node-leftson tree-node)))
	    (return tree-node)
	    (setq tree-node next))))



(defun rightmost (header &aux next tree-node)
  "return rightmost descendant of header"
  ;; used by pop-largest-element
  ;; recall that root of tree is leftson of header, which is a dummy
  (setq tree-node (search-tree-node-leftson header))
  (loop (if (null (setq next (search-tree-node-rightson tree-node)))
	    (return tree-node)
	    (setq tree-node next))))


 
(defun pop-least-element (header)
  "return least element of binary search tree; delete from tree as side-effect"
  ;; Note value slots of search-tree-nodes are lists of a-star-nodes, all of
  ;; which have same f-cost = key slot of search-tree-node.  This function
  ;; arbitrarily returns first element of list with smallest f-cost,
  ;; then deletes it from the list.  If it was the last element of the list
  ;; for the node with smallest key, that node is deleted from the search
  ;; tree.  (That's why we have a pointer to the node's parent).
  ;; Node with smallest f-cost is leftmost descendant of header.
  (let* ( (place (leftmost header))
	 (result (pop (search-tree-node-value place))) )
      (decf (search-tree-node-num-elements place))
      (when (null (search-tree-node-value place))
	(when (search-tree-node-rightson place)
	  (setf (search-tree-node-parent
		  (search-tree-node-rightson place))
		(search-tree-node-parent place)))
	(setf (search-tree-node-leftson
	        (search-tree-node-parent place))
	      (search-tree-node-rightson place)))
      result))




(defun pop-largest-element (header)
  "return largest element of binary search tree; delete from tree as side-effect"
  ;; Note value slots of search-tree-nodes are lists of a-star-nodes, all of
  ;; which have same  key slot of search-tree-node.  This function
  ;; arbitrarily returns first element of list with largest key
  ;; then deletes it from the list.  If it was the last element of the list
  ;; for the node with largest key, that node is deleted from the search
  ;; tree. We need to take special account of the case when the largest element
  ;; is the last element in the root node of the search-tree.  In this case, it
  ;; will be in the leftson of the dummy header.  In all other cases,
  ;; it will be in the rightson of its parent.
  (let* ( (place (rightmost header)) 
	 (result (pop (search-tree-node-value place))) )
      (decf (search-tree-node-num-elements place))      
      (when (null (search-tree-node-value place))
	(cond ( (eq place (search-tree-node-leftson header))
	       (setf (search-tree-node-leftson header)
		     (search-tree-node-leftson place)) )
	      (t (when (search-tree-node-leftson place)
		   (setf (search-tree-node-parent
			   (search-tree-node-leftson place))
			 (search-tree-node-parent place)))
		 (setf (search-tree-node-rightson
			 (search-tree-node-parent place))
		       (search-tree-node-leftson place)))))
      result))




(defun least-key (header)
  "return least key of binary search tree; no side effects"
  (search-tree-node-key (leftmost header)))


(defun largest-key (header)
  "return least key of binary search tree; no side effects"
  (search-tree-node-key (rightmost header)))



(defun insert-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "insert new element at proper place in binary search tree"
  ;; See Reingold and Hansen, Data Structures, sect. 7.2.
  ;; When called initially, parent will be the header, hence go left.
  ;; Element is an a-star-node.  If tree node with key = f-cost of
  ;; element already exists, just push element onto list in that
  ;; node's value slot.  Else have to make new tree node.
  (loop (cond ( (null (setq place (funcall direction parent)))
	       (let ( (new-node (make-search-tree-node
				  :value (list element) :num-elements 1
				  :parent parent :key key
				  :leftson nil :rightson nil)) )
		 (if (eq direction #'search-tree-node-leftson)
		     (setf (search-tree-node-leftson parent) new-node)
		     (setf (search-tree-node-rightson parent) new-node)))
	       (return t))
	      ( (= key (search-tree-node-key place))
	       (push element (search-tree-node-value place))
	       (incf (search-tree-node-num-elements place))
	       (return t))
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson) )
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))




(defun randomized-insert-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "insert new element at proper place in binary search tree -- break
   ties randomly"
  ;; This is just like the above, except that elements with equal keys
  ;; are shuffled randomly.  Not a "perfect shuffle", but the point is
  ;; just to randomize whenever  an arbitrary choice is to be made.

  (loop (cond ( (null (setq place (funcall direction parent)))
	       (let ( (new-node (make-search-tree-node
				  :value (list element) :num-elements 1
				  :parent parent :key key
				  :leftson nil :rightson nil)) )
		 (if (eq direction #'search-tree-node-leftson)
		     (setf (search-tree-node-leftson parent) new-node)
		     (setf (search-tree-node-rightson parent) new-node)))
	       (return t))
	      ( (= key (search-tree-node-key place))
	       (setf (search-tree-node-value place)
		     (randomized-push element (search-tree-node-value place)))
	       (incf (search-tree-node-num-elements place))	       
	       (return t))
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson) )
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))




(defun randomized-push (element list)
  "return list with element destructively inserted at random into list"
  (let ((n (random (+ 1 (length list)))) )
    (cond ((= 0 n)
	   (cons element list))
	  (t (push element (cdr (nthcdr (- n 1) list)))
	     list))))




(defun find-element (element parent key
		       &optional (direction #'search-tree-node-leftson)
		       &aux place)
  "return t if element is int tree"
  (loop (cond ( (null (setq place (funcall direction parent)))
		  (return nil) )
		 ( (= key (search-tree-node-key place))
		  (return (find element (search-tree-node-value place)
				:test #'eq)) ) 
		 ( (< key (search-tree-node-key place))
		  (setq parent place)
		  (setq direction #'search-tree-node-leftson) )
		 (t (setq parent place)
		    (setq direction #'search-tree-node-rightson)))))





(defun delete-element (element parent key &optional (error-p t)
		       &aux (direction #'search-tree-node-leftson)
		       place)
  "delete element from binary search tree"
  ;; When called initially, parent will be the header.
  ;; Have to search for node containing element, using key, also
  ;; keep track of parent of node.  Delete element from list for
  ;; node;  if it's the last element on that list, delete node from
  ;; binary tree.  See Reingold and Hansen, Data Structures, pp. 301, 309.
  ;; if error-p is t, signals error if element not found;  else just
  ;; returns t if element found, nil otherwise.
  (loop (setq place (funcall direction parent))
	(cond ( (null place) (if error-p
				 (error "delete-element: element not found") 
				 (return nil)) )
	      ( (= key (search-tree-node-key place))
	       (cond ( (find element (search-tree-node-value place) :test #'eq)
		      ;; In this case we've found the right binary
		      ;; search-tree node, so we should delete the
		      ;; element from the list of nodes 
		      (setf (search-tree-node-value place)
			    (remove element (search-tree-node-value place)
				    :test #'eq))
		      (decf (search-tree-node-num-elements place))
		      (when (null (search-tree-node-value place))
			;; If we've deleted the last element, we
			;; should delete the node from the binary search tree.
			(cond ( (null (search-tree-node-leftson place))
			       ;; If place has no leftson sub-tree, replace it
			       ;; by its right sub-tree.
			       (when (search-tree-node-rightson place)
				 (setf (search-tree-node-parent
					 (search-tree-node-rightson place))
				       parent))
			       (if (eq direction #'search-tree-node-leftson)
				   (setf (search-tree-node-leftson parent)
					 (search-tree-node-rightson place))
				   (setf (search-tree-node-rightson parent)
					 (search-tree-node-rightson place))) )
			      ( (null (search-tree-node-rightson place) )
			       ;; Else if place has no right sub-tree,
			       ;; replace it by its left sub-tree.
			       (when (search-tree-node-leftson place)
				 (setf (search-tree-node-parent
					 (search-tree-node-leftson place))
				       parent))
			       (if (eq direction #'search-tree-node-leftson)
				   (setf (search-tree-node-leftson parent)
					 (search-tree-node-leftson place))
				   (setf (search-tree-node-rightson parent)
					 (search-tree-node-leftson place))) )
			      (t ;; Else find the "inorder-successor" of
			       ;; place,  which must have nil leftson.
			       ;; Let it replace place, making its left
			       ;; sub-tree be place's current left
			       ;; sub-tree, and replace it by its own
			       ;; right sub-tree. (For details, see
			       ;; Reingold & Hansen, Data Structures, p. 301.)
			       (let ( (next (inorder-successor place)) )
				 (setf (search-tree-node-leftson next)
				       (search-tree-node-leftson place))
				 (setf (search-tree-node-parent
					 (search-tree-node-leftson next))
				       next)
				 (if (eq direction #'search-tree-node-leftson)
				     (setf (search-tree-node-leftson
					    parent) next) 
				     (setf (search-tree-node-rightson parent)
					   next))
				 (unless (eq next (search-tree-node-rightson
						    place))
				   (setf (search-tree-node-leftson
					   (search-tree-node-parent next))
					 (search-tree-node-rightson next))
				   (when (search-tree-node-rightson next)
				     (setf (search-tree-node-parent
					     (search-tree-node-rightson next))
					   (search-tree-node-parent next)))
				   (setf (search-tree-node-rightson next)
					 (search-tree-node-rightson
					   place))
				   (setf (search-tree-node-parent
					   (search-tree-node-rightson next))
					 next))
				 (setf (search-tree-node-parent next)
				       (search-tree-node-parent place))))))
		      (return t))
		     (t (if error-p
			    (error "delete-element:  element not found") 
			    (return nil)))) )
	      ( (< key (search-tree-node-key place))
	       (setq parent place)
	       (setq direction #'search-tree-node-leftson))
	      (t (setq parent place)
		 (setq direction #'search-tree-node-rightson)))))





(defun inorder-successor (tree-node)
  "return inorder-successor of tree-node assuming it has a right son"
  ;; this is used by function delete-element when deleting a node from
  ;; the binary search tree.  See Reingold and Hansen, pp. 301, 309.
  ;; The inorder-successor is the leftmost descendant of the rightson.
  (leftmost (search-tree-node-rightson tree-node)))



(defun list-elements (parent &aux child)
  "return list of elements in tree"
  (append (when (setq child (search-tree-node-leftson parent))
            (list-elements child))
          (search-tree-node-value parent)
          (when (setq child (search-tree-node-rightson parent))
            (list-elements child))))
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/queue.lisp

;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front, to the back, or ordered by some numeric score.
;;; This is done with the following enqueing functions, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-AT-FRONT - elements are a list
;;;   ENQUEUE-AT-END   - elements are a list, with a pointer to end
;;;   ENQUEUE-BY-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.

;;; The heap implementation is taken from "Introduction to Algorithms" by
;;; Cormen, Lieserson & Rivest [CL&R], Chapter 7.  We could certainly speed
;;; up the constant factors of this implementation.  It is meant to be clear
;;; and simple and O(log n), but not super efficient.  Consider a Fibonacci
;;; heap [Page 420 CL&R] if you really have large queues to deal with.

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

;;;; Basic Operations on Queues

(defun make-empty-queue () (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

;;;; The Three Enqueing Functions

(defun enqueue-at-front (q items)
  "Add a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q))))

(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (cond ((null items) nil)
	((or (null (q-last q)) (null (q-elements q)))
	 (setf (q-last q) (last items)
	       (q-elements q) (nconc (q-elements q) items)))
	(t (setf (cdr (q-last q)) items
		 (q-last q) (last items)))))

(defun enqueue-by-priority (q items key)
  "Insert the items by priority according to the key function."
  ;; First make sure the queue is in a consistent state
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (for each item in items do
       (heap-insert (q-elements q) item key)))

;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

;; These could be made inline

(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, move heap[i] down where it belongs.
  [Page 143 CL&R]."
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest valued) item off the heap. [Page 150 CL&R]."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Put an item into a heap. [Page 150 CL&R]."
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
      do (setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(defun heap-sort (numbers &key (key #'identity))
  "Return a sorted list, with elements that are < according to key first."
  ;; Mostly for testing the heap implementation
  ;; There are more efficient ways of sorting (even of heap-sorting)
  (let ((heap (make-heap))
	(result nil))
    (for each n in numbers do (heap-insert heap n key))
    (while (> (length heap) 0) do (push (heap-extract-min heap key) result))
    (nreverse result)))
;;; File: cltl2.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Compatibility package for 'Common Lisp the Language: 2nd edition'

;;; Functions and macros in CLtL2 that are not in the first edition of
;;; the book, and thus not in some old implementations of Common Lisp.

#+Allegro ;; Allow us to create missing functions in Allegro 
(when (fboundp 'package-definition-lock)
  (setf (package-definition-lock (find-package "COMMON-LISP")) nil))

(define-if-undefined
  
(defmacro with-simple-restart (restart &rest body)
  "Like PROGN, except provides control over restarts if there is an error."
  (declare (ignore restart))
  `(progn ,@body))

(defmacro destructuring-bind (lambda-list list &body body)
  "Bind the variables in lambda-list to the result list and execute body."
  ;; This implementation does not do the defmacro extensions,
  ;; Except that it does handle a trailing dot: (x y . z)
  (cond ((null lambda-list)
	 `(progn ,@body))
	((not (symbolp list))
	 (let ((var (gensym)))
	   `(let ((,var ,list))
	      (destructuring-bind ,lambda-list ,var ,@body))))
	((symbolp lambda-list)
	 `(let ((,lambda-list ,list)) ,@body))
	((atom lambda-list)
	 (error "Can't bind ~A to a value." lambda-list))
	((member (first lambda-list) '(&rest &optional &key &aux))
	 `(apply #'(lambda ,lambda-list ,@body) ,list))
	(t `(destructuring-bind ,(first lambda-list) (first ,list)
	      (destructuring-bind ,(rest lambda-list) (rest ,list)
		,@body)))))

  ) ; end define-if-undefined

;;;; Mini Implementation of CLOS

;;; If you don't have CLOS (the Common Lisp Object System) installed,
;;; then this defines a simple version of DEFMETHOD which only
;;; dispatches on the first argument, and works for structures (and
;;; some other types) but not classes.  Note that you can still do
;;; (single) inheritance with structures using the :include option.
;;; To properly inform DEFMETHOD of the inheritance tree, you should
;;; use DEFSTRUCTURE rather than DEFSTRUCT.  This has the added
;;; benefit of allowing you to write PRINT-STRUCTURE methods rather
;;; than :print-function functions, if you like (they will be
;;; inherited properly, and they don't have the silly DEPTH argument).

(defmacro defstructure (type-and-args &rest slots)
  "This is just like DEFSTRUCT, except it keeps track of :include types, for
  the benefit of METHOD-FOR, and it makes printing go through PRINT-STRUCTURE."
  (if (atom type-and-args) (setf type-and-args (list type-and-args)))
  (let* ((type (first type-and-args))
	 (args (rest type-and-args))
	 (supertype (or (second (assoc ':include args)) 'structure))
	 (print-fn (if (null (assoc ':print-function args))
		       '((:print-function (lambda (x s d)
					    (declare (ignore d))
					    (print-structure x s)))))))
    `(progn (setf (get ',type ':supertype) ',supertype)
	    (defstruct (,type ,@print-fn ,@args) ,@slots))))

(defmethod print-structure ((structure t) stream)
  "Print a structure.  You can specialize this function.
  It will be called to print anything defined with DEFSTRUCTURE."
  (format stream "#<a ~A>" (type-of structure)))

(eval-when (compile eval)
  (when (macro-function 'defmethod) 
    (pushnew :clos *features*)))

#-CLOS
(progn ;; when you don't have CLOS, use this simple version ...

(defmacro defmethod (name ((var class) &rest other-args) &rest body)
  "This version of DEFMETHOD is like the CLOS version, except it only
  dispatches on the first argument, and it only handles structures and
  some built-in types, not classes."
  `(setf (get ',name :generic) (ensure-generic-function ',name)
	 (get ',name ',class) #'(lambda (,var . ,other-args) . ,body)))

(defun ensure-generic-function (name)
  "Define NAME to be a generic function."
  (unless (eq (symbol-function name) (get name :generic))
    (setf (symbol-function name)
	  #'(lambda (var &rest args)
	      (labels ((call-next-method ()
		         (call-method-for name (supertype (type-of var))
					  var args)))
		(call-method-for name (type-of var) var args))))))
  
(defun supertype (type)
  "Find the most specific supertype of this type."
  (cond ((eq type t) nil)
	((get type :supertype))
	(t 'atom)))

(defun call-method-for (name type var args)
  "Find the method for this type, following :supertype links if needed."
  (let ((m (get name type)))
    (cond (m (apply m var args))
	  ((eq type nil) (error "Can't find method ~A for ~A." name var))
	  (t (call-method-for name (supertype type) var args)))))

;; Construct a small part of the built-in type hierarchy
(mapc 
 #'(lambda (pair) (setf (get (first pair) :supertype) (second pair)))
 '((null list) (cons list) (list t) (atom t) (keyword symbol) (null symbol)
   (fixnum integer) (bignum integer) (integer rational) (ratio rational) 
   (rational real) (float real) (real number) (complex number) 
   (string vector) (bit-vector vector) (vector array) (error condition)))

) ; end when you don't have CLOS ...
 
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The basic environment simulator code

;;; This file defines the environment simulator function: RUN-ENVIRONMENT.  It
;;; is a little different from the pseudo-code in the book: to make it easier
;;; to write new environments, we used an object-oriented approach. Rather
;;; than requiring the caller to pass in a bunch of functions to
;;; RUN-ENVIRONMENT, you will define methods for the functions for each
;;; subtype of environment.  We added a DISPLAY parameter to control whether
;;; intermediate results are displayed. As an example, the following
;;; expression builds an environment and runs an agent in it, displaying the
;;; results:
;;;
;;; (run-environment (make-vacuum-world :aspec '(random-vacuum-agent)))
;;;
;;; We also define AGENT-TRIALS to compare the performance of several
;;; different agents in a series of random environments, all drawn from an
;;; environment subtype.  For example to run 2 agents in 20 environments each:
;;;
;;; (agent-trials 'vacuum-world
;;;   '(random-vacuum-agent reactive-vacuum-agent) :n 20)

(defstructure environment
  "The world in which agents exist."
  (agents '())       ;; A list of the agents in the environment
  (step 0)           ;; The number of time steps simulated so far
  (max-steps 1000)   ;; Stop the simulation after this number
  (stream t)         ;; Stream to display output on
  (initialized nil)  ;; Have we run initialize on this environment yet?
  (state nil)        ;; Current state of the environment; other subtypes
  (illegal-actions nil)   ;; Remembers illegal action attempts
                     ;; add new slots to hold various state information
  )

;;; An agent is something that perceives and acts.  As such, each agent has a
;;; slot to hold its current percept, and its current action.  The action
;;; will be handed back to the environment simulator to perform (if legal).
;;; Each agent also has a slot for the agent program, and one for its score
;;; as determined by the performance measure.

(defstructure agent
  "Agents take actions (based on percepts and the agent program) and receive
  a score (based on the performance measure).  An agent has a body which can
  take action, and a program to choose the actions, based on percepts."
  (program #'nothing)			; fn: percept -> action
  (body (make-agent-body))
  (score 0)
  (percept nil)
  (action nil)
  (name nil))

;;;; Top level functions

(defun run-environment (env)
  "Basic environment simulator.  It gives each agent its percept, gets an
  action from each agent, and updates the environment. It also keeps score
  for each agent, and optionally displays intermediate results. [p 48]"
  (initialize env)
  (display-environment env)
  (dotimes (i (environment-max-steps env))
    (incf (environment-step env))
    ;; Deliver percept and get action from each agent
    (for each agent in (environment-agents env) do
	 (setf (agent-percept agent) (get-percept env agent))
	 (setf (agent-action agent) 
	       (funcall (agent-program agent) (agent-percept agent))))
    ;; Execute the actions and otherwise update the world
    (update-fn env)
    ;; Update the agent scores, then optionally display the current state
    (for each agent in (environment-agents env) do
	 (setf (agent-score agent) (performance-measure env agent)))
    (display-illegal-actions env)
    (display-environment env)
    (when (termination? env) (RETURN)))
  env)

(defun agent-trials (environment-fn agent-types &key (n 10))
  "Report how well a single agent does in a set of N similar environments,
  and compare that to other agents in the same set of environments.
  Environment-fn takes a :agents keyword argument, and returns an environment.
  Agent-types is a list of names of functions that each create an agent."
  (let ((env-gen-random-state (make-random-state t)))
    (mapcar #'(lambda (agent-type)
		(agent-trial environment-fn agent-type
			     (make-random-state env-gen-random-state) n))
	    agent-types)))

;;;; Generic Functions that must be defined for each environment

;;; For each new type of environment you want to define, you will need a
;;; defstruct that inherits from (includes) ENVIRONMENT, and you will need
;;; to write new methods (or inherit existing methods) for each of the
;;; following eight functions.  Here are the ones that will change for each
;;; new environment:

(defmethod get-percept ((env environment) agent)
  "Return the percept for this agent."
  (declare-ignore env agent)
  nil)

(defmethod update-fn ((env environment))
  "Modify the environment, based on agents actions, etc."
  (execute-agent-actions env))

(defmethod legal-actions ((env environment))
  "A list of the action operators that an agent can do."
  nil)

(defmethod performance-measure ((env environment) agent)
  "Return a number saying how well this agent is doing."
  ;; The default is to subtract one point for each time step.
  (declare-ignore agent)
  (- (environment-step env)))

;;; Here are the ones that can usually be inherited:

(defmethod initialize ((env environment))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation."
  (initialize-agent-names env)
  (setf (environment-initialized env) t)
  env)

(defmethod termination? ((env environment))
  "Return true if the simulation should end now."
  nil)

(defmethod display-environment ((env environment))
  "Display the current state of the environment."
  ;; You probably won't need to specialize this, unless you want to do
  ;; a fancy graphical user interface
  (let ((stream (environment-stream env)))
    (when stream 
      (format stream "~&At Time step ~D:~%" (environment-step env))
      (when (> (environment-step env) 0)
	(for each agent in (environment-agents env) do
	     (format stream 
		     "~&Agent ~A perceives ~A~6Tand does ~A~%"
		     agent (agent-percept agent)
		     (agent-action agent))))
      (display-environment-snapshot env))))

(defmethod display-environment-snapshot ((env environment))
  "Display a 'picture' of the current state of the environment."
  ;; This is what you will specialize 
  (print env (environment-stream env)))

;;;; Auxiliary Functions

(defun run-eval-environment (env)
  "Basic environment simulator; the same as run-environment. [p 48]
  We decided it was silly to run the environment and NOT eval the agents,
  so in this code, and in future editions of the book, we will only have
  RUN-ENVIRONMENT, and it will evaluate the agents."
  (run-environment env))

(defun agent-trial (environment-fn agent-type env-gen-random-state n)
  "Run n environments with an identical agent in each, and average the scores."
  ;; By binding *random-state* to env-gen-random-state, we hope to reproduce
  ;; the same set of environments each time AGENT-TRIAL is called with the
  ;; same environment-fn.
  (let ((total 0) (score 0))
    (for i = 1 to n do
	 (let* ((env (let ((*random-state* env-gen-random-state))
		       (funcall environment-fn 
				:stream nil
				:aspec (list agent-type)))))
	   (run-environment env)
	   (incf total (agent-score (first (environment-agents env))))))
    (setf score (float (/ total n)))
    (format t "~&~10,2F average for ~A" score agent-type)
    score))

(defun execute-agent-actions (env)
  "Each agent (if the agent is alive and has specified a legal action)
  takes the action, illegal actios recorded in illegal-actions slot."
  (for each agent in (environment-agents env) do
       (let ((act (agent-action agent)))
	 (if (member (op act) (legal-actions env))
	     (apply (op act) env (agent-body agent) (args act))
             (setf (environment-illegal-actions env)
                   (nconc (environment-illegal-actions env) 
                           (list agent act)))))))

(defun display-illegal-actions (env)
  (let ((actions (environment-illegal-actions env))
        (stream  (environment-stream env)))
    (when actions
      (format stream 
              "~%Illegal actions attempted: ~{~%agent ~A attempted action ~A~}"
              actions)
      (setf (environment-illegal-actions env) nil))))

(defmethod print-structure ((env environment) stream)
  (format stream "#<~A; Step: ~D, Agents:~{ ~A~}>"
	  (type-of env) (environment-step env)
	  (environment-agents env)))





;;; File: grid-env.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Environments with a two-dimensional grid layout occupied by objects

;;; This file defines a GRID-ENVIRONMENT, a kind of environment where there is
;;; a rectangular grid of spaces, each potentially containing objects.
;;; (Notice that the ENVIRONMENT makes no mention of space or objects.)  You
;;; won't be creating instances of grid-environment directly, but it is the
;;; key structure that is inherited by the Vacuum, Wumpus, Shopping and
;;; Elevator worlds.

(defstructure (grid-environment (:include environment))
  (size (@ 10 10))          ; Size of the 2-D array
  (grid nil)		    ; Will be a 2-D array of squares
  (objects '())             ; List of objects currently in this env.
  (start (@ 1 1))	    ; Where agents begin
  (bspec '((at edge wall))) ; Specify Basic objects, common to all envs.
  (cspec '())               ; Specify objects that Change for each env.
  (aspec '(ask-user-agent)) ; Specify default list of Agents
  )

(defstructure object
  "An object is anything that occupies space.  Some objects are 'alive'."
  (name "?")			; Used to print the object on the map
  (alive? nil)                  ; Is the object alive?
  (loc (@ 1 1))			; The square that the object is in
  (bump nil)			; Has the object bumped into something?
  (size 0.5)			; Size of object as proportion of loc
  (color 'black)		; Some objects have a color
  (shape 'rectangle)		; Some objects have a shape
  (sound nil)			; Some objects create a sound
  (contents '())		; Some objects contain others
  (max-contents 0.4)            ; How much (total size) can fit inside?
  (container nil)		; Some objects are contained by another
  (heading (my-nth (random 4) '((1 0) (-1 0) (0 1) (0 -1)))) ; Direction
)

(defun my-nth (n seq) (copy-list (nth n seq)))

(defstructure (obstacle (:include object (name "#"))))

(defstructure (wall (:include obstacle)))

(defstructure (agent-body (:include object (alive? t) (name nil)))
  "An agent body is an object; some bodies have a hand that can hold 1 thing."
  (holding nil))

;;;; Generic Functions

(defmethod update-fn ((env grid-environment))
  "Execute the actions and do bookkeeping on the bump sensor."
  (for each agent in (environment-agents env) do
       (setf (object-bump (agent-body agent)) nil)) ; dissipate bumps
  (execute-agent-actions env))

(defmethod legal-actions ((env grid-environment))
  '(turn forward grab release speak))

(defmethod initialize ((env grid-environment))
  "Build a new environment with all the agents and objects in place.
  This gets passed an environment which may need to have the objects placed.
  See PARSE-SPECS below in this file for more on initialization."
  (unless (environment-initialized env)
    ;; Build the grid and place objects where they belong
    (setf (grid-environment-grid env) 
	  (make-array (grid-environment-size env) :initial-element '()))
    (parse-specs env (grid-environment-bspec env))
    (parse-specs env (grid-environment-cspec env))
    (parse-specs env (grid-environment-aspec env))
    (setf (environment-agents env) (reverse (environment-agents env)))
    (call-next-method)
) )

(defmethod termination? ((env grid-environment)) 
  "By default, we stop when there are no live agents."
  (every #'(lambda (agent) (not (object-alive? (agent-body agent))))
	 (environment-agents env)))

(defmethod display-environment-snapshot ((env grid-environment))
  "Show what is in each location in the environment."
  (print-grid (grid-environment-grid env) :width 4
	      :stream (environment-stream env)
	      :key #'(lambda (objects)
		       (format nil "~{~A~}" objects))))

(defmethod print-structure ((object object) stream)
  "Show an object's name, and if it is alive, the direction it faces."
  (let ((name (or (object-name object) (type-of object)))
        (cont (object-contents object)))
    (if (object-alive? object)
        (if cont 
            (format stream "~A.~A" name (heading->string (object-heading object)))
	    (format stream "~A~A" name (heading->string (object-heading object))))
        (if cont 
            (format stream "~A." name)
            (format stream "~A" name)))))

;;;; Actions 

(defmethod speak ((env grid-environment) agent-body sound) ; speak
  "The agent emits a sound."
  (declare-ignore env)
  (setf (object-sound agent-body) sound))

(defmethod turn ((env grid-environment) agent-body direction)
  "The agent changes its heading by turning right or left."
  (declare-ignore env)
  (let* ((headings '#((1 0) (0 1) (-1 0) (0 -1)))
	 (now (position (agent-body-heading agent-body) headings
			:test #'equal))
	 (delta (case direction (right -1) (left +1) (t 0))))
    (setf (object-heading agent-body)
	  (elt headings (mod (+ now delta) 4)))))

(defmethod forward ((env grid-environment) agent-body)
  "Move the object to the location that is one step directly ahead of it."
  (move-object-to 
   agent-body 
   (add-locs (object-loc agent-body) (object-heading agent-body))
   env))

(defmethod grab ((env grid-environment) agent-body &optional args)
  "Grab an object at the specified location.  Assumes a one-handed agent."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (find-object-if #'grabable? (object-loc agent-body) env)))
    (when (and object 
	       (not (agent-body-holding agent-body))
	       (place-in-container object agent-body env))
      (setf (agent-body-holding agent-body) object))))

(defun grabable? (object)
  (and (not (obstacle-p object)) (not (agent-body-p object))))

(defmethod release ((env grid-environment) agent-body &optional args)
  "Release an object that is in the hand, putting it at the specified loc."
  (declare-ignore args) ;; They are used in other environments
  (let ((object (agent-body-holding agent-body)))
    (when object
      (place-object object (object-loc agent-body) env)
      (setf (agent-body-holding agent-body) nil))))

;;;; Initializing Environments

;;; The grammar for the object-specs language is as follows:             
;;;<PRE>
;;;   specs  =>  (spec...)
;;;   spec   =>  (AT where what...) | (* n spec...) | what
;;;   where  =>  EDGE | ALL | FREE? | START | (x y) | (AND where...)
;;;   what   =>  object | type | (type arg...) | (* n what...)  | (P x what...) |
;;;              (U what ...) | (C what what)
;;;   n      =>  integer | (+- integer integer)
;;; 
;;; The location FREE? means a randomly chosen free loc, ALL means every loc.
;;; If no location is specified, the default is START for agents, FREE?
;;; otherwise.  
;;; 
;;; Examples of spec:
;;; 
;;;  (at edge wall)                  1 wall in every perimeter location
;;;  (at free? wumpus)               1 wumpus in some random free location
;;;  wumpus                          Same as above 
;;;  ask-user-agent                  1 ask-user-agent in the start cell
;;;  (* 2 apple)                     An apple in each of 2 random locations
;;;  (* 2 (apple :color green))      A green apple in each of 2 random locs
;;;  (at all (p 0.25 dirt))          All free locations have 1/4 chance of dirt
;;;  (at (2 3) (* 8 apple) sign)     Location (2 3) has 8 apples and a sign
;;;  (* (+- 10 4) apple)             10 plus or minus 4 (at random) apples
;;;  (at (and (1 2) (1 4)) cashier)  These two locations each get a cashier
;;;  (* 2 smoke fire)                2 random locs get both smoke and fire
;;;</PRE>

(defun parse-specs (env specs)
  "Place objects, defined by specs, in the environment."
  (for each spec in specs do
       (parse-spec env spec)))

(defun parse-spec (env spec)
  (case (op spec)
   (AT (parse-where env (arg1 spec) (rest (args spec))))
   (*  (for i = 1 to (parse-n (arg1 spec)) do
	 (parse-specs env (rest (args spec)))))
   (t  (parse-what env nil spec))))

(defun parse-where (env where whats)
  (cond
   ((eq where 'EDGE)    (let ((x-size (xy-x (grid-environment-size env)))
			      (y-size (xy-y (grid-environment-size env))))
			  (for i = 0 to (- x-size 1) do
			       (parse-whats env (@ i 0) whats)
			       (parse-whats env (@ i (- y-size 1)) whats))
			  (for i = 1 to (- y-size 2) do
			       (parse-whats env (@ 0 i) whats)
			       (parse-whats env (@ (- x-size 1) i) whats))))
   ((eq where 'ALL)     (dotimes (x (xy-x (grid-environment-size env)))
			  (dotimes (y (xy-y (grid-environment-size env)))
			    (when (free-loc? (@ x y) env)
			      (parse-whats env (@ x y) whats)))))
   ((eq where 'FREE?)   (parse-whats env (random-loc env :if 'free-loc?) whats))
   ((eq where 'START)   (parse-whats env (grid-environment-start env) whats))
   ((xy-p where)        (parse-whats env where whats))
   ((eq (op where) 'AND)(for each w in (args where) do 
			     (parse-where env w whats)))
   (t (warn "Unrecognized object spec ignored: ~A" `(at ,where ,@whats)))))

(defun parse-whats (env loc what-list)
  (for each what in what-list do
       (parse-what env loc what)))

(defun parse-what (env loc what)
  "Place the objects specified by WHAT-LIST at the given location
  The default location is START for an agent, random otherwise.
  The notation (P 0.5 what...) means 50% chance of placing each what,
  and (* n what...) means place n copies of each what."
  (case (op what)
    (* (for i = 1 to (parse-n (arg1 what)) do
	 (parse-whats env loc (rest (args what)))))
    (P (for each w in (rest (args what)) do
	    (when (< (random 1.0) (arg1 what))
	      (parse-what env loc w))))
    (U (let ((location (if loc loc (random-loc env :if #'free-loc?)))) 
         (for each w in (args what) do
	      (parse-what env location w))))
    (C (let* ((container (parse-what env loc (arg1 what)))
              (contents (parse-what env loc (arg2 what))))
         (place-in-container contents container env)))
    (t (let* ((object (if (object-p what) what
			(apply #'make (op what) (args what))))
	      (location (or loc (if (agent-p object)
				    (grid-environment-start env)
				    (random-loc env :if #'free-loc?)))))
	 (place-object object location env t)))))
    
(defun parse-n (n)
  (if (eq (op n) '+-)
      (round (+ (arg1 n) (random (float (arg2 n)))
		(- (random (float (arg2 n))))))
      n))

(defun make (type &rest args)
  "Make an instance of the specified type by calling make-TYPE."
  (apply (concat-symbol 'make- type) args))

(defun random-loc (env &key (if #'true) (tries 100))
  "Return a random loc, somewhere in the environment.
  The loc must satisfy the :IF predicate.  If it can't find such a location
  after a number of TRIES, it signals an error."
  (or (for i = 1 to tries do
	   (let ((loc (mapcar #'random (grid-environment-size env))))
	     (when (funcall if loc env) (RETURN loc))))
      (error "Can't find a location.")))

(defun free-loc? (loc env)
  "A location is free if there is no obstacle there and it is not the start."
  (and (empty-loc? loc env)  ;(not (find-object-if #'obstacle-p loc env))
       (not (equal loc (grid-environment-start env)))))

(defun empty-loc? (loc env)
  (let ((g-cont (grid-contents env loc)))
    ; (format t "~%location: ~S  contants: ~S~%" loc g-cont)
    (null g-cont))) 
  

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Definition of basic AGENT functions

(defstructure (ask-user-agent (:include agent (program 'ask-user)))
  "An agent that asks the user to type in an action.")

(defun ask-user (percept)
  "Ask the user what action to take."
  (format t "~&Percept is ~A; action? " percept)
  (read))

(defmethod print-structure ((agent agent) stream)
  "Agents are printed by showing their name (or body) and score."
  (format stream "[~A = ~D]" (or (agent-name agent) (agent-body agent))
	  (agent-score agent)))

(defun initialize-agent-names (env)
  "Name the agents 1, 2, ... if they don't yet have a name."
  (for each agent in (environment-agents env) do
       (when (null (agent-name agent))
	 (let ((i (+ 1 (position agent (environment-agents env))))
	       (body (agent-body agent)))
	   (setf (agent-name agent) i)
	   (when (and body (null (object-name body)))
	     (setf (object-name body) i))))))

;; Design Decision Notes

;; We have decided that the agent and its body are two separate objects.
;; We could have combined the agent and its body into one object.  But then
;; each new type of agent would need to inherit from both AGENT and some
;; body type, such as OBJECT.  This would require multiple inheritance,
;; which is part of CLOS, but is not in our simple implementation for those
;; who don't have CLOS.  In any case, it would get messy.  We think that
;; separating agents from agent-bodies is a good thing for this
;; implementation.  (Just don't take it too far and assume that this says
;; anything about the mind-body problem.)
;;
;; We could have defined the agent program as a generic function on the
;; agent.  But that would mean that everytime you want to try out a
;; slightly different agent program, you would need to define a new type.  You
;; would also need to hold state information in slots rather than in local
;; variables, and we would need to have some kind of discipline to ensure
;; that the slots representing intermediate state could be accessed and
;; modified by the agent program, while the slot representing, say, the score
;; could not.  All in all, a closure (a different one for every agent) is
;; exactly what we want in an agent program: a closure encapsulates local
;; state and behavior, and can access only its arguments and closed-over
;; variables.

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Algorithms for manipulating objects in a grid

(defun grid-contents (env loc)
  "Return a list of objects in this location, optionally including
  objects that are contained within containers here."
  (aref (grid-environment-grid env) (xy-x loc) (xy-y loc)))

(defsetf grid-contents (env loc) (val)
  `(setf (aref (grid-environment-grid ,env) (xy-x ,loc) (xy-y ,loc))
	 ,val))

(defun move-object-to (object loc env)
  "Move an object to an absolute location and return that location.  However,
  attempting to move into a location with an obstacle fails (returns nil)
  and the object receives a bump."
  (let ((size (grid-environment-size env)))
    (when (inside loc (xy-x size) (xy-y size))
      (cond ((find-object-if #'obstacle-p loc env)
	     (setf (object-bump object) 'bump)
	     nil)
	    (t (remove-object object env)
	       (place-object object loc env)
	       loc)))))

(defun move-object-by (object delta env)
  "Move an object by delta location and return that location.  However,
  attempting to move into a location with an obstacle fails (returns nil)
  and the object receives a bump."
  (move-object-to object 
                  (xy-add delta (object-loc object))
                  env))

(defun place-object (object loc env &optional (initial? t))
  "Put the object in its initial position or a new position in environment."
  ;; Coerce agents into agent-bodies
  (when (agent-p object)
    (pushnew object (environment-agents env))
    (setf object (agent-body object)))
  ;; Place the object
  (setf (object-loc object) loc)
  (pushnew object (grid-contents env loc))
  (when initial?
    (push object (grid-environment-objects env)))
  object)

(defun place-in-container (object container env)
  "Put the object inside the container, if there is room."
  ;; First, check to see if there is space
  (when (< (+ (object-size object) 
	      (sum (object-contents container) #'object-size))
	   (object-max-contents object))
    ;; If there is, remove it from where it was.
    (remove-object object env) 
    ;; Now place it in its new container
    (setf (object-container object) container)
    (setf (object-loc object) (object-loc container))
    (pushnew object (object-contents container))
    object))
    
(defun remove-object (object env)
  "Remove the object from its current location."
  (let ((loc (object-loc object))
	(old-container (object-container object)))
    (deletef object (grid-contents env loc))
    (when old-container
      (deletef object (object-contents old-container))
      (setf (object-container object) nil))))

(defun find-object-if (predicate loc env)
  "Return an object in this location that satisfies this predicate."
  (find-if predicate (grid-contents env loc)))

(defun find-neighbor-if (predicate loc env)
  "Return an object in a neighboring square that satisfies the predicate."
  (let ((x (xy-x loc))
	(y (xy-y loc)))
    ;; Look in the four neighboring squares
    (or (find-object-if predicate (@ x (+ y 1)) env)
	(find-object-if predicate (@ x (- y 1)) env)
	(find-object-if predicate (@ (+ x 1) y) env)
	(find-object-if predicate (@ (- x 1) y) env))))

(defun find-object-or-neighbor-if (predicate loc env)
  "Return an object either in loc or a neighboring square that satisfies
  the predicate."
  (or (find-object-if predicate loc env)
      (find-neighbor-if predicate loc env)))

(defun near? (loc1 loc2 &optional (tolerance 1))
  "Are the two locations nearby each other?"
  (and (<= (abs (- (xy-x loc1) (xy-x loc2))) tolerance)
       (<= (abs (- (xy-y loc1) (xy-y loc2))) tolerance)))

;;;; Maintaining and manipulating orientation

(defun add-locs (&rest locations)
  "Coordinate-wise addition of locations: (add-locs '(1 2) '(10 20)) = (11 22)"
  (apply #'mapcar #'+ locations))

(defun subtract-locs (&rest locations)
  "Coordinate-wise subtraction of locations."
  (apply #'mapcar #'- locations))

(defun heading->string (heading)
  "Convert a heading like (0 1) to a depictive string like ^."
  (cond ((equal heading '(1 0)) ">")
	((equal heading '(0 1)) "^")
	((equal heading '(-1 0)) "<")
	((equal heading '(0 -1)) "V")
	(t "?")))

(defun absolute-loc (agent-body offset)
  "Return an absolute location given an offset from an agent, taking the
  agent's orientation into account.  An offset of (1 2) means 1 square to
  the right and two ahead of the agent, given its present orientation."
  (let ((x (xy-x offset))
	(y (xy-y offset))
	(heading (agent-body-heading agent-body)))
    (add-locs (object-loc agent-body)
	      (cond ((equal heading '(1 0)) (@ y (- x)))
		    ((equal heading '(0 1)) offset)
		    ((equal heading '(-1 0)) (@ (- y) x))
		    ((equal heading '(0 -1)) (@ (- x) (- y)))
		    (t "?")))))

(defun offset-loc (agent-body loc)
  "Return an offset from an agent that corresponds to the absolute loc."
  (let ((x (- (xy-x loc) (xy-x (object-loc agent-body))))
	(y (- (xy-y loc) (xy-y (object-loc agent-body))))
	(heading (agent-body-heading agent-body)))
    (cond ((equal heading '(1 0)) (@ (- y) (+ x)))
	  ((equal heading '(0 1)) (@ x y))
	  ((equal heading '(-1 0)) (@ (+ y) (- x)))
	  ((equal heading '(0 -1)) (@ (- x) (- y)))
	  (t "?"))))
