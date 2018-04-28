(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

#|
Some test uses of the functions here:

(create-sub-matrices (list (matrix-transpose '(
(1 0 0 0 -1 0 0) 
(0 0 0 1 -2 0 0)
(0 0 1 0 0 -1 0)
(0 1 0 0 0 0 -1)
(0 3 0 0 0 0 -3)))))


((#(NIL NIL T NIL NIL) ((0 0 1 0 0) (0 0 -1 0 0)))
 (#(NIL NIL NIL T T) ((0 0 0 1 3) (0 0 0 -1 -3)))
 (#(T T NIL NIL NIL) ((1 0 0 0 0) (0 1 0 0 0) (-1 -2 0 0 0))))

(mapcar #'solve-one (create-sub-matrices (list (matrix-transpose '(
(1 0 0 0 -1 0 0) 
(0 0 0 1 -2 0 0)
(0 0 1 0 0 -1 0)
(0 1 0 0 0 0 -1)
(0 3 0 0 0 0 -3))))))





(matrix-transpose '((1 0 0 0 0) (0 1 0 0 0) (-1 -2 0 0 0)))

(solve-one '(#(NIL NIL T NIL NIL) ((0 0 1 0 0) (0 0 -1 0 0))))
(solve-one '(#(NIL NIL NIL T T) ((0 0 0 1 3) (0 0 0 -1 -3))))
(solve-one '(#(T T NIL NIL NIL) ((1 0 0 0 0) (0 1 0 0 0) (-1 -2 0 0 0))))

((1 0 0 0 -1 0 0) (0 0 0 1 -2 0 0) (0 0 1 0 0 -1 0) (0 1 0 0 0 0 -1)
 (0 3 0 0 0 0 -3))


(solve-with-sub-matrices '(
(1 0 0 0 -1 0 0) 
(0 0 0 1 -2 0 0)
(0 0 1 0 0 -1 0)
(0 1 0 0 0 0 -1)
(0 3 0 0 0 0 -3)))

Pb(1 0 0 0 -1 0 0) 
I (0 0 0 1 -2 0 0)
K (0 0 1 0 0 -1 0)
N (0 1 0 0 0 0 -1)
O (0 3 0 0 0 0 -3)


|#

#|
The problem with the original equation balancing was that it would find the first non-trivial solution it could, 
and this might result in zero coefficients for some chemicals.  Although this behavior was most common with ionic
equations, it could, in principle, happen for other reactions.

The solution is to break the problem into multiple separate, independent sub-problems.  Each of this can be solved
with a simple application of linear equation solving.  For example the reaction:
(a Reaction with
  (raw-material ((a Chemical with
                    (has-basic-structural-unit ((a Pb-Plus-2))))
                 (a Chemical  with
                    (has-basic-structural-unit ((a NO3-Minus))))
                 (a Chemical with
                    (has-basic-structural-unit ((a K-Plus))))
                 (a Chemical with
                    (has-basic-structural-unit ((a I-Minus))))))
  (result ((a Chemical with
               (has-basic-structural-unit
                  ((a Chemical-Entity with
                      (nested-atomic-chemical-formula
                ((a Chemical-Formula with
                            (term ((:seq (:pair 1 Pb)
                                         (:pair 2 I)))))))))))
            (a Chemical with
               (has-basic-structural-unit ((a K-Plus))))
            (a Chemical  with
               (has-basic-structural-unit ((a NO3-Minus)))))))

? Pb-Plus-2  +  ? NO3-Minus  +  ? K-Plus  +  ? I-Minus  --->  ? Pb2I  +  ? K-Plus  +  ? NO3-Minus

The K-Plus and NO3-Plus are a spectator ions.  Therefore these are an independent parts of the equation
? K-Plus  --->  ? K-Plus
? NO3-Minus  --->  ? NO3-Minus 
The rest is:
? Pb-Plus-2  +  ? I-Minus  --->  ? Pb2I

Each of these equations is solved separately.

Outline:
The matrix of equations is constructed as before.  The code calls this the lhs matrix.
create-sub-matrices separates the matrix into its independent components
solve-one is applied to each sub-matrix
gather-solutions then combines them
|#

(defun array-to-list (ar)
   (let ((l nil))
      (do ((i 0 (1+ i)))
         ((= i (array-dimension ar 0)))
         (push (aref ar i) l))
      (reverse l)))

(defun empty-matrix-p (m)
  (null m))

(defun add-column (column m)
  (if (empty-matrix-p m)
      (mapcar #'list column)
      (mapcar #'cons column m)))

(defun matrix-transpose (m)
  (if (empty-matrix-p m)
      m
      (add-column (car m) (matrix-transpose (cdr m)))))


(defun variable-overlap (variables-present column)
   (let ((ndx 0))
      (dolist (item column)
         (if (and (neq item 0) (aref variables-present ndx))
            (return t))
         (setf ndx (1+ ndx)))))

(defun all-zero-column (column)
   (= 0 (reduce #'(lambda (x y) (if (and (= x 0) (= y 0)) 0 1)) column)))

(defun split-matrix-by-variables (variables-present matrix)
   (let (
         (present nil)
         (remainder nil))
   (dolist (col matrix)
      (if (or (variable-overlap variables-present col) (all-zero-column col)) ;include all zero columns arbitrarily
         (push col present)
         (push col remainder)))
   (list (reverse remainder) (list variables-present (reverse present)))))

(defun create-sub-matrices (matrices)
   (let (
         (remainder-matrix (first matrices))
         )
   (if (null remainder-matrix)
      (rest matrices)
      (let (
            (variables-present (make-array (length (first remainder-matrix))))
            (ndx 0)
            (stable nil)
            )
      (dolist (item (first remainder-matrix))
         (if (neq item 0)
            (setf (aref variables-present ndx) t))
         (setf ndx (1+ ndx)))
      (do () (stable)
         (setf stable t) 
         (dolist (col remainder-matrix)
            (setf ndx 0)
            (if (variable-overlap variables-present col)
               (dolist (item col)
                  (if (and (neq item 0) (not (aref variables-present ndx)))
                     (progn
                        (setf (aref variables-present ndx) t)
                        (setf stable nil)))
                  (setf ndx (1+ ndx))))))
      ;recursive call
      (create-sub-matrices (append 
            ;now split up the matrix
            (split-matrix-by-variables variables-present remainder-matrix) 
            (rest matrices)))))))

(defun solve-one (sub-matrix)
   (if (= (length (second sub-matrix)) 1)
      (list 
         (first sub-matrix)
         (list 0))
      (let* (
            (lhs (matrix-transpose (second sub-matrix)))
            (solution (call-equation-solver (length lhs) (reduce-lhs-matrix lhs) (construct-rhs-matrix lhs)))
            (normalized-solution 
               (if (and solution (not (member nil solution :test #'equal)))
                  (normalize-solution solution))))
         (list
            (first sub-matrix)
            normalized-solution))))

(defun gather-solutions (sub-solutions tran-matrix)
   (let (
         (solution (make-array (length tran-matrix))))
   (dolist (sub sub-solutions solution)
      (let (
            (variables-present (first sub))
            (sol-list (second sub))
            (colndx 0))
      (dolist (col tran-matrix)
         (if (and (variable-overlap variables-present col) sol-list)
            (progn
               (setf (aref solution colndx) (first sol-list))
               (setf sol-list (rest sol-list))))
         (setf colndx (1+ colndx)))))))

(defun solve-with-sub-matrices (lhs-matrix)
   (let (
         (tran-matrix (matrix-transpose lhs-matrix)))
   (array-to-list 
      (gather-solutions 
         (mapcar #'solve-one (create-sub-matrices (list tran-matrix)))
         tran-matrix))))

;; INPUT: an reaction
;; OUTPUT: the list of subscripts for the chemicals involved in the reaction
(defun balance-equation-hashed (reaction)
  (let* ((reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction))
	 (reaction-elements (get-reaction-elements reaction :reactants reactants :products products))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (if (and reactants products reaction-elements)
			 (construct-lhs-matrix reaction-elements reactants products)))
	 (normalized-solution (if (and lhs-matrix (> number-of-elements 0))
          (solve-with-sub-matrices lhs-matrix))))
    (if (and normalized-solution (not (member nil normalized-solution :test #'equal)))
	(mapcar #'(lambda (chem coeff)
		    (cons chem coeff))
		(mapcar #'chem-string-name 
			(append reactants products))
		normalized-solution))))

(defun balance-reaction (reaction)
  (new-situation)
  (let* ((reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction))
	 (reaction-elements (get-reaction-elements reaction :reactants reactants :products products))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (construct-lhs-matrix reaction-elements reactants products))
	 (normalized-solution (solve-with-sub-matrices lhs-matrix)))
	 (put-reaction-coefficients reactants products normalized-solution)
	 normalized-solution
	 ))


(defun sri-balance-reaction (reaction)  
  (with-standard-bps-parameters()
  (let* ((reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction)) 
	 (reaction-elements (get-reaction-elements reaction :reactants reactants :products products))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (if (and reactants products reaction-elements)
			 (construct-lhs-matrix reaction-elements reactants products)))
	 (normalized-solution (if (and lhs-matrix (> number-of-elements 0))
				  (solve-with-sub-matrices lhs-matrix))))
    (if (and normalized-solution 
	     (not (member nil normalized-solution :test #'equal))
	     (not (member 0 normalized-solution)))
	(list (mapcar #'(lambda (chem)
			  (list :pair (pop normalized-solution) chem))
		      reactants)
	      (mapcar #'(lambda (chem)
			  (list :pair (pop normalized-solution) chem))
		      products)))))
	      )