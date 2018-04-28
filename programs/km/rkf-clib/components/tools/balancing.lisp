;;
;; $Id: balancing.lisp,v 1.20 2010/05/19 20:44:51 kbarker Exp $
;;
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoke ReactionBalancing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construct-lhs-matrix (reaction-elements reactants products)
  ;;; For each element participating in the reaction
  (loop for i from 0
      for element in reaction-elements
      collect
	;;; Count the number of times each appears in every reactant/product
	(append
	 (loop for reactant in reactants 
	     collect (get-number-of-atoms reactant (nth i reaction-elements)))
	 ;;; The products have a negative coefficients
	 (loop for product in products 
	     collect (* -1 (get-number-of-atoms product (nth i reaction-elements)))))))


;;; One of the coefficients is set to 1.  This is the coefficient for the 
;;; chemical compound that is the last in the equation.
(defun reduce-lhs-matrix (lhs-matrix)
  (loop for row in lhs-matrix
      collect (butlast row)))

;;; The RHS corresponds to the values corresponding the compound whose coefficient
;;; was set to 1.
(defun construct-rhs-matrix (lhs-matrix)
  (loop for row in lhs-matrix
      collect (* -1 (first (last row)))))
  

;;; Add 1 back into the solution for the coefficient that was set to 1
(defun unreduce-solution (solution)
  (let* ((x1 (reverse solution))
	 (x2 (push 1 x1))
	 (x3 (reverse x2)))
    x3))


(defun normalize-solution (solution)
  (let* (
   ;;; Un-reduce the solution
	 (unreduced-solution (unreduce-solution solution))
  ;;; Determine the minimum
	 (denominators (loop for x in unreduced-solution collect (denominator x)))
	 (lcm  (apply #'lcm denominators))
	 (normalized-solution (loop for x in unreduced-solution 
				  collect (* x lcm))))
    normalized-solution)
  )

(defun put-reaction-coefficients (reactants products solution)
  (loop for chemical in (append reactants products)
       for i from 0
      do
	(km0 `(,CHEMICAL |now-has| (|coefficient| ((|a| |Coefficient-Value| |with| (|value| ((:|pair| ,(NTH I SOLUTION) |nil|))))))))
	)
  )

#|Deprecated code
(defun call-equation-solver-old (number-of-elements lhs-matrix-reduced rhs-matrix)
  (let ((solution nil))
    (multiple-value-bind (i1 i2 solution-array)
	(solve-linear-system (make-array `(,number-of-elements ,number-of-elements)
					      :initial-contents lhs-matrix-reduced)
				  :b (make-array `(,number-of-elements)
						 :initial-contents rhs-matrix))
      (setq solution
	(loop for i from 0 to (- (array-dimension solution-array 0) 1)
	  collect (aref solution-array i)))
      solution)))
|#

(defun unit-vector-solution? (lhs rhs)
  (let ((answer? t))
    (dotimes (i (length rhs))
      (if (/= (nth i rhs)
	      (eval (cons '+ (nth i lhs))))
	  (setf answer? nil)))
    answer?
    ))

;test for unsolvable equation
;  (CALL-EQUATION-SOLVER 3 '((3 0 0) (6 0 -2) (0 2 -1)) '(0 2 1))
(defun call-equation-solver (number-of-elements lhs-matrix-reduced rhs-matrix)
   (let ((solution nil)
         (ncolumns (length (first lhs-matrix-reduced))))
   (if (unit-vector-solution? lhs-matrix-reduced rhs-matrix)
      (make-list number-of-elements :initial-element 1)
      (multiple-value-bind 
         (i1 i2 solution-array)
         (reduce-integer-system 
            (make-array `(,number-of-elements ,ncolumns)
               :initial-contents lhs-matrix-reduced)
            :b (make-array `(,number-of-elements)
               :initial-contents rhs-matrix))
         (if (>= i1 0) ;;CONSIDER: test for = 0? (to require a point solution)
            (setq solution
               (loop for i from 0 to (- (array-dimension solution-array 0) 1)
                  collect (aref solution-array i))))
         ;check for negative coefficients
         (if (mapcan #'(lambda (x) (and (< x 0) (list x))) solution)
            (setq solution nil))    
         solution))))

;;mrglas - this is the old balance reaction
;;The new one is in sub-matrices.lisp
(defun balance-reaction-old (reaction)
  (new-situation)
  (let* ((reaction-elements (get-reaction-elements reaction))
	 (reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (construct-lhs-matrix reaction-elements reactants products))
	 (lhs-matrix-reduced (reduce-lhs-matrix lhs-matrix))
	 (rhs-matrix (construct-rhs-matrix lhs-matrix))
	 (solution (call-equation-solver number-of-elements lhs-matrix-reduced rhs-matrix))
	 (normalized-solution (normalize-solution solution)))
	 (put-reaction-coefficients reactants products normalized-solution)
	 normalized-solution
	 )
  )

(defun balancing-test-harness (R)
  (format *terminal-io* "Before balancing~%")
  (showme R)
  (balance-reaction R)
  (format *terminal-io* "After balancing~%")
  (showme R)
  (loop for x in (km0 `(|the| |raw-materials| |of| ,R))
      do (showme x))
  (loop for x in (km0 `(|the| |products| |of| ,R))
      do (showme x))
  )

;;Missing definitions for test case constants, *R1, *R2, ...
(defun test-1 ()
  (balancing-test-harness '*R1))

(defun test-2 ()
  (balancing-test-harness '*R2))

(defun test-3 ()
  (balancing-test-harness '*R3))

(defun test-4 ()
  (balancing-test-harness '*R4))


(defun test-5 ()
  (balancing-test-harness '*R5))

(defun balancing-test-cases ()
  (test-1)
  (test-2)
  (test-3)
  (test-4)
  (test-5))
