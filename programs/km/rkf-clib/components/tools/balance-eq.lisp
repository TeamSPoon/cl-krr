;; 
;;    $Id: balance-eq.lisp,v 1.29 2010/05/19 20:44:51 kbarker Exp $ 
;; 
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defvar *balanced-eq-hash* (make-hash-table))

;; INPUT: an reaction and a chemical
;; OUTPUT: the subscript for the chemical based on the reaction
(defun balanced-equation-coefficient (reaction chemical)
  ;;(if *during-reaction-identification* (return-from balanced-equation-coefficient))
  (let ((chem-name (chem-string-name CHEMICAL)))
    (format t "Balancing equation for reaction ~a participant ~a (~a)...hash: ~a~%" 
	    reaction chemical chem-name (gethash reaction *balanced-eq-hash*))
    
    (if (not (gethash reaction *balanced-eq-hash*))
	(setf (gethash reaction *balanced-eq-hash*)
	      (balance-equation-hashed reaction)))
    (if (null (CDR (ASSOC chem-name (GETHASH REACTION *BALANCED-EQ-HASH*) :TEST #'STRING=)))
	(km0 `(|a| |Coefficient-Value|))
      (km0 `(|a| |Coefficient-Value| |with|
		 (|value| ((:|pair| ,(CDR (ASSOC CHEM-NAME (GETHASH REACTION *BALANCED-EQ-HASH*) :TEST #'STRING=)) nil))))))))


(defun zero-vector (vector)
  (not (remove 0 vector :test #'=))
  )

;; the following function added to interface w/ the routine from SRI
(defun get-coefficient-of-chemical (reaction chemical)
  (let* ((reaction-elements (get-reaction-elements reaction))
	 (reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (if (and reactants products reaction-elements)
			 (construct-lhs-matrix reaction-elements reactants products)))
	 (lhs-matrix-reduced (if (and reactants products reaction-elements)
				 (reduce-lhs-matrix lhs-matrix)))
	 (rhs-matrix (if (and reactants products reaction-elements)
			 (construct-rhs-matrix lhs-matrix)))
	 (solution (if (and reactants products reaction-elements
			    (> number-of-elements 0)
			    )
		       (call-equation-solver number-of-elements lhs-matrix-reduced rhs-matrix)))
	 (normalized-solution (if (and 
				   reactants products reaction-elements
				   (> number-of-elements 0)
				   (not (member nil solution :test #'equal)))
				  (normalize-solution solution)))
	 ;; default coefficient to 1;
	 (result 1))

    (if (and 
	 reactants products reaction-elements
	 (> number-of-elements 0)
	 normalized-solution)
	;;normalized-solution
	(loop for participant in (append reactants products)
	      for i from 0
	      do
	      (if (equal chemical participant)
		  (setf result (nth i normalized-solution)))))
    result
    )
  )


;; INPUT: an reaction
;; OUTPUT: the list of subscripts for the chemicals involved in the reaction
;;mrglas - this is the old balance equation
;;The new one is in sub-matrices.lisp
(defun balance-equation-hashed-old (reaction)
  (let* ((reaction-elements (get-reaction-elements reaction))
	 (reactants (get-reaction-reactants reaction))
	 (products (get-reaction-products reaction))
	 (number-of-elements (length reaction-elements))
	 (base-element (first reaction-elements))
	 (lhs-matrix (if (and reactants products reaction-elements)
			 (construct-lhs-matrix reaction-elements reactants products)))
	 (lhs-matrix-reduced (if (and reactants products reaction-elements)
				 (reduce-lhs-matrix lhs-matrix)))
	 (rhs-matrix (if (and reactants products reaction-elements)
			 (construct-rhs-matrix lhs-matrix)))
	 (solution (if (and reactants products reaction-elements
			    (> number-of-elements 0)
			    (not (find-if #'zero-vector lhs-matrix-reduced)) ;; matrix shouldn't have a zero vector
			    (not (zero-vector rhs-matrix))
			    )
		       (call-equation-solver number-of-elements lhs-matrix-reduced rhs-matrix)))
	 (normalized-solution (if (and 
				   reactants products reaction-elements
				   (> number-of-elements 0)
				   (not (member nil solution :test #'equal)))
				  (normalize-solution solution))))
    (if (and reactants products reaction-elements (> number-of-elements 0) normalized-solution)
	(mapcar #'(lambda (chem coeff)
		    (cons chem coeff))
		(append reactants products)
		normalized-solution))))

(defun same-atom-types (chemical chemical-list)
  ;; test only if  all three condition is true
  (if (and 
       ;; 1. chemical-list is non-empty
       chemical-list
       ;; WAS: 2. some in chemical-list have atom types
       ;; 2. EVERY chemical in chemical list must have atom types
       ;;    if there is some other generic Chemical, it could have all the atom types you're missing
       (every #'(lambda (ch)
		 (atom-types ch))
	     chemical-list)
       ;; 3. chemical has atom-types !!!
       (some #'atom-types chemical))
      (let ((raw-mat-atom-types (remove-duplicates (mappend #'atom-types chemical)))
	    (result-atom-types (remove-duplicates (mappend #'atom-types chemical-list))))
	(null (set-difference raw-mat-atom-types result-atom-types)))
    t))

(defun atom-types (chemical)
  (km0 `(|forall| |?atomic-chf-term| |in|
		  (|the| |term| |of| 
		       (|the| |atomic-chemical-formula| |of|
			    (|the| |has-basic-structural-unit| |of| ,CHEMICAL)))
		  (|the| |elements| |of| (|forall-seq| |?pair| |in| |?atomic-chf-term| 
					       (|the2| |of| |?pair|))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'mappend)
    (defun mappend (fn &rest lsts)
      (apply #'append (apply #'mapcar fn lsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Takes a chemical reaction as input, and returns its reactants
(defun get-reaction-reactants (reaction)
  ;;(km0 `(|showme| ,REACTION))
  (km0 `(|the| |raw-material| |of| ,REACTION)))

;;; Takes a chemical reaction as input, and returns its products
(defun get-reaction-products (reaction)
  ;;(km0 `(|showme| ,REACTION))
  (km0 `(|the| |result| |of| ,REACTION)))

;;; Takes a chemical as input, and returns the atoms 
;;; that participate in it

(defun get-chemical-elements (chemical)

  (let ((chemical-formula (km0 `(|the| |atomic-chemical-formula| |of| (|the| |has-basic-structural-unit| |of| ,CHEMICAL)))))
    (loop for element in (km0 `(|the| |elements| |of| (|the| |term| |of| (|the| |atomic-chemical-formula| |of| (|the| |has-basic-structural-unit| |of| ,CHEMICAL)))))
	if (is-subclass-of (third element) '|Atom|)
	collect (third element)
	else
	  append (get-chemical-elements (km0 `(|a| ,(third element)))))))

;;; Takes a chemical reaction as input, and returns the atoms 
;;; that participate in it

(defun get-reaction-elements (reaction &key (reactants nil) (products nil))
  ;;(km0 `(|showme| ,REACTION))
  (remove-duplicates 
   (loop for chemical in (append (if reactants reactants (get-reaction-reactants reaction))
				 (if products products (get-reaction-products reaction)))
      append (get-chemical-elements chemical))
   )
  )


;;; Given a chemical, determines if its formula contains a given atom.

(defun chemical-contains-atom (chemical atom)
  ;;; use of first is not principled
  (member atom (get-chemical-elements (first (km0 `(|a| ,chemical))))))


;;; Given a chemical, determine the number of atoms it contains 
;;; for a given atom.
(defun get-number-of-atoms (chemical atom)
   (let ((chemical-formula (km0 `(|the| |elements| |of| (|the| |term| |of| 
                     (|the| |atomic-chemical-formula| |of| (|the| |has-basic-structural-unit| |of| ,CHEMICAL))))))
         (number-of-atoms 0))
   ;(print chemical-formula)
   (loop for element in CHEMICAL-FORMULA
      if (and (equal (third element) atom)
         (numberp (second element)))
      do (setq number-of-atoms (second element)))
   number-of-atoms))
