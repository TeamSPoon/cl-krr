;;
;; $Id: keq-calc.lisp,v 1.8 2006/01/30 16:02:07 jfan Exp $
;;
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;
;; TOP-LEVEL function called by the method Compute-Equilibrium-Constant
;; to compute the equilibrium constant.
;; 
(defun calc-equilibrium-constant (keq-expr x reaction-type)
  (let (A B C D a-init b-init c-init d-init a-coeff b-coeff c-coeff d-coeff
	(hydrolysis? (if (member '|Hydrolysis| reaction-type :test #'equal) t nil))
	(complete-dissoc? (if (member '|Complete-Dissociation-Reaction| reaction-type :test #'equal) t nil))
	polyprotic-acid?)
    (cond 
      ((or (null keq-expr) (null x) (null reaction-type) (not (numberp x)))
       nil)
      (t
       ;; Get the chemicals.
       (setf a-init (list nil (if (or hydrolysis? complete-dissoc?)
			      	  (third (third (first keq-expr)))
			      	  (third (second (third (first keq-expr))))))
	     b-init (list (if (or hydrolysis? complete-dissoc?) 0 nil) 
			  (if (or hydrolysis? complete-dissoc?) '|Thing| (third (third (third (first keq-expr))))))
	     c-init (list (if (or hydrolysis? complete-dissoc?) 0 nil) (third (second (second (first keq-expr)))))
	     d-init (list (if (or hydrolysis? complete-dissoc?) 0 nil) (third (third  (second (first keq-expr))))))
       ;; Get the coefficients.
       (setf a-coeff (if (or hydrolysis? complete-dissoc?)
			 1
			 ;; (second (third (first keq-expr)))
			 (second (second (third (first keq-expr)))))
	     b-coeff (if (or hydrolysis? complete-dissoc?) 0 (second (third (third (first keq-expr)))))
	     c-coeff (if (or hydrolysis? complete-dissoc?) 1 (second (second (second (first keq-expr)))))
	     d-coeff (if (or hydrolysis? complete-dissoc?) 1 (second (third  (second (first keq-expr))))))
       ;; Now calculate the equilibrium concentration for each component.
       (setf A (calc-eq-concentration a-init a-coeff x nil)
	     B (calc-eq-concentration b-init b-coeff x nil)
	     C (calc-eq-concentration c-init c-coeff x t)
	     D (calc-eq-concentration d-init d-coeff x t))
       ;; If the reaction is a complete dissociation, check to make sure the substance
       ;; is a polyprotic acid.
       (if (and complete-dissoc? (km0 `(,(second a-init) |isa| |Polyprotic-Acid|)))
	   (setf polyprotic-acid? (number-of-h-plus (second a-init))))
       ;; Now calculate the constant, if a value was found for each
       ;; component
       (if (and (numberp A) (numberp B) (numberp C) (numberp D)
		(numberp a-coeff) (numberp b-coeff) (numberp c-coeff) (numberp d-coeff)
		(> A 0) (>= a-coeff 0) (>= b-coeff 0) (>= c-coeff 0) (>= d-coeff 0)
		(> (* (expt A a-coeff) (expt B b-coeff)) 0))
	   (first (km0 `(|a| |Equilibrium-Constant-Value| |with|
			     (|value| ((:|pair| 
					 ,(/ (cond 
  					       (complete-dissoc? 
						(if (numberp polyprotic-acid?) 
						    (* D (expt C polyprotic-acid?))
						    (* D (expt C 2))	;; Assume 2 if n is not known.

					       ))
  					       (hydrolysis? 
						(* D C))
  					       (t
						(* (expt D d-coeff) (expt C c-coeff))))
     					     (* (expt A a-coeff) (expt B b-coeff)))
					 nil)))))))))))

;; 
;; This function calculates the equilibrium concentration given the initial
;; concentration (which is either a number or a chemical to query for this 
;; value), the coefficient and the equilibrium change constant.
;;
;; If the equilibrium concentration cannot be compute, this function will
;; try to query for it directly.
;;
(defun calc-eq-concentration (init-concentration coeff x &optional (num? t))
  (let ((concentration 
	 (if (not (numberp (first init-concentration)))
	     (first (km0 `(|the1| |of| (|the| |value| |of| 
		    	    (|the| |concentration| |of| ,(second init-concentration))))))
	     (first init-concentration))))
    (cond 
      ((and (numberp concentration) (numberp coeff))
       (if num? (+ concentration (* coeff x)) (- concentration (* coeff x))))
      (t
       (first (km0 `(|the1| |of| (|the| |value| |of| 
	       	      (|the| |equilibrium-concentration| |of| ,(second init-concentration))))))))))

;;
;; Return the estimated value for x.
;;
(defun estimate-x (concentration k chemical)
  (let (n)
    (cond 
      ((km0 `(,chemical |isa| |Polyprotic-Acid|))
       (setf n (number-of-h-plus chemical))
       (if (numberp n) (expt (* concentration k) (/ 1 (+ 1 n)))))
      (t
       (expt (* concentration k) (/ 1 2))))))

;;
;; returns the number of H+ a chemical has.
;;
(defun number-of-h-plus (chemical)
  (first
    (km0 `(|the1| |of| 
	    (|oneof| (|the| |elements| |of| 
		       (|the| |term| |of| 
			 (|the| |has-chemical-formula| |of|
			   (|the| |has-basic-structural-unit| |of| ,chemical))))
		|where|
		((|the2| |of| |It|) = |H-Plus|))))))
    
;;
;; Computes the individual dissociations given a compound. 
;;
(defun compute-dissociations (input)
  (let ((curr (clone-hydrolysis-result input)) 
	result curr-hydrolysis prev-hydrolysis 
	hydrolysis-result x)
    (setf x (first 
             (km0 `(|the1| |of| 
		    (|oneof| (|the| |elements| |of| (|the| |term| |of| 
			       (|the| |has-chemical-formula| |of|
			         (|the| |has-basic-structural-unit| |of| ,input))))
			|where|
			((|the2| |of| |It|) = |H-Plus|))))))
    (if (numberp x)
	(progn
    	  (dotimes (n x)
	    (setf curr-hydrolysis 
		  (km-unique0 `(|a| |Hydrolysis| |with|
			  	    (|raw-material| (,curr)))))
	    (setf hydrolysis-result
		  (km-unique0 `(|the| |second| |of| (|the| |result| |of| ,curr-hydrolysis))))
	    (setf curr (clone-hydrolysis-result hydrolysis-result))
	    (setf result (cons curr-hydrolysis result))
	    (if prev-hydrolysis 
		(km0 `(,prev-hydrolysis |has|
			(|next-event| (,curr-hydrolysis)))))
	    (setf prev-hydrolysis curr-hydrolysis))
	  (reverse result)))))	

;;
;; Clones a instance. 
;;
;; !!!WARNING!!!: Does not clone everything. Just properties
;;	pertinent to hydrolsis.
;;
(defun clone-hydrolysis-result (source)
  (let ((charge-value 
	 (km-unique0 `(|the| |value| |of| (|the| |charge| |of|
		 	(|the| |has-basic-structural-unit| |of| ,source)))))
	(term 
	 (km-unique0 `(|the| |term| |of| (|the| |has-chemical-formula| |of|
		 	(|the| |has-basic-structural-unit| |of| ,source))))))
    (cond 
     ((and charge-value term)
      (km-unique0 `(|a| |Chemical| |with|
		    (|has-basic-structural-unit| (
						  (|a| |Chemical-Entity| |with|
						   (|charge| ((|a| |Charge-Value| |with|
							       (|value| (,charge-value)))))
						   (|has-chemical-formula| (
									    (|a| |Chemical-Formula| |with|
									     (|term| (,term)))))))))))
     (charge-value
      (km-unique0 `(|a| |Chemical| |with|
		    (|has-basic-structural-unit| (
						  (|a| |Chemical-Entity| |with|
						   (|charge| ((|a| |Charge-Value| |with|
							       (|value| (,charge-value)))))
						   ))))))
     (term
      (km-unique0 `(|a| |Chemical| |with|
		    (|has-basic-structural-unit| 
		     (
		      (|a| |Chemical-Entity| |with|
		       (|has-chemical-formula| 
			(
			 (|a| |Chemical-Formula| |with|
			  (|term| (,term)))))))))))
     )))
      
