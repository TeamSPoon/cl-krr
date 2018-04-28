;;
;;    $Id: make-string-name.lisp,v 1.32 2010/05/19 20:44:51 kbarker Exp $
;;

;; ################################################
;; Interface functions
;;
;; chf-to-string (km-ChF-term)
;; compute-chf-of-ionic-compound (cation cation-charge anion anion-charge)
;; compute-atomic-chemical-formula (chf-list)
;;
;; ################################################

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; translates the term of a Chemical-Formula into a string
(defun chf-to-string (km-ChF-term)
  (let ((chf (cdar km-ChF-term)))
    (join "" 
	  (mapcar #'(lambda (term)
		      (let ((chemical (km-unique0 (third term)))
			    (subscript (second term)))
			(cond
			 ((chf-seq chemical) ;; nested formula
			  (if (not (equal subscript 1))
			      (format nil "(~A)~A" (chf-to-string (list chemical)) subscript)
			    (chf-to-string (list chemical)))
			  )
			 ((or (is-atom chemical)
			      (is-undecl chemical))
			  ;; atom
			  (if (not (equal subscript 1))
			      (format nil "~A~A" (atom-chf-to-string chemical) subscript)
			    (atom-chf-to-string chemical)))
			 (t
			  (let ((chemical-chf (km0 `(|the| |term| |of| 
						     (|the| |has-chemical-formula| |of| 
						      (|a| ,chemical))))))
			    (if (not (equal subscript 1))
				(let ((subf-str (chf-to-string chemical-chf)))
				  (if (needs-parens subf-str)
				      (format nil "(~A)~A" (chf-to-string chemical-chf) subscript)
				    (format nil "~A~A" (chf-to-string chemical-chf) subscript)))
			      (chf-to-string chemical-chf)))))))
		  chf))))

(defun needs-parens (chf-string)
  (> (count-if #'(lambda (c)
		   (eq (char-upcase c) c))
	       chf-string)
     1))

(defun find-gcf (n1 n2)
  ;; Use the super-inefficient method
  ;; The numbers are going to be small enough, so optimizing won't make much of
  ;; a difference
  (if (or (zerop n1) (zerop n2))
     (progn (print "bad arguments to find-gcf in make-string-name.lisp") 1)
     (loop for f from n1 downto 1
        when (and (zerop (rem n1 f)) (zerop (rem n2 f)))
        return f)))

(defun find-first-non-zero (num-list)
   (if num-list
      (if (not (zerop (first num-list))) 
         (first num-list)
         (find-first-non-zero (rest num-list)))
      0))

;TODO: fix with Jason's subsumption function
(defun compute-chf-of-ionic-compound (cation cation-charge anion anion-charge)
  (when (and anion-charge cation-charge (first anion-charge) (first cation-charge))
    (let* ((anion-charge (abs (find-first-non-zero anion-charge)))
           (cation-charge (abs (find-first-non-zero cation-charge)))
           (factor (find-gcf anion-charge cation-charge))
           (anion-count (truncate (/ cation-charge factor)))
           (cation-count (truncate (/ anion-charge factor)))
           (cation-class (first (remove-subsumers (km0 `(|the| |instance-of| |of| ,cation)))))
           (anion-class (first (remove-subsumers (km0 `(|the| |instance-of| |of| ,anion)))))
           )
      `((:|seq| 
            (:|pair| ,CATION-COUNT ,CATION-CLASS)
            (:|pair| ,ANION-COUNT ,ANION-CLASS))))))

;; given an ionic chemical formula, computes the atomic has-chemical-formula
;; (:seq (:pair 2 Na-Plus) (:pair 1 CO3-Minus-2)) -> 
;; (:seq (:pair 2 Na) (:pair 1 C) (:pair 3 O))
(defun compute-atomic-chemical-formula (chf-list)
  (let ((atoms (compute-atomic-chemical-formula-0 chf-list nil)))
    `(:|seq| ,@(mapcar #'(lambda (p)
			   (list ':|pair| (cdr p) (km-unique0 (car p))))
		       atoms))))
       
    
#|
		       (sort atoms
		       #'(lambda (t1 t2)
			   (string< (symbol-name (car t1))
				    (symbol-name (car t2))))))))
|#


;; ###################
;; Auxiliary functions 

(defun to-string (x)
  (format nil "~A" x))

;; glues the elements of list with glue-string
(defun join (glue-string list)
  (let ((result (first list)))
    (dolist (el (rest list) result)
            (setf result (format nil "~A~A~A" result glue-string el)))
    result))



(defun atom-chf-to-string (chemical)
  ;;(km0 `((|the| |symbol| |of| (|a| ,chemical)))))
  (concatenate 'string (format nil "~A" (string-capitalize chemical))))



(defun compute-atomic-chf-of-ionic-compound (cation-chf cation-charge anion-chf anion-charge)
  (let ((result1
         (mapcar #'(lambda (cation-chf-term)
		     `(:|pair| ,(* (abs (first anion-charge)) 
				   (second cation-chf-term))
			,(km-unique0 (third cation-chf-term))))
		 (cdar cation-chf)))
        (result2
         (mapcar #'(lambda (anion-chf-term)
		     `(:|pair| ,(* (abs (first cation-charge) )
				   (second anion-chf-term))
			,(km-unique0 (third anion-chf-term))))
		 (cdar anion-chf))))
    (if (and result1 result2)
        (list
         (append `(:|seq|)
                 result1 result2)))))




(defun compute-atomic-chemical-formula-0 (chf-list &optional (atoms nil))
  (dolist (term (cdar chf-list) atoms)
	  (let* ((chemical (km-unique0 (third term)))
		 (subscript (second term)))
	    (if (or (is-undecl chemical)
		    (is-atom chemical))
		(setf atoms (append atoms (list (cons chemical subscript))))
	      ;; multiply 
	      (setf atoms (alist-union atoms 
				       (if (> subscript 1)
					   (alist-mult subscript
						       (compute-atomic-chemical-formula-0 
							(km0 `(|the| |term| |of|
							       (|the| |has-chemical-formula| |of| 
								(|a| ,chemical))))))
					 (compute-atomic-chemical-formula-0 
					  (km0 `(|the| |term| |of|
						 (|the| |has-chemical-formula| |of| 
						  (|a| ,chemical))))))))))))

;; returns true if chemical is atom
(defun is-atom (chemical)
  (and (or (atom chemical)
	   (< (length chemical) 2)) ;; either atom or a singleton list
       (km0 `((|the| |all-superclasses| |of| ,(km-unique0 chemical)) 
	      |includes| |Atom|))))

(defun is-undecl (chemical)
  (and (or (atom chemical)
	   (< (length chemical) 2)) ;; either atom or a singleton list
       (km0 `((|the| |all-superclasses| |of| ,(km-unique0 chemical)) 
	      = |Thing|))))

;; computes union of two alists
;; if they contain duplicate elements, sums up their respective number
(defun alist-union (al1 al2)
  (if (null al1) 
      al2
    (if (null al2)
	al1
      (let ((res al2))
	(dolist (p1 al1 res)
		(let ((p2 (assoc (car p1) al2)))
		  (if p2 
		      ;; if duplicate -> add up counts
		       (incf (cdr (assoc (car p2) res)) 
			     (cdr p1))
		    ;; else add it to alist
		    (setf res (append (list (cons (car p1) (cdr p1))) res)))))))))


(defun alist-mult (subs alist)
  (dolist (p alist alist)
	  (setf (cdr (assoc (car p) alist)) (* (cdr p) subs))))

;; finds the subscript of ch-el in the chemical formula of ch-comp
;; TODO: for now, ch-el can be an ion or an atom
(defun subscript-of (ch-el ch-comp) 
  (second (car (member ch-el (if (is-atom ch-el)
				 (cdar (compute-atomic-chemical-formula 
					(km0 `(|the| |term| |of|
					       (|the| |has-chemical-formula| |of| 
						(|a| ,ch-comp))))))
			       (cdar (km0 `(|the| |term| |of|
					     (|the| |has-chemical-formula| |of| 
					      (|a| ,ch-comp))))))
		       :key #'third
		       :test #'eq))))


(defun chf-pair (x)
  (and (consp x)
       (equal (first x) ':|pair|))
  )

(defun chf-seq (x)
  (and (consp x)
       (equal (first x) ':|seq|))
  )
