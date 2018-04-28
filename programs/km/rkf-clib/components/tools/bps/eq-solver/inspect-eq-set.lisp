;;
;; $Id: inspect-eq-set.lisp,v 1.6 2008/02/02 19:14:26 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Lists all equation-expressions in SME authored concepts.
(defun ls-all-equation(&optional (stream t))
  (dolist (concept *all-user-defined-concepts*)
    (progn
      (format stream "Listing eq-expr(s) for concept, ~a~%" concept)
      (ls-equation concept)
      (format t "~%"))))

;;Lists all unique equation-expressions in SME authored concepts.
(defun ls-all-unique-equation(&optional (stream t))
  (dolist (concept *all-user-defined-concepts*)
    (progn
      (format stream "Listing unique eq-expr(s) for concept, ~a~%" concept)
      (ls-unique-equation concept)
      (format t "~%"))))

;;Lists equation-expressions in SME authored concept.
(defun ls-equation(concept 
		   &optional
		   (all-eq-provenance (get-equation-provenance-for-concept concept)) 
		   (stream t))
  (let ((*EQ-FLAG* nil))
  (dolist (eq-provenance (firstn all-eq-provenance (1- (length all-eq-provenance))))
      (let ((concept (car eq-provenance))
	    (eq-expr (cadr eq-provenance)))
	(if (> (length eq-expr) 0)
	    (progn 
	      (format stream "  ~a eq-expr(s) introduced by superclass, ~a~%" (length eq-expr) concept)
	      (dolist (expr eq-expr)
		(format stream "    ~a~t~%" expr))))))
    (ls-unique-equation concept all-eq-provenance stream)))

;;Lists unique equation-expressions in SME authored concept, i.e. equations that are not inherited.
(defun ls-unique-equation(concept 
			  &optional
			  (all-eq-provenance (get-equation-provenance-for-concept concept))
			  (stream t))
  (let ((concept (car  (car (last all-eq-provenance))))
	(eq-expr (cadr (car (last all-eq-provenance)))))
    (format stream "  ~a eq-expr(s) uniquely introduced by concept definition~%" (length eq-expr))
    (dolist (expr eq-expr)
      (format stream "    ~a~t~%" expr))))
      
;;Gets all equation-expressions in SME authored concept.
(defun get-equation-for-concept(concept)
  (multiple-value-bind
    (expr-list symbol-table specialized-symbols)
    (extract-equation-expression-and-symbols concept)
    (let ((target-expr-list (replace-all-expressions-with-km-instance expr-list symbol-table))
	  (normalize-map    (determine-normalize-map symbol-table)))
      (mappend #'(lambda(expr)
		   (normalize-expression expr normalize-map))
	       target-expr-list))))

(defun get-equation-provenance-for-concept(concept)
  (let ((all-eq-provenance (reverse (get-equation-provenance-for-concept0 concept)))
	(all-eq-expr-list  ())
	(*EQ-FLAG* nil))
    (mapcar #'(lambda(eq-provenance)
		(multiple-value-bind
		  (concept eq-expr)
		  (get-novel-eq-expr eq-provenance all-eq-expr-list)
		  (setq all-eq-expr-list 
			(union all-eq-expr-list eq-expr :test 'equal))
		  (list concept eq-expr)))
		all-eq-provenance)))

(defun get-novel-eq-expr(eq-provenance &optional(previously-introduced-eq-expr-list nil))
  (let* ((concept (car  eq-provenance))
	 (eq-expr (cadr eq-provenance)))
    (values concept (set-difference eq-expr previously-introduced-eq-expr-list :test 'equal))))
    
(defun get-equation-provenance-for-concept0(concept)
  (let ((superclass-list (immediate-superclasses concept))
	(eq-expr         (get-equation-for-concept concept)))
    (if (not (null eq-expr))
	(cons (list concept eq-expr)
	      (mappend #'(lambda(superclass)
			   (get-equation-provenance-for-concept0 superclass))
		       superclass-list)))))
  
(defun normalize-expression(expr normalize-map)
  (mapcar #'(lambda (entry)
	      (unquote (replace-elements-in-list expr entry)))
	  normalize-map))

(defun extract-all-km-instances-in-expression(expr)
  (remove-duplicates 
   (remove nil
	   (mapcar #'(lambda (token)
		       (cond ((km-instancep token) token)))
		   (flatten expr)))))

(defun determine-normalize-map(symbol-table)
  (let* ((normalize-map (determine-normalize-map0 symbol-table))
	 (valid-inst-list (mapcar #'(lambda(x) (first  x)) normalize-map))
	 (valid-slot-list (mapcar #'(lambda(x) (second x)) normalize-map))
	 (specialized-value-list (mapcar #'(lambda(x) (third x)) normalize-map))
	 (all-permutation (permute valid-slot-list)))
    (mapcar #'(lambda(permutation)
		(repack-normalize-map valid-inst-list specialized-value-list permutation))
	    all-permutation)))

(defun determine-normalize-map0(symbol-table)
  (let((km-instance-list (get-all-eq-km-instances symbol-table)))
    (remove-duplicates
     (mapcar #'(lambda(km-instance)
		 (let ((slot-list         (invert-slot-list (get-valid-inverse-slots-for-instance km-instance)))
		       (specialized-value (car (ps-km-query `(|the| |text-gen| |of| ,km-instance)))))
		   (if (not (null slot-list))
		       (list km-instance slot-list specialized-value)  
		       (list km-instance (ps-km-query `(|the| |called| |of| ,km-instance)) specialized-value)))) ;;For constant values
	     km-instance-list)
     :test 'equal)))

(defun repack-normalize-map(inst-list specialized-value-list permutation)
  (mapcar #'(lambda(inst specialized-value slot)
	      (cond ((string= specialized-value "")
		     (cons inst (format nil "~a" slot)))
		    (t (cons inst (format nil "~a[~a]" slot specialized-value)))))
	  inst-list specialized-value-list permutation))

(defun replace-all-expressions-with-km-instance (expr-list symbol-table)
  (mapcar #'(lambda(expr)
	      (replace-symbols-with-km-instance expr symbol-table))
	  expr-list))

(defun replace-symbols-with-km-instance(expr symbol-table)
  (replace-elements-in-list expr symbol-table))

(defun extract-equation-expression-and-symbols(concept)
  (let* ((*EQ-FLAG*    nil)
	 (skolem       (ps-instantiate-concept concept))
	 (system       (car (ps-km-query `(|the| |component-of| |of| ,skolem))))
	 (eq-set       (ps-km-query `(|the| |equation| |of| ,system)))
	 (expr         (extract-equation-expression-for-system system))
	 (symbol-table (extract-equation-symbol eq-set))
	 (symbol-table-assoc-map 	    (make-symbol-table-assoc-map symbol-table)))
    (values expr 
	    symbol-table-assoc-map 
	    (extract-specialized-values  symbol-table-assoc-map))))

(defun extract-specialized-values (symbol-table)
(remove nil  
	(mapcar #'(lambda(entry)
		    (extract-specialized-value-for-symbol-table-entry entry))
		(remove-duplicates symbol-table
				   :test #'(lambda (x y) (and (equal (car x) (car y)) (equal (cdr x) (cdr y))))))))

(defun extract-specialized-value-for-symbol-table-entry(entry)
  (let ((slot (car entry))
	(P-value    (cdr entry)))
    (cond ((ps-km-query `(|the| |value| |of| ,P-Value))
	   (cons slot (car (ps-km-query `(|the| |text-gen| |of| ,P-Value))))))))

(defun extract-specialized-values-for-km-instances(input)
  (cond ((null input) ())
	((atom input)
	 (let ((specialized-value (car (ps-km-query `(|the| |text-gen| |of| ,input)))))
	   (if (not (string= specialized-value "")) (cons input specialized-value))))
	(t (remove nil
		   (cons (extract-specialized-values-for-km-instances (car input))
			 (extract-specialized-values-for-km-instances (cdr input)))))))

(defun extract-equation-expression (input)
  (cond ((null input) nil)
	((atom input) 
	 (ps-km-query `(|the| |equation-expression| |of| ,input)))
	(t
	 (append (extract-equation-expression (car input))
		 (extract-equation-expression (cdr input))))))

(defun extract-equation-symbol (input)
  (cond ((null input) nil)
	((atom input) 
	 (ps-km-query `(|the| |equation-symbol| |of| ,input)))
	(t
	 (append (extract-equation-symbol (car input))
		 (extract-equation-symbol (cdr input))))))

(defun make-symbol-table-assoc-map (symbol-table)
  (mapcar #'(lambda(entry)
	      (let ((symbol (second entry))
		    (km-instance (third entry)))
		(if (quotep symbol) (setf symbol (unquote symbol)))
		(cons symbol km-instance)))
	  symbol-table))