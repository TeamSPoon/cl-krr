;;
;; $Id: triple-list-ablation-routines.lisp,v 1.10 2009/05/29 20:37:13 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun find-valid-ablation(scenario)
  (let ((debug nil)
	(valid-result   nil)
	(invalid-result nil))
    (if (numberp (get-clib-depth-for-triple-list scenario))
	(dotimes (i (get-clib-depth-for-triple-list scenario))
	  (multiple-value-bind
	      (valid? reason)
	      (is-valid-ablation scenario i)
	    (if debug (format t "valid ablation depth ~a => ~a ~a~%" i valid? reason))
	    (if valid? (push i valid-result)
	      (push (list i reason) invalid-result))))
      (values valid-result invalid-result))))

(defun is-valid-ablation(scenario clib-depth)
  (test-ablation (generalize-triples-to-clib-depth scenario clib-depth)))

(defun test-ablation(scenario)
  (let ((invalid-reason nil))
    #|
  (if (ablation-violate-constraint? scenario)
      (push 'constraint-violation invalid-reason))
    |#
  (if (ablation-violate-domain/range? scenario)
      (push 'domain/range-violation invalid-reason))
  (if (null invalid-reason) t
    (values nil invalid-reason))))
     
(defun ablation-violate-constraint?(scenario)
  (let ((*logging* t)
	(*on-error* 'abort))
    (bps-set-checkpoint 'test-ablation)
    (let ((result
	   (catch 'BPS-REASONING-ERROR (ps-assert-triples scenario t))))
      (bps-undo 'test-ablation)
      (and (listp result)
	   (eq (car result) 'BPS-REASONING-ERROR)))))
	
(defun ablation-violate-domain/range?(triple-list)
  (not 
   (eval 
    (cons 'and 
	  (mapcar #'(lambda(triple)
		      (quasi-check-triple triple triple-list))
		  (extract-non-instance-triples triple-list))))))

(defun get-most-general-valid-ablation-graph(src-graph)
  (let* ((valid-ablation-depth-lst (find-valid-ablation src-graph))
	 (most-general-valid-ablation-depth     
	  (if (not (null valid-ablation-depth-lst))
	      (apply 'min valid-ablation-depth-lst))))
    (if (not (null most-general-valid-ablation-depth))
	(values 
	 (generalize-triples-to-clib-depth 
	  src-graph
	  most-general-valid-ablation-depth)
	 most-general-valid-ablation-depth))))

(defun quasi-check-triple(triple triple-list)
  (let ((head     (triple-head     triple))
	(relation (triple-relation triple))
	(tail     (triple-tail     triple)))
    (and (valid-domain-of-p relation  (quasi-get-concept-for-kb-instance head triple-list))
	 (valid-range-of-p  relation  (quasi-get-concept-for-kb-instance tail triple-list)))))

(defun valid-domain-of-p(relation concept-list)
  (or (null (domains-of relation))
      (not (null (intersection concept-list
		    (domains-of relation)
		    :test 
		    'is-subclass-of)))))

(defun valid-range-of-p(relation concept-list)
  (or (null (ranges-of relation))
      (not (null (intersection concept-list
		    (ranges-of relation)
		    :test 
		    'is-subclass-of)))))
