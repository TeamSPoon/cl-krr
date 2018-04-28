;;
;; $Id: reasonable-elaboration-predicates.lisp,v 1.20 2009/06/29 02:37:28 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun update-pruned-vp-reason(vp-inst explanation)
  (push (list vp-inst explanation)
	*pruned-vp-reason*))

(defun get-pruned-vp-reason(vp-inst)
  (cadr (assoc vp-inst *pruned-vp-reason*)))

;;Checks if the candidate root is a participant in the semantic match.
;;Specifically, it should be a mapping to some node in the scenario graph
(defun is-candidate-root-mapped-in-semantic-match-p(candidate-root 
						    instance-map
						    vp-inst
						    &optional(verbose nil))
  (let ((result (not (null (member candidate-root (flatten instance-map))))))
    (if (null result)
	(update-pruned-vp-reason vp-inst
				 (format nil "Candidate concept not mapped")))
    result))

;;Returns mapped-slot triples of the form 
;;(KM-instance relation ?)
;;for all semantic matches.
(defun get-mapped-slots(match-result)
  (let ((triple-map   (convert-match-result-to-triples match-result)))
    (mappend 'get-mapped-slots0
	     triple-map)))

(defun sweep-viewpoint-list()
  (dolist (x (get-ordered-viewpoint-list))
    (let ((result (reasonable-viewpoint-p x)))
      (if result (format t "BPS: ~a => reasonable~%" x)
	         (format t "BPS: ~a => not reasonable~%" x)))))

(defun get-mapped-slots0(match-result-entry)
  (let ((matched-triples      (car match-result-entry))
	(matched-triple-score (cdr match-result-entry)))
    (let ((lhs (first  matched-triples))
	  (rhs (second matched-triples)))
      (cond ((triple-p lhs)
	     (list (list (triple-head     lhs)
			 (triple-relation lhs)
			 '*)))
	    (t (mapcar #'(lambda(lhs-triple)
			   (list (triple-head     lhs-triple)
				 (triple-relation lhs-triple)
				 '?))
		       lhs))))))

;;Checks that all slot mappings returns by the semantic matcher are complete. 
(defun all-slot-mapping-complete-p (scenario 
					match-result 
					vp-inst
					&optional(verbose nil))
  (let* ((uncloned-scenario        (unclone-triple-list scenario))
	 (matched-slot-triple-list (unclone-triple-list (get-mapped-slots match-result)))
	 (boolean-stmt (cons 'and (mapcar #'(lambda(match-slot-triple)
					      (complete-slot-mapping-p uncloned-scenario 
								       match-slot-triple
								       matched-slot-triple-list))
					  matched-slot-triple-list))))
    (let ((result (eval boolean-stmt)))
      (if (null result)
	  (update-pruned-vp-reason vp-inst
				   (format nil "Incomplete slot mappings")))
    result)))

;;A slot-mapping returned by the semantic matcher is complete.
;;if all slot values in the scenario are mapped to something.
(defun complete-slot-mapping-p (scenario 
				match-slot-triple 
				matched-slot-triple-list 
				&optional(verbose nil))
  (let ((instance (first match-slot-triple))
	(slot     (second match-slot-triple)))
    (let ((scenario-cardinality (quasi-get-cardinality-for-instance-slot instance slot scenario))
	  (match-cardinality    (count match-slot-triple matched-slot-triple-list :test 'equal)))
      (if verbose (format t "BPS: (the ~a of ~a) has scenario cardinality ~a, and matched cardinality ~a~%" 
			  (second match-slot-triple)
			  (first  match-slot-triple)
			  scenario-cardinality 
			  match-cardinality))
      (>= match-cardinality scenario-cardinality))))

(defun has-no-plausible-mapping-conflict-p(instance-map scenario model-graph vp-inst)
  (let ((result (eval 
		 (cons 'and 
		       (mapcar #'(lambda (mapping)
				   (ps-property-value-unify-p (car mapping)
							      (cadr mapping)
							      scenario
							      model-graph))
			       instance-map)))))
      (if (null result)
	  (update-pruned-vp-reason vp-inst
		    (format nil "Plausibly incorrect assumption")))
    result))
  
(defun ps-property-value-unify-p(x y 
				 scenario-graph
				 model-graph
				 &optional(verbose nil))
  (let* ((in-edge-count  (get-in-edge-count  x scenario-graph))
	 (out-edge-count (get-out-edge-count x scenario-graph))
	 (result (or (<= in-edge-count 1)
		     (null (extract-triples-for-root-slot y '|value| (make-triple-proper model-graph))))))
    (if verbose (format t "(ps-property-value-unify-p ~a ~a) => ~a~%" x y result))
    result))
       
(defun reasonable-semantic-match-p(candidate-concept 
				   candidate-root 
				   candidate-graph 
				   scenario 
				   match-result 
				   instance-map
				   vp-inst)
  (let ((debug nil)
	(expr nil))
    (setq expr
	(cond ((equal vp-inst (get-root-viewpoint)) t) ;; root viewpoint is always reasonable
	      ((equal (get-viewpoint-parent vp-inst) (get-root-viewpoint)) t) ;; 1st ply is always reasonable, we want to use the 1st ply mappings to validate triples in the CPL input scenario.
	      (t ;; okay check all vp-instances in the 2nd ply onwards.
	       (cons 'and 
		    (list
		     (has-NO-PLAUSIBLE-MAPPING-CONFLICT-P instance-map
							  scenario
							  candidate-graph
							  vp-inst)
		     (is-not-lateral-redundant-elaboration-p vp-inst)
		     (not (is-vertical-redundant-elaboration-p
			   scenario
			   candidate-graph
			   instance-map 
			   vp-inst))
		     (all-slot-mapping-complete-p scenario
						  match-result
						  vp-inst)
		     )))))
    (if debug (format t "(reasonable-semantic-match-p)=>~a~%" expr))
    (eval expr)
))

(defun is-not-lateral-redundant-elaboration-p(vp-inst)
  (multiple-value-bind
      (laterally-redundant? laterally-redundant-to)
      (lateral-redundant-elaboration-p vp-inst)
    (if laterally-redundant?
	(update-pruned-vp-reason vp-inst
				 (format nil "Laterally redundant elaboration, see ~a"
					 (get-viewpoint-ordering (car laterally-redundant-to)))))
    (not laterally-redundant?)))
     
;;returns true if all nodes in candidate graph match something.
(defun candidate-graph-completely-match?(candidate-graph instance-map)
  (null (set-difference 
	 (extract-all-km-instances candidate-graph)
	 (mapcar 'cadr instance-map))))

(defun is-vertical-redundant-elaboration-p (scenario
					    candidate-graph
					    instance-map 
					    vp-inst
					    &optional (verbose nil))
  (let ((result (and
		 (superfluous-instance-map? scenario candidate-graph
					    instance-map)
		 (candidate-graph-completely-match? candidate-graph instance-map))))
    (if result
	(update-pruned-vp-reason 
	 vp-inst
	 (format nil "Vertically redundant elaboration, see ~a"
		 (get-viewpoint-ordering (car (get-viewpoint-parent vp-inst))))))
    result))

(defun superfluous-instance-map?(scenario
				 candidate-graph
				 instance-map
				 &optional(verbose nil))
  (eval 
   (cons 'and 
	 (mapcar #'(lambda(entry)
		     (let* ((candidate-instance  (first  entry))
			    (model-instance      (second entry))
			    (candidate-concept-type 
			     (quasi-get-concept-for-kb-instance 
			      candidate-instance
			      scenario))
			    (model-concept-type
			     (quasi-get-concept-for-kb-instance
			      model-instance 
			      candidate-graph)))
		       (superfluous-mapping-p model-concept-type
					      candidate-concept-type
					      verbose)))
		 instance-map))))

(defun superfluous-mapping-p(original-concept-list
			     mapped-to-concept-list
			     &optional(verbose t))
  (let ((result (eval 
		 (cons 'and 
		       (mapcar #'(lambda(original-concept)
				   (superfluous-mapping-p0 original-concept
							   mapped-to-concept-list
							   verbose))
			       (remove-subsumers original-concept-list))))))
    result))

(defun superfluous-mapping-p0 (original-concept  
			       mapped-to-concepts 
			       &optional (verbose nil))
  (let ((eval-expr (cons 'or
			 (mapcar #'(lambda(x)
				     (not (null (is-subclass-of x
								original-concept))))
				 (remove-subsumers mapped-to-concepts)))))
    (eval eval-expr)))

(defun reasonable-viewpoint-p(vp-inst)
  (cond ((equal *bps-mode* 'forward-chaining) t) ;; no pruning.
	(t (reasonable-viewpoint-p-aux vp-inst)) ;; otherwise do pruning.
))

(defun reasonable-viewpoint-p-aux(vp-inst)
  (let ((candidate-concept   (first (get-mapped-concepts vp-inst)))
	(candidate-root      (get-viewpoint-model-root vp-inst))
	(candidate-graph     (strip-triple-prefix (get-viewpoint-model vp-inst)))
	(scenario            (strip-triple-prefix (get-viewpoint-scenario vp-inst)))
	(instance-map        (unformat-viewpoint-correspondence (get-viewpoint-correspondence vp-inst)))
	(match-result        (unformat-triple-map (get-viewpoint-triple-map vp-inst))))
    (or (root-viewpointp vp-inst)
	(alt-root-viewpointp vp-inst)
	(reasonable-semantic-match-p
	 candidate-concept 
	 candidate-root 
	 candidate-graph 
	 scenario 
	 match-result 
	 instance-map
	 vp-inst))
))

(defun get-viewpoint-targets-for-viewpoints-in-closedlist()
  (mapcar #'(lambda(vp-inst)
	      (list vp-inst (get-viewpoint-target vp-inst)))
	  (set-difference *closedlist* *ignoredlist*)))

(defun get-viewpoints-in-closedlist-with-same-mapping-concepts(vp-inst)
  (let ((mapped-concepts (get-viewpoint-target vp-inst)))
    (remove vp-inst
	    (remove nil
		    (mapcar #'(lambda(x)
				(let ((x-vp (car x))
				      (x-mapped-concepts (cadr x)))
				  (if (and (= (length x-mapped-concepts)
					      (length mapped-concepts))
					   (set-equal mapped-concepts x-mapped-concepts))
				      x-vp)))
			    (get-viewpoint-targets-for-viewpoints-in-closedlist))))))

(defun lateral-redundant-elaboration-p(target-vp)
  (let* ((target-vp-naively-uncloned-simplified-context-triples 
	  (unclone-triple-list-naively
	   (get-simplified-context-for-viewpoint target-vp))))
    (let ((result (remove nil
			  (mapcar #'(lambda(vp-inst)
				      (if (triple-list-equal 
					   target-vp-naively-uncloned-simplified-context-triples 
					   (unclone-triple-list-naively
					    (get-simplified-context-for-viewpoint vp-inst)))
					  vp-inst))
				  (get-viewpoints-in-closedlist-with-same-mapping-concepts target-vp)))))
      (values (not (null result)) result))))

;; (defun reasonable-mapping-for-instance-of-query-list(candidate-root
;; 						     scenario
;; 						     instance-map
;; 						     vp-inst)
;;   (let ((vp-query-list (extract-instance-of-vp-query (get-viewpoint-query vp-inst))))
;;     (or (null vp-query-list)
;; 	(eval (cons 'or
;; 		    (mapcar #'(lambda(vp-query-entry)
;; 				(reasonable-mapping-for-instance-of-query candidate-root
;; 									  scenario
;; 									  instance-map
;; 									  vp-query-entry))
;; 			    vp-query-list))))))

;; (defun reasonable-mapping-for-instance-of-query(candidate-root
;; 						scenario
;; 						instance-map
;; 						vp-query-entry)
;;   (cons 'or 
;; 	(mapcar #'(lambda(instance-map-entry)
;; 		    (reasonable-mapping-for-instance-of-query0 candidate-root
;; 							       instance-map-entry
;; 							       vp-query-entry))
;; 		instance-map)))

;; (defun reasonable-mapping-for-instance-of-query0(candidate-root
;; 						 instance-map-entry
;; 						 vp-query-entry)
;;   (multiple-value-bind 
;;       (type query-triple query-head query-relation query-tail)
;;       (parse-vp-query-entry vp-query-entry)
;;     (and (not (null (member query-head instance-map-entry)))
;; 	 (not (null (member candidate-root instance-map-entry))))))




