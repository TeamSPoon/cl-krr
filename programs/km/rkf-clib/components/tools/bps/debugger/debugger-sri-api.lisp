;;
;; $Id: debugger-sri-api.lisp,v 1.32 2008/08/18 22:33:33 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Fix, remove as it is deprecated?
(defun sri-whynot-wrapper(concept-name)
  (if (is-match-for-viewpointp concept-name (find-matches-for-viewpoint *CURRENT-VP-INST*))
      (progn
	(pick-command (list concept-name))
	(list t (car (ps-km-query `(|the| |viewpoint-answer-page| |of| ,*CURRENT-VP-INST*)))))
    (list nil 
	  (let ((stream (make-string-output-stream)))
	      (if (not (known-frame concept-name))
		  (format stream "Error : ~a is not a valid concept." concept-name)
		(parse-whynot-output stream
				     (compute-whynot-match-reasons (get-simplified-context-for-viewpoint *CURRENT-VP-INST*)
								   concept-name)))
	      (get-output-stream-string stream)))))

;;Fix
(defun sri-get-equation-expressions-for-current-viewpoint ()
  (let ((*LOGGING* t))
    (reset-eq-solver)
    (multiple-value-bind
	(eq-expr-list eq-symbol-table symbol-value-mapping)
	(collate-equation-system)
      eq-expr-list)))

;;Fix
(defun sri-get-equation-symbol-bindings-for-current-viewpoint ()
 (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
    (multiple-value-bind
	(eq-expr-list eq-symbol-table symbol-value-mapping)
	(collate-equation-system)
      symbol-value-mapping))))

;;Fix
(defun sri-get-equation-symbols-for-current-viewpoint ()
  (let ((*LOGGING* t))
    (reset-eq-solver)
    (multiple-value-bind
	(eq-expr-list eq-symbol-table symbol-value-mapping)
	(collate-equation-system)
	 (get-symbols-from-eq-expr-list
	  (remove-duplicates (flatten eq-expr-list))))))

;;---------------------------------------------------------
;;Specialized routines operating on (get-current-viewpoint)
;;---------------------------------------------------------

;;Specialization for (sri-get-triples-for-viewpoint <vp-inst>)
(defun sri-get-triples-for-current-viewpoint()
  (sri-get-triples-for-viewpoint *CURRENT-VP-INST*))

;;Specialization for (sri-get-unmatched-triples-for-viewpoint <vp-inst>)
(defun sri-get-unmatched-triples-for-current-viewpoint(concept-name)
  (sri-get-unmatched-triples-for-viewpoint (get-current-viewpoint concept-name)))

;;---------------------------------------------------------
;;Generic routines for different viewpoints
;;---------------------------------------------------------

(defun sri-get-triples-for-concept(concept-name)
    (multiple-value-bind
	(root concept-graph)
	(ps-get-graph concept-name)
      (ps-assert-triples concept-graph)
      (values root concept-graph)))

(defun sri-get-matching-triples-for-concept(vp-inst concept-name)
  (let ((source-graph (sri-get-triples-for-viewpoint vp-inst)))
    (multiple-value-bind
	(root concept-graph)
	(sri-get-triples-for-concept concept-name)
      (multiple-value-bind
	  (triple-map-listing instance-map-listing taxonomic-score)
	  (IDIOT-KB-MATCH source-graph concept-graph)	
	(values 
	 (mapcar #'(lambda(x)
		     (car x))
		 triple-map-listing)
	 instance-map-listing
	 source-graph
	 concept-graph)))))

(defun sri-get-unmatched-triples-for-concept(concept-name)
  (multiple-value-bind
      (root concept-graph)
      (sri-get-triples-for-concept concept-name)
    (multiple-value-bind
	(triple-map instance-map)
	(sri-get-matching-triples-for-concept concept-name)
      (let ((tmp-result
	     (extract-non-instance-triples
	      (set-difference concept-graph 
			      (mapcar #'(lambda(x)
					  (cadr x))
				      triple-map) :test 'ps-triple-equal))))
	(append tmp-result
		(extract-instance-triple-for-instance 
		 (extract-all-instances-from-triple-list tmp-result)
		 concept-graph))))))

(defun sri-get-triples-for-viewpoint(vp-inst)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
   (let* ((triple-lst (get-simplified-context-for-viewpoint vp-inst))
	 (vp-query-triple-lst
	  (determine-triples-for-slot-value-query 
	   (get-viewpoint-query vp-inst)
	   triple-lst)))
     (values
      (remove-duplicates
       (append 
	vp-query-triple-lst
	triple-lst)
       :test 'ps-triple-equal)
      vp-query-triple-lst))))

(defun determine-triples-for-slot-value-query(vp-query triple-lst)
  (mappend #'(lambda(vp-query-entry)
	       (determine-triples-for-slot-value-query-entry 
		vp-query-entry 
		triple-lst))
	   vp-query))

(defun determine-triples-for-slot-value-query-entry(vp-query-entry triple-lst)
  (multiple-value-bind 
      (query-type frame relation filler)
      (parse-viewpoint-query-entry vp-query-entry)
    (if (equal query-type '|*slot-value|)
	(let ((filler (ps-slot-lookup frame relation)))
	  (cond ((null filler)
		 (cadr (generate-triple-for-root-relation frame relation triple-lst)))
		(t
		 (mappend #'(lambda(x)
			      (list (list frame relation x)))
			  filler)))))))

(defun generate-triple-for-root-relation(root relation triple-list)
  (if (and (intersection (quasi-get-concept-for-kb-instance root triple-list) (domains-of relation) :test 'is-subclass-of)
	   (not (null (car (ranges-of relation)))))
      (multiple-value-bind 
	  (filler-instance filler-instance-of-triple)
	  (ps-instantiate-concept-as-triple-list (ranges-of relation))
	(list filler-instance (list filler-instance-of-triple (list root relation filler-instance))))))

(defun parse-viewpoint-query-entry(vp-query-entry)
  (let* ((query-type (nth 1 vp-query-entry))
	 (query-triple (cdr (car (last vp-query-entry))))
	 (frame (triple-head query-triple))
	 (relation (triple-relation query-triple))
	 (filler   (triple-tail query-triple)))
    (values query-type frame relation filler)))	 

(defun sri-get-concepts-for-viewpoint(vp-inst)
  (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
  (get-viewpoint-target vp-inst))))

(defun sri-compute-whynot-match-reasons (concept-name vp-inst)
  (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
    (let* ((simplified-context (get-simplified-context-for-viewpoint vp-inst)))
      (multiple-value-bind 
	  (whynot-reasons instance-of-triples)
	  (compute-whynot-match-reasons simplified-context concept-name)
	(append (mapcar #'(lambda(reason)
			    (car reason))
			whynot-reasons)
		instance-of-triples))))))

(defun sri-get-unmatched-triples-for-viewpoint(vp-inst concept-name)
  (let ((scenario (sri-get-triples-for-viewpoint vp-inst)))
    (multiple-value-bind
	(triple-map instance-map)
	(sri-get-matching-triples-for-concept concept-name)
      (let ((tmp-result
	     (extract-non-instance-triples
	      (set-difference scenario
			      (mapcar #'(lambda(x)
					  (car x))
				      triple-map) :test 'ps-triple-equal))))
	(append tmp-result
		(extract-instance-triple-for-instance 
		 (extract-all-instances-from-triple-list tmp-result)
		 scenario))))))

;;Returns novel triples, i.e., (a) specialized triples, and (b) new triples, in descendant context triples
;;FIXME: Does not eagerly evaluate new information derived in descendant.
;;FIXME: Always return query triples as it has a different KM instance name in different viewpoints
(defun sri-get-novel-triples-in-descendant-viewpoint(ancestor-vp-inst descendant-vp-inst)
  (if (descendant-viewpoint-p ancestor-vp-inst descendant-vp-inst)
      (unclone-triple-diff
       (sri-get-triples-for-viewpoint descendant-vp-inst)
       (sri-get-triples-for-viewpoint ancestor-vp-inst))))

(defun sri-map-viewpoint-triples (source-viewpoint target-viewpoint)
  ;; returns triples in target-viewpoint that are derived from the
  ;; source-viewpoint.
  ;; source-viewpoint is an ancestor of the target-viewpoint.
  (when (descendant-viewpoint-p source-viewpoint target-viewpoint)
    ;; find each triple in the target viewpoint that matches some
    ;; naively uncloned triple in the source viewpoint
    
    (loop with target-triple-map = (mapcar (lambda (triple)
					          (cons (unclone-triple-naively triple) triple))
					      (sri-get-triples-for-viewpoint target-viewpoint))
       for triple in (sri-get-triples-for-viewpoint source-viewpoint)
       for uncloned-triple = (unclone-triple-naively triple)
       for mapping = (assoc uncloned-triple target-triple-map :test #'equal)
       when mapping
       collect (cdr mapping))))  

;;Matches graph for original CPL interpretation against concept graph
;;original CPL interpretation is stored in root viewpoint.
;;Returns a list of sri-match-configurations.
;;Sri-match-configuration is a 4-tuple
;; (setup-matched setup-unmatched concept-matched concept-unmatched)
(defun sri-match-concept-against-question (concept)
  (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
  (let* ((source-graph (sri-get-triples-for-viewpoint (root-viewpoint))))
    (sri-match-graph-against-concept source-graph concept)))))

;;Matches a graph for original CPL interpretation against concept graph
;;Returns a list of sri-match-configurations.
;;Sri-match-configuration is a 4-tuple
;; (setup-matched setup-unmatched concept-matched concept-unmatched)
(defun sri-match-graph-against-concept(source-graph concept)
  (let ((match-result-lst (perform-kb-match source-graph 
					    concept))
	(result nil))
    (delete-frames-in-triple-lst source-graph)
    (ps-assert-triples source-graph t)
    (if (null match-result-lst)
	(multiple-value-bind
	    (concept-root concept-graph)
	    (ps-get-new-graph concept)
	  (ps-assert-triples concept-graph t)
	(push (list nil
		    (remove-irrelevant-km-triples source-graph)
		    nil
		    (remove-irrelevant-km-triples concept-graph)
		    concept-root
		    concept
		    )
		  result))
      (dolist (x match-result-lst)
      (multiple-value-bind 
	  (candidate-concept candidate-root scenario-graph scenario-clone-map model-graph assumption-triples instance-map triple-map simple-score taxonomic-score)
	  (parse-concept-selector-result x)
	(ps-assert-triples (append model-graph assumption-triples) t)
	(dolist (x instance-map)
	  (regular-km-unify (cdr (assoc (car  x) (invert-map scenario-clone-map)))
			    (cadr x)))
	(let* ((candidate-graph (remove-duplicates
				 (remove-irrelevant-km-triples
				  (append model-graph assumption-triples))))
	       (setup-matched     (determine-sri-scenario-matched   triple-map scenario-graph scenario-clone-map))  
	       (setup-unmatched   (determine-sri-scenario-unmatched triple-map scenario-graph scenario-clone-map))
	       (concept-matched   (determine-sri-concept-matched    triple-map instance-map candidate-graph scenario-clone-map))
	       (concept-unmatched (determine-sri-concept-unmatched  triple-map instance-map candidate-graph scenario-clone-map)))
	  (mappend #'(lambda(property-value-instance)
		       (let ((target-setup-unmatched-property-value-triples
			      (ps-get-subgraph property-value-instance 
					       setup-unmatched)))
			 (setf setup-matched 
			       (union
				target-setup-unmatched-property-value-triples
				setup-matched
				:test 'ps-triple-equal))
			 (setf setup-unmatched 
			       (set-difference
				setup-unmatched
				target-setup-unmatched-property-value-triples
				:test 'ps-triple-equal))))
		   (remove-if-not 'property-value-instance-p 
				  (extract-all-km-instances setup-matched)))
	  (mappend #'(lambda(property-value-instance)
		       (let ((target-concept-unmatched-property-value-triples
			      (ps-get-subgraph 
			       property-value-instance 
			       (remove-if 
				#'(lambda(unmatched-concept-triple)
				    (member (triple-relation unmatched-concept-triple) 
					    '(|used-in-equation| |user-equation-expression| 
					      |equation-symbol| |equation-expression| 
					      |equation-uses|)))
				concept-unmatched))))
			 (setf concept-matched 
			       (union
				target-concept-unmatched-property-value-triples
				concept-matched
				:test 'ps-triple-equal))
			 (setf concept-unmatched 
			       (set-difference
				concept-unmatched
				target-concept-unmatched-property-value-triples
				:test 'ps-triple-equal))))
		   (remove-if-not 'property-value-instance-p 
				  (extract-all-km-instances concept-matched)))
	  (let* ((setup-nodes-matched     (extract-instance-triples setup-matched))
		 (setup-nodes-unmatched   (set-difference 
					   (extract-instance-triples setup-unmatched) 
					   setup-nodes-matched :test 'ps-triple-equal))
		 (concept-nodes-matched   (extract-instance-triples concept-matched))
		 (concept-nodes-unmatched (set-difference 
					   (extract-instance-triples concept-unmatched) 
					   concept-nodes-matched :test 'ps-triple-equal)))
	    (format t "concept-nodes-unmatched(~A): ~A~%" (length concept-nodes-unmatched) concept-nodes-unmatched)
	    (format t "concept-unmatched(~A): ~A~%" (length concept-unmatched) concept-unmatched)
	    (format t "returned concept-unmatched(~A): ~A~%"
		    (length 
		      (append (extract-non-instance-triples concept-unmatched) 
			      concept-nodes-unmatched))
		     (append (extract-non-instance-triples concept-unmatched) 
			     concept-nodes-unmatched))
	    (push (list	(append (extract-non-instance-triples setup-matched) 
				setup-nodes-matched)
			(append (extract-non-instance-triples setup-unmatched) 
				setup-nodes-unmatched)
			(append (extract-non-instance-triples concept-matched) 
				concept-nodes-matched)
			(append (extract-non-instance-triples concept-unmatched) 
				concept-nodes-unmatched)
			(find-scenario-root-in-concept-match candidate-root 
							     instance-map 
							     scenario-clone-map)
			concept
			)
		  result))))))
    result))

(defun determine-sri-scenario-matched(triple-map scenario-graph scenario-clone-map)
  (unclone-triple-list-with-clone-map
   (determine-matched-scenario-triples triple-map 
				       scenario-graph)
   scenario-clone-map))

(defun determine-sri-scenario-unmatched(triple-map scenario-graph scenario-clone-map)
  (unclone-triple-list-with-clone-map
   (quasi-properly-terminate-triple-list
    (set-difference scenario-graph 
		    (determine-matched-scenario-triples triple-map 
							scenario-graph)
		    :test 'ps-triple-equal)
    scenario-graph)
   scenario-clone-map))

(defun determine-sri-concept-matched(triple-map instance-map candidate-graph scenario-clone-map)
  (unclone-triple-list-with-clone-map
   (unclone-triple-list-with-clone-map
    (quasi-properly-terminate-triple-list
     (determine-matched-candidate-triples triple-map
					  candidate-graph)
     candidate-graph)
    (consify-pair-lst instance-map))
   scenario-clone-map))

(defun determine-sri-concept-unmatched(triple-map instance-map candidate-graph scenario-clone-map)
  (unclone-triple-list-with-clone-map
   (unclone-triple-list-with-clone-map
    (quasi-properly-terminate-triple-list
     (set-difference candidate-graph 
		     (determine-matched-candidate-triples triple-map
							  candidate-graph)
		     :test 'ps-triple-equal)
     candidate-graph)
    (consify-pair-lst instance-map))
   scenario-clone-map))

(defun find-scenario-root-in-concept-match (candidate-root instance-map scenario-clone-map)
  (cdr (assoc (cadr (assoc candidate-root 
			   (invert-map instance-map)))
	      (invert-map scenario-clone-map))))

(defun consify-pair-lst(input)
  (mapcar #'(lambda(x)
	      (cons (car x)(cadr x)))
	  input))

(defun sri-get-viewpoint-model-root(vp-inst)
  (get-viewpoint-model-root-in-asserted-triples vp-inst))

