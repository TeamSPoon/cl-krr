;;
;; $Id: concept-selector.lisp,v 1.94 2009/06/06 19:16:03 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Routine to setup the necessary variables for perform-kb-match0 routine.
(defun setup-perform-kb-match(unmatched-scenario-triples previously-matched-scenario-triples candidate-concept)
    (multiple-value-bind
	(candidate-root candidate-graph)
	(ps-get-new-graph candidate-concept)
      (multiple-value-bind 
	  (cloned-unmatched-scenario-triples cloned-previously-matched-scenario-triples orig->clone clone->orig)
	  (clone-both-matched-and-unmatched-triples unmatched-scenario-triples previously-matched-scenario-triples)
	(values candidate-root 
		(order-km-triple-lst candidate-graph)
		(order-km-triple-lst cloned-unmatched-scenario-triples )
		(order-km-triple-lst cloned-previously-matched-scenario-triples )
		orig->clone
		clone->orig))))

;;Performs flexible semantic matching and necessary pre/post processing for semantic matcher.
;;Finds a single match configurations
(defun perform-kb-match (scenario-triples candidate-concept &optional(strength 'weak))
  (let ((result (perform-kb-match0 (order-km-triple-lst scenario-triples)
				   nil
				   candidate-concept 
				   strength)))
    (if (null result)
	(multiple-value-bind 
	    (candidate-root candidate-graph 
             unmatched-scenario-triples matched-scenario-triples 
	     orig->clone clone->orig)
	    (setup-perform-kb-match (order-km-triple-lst scenario-triples)
				    nil
				    candidate-concept)
	  (let* ((full-scenario unmatched-scenario-triples))
	    (values 
	     (list 
	      (generate-concept-selector-result candidate-concept
						candidate-root
						full-scenario
						nil ; orig->clone
						nil ; model-graph
						candidate-graph ;assumption-triples
						nil ;instance-map-list
						nil ;triple-map-list
						0   ;taxonomic-score
						nil ; add-valid-abl-match-detail 
						nil ; add-invalid-abl-match-detail
						))
	     nil)))
      (values result t))))

(defun get-triple-lists-for-kb-match-result(triple-map-list lhs-triple-lst rhs-triple-lst)
  (let* ((lhs-matched-triple-lst (determine-matched-scenario-triples 
				  triple-map-list lhs-triple-lst))
	 (rhs-matched-triple-lst (determine-matched-candidate-triples 
				  triple-map-list rhs-triple-lst))
	 (lhs-unmatched-triple-lst (set-difference lhs-triple-lst lhs-matched-triple-lst :test 'ps-triple-equal))
	 (rhs-unmatched-triple-lst (set-difference rhs-triple-lst rhs-matched-triple-lst :test 'ps-triple-equal)))
    (values 
     (quasi-properly-terminate-triple-list lhs-matched-triple-lst   lhs-triple-lst)
     (quasi-properly-terminate-triple-list lhs-unmatched-triple-lst lhs-triple-lst)
     (quasi-properly-terminate-triple-list rhs-matched-triple-lst   rhs-triple-lst)
     (quasi-properly-terminate-triple-list rhs-unmatched-triple-lst rhs-triple-lst))))

;;Performs flexible semantic matching and necessary pre/post processing for semantic matcher.
;;Finds all disjoint match configurations for a concept
(defun perform-kb-match0 (input-unmatched-scenario-triples
			  input-matched-scenario-triples
			  candidate-concept
			  &optional(strength 'weak))
  (multiple-value-bind 
   (candidate-root candidate-graph 
    unmatched-scenario-triples matched-scenario-triples 
    orig->clone clone->orig)
   (setup-perform-kb-match input-unmatched-scenario-triples input-matched-scenario-triples candidate-concept)
   (multiple-value-bind
    (regular-match-config add-valid-abl-match-detail add-invalid-abl-match-detail)
    (ps-perform-semantic-match
     (ps-get-subgraph-for-matching unmatched-scenario-triples)
     (ps-get-subgraph-for-matching candidate-graph))
    (let ((triple-map-list   (nth 0 regular-match-config))
	  (instance-map-list (nth 1 regular-match-config))
	  (taxonomic-score   (nth 2 regular-match-config)))
      (if (not (null regular-match-config))
	  (multiple-value-bind
	   (new-scenario-matched scenario-unmatched concept-matched concept-unmatched)
	   (get-triple-lists-for-kb-match-result triple-map-list unmatched-scenario-triples candidate-graph)
	   (let* ((full-scenario (append matched-scenario-triples unmatched-scenario-triples))
		  (all-unmatched-scenario-triples
		   (replace-elements-in-list scenario-unmatched clone->orig))
		  (all-matched-triples	(replace-elements-in-list
					 (append new-scenario-matched
						 matched-scenario-triples)
					 clone->orig)))
	     (multiple-value-bind
	      (model-graph assumption-triples)
	      (determine-model-graph candidate-root   ;; having eq-set triples will choke (ps-get-path ...)
				     (ps-get-subgraph-for-matching
				      candidate-graph)
				     instance-map-list)
	      (cons  (generate-concept-selector-result candidate-concept
						       candidate-root
						       full-scenario
						       orig->clone
						       model-graph
						       assumption-triples
						       instance-map-list
						       triple-map-list
						       taxonomic-score
						       add-valid-abl-match-detail 
						       add-invalid-abl-match-detail
						       )
		     (perform-kb-match0     all-unmatched-scenario-triples
					    all-matched-triples
					    candidate-concept
					    strength))))))))))

(defun determine-model-graph-triples-for-mapped-instances(candidate-root 
							  candidate-graph 
							  instance-map-list)
  (remove-duplicate-triples
   (quasi-properly-terminate-triple-list
    (remove-reflexive-triples-from-triple-list
     (mappend #'(lambda(instance-map)
		  (determine-model-graph-triples-for-mapped-instances0 
		   candidate-root 
		   candidate-graph 
		   instance-map))
	      instance-map-list))
    candidate-graph)))

(defun determine-model-graph-triples-for-mapped-instances0(candidate-root original-candidate-graph instance-map)
  (let ((debug nil)
	(scenario-instance (car  instance-map))
	(model-instance    (cadr instance-map)))
    (let ((result (ps-get-path candidate-root model-instance
			       (append original-candidate-graph
				       (invert-triple-list original-candidate-graph)))))
      (if debug (format t "~a returning ~a~%" instance-map result))
      result)))

;;Installs reflexive triples and its inverses.
;;e.g., include both (X abuts Y) and (Y abuts X) since abuts and its inverse are the same.
;;Necessary to work-around semantic-matcher bug.
(defun install-reflexive-triples (triple-lst)
  (remove-duplicates
   (mappend #'(lambda(triple)
		(if (equal (triple-relation triple) (invert-slot (triple-relation triple)))
		    (list triple (invert-triple triple))
		  (list triple)))
	    triple-lst)
   :test 'equal))

;;Main piece of code to interface with pzyeh's semantic matcher.
(defun ps-perform-semantic-match-without-using-ablation
  (input-lhs-graph 
   input-rhs-graph 
   &key
   (match-type? 'weak) 
   (depth 2)
   (discard-unit-matches? nil) 
   (only-consider-strongest? t)
   (matcher-type 'flexible-semantic-pzyeh)
   (apply-transforms? t)
   )
  (let* ((lhs-graph (install-reflexive-triples input-lhs-graph))
	 (rhs-graph (install-reflexive-triples input-rhs-graph))
	 (match-result 
	  (ps-perform-semantic-match-without-using-ablation0
	   lhs-graph 
	   rhs-graph
	   :match-type? match-type?
	   :depth depth
	   :discard-unit-matches? discard-unit-matches?
	   :only-consider-strongest? only-consider-strongest?
	   :matcher-type matcher-type
	   :apply-transforms? apply-transforms?))
	 (triple-map-listing (convert-match-result-to-triples match-result)))
    (values 
     triple-map-listing
     (generate-correspondence-pairs match-result)
     (ps-compute-taxonomic-match-score triple-map-listing)
     (mapcar 'caar triple-map-listing)
     (mapcar #'(lambda(entry)
		 (cadr (car entry)))
	     triple-map-listing)
       )))

;;FIXME: API changed. Need to fix other calls, e.g., viewpoint-yn.lisp and debugger-why.lisp
(defun ps-perform-semantic-match-without-using-ablation0
  (graph 
   target-graph 
   &key
   (match-type? 'weak) 
   (depth 2)
   (discard-unit-matches? nil) 
   (only-consider-strongest? t)
   (matcher-type 'flexible-semantic-pzyeh)
   (apply-transforms? t)
   )
  (let ((match-degenerates? t))
    (reset-semantic-matcher)
    (if (not (degenerate-encoding-p target-graph))
	(let* ((pzyeh-scenario-graph    (ps-convert-to-semantic-matcher-form graph))
	       (pzyeh-candidate-graph   (ps-convert-to-semantic-matcher-form target-graph))
	       (kb-match-output         
		(kb-match 
		 (remove-irrelevant-triples-for-matching pzyeh-scenario-graph)
		 (remove-irrelevant-triples-for-matching pzyeh-candidate-graph)
		 depth match-type? 
		 discard-unit-matches? 
		 only-consider-strongest? 
		 match-degenerates?
		 :matcher-type matcher-type
		 :apply-transforms? apply-transforms?))
	       (validated-kb-match-output (validate-pzyeh-match-result-list kb-match-output))
	       (match-result              (pick-strongest-match validated-kb-match-output)))
	  (hash-match-result            kb-match-output)  ;;Necessary to compute node->km mappings.
	  (filter-inconsistent-mappings kb-match-output)  ;;Why? 
	  match-result))))

(defun validate-pzyeh-match-result-list(match-result-list)
  (remove nil
	  (mapcar #'(lambda(match-result)
		      (if (sane-pzyeh-match-result-p match-result) match-result))
		  match-result-list)))

;;Need a way to sanitise matches
;; - All mappings in the tail should have corresponding ones, 1 to 1.
;; - For everything else, a strict string match.
(defun sane-pzyeh-match-result-p(match-result)
  (eval
   (cons 'and 
	 (mapcar #'(lambda (pzyeh-match-result-mapping)
		     (sane-pzyeh-match-result-mapping-p pzyeh-match-result-mapping))
		 match-result))))

(defun sane-pzyeh-match-result-mapping-p(pzyeh-match-result-mapping)
  (let* ((get-mapping-source pzyeh-match-result-mapping)
	   (get-mapping-target pzyeh-match-result-mapping)
	   (node-bindings   (get-node-bindings-for-mapping pzyeh-match-result-mapping))
	   (triple-head-map (car  node-bindings))
	   (triple-tail-map (cadr node-bindings)))
      (and (valid-pzyeh-node-binding-p triple-head-map)
	   (valid-pzyeh-node-binding-p triple-tail-map))))

(defun parse-pzyeh-match-result(match-result)
  (let ((triple-map      (convert-match-result-to-triples  match-result))	
	(instance-map    (generate-correspondence-pairs    match-result))
	(taxonomic-score (ps-compute-taxonomic-match-score match-result)))
    (if (sane-pzyeh-match-result-p match-result)
	(values triple-map instance-map taxonomic-score))))

(defun valid-pzyeh-node-binding-p(input &optional(verbose t))
  (and (listp input)
       (or (is-pzyeh-node-binding-p input)
	   (let ((lhs (car  input))
		 (rhs (cadr input)))
	     (if verbose (format verbose "(valid-pzyeh-binding-p) test on non-pzyeh-node-binding. (equal ~s ~s)~%" lhs rhs))
	     (and (consp lhs)
		  (consp rhs)
		  (equal lhs rhs))))))

;;Checks to see if input is of the form 
;;((|Chemical-Formula| . "Node1") (|Chemical-Formula| . "Node2"))
(defun is-pzyeh-node-binding-p (input)
  (and (listp input)
       (let ((lhs (car  input))
	     (rhs (cadr input)))
	 (and (consp lhs)
	      (consp rhs)
	      (stringp (cdr lhs))
	      (stringp (cdr rhs))))))

;;Clones both unmatched and matched triple list using same clone-map
(defun clone-both-matched-and-unmatched-triples(unmatched-triple-list matched-triple-list)
  (let* ((full-scenario (append unmatched-triple-list matched-triple-list)))
    (multiple-value-bind
	(cloned-triple-list orig->clone clone->orig)
	(ps-clone-triple-list full-scenario)
      (values (replace-elements-in-list unmatched-triple-list orig->clone)
	      (replace-elements-in-list matched-triple-list   orig->clone)
	      orig->clone
	      clone->orig))))

;;Returns candidate graph triples that matches
;;ideally, it will return subgraph for participating nodes.
(defun determine-model-graph(candidate-root
			     candidate-graph
			     instance-map-list)
  (let ((model-graph (quasi-properly-terminate-triple-list 
		      (determine-model-graph-triples-for-mapped-instances candidate-root 
									  candidate-graph 
									  instance-map-list)
		      candidate-graph)))
    (values model-graph (set-difference candidate-graph model-graph :test 'ps-triple-equal))))

;;Returns candidate graph triples that matches
;;ideally, it will return subgraph for participating nodes.
(defun determine-matched-candidate-triples(triple-map-list 
					   candidate-triple-lst)
  (quasi-properly-terminate-triple-list
   (mappend #'(lambda(entry) 
	       (let ((matched-triples (car entry))
		     (matched-score   (cdr entry)))
		 (if (atom (car (nth 1 matched-triples)))
		     (list (nth 1 matched-triples)) ;; no transformation.
		     (nth 1 matched-triples))))     ;; transformation applied. multiple triples.
	   triple-map-list)
   candidate-triple-lst))

;;Returns scenario graph triples that matches
;;ideally, it will return subgraph for participating nodes.
(defun determine-matched-scenario-triples(triple-map-list
					  full-triple-lst)
  (quasi-properly-terminate-triple-list
   (mappend #'(lambda(entry) 
	       (let ((matched-triples (car entry))
		     (matched-score   (cdr entry)))
		 (if (atom (car (nth 0 matched-triples)))
		     (list (nth 0 matched-triples))
		   (nth 0 matched-triples))))
	   triple-map-list)
   full-triple-lst))

;;Gets unmatched scenario triples
(defun determine-unmatched-scenario-triples(scenario-triples matched-triples)
  (let ((unmatched-non-instance-triples (set-difference 
					 (extract-non-instance-triples scenario-triples)
					 (extract-non-instance-triples matched-triples)
					 :test 'ps-triple-equal)))
    (append (extract-instance-triple-for-instance 
	     (extract-all-instances-from-triple-list unmatched-non-instance-triples)
	     scenario-triples)
	    unmatched-non-instance-triples)))

;;What is the purpose of hashing these match mappings via the hash-match-mappings routine?
(defun hash-match-result(kb-match-output)
  (dolist (mapping kb-match-output)
    (hash-match-mappings mapping
			 *htable-mappings-for-g1* 
			 *htable-mappings-for-g2*)))

;;Removes inconsistent mappings.
(defun filter-inconsistent-mappings(kb-match-output)
  (remove nil
	  (mapcar #'(lambda(mapping)
		      (if (not (check-for-inconsistent-mappings mapping 
								*htable-mappings-for-g1* 
								*htable-mappings-for-g2*))
			  mapping))
		  kb-match-output)))

(defun pick-strongest-match(match-result)
  (car (last (sort match-result 'kb-match<)))) ;;sorted ascendingly. Weakest to strongest

(defun kb-match<(match-result1 match-result2)
  (let ((qty1 (length match-result1))
	(qty2 (length match-result2))
	(quality1 (eval (cons '+ (mapcar #'(lambda(x) (abs (cdr x))) match-result1))))
	(quality2 (eval (cons '+ (mapcar #'(lambda(x) (abs (cdr x))) match-result2)))))
    (or (< qty1 qty2)
	(and (= qty1 qty2)
	     (< quality1 quality2)))))

;;Computes taxonomic match score
(defun ps-compute-taxonomic-match-score(match-result &optional(verbose nil))
  (if (not (null match-result))
      (let* ((match-count      (length match-result))
	     (taxonomic-score  (ps-aggregate-taxonomic-match-score match-result match-count)))
	(if verbose (format t "ps-compute-taxonomic-match-score: match-count(~a), taxonomic-score(~a)~%"
			    match-count
			    taxonomic-score))
	taxonomic-score)))

(defun ps-aggregate-taxonomic-match-score (match n)
  (let ((score 0))
    (dolist (mapping match)
      (setf score (+ score (abs (get-mapping-score mapping)))))
    (if (and (numberp n)
	     (not (zerop n)))
	(/ score n)
      0)))

;;Computes simple match score
(defun ps-compute-simple-match-score(scenario-graph instance-map)  
  (if (and (numberp (length instance-map))
	   (numberp (length (extract-all-instances-from-triple-list scenario-graph)))
	   (not (zerop (length (extract-all-instances-from-triple-list scenario-graph)))))
      (/ (length instance-map)
	 (length (extract-all-instances-from-triple-list scenario-graph)))
    0))

;;Selects all matching concepts that matches a given scenario
(defun get-all-selectable-concepts(scenario-triples				  
				   candidate-concept-list
				   &optional (strength 'weak)
				             (verbose nil))
  (multiple-value-bind
    (selected-concept concept-selector-result unexamined-concepts)
    (concept-selector scenario-triples
		      candidate-concept-list 
		      strength
		      verbose)
    (cond ((null unexamined-concepts) concept-selector-result)
	  (t (append concept-selector-result
		     (get-all-selectable-concepts scenario-triples
						  unexamined-concepts
						  strength
						  verbose))))))

;;Selects a matching concept that matches a given scenario
(defun concept-selector(scenario-triples 
			candidate-concept-list
			&optional (strength 'weak)
			          (verbose nil))
  (progn 
    (if (atom candidate-concept-list) (setf candidate-concept-list (list candidate-concept-list)))
    (if verbose (format t "BPS: Concept Selector is considering ~a candidate(s)~%" (length candidate-concept-list)))
    ;(cache-all-uncached-concepts candidate-concept-list)
    (concept-selector0 scenario-triples
		       candidate-concept-list
		       strength
		       verbose)))

(defun concept-selector0(scenario-triples 
			 candidate-concept-list
			 &optional (strength 'weak)
			           (verbose nil))
  (cond ((or (null candidate-concept-list)
	     (null scenario-triples))
	 (format t "~a#" #\Backspace))
	(t 
	 (let* ((candidate-concept (first candidate-concept-list))
		(is-concept-selectable-result  (is-concept-selectable 
						scenario-triples
						candidate-concept
						strength
						verbose)))
	   (format t ".")
	   (cond (is-concept-selectable-result
		  (progn 
		    (values candidate-concept 
			    is-concept-selectable-result
			    (cdr candidate-concept-list))))
		 (t (concept-selector0 scenario-triples
				       (cdr candidate-concept-list)
				       strength
				       verbose)))))))

;;Tests if a concept is selectable. If so, how does scenario and concept graph match up?
(defun is-concept-selectable(scenario-triples 
			     candidate-concept
			     &optional (strength 'weak)
			               (verbose nil))
  (let ((timeout 60))
    (sys:with-timeout (timeout
		       (progn 
			 (format t "BPS: Ignoring ~a, kb-match timeout'ed after ~a seconds~%" 
				 candidate-concept timeout)
			 nil))
		      (is-concept-selectable0 scenario-triples
					      candidate-concept
					      strength
					      verbose))))

;;Tests if a concept is selectable. If so, how does scenario and concept graph match up?
(defun is-concept-selectable0(scenario-triples
			     candidate-concept
			     &optional (strength 'weak)
			               (verbose nil))
  (progn
    (reset-semantic-matcher)
    (multiple-value-bind
	(result matched?)
	(perform-kb-match scenario-triples
			  candidate-concept
			  strength)
      (cond ((member *bps-mode* '(forward-chaining))
	     result)
	    (matched? result)))))

(defun reset-semantic-matcher()
  (let ((debug nil)
	(rulebase (if *use-heuristic-rulebase*
		      (append
		       *domain-neutral-transformations*
		       *heuristic-transformations*)
		    *domain-neutral-transformations*)))
    (if debug (format t "BPS: Resetting semantic matcher.~%"))
    (setq *semantic-match-constraints* nil) ;; <- Semantic matcher specific variable
    (clrhash *km-to-node-mappings*)
    (clrhash *node-to-km-mappings*)
    (clrhash *transformation-hash*)
    (hash-transformation-rules rulebase)
    (if debug 
	(if *use-heuristic-rulebase*
	    (format t "BPS: ~a transformation rules in use. Neutral + Heuristic rules apply.~%"
		    (hash-table-count *transformation-hash*))
	    (format t "BPS: ~a transformation rules in use. Only neutral rules apply.~%"
		    (hash-table-count *transformation-hash*))))
))

;;Generates the match-result
(defun generate-concept-selector-result(candidate-concept 
					candidate-root
					scenario-triples 
					orig->clone
					model-graph
					assumption-triples
					instance-map
					triple-map
					taxonomic-score
					add-valid-abl-match-detail 
					add-invalid-abl-match-detail
					)
  (let ((concept-graph (append model-graph assumption-triples)))
    (list candidate-concept 
	  candidate-root
	  scenario-triples
	  orig->clone
	  (quasi-properly-terminate-triple-list model-graph concept-graph)
	  (quasi-properly-terminate-triple-list assumption-triples concept-graph)
	  instance-map
	  triple-map
	  (ps-compute-simple-match-score scenario-triples instance-map)
	  taxonomic-score
	  add-valid-abl-match-detail 
	  add-invalid-abl-match-detail)))

;;Parses the result element of concept-selector output pair
(defun parse-concept-selector-result(result)
  (let ((target             (nth 0 result))
	(target-root        (nth 1 result))
	(scenario-graph     (nth 2 result))
	(orig->clone        (include-additional-km-bindings-for-map 
			     (nth 3 result)))
	(candidate-graph    (nth 4 result))
	(assumption-triples (nth 5 result))
	(instance-map       (nth 6 result))
	(triple-map         (nth 7 result))
	(simple-score       (nth 8 result))
	(taxonomic-score    (nth 9 result))
	(add-valid-abl-match-detail   (nth 10 result))
	(add-invalid-abl-match-detail (nth 11 result))
	)
    (values target target-root scenario-graph orig->clone candidate-graph assumption-triples instance-map triple-map simple-score taxonomic-score add-valid-abl-match-detail add-invalid-abl-match-detail)))

;;each entry is (X . Y)
(defun include-additional-km-bindings-for-map(map)  
  (mappend #'(lambda(entry)
	       (let* ((lhs (car entry))
		      (rhs (if (atom (cdr entry)) (cdr entry) (cadr entry))))
		 (cross-product (list (remove nil (list lhs (get-binding lhs)))
				      (remove nil (list rhs (get-binding rhs)))))))
	   map))

(defun convert-match-result-to-triples(match-result)
  (mapcar 'convert-mapping-to-triples
	  match-result))

(defun update-instance-map (instance-map bindings)
  (replace-elements-in-list instance-map (invert-map bindings)))

(defun update-triple-map (triple-map bindings)
  (replace-elements-in-list triple-map (invert-map bindings)))

(defun get-all-node-bindings(mapping)
  (cond ((null mapping) ())
	((atom mapping) ())
	((node-binding-p mapping)
	 (list mapping))
	(t (append (get-all-node-bindings (car mapping))
		   (get-all-node-bindings (cdr mapping))))))
	 
(defun node-binding-p(input)
  (and (stringp (cdr input))
       (not     (null (search "Node" (cdr input))))
       (zerop   (search "Node" (cdr input)))))

(defun convert-mapping-to-triples(mapping)
  (convert-mapping-to-triples0 mapping
			      (get-all-node-bindings mapping)))

(defun convert-mapping-to-triples0(mapping node-bindings)
  (cond ((null node-bindings) mapping)
	(t (let ((binding (car node-bindings)))
	     (convert-mapping-to-triples0 (replace-if #'(lambda(x) (equal x binding)) 
						      (ps-get-node-to-km-mapping binding *node-to-km-mappings*)
						      mapping)
					  (cdr node-bindings))))))

(defun assoc-instance-map(instance instance-map)
  (let (result)
    (dolist (mapping instance-map)
      (let ((lhs (car mapping))
	    (rhs (if (atom (cdr mapping)) (cdr mapping) (cadr mapping))))
      (cond ((equal instance lhs)
	     (progn (setf result rhs) (return)))
	    ((equal instance rhs)
	     (progn (setf result lhs) (return))))))
    result))

#|
;;PICK-SIMILAR-CONCEPT-FOR-ROOT routine
;;General algorithm 
1) Progressively find concepts in KB that matches up in some way.
  1.1) If match involves root, then consider these as plausible concepts to narrow search.
      1.1.1) If exactly 1 match and is directly with root. Then return match as result.
      1.1.2) If plausible concepts are nil or do not pan out, continue with unexamined concepts.
  1.2) Match did not involve root, so continue with unexamined concepts.

Notes
a) If we need to find another result, then remove previous results from target-concepts
b) Efficiency can be improved for finding other results by keeping track of previous futile target-concepts
|#
(defun pick-similar-concept-for-root(scenario 
				     root 
				     target-concepts 
				     &optional(strength 'strong)
				              (verbose t))
  (multiple-value-bind 
    (target result-list unexamined-target-concepts)
    (concept-selector scenario target-concepts strength verbose) ;;Find anything in KB that matches up in some way
    (let ((plausible-results (extract-corresponding-concept-from-match-result-list result-list root)))
      (if (or (null target-concepts)
	      (null result-list))
	  (if verbose (format t "BPS: Nothing similar was found.~%"))
	  (cond 
	   ;;Nothing matched up between root and target concept. Thus no plausible results.
	   ;;No plausible results, let continue trying the unexamined concepts
	   ((null plausible-results) 
	    (pick-similar-concept-for-root scenario root unexamined-target-concepts strength verbose))
	   ;;Found an isolated match was between target concept and root concept. 
	   ((and (= (length plausible-results) 1)
		 (equal target (car plausible-results)))
	    (progn (if verbose (format t "BPS: Found something!~%"))
		   (values (first result-list) unexamined-target-concepts)))
	   ;;Plausible results, let's investigate these!
	   (t
	    (progn
	      (if (atom plausible-results) (setf plausible-results (list plausible-results)))
	      (let ((reordered-target-concepts (promote-elements plausible-results unexamined-target-concepts)))
		(if (and verbose (not (equal reordered-target-concepts unexamined-target-concepts)))
		    (format t "BPS: Reordering concept list to lead with ~a.~%" plausible-results))
		(pick-similar-concept-for-root scenario 
					       root 
					       reordered-target-concepts
					       strength 
					       verbose)))))))))

(defun pick-all-similar-concepts-for-root(scenario root target-concepts &optional(strength 'strong) (verbose nil))
  (cond ((null target-concepts) ())
	(t (multiple-value-bind
	     (match-result unexamined-concepts)
	     (pick-similar-concept-for-root scenario root target-concepts strength verbose)
	     (if match-result
		 (cons match-result
		       (pick-all-similar-concepts-for-root scenario root unexamined-concepts strength verbose))
  	         (pick-all-similar-concepts-for-root scenario root unexamined-concepts strength verbose))))))

(defun pick-similar-concept-for-root-having-simple-score(scenario root target-concepts threshold 
							 &optional 
							 (strength 'strong) 
							 (verbose nil))
  (if target-concepts
      (multiple-value-bind
	(result unexamined-concepts)
	(pick-similar-concept-for-root scenario root target-concepts strength verbose)
	(if result
	    (multiple-value-bind
	      (target target-root scenario-graph scenario-clone-map candidate-graph
	       assumption-triples instance-map triple-map simple-score taxonomic-score)
	      (parse-concept-selector-result result)
	      (cond ((< simple-score threshold)
		     (progn
		       (if verbose (format t "BPS: Discarding as simple-score ~a < ~a.~%" simple-score threshold))
		       (pick-similar-concept-for-root-having-simple-score scenario root unexamined-concepts threshold strength verbose)))
		    (t (values result unexamined-concepts))))))))	     

;;Given some triples and an instance. Go into KB and return match that is most similar.
(defun pick-most-similar-concept-for-root(scenario root target-concepts 
						   &optional (strength 'strong) 
						             (verbose nil))
  (let ((candidates (pick-all-similar-concepts-for-root scenario root target-concepts strength verbose)))
    (first (sort-similar-concepts-by-strength candidates))))

;;Given some triples and an instance. Go into KB and return concept that is most similar.
(defun find-strongly-similar-concept(scenario root)
  (let* ((existing-concept (car (get-concept-for-kb-instance root)))
	 (target-concepts (get-all-subclasses existing-concept)))
    (multiple-value-bind
      (result unexamined-concepts)      	
      (pick-most-similar-concept-for-root scenario 
					  root
					  target-concepts 
					  'weak)
      (multiple-value-bind 
	(target target-root scenario-graph scenario-clone-map candidate-graph 
	 assumption-triples instance-map triple-map simple-score taxonomic-score)
	(parse-concept-selector-result result)
	(if target (format t "BPS: ~a strongly similar to ~a.~%" root target))
	target))))

(defun sort-similar-concepts-by-strength(candidate-list)
  (sort candidate-list #'(lambda (result1 result2)
			   (multiple-value-bind
			     (target1 target1-root scenario-graph1 scenario-clone-map1 candidate-graph1 
			      assumption-triples1 instance-map1 triple-map1 simple-score1 taxonomic-score1)
			     (parse-concept-selector-result result1)
			     (multiple-value-bind
			       (target2 target2-root scenario-graph2 scenario-clone-map2 candidate-graph2 
				assumption-triples2 instance-map2 triple-map2 simple-score2 taxonomic-score2)
			       (parse-concept-selector-result result2)
			       (cond ((= simple-score1 simple-score2)
				      (> taxonomic-score1 taxonomic-score2))
				     (t (> simple-score1 simple-score2))))))))

(defun extract-corresponding-concept-from-match-result-list(result-list root)
  (remove-duplicates
   (mappend #'(lambda(result)
		(extract-corresponding-concept-from-match-result result root))
	    result-list)))

(defun extract-corresponding-concept-from-match-result(result root)
  ;;Some portion of candidate graph matched up in some way with scenario
  ;;Let's see if a mapping for root exists. Let these be plausible results.
  (progn 
    (multiple-value-bind
      (target target-root scenario-graph scenario-clone-map candidate-graph 
       assumption-triples candidate-map triple-map simple-score taxonomic-score)
      (parse-concept-selector-result result)
      (let* ((scenario-root (cdr (assoc root scenario-clone-map)))
	     (candidate-root (cdr (assoc scenario-root candidate-map)))
	     (result (triple-tail (car (aggregate-similar-relations
				       (extract-instance-triple-for-instance 
					candidate-root
					candidate-graph))))))
	(if (and result (atom result)) 
	    (list result)
	  result)))))

(defun remove-invalid-triple-map-entries(triple-map-list instance-map-list)
  (remove nil
	  (mapcar #'(lambda(triple-map-entry)
		      (if (not (invalid-triple-map-entry-p triple-map-entry instance-map-list))
			  triple-map-entry))
		  triple-map-list)))

(defun invalid-triple-map-entry-p(triple-map instance-map-list &optional(verbose nil))
  (let ((predicate
	 (cons 'or 
	       (mapcar #'(lambda(x)
			   (null (member x (flatten instance-map-list))))
		       (extract-all-km-instances (car triple-map))))))
    (format verbose  "invalid-triple-map-entry-p predicate : ~A~%" predicate)
    (eval predicate)))
   
(defun get-specialized-triples-with-skolem-target-instance-mappings(specialized-graph target-graph instance-map-list)
  (let ((skolem-target-instance-map-list (determine-instance-mappings-with-skolem-targets target-graph instance-map-list)))
    (cond ((null skolem-target-instance-map-list) ())
	  (t (let* ((target-skolem-instance-mapping (car skolem-target-instance-map-list))
		    (source-instance (car target-skolem-instance-mapping)))
	       (append (extract-non-instance-triples (extract-all-forward-pointing-triples-for-instance source-instance specialized-graph))
		       (get-specialized-triples-with-skolem-target-instance-mappings 
		        specialized-graph
			target-graph
			(cdr skolem-target-instance-map-list))))))))

;;Identify instance mappings whose rhs, i.e., target is a skolem in target-graph.
(defun determine-instance-mappings-with-skolem-targets(target-graph instance-map-list)
  (remove nil
	  (mapcar #'(lambda(mapping)
		      (let* ((target-instance (cadr mapping)))
			(if (all-instance-triple-p 
			     (extract-all-forward-pointing-triples-for-instance target-instance target-graph))
			    mapping)))
		  instance-map-list)))

;;Return triple-map-list entries with scores of -1 or 1 only.
(defun get-perfect-match-in-triple-map-list(triple-map-list)
  (remove nil
	  (mapcar #'(lambda(triple-map)
		      (if (= (abs (cdr triple-map)) 1)
			  triple-map))
		  triple-map-list)))

;;Return triple-map-list entries with scores of -1 or 1 only.
(defun get-perfect-match-in-idiot-kb-match-result(triple-map-list instance-map-list)
  (let* ((new-triple-map-list (get-perfect-match-in-triple-map-list triple-map-list))
	 (all-instances       (get-all-instances-in-triple-map-list new-triple-map-list)))
    (values new-triple-map-list
	    (remove nil (mapcar #'(lambda(instance-map)
				    (let ((lhs (car instance-map))
					  (rhs (cadr instance-map)))
				      (if (and (member lhs all-instances)
						(member rhs all-instances))
					  instance-map)))
				instance-map-list)))))


#|
Examples

;;Returns binding in for instance in instance-map
(ASSOC-INSTANCE-MAP '|_Chemical1| '((|_Chemical1| |_H2O-Substance14506_c27|) 
				    (|_H2o| |_Chemical-Entity14505_c27|)))
=> |_H2O-Substance14506_c27|

match-result example
(length '((((((Substance . Node1647790) has-basic-structural-unit (H2O . Node1647791))
	     ((Ionic-Compound-Substance . Node1647795) has-basic-structural-unit (Ionic-Compound . Node1647792)))
	    . 5/12))))

;;PICK-SIMILAR-CONCEPT-FOR-ROOT example
;; (let* ((scenario (make-triple-proper 
;; 		  '((|_Chemical1| |instance-of| |Chemical|)
;; 		    (|_Chemical-Entity| |instance-of| |H2|)
;; 		    (|_Chemical-Entity| |is-basic-structural-unit-of| |_Chemical1|))))
;;        (question  '|_Chemical1|)
;;        (target-concepts (get-target-concepts scenario question))
;;        (kb (get-kb)))
;;   (let ((*prototype-classification-enabled* nil)   ;disable KM prototype classification for non-root viewpoints.
;; 	(*classification-enabled*           nil))  ;disable KM classification for non-root viewpoints.
;;     (ps-assert-triples scenario))
;;   (multiple-value-bind
;;     (match-result unexamined-concepts)
;;     (pick-all-similar-concepts-for-root scenario question target-concepts 'strong)
;;     (pprint unexamined-concepts)
;;     (pprint match-result)
;;     (put-kb kb)))

 Example #1
;;CONCEPT-SELECTOR example
(let* ((triples    '((|_Substance| |instance-of| |Substance|) 
		     (_|H2O| |instance-of| |H2O|)
		     (|_Substance| |has-basic-structural-unit| |_H2O|)))
       (candidates '(|Substance| |H2O-Substance|))
       (result      (concept-selector triples
				      candidates)))
  result)


(let ((*scenario* '((|_Move771| |instance-of| |Move|)
		    (|_Move771| |object| |_Tangible-Entity773|)
		    (|_Tangible-Entity773| |instance-of| |Tangible-Entity|))))
  (trace concept-selector)
  (concept-selector *scenario* '(|Duration-Value| |Move| |Length-Value| |Locomotion|))
  (untrace concept-selector))

 Example #2
;;Same concept, multi-configuration match example
(mapcar #'(lambda(x)
	    (multiple-value-bind
	      (target target-root scenario-graph scenario-clone-map candidate-graph instance-map triple-map simple-score taxonomic-score)
	      (parse-concept-selector-result x)
	      triple-map))
	(PERFORM-KB-MATCH 
	 '((|_Move1| |instance-of| |Move|) 
	   (|_Move1| |distance| |_Distance-Value|)
	   (|_Move1| |duration| |_Duration-Value1|) 
	   (|_Move1| |velocity| |_Velocity-Value-shared|)
	   (|_Move2| |instance-of| |Move|) 
	   (|_Move2| |velocity| |_Velocity-Value-shared|)
	   (|_Move2| |duration| |_Duration-Value2|) (|_Distance-Value| |instance-of| |Distance-Value|)
	   (|_Duration-Value1| |instance-of| |Duration-Value|) (|_Duration-Value2| |instance-of| |Duration-Value|)
	   (|_Velocity-Value-shared| |instance-of| |Velocity-Value|) (|_Duration-Value1| |value| ((:|pair| 2 |*second|)))
	   (|_Duration-Value2| |value| ((:|pair| 4 |*second|))) (|_Distance-Value| |value| ((:|pair| 10 |*meter|))))
	 '|Motion-with-constant-velocity|
	 'WEAK))
=>
((((|_Move2| |duration| |_Duration-Value2|) (|_My-Move772| |duration| |_Duration-Value779|) 3/4)
  ((|_Move2| |velocity| |_Velocity-Value-shared|) (|_My-Move772| |velocity| |_Velocity-Value778|) 3/4))
 (((|_Move1| |duration| |_Duration-Value1|) (|_My-Move772| |duration| |_Duration-Value779|) 3/4)
  ((|_Move1| |velocity| |_Velocity-Value-shared|) (|_My-Move772| |velocity| |_Velocity-Value778|) 3/4)))

|#

