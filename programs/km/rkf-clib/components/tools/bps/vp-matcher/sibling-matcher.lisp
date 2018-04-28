;;
;; $Id: sibling-matcher.lisp,v 1.2 2008/06/16 18:52:10 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Candidates for sibling match are dominant instances
(defun find-sibling-match-candidate-instances(scenario)
  (get-dominant-instances scenario))

;;Returns list of sibling concepts
(defun find-sibling-concept(concept)  
  (remove concept
	  (immediate-subclasses 
	   (car (remove-subsumers (immediate-superclasses concept))))))

;;Returns list of sibling concepts for instance in scenario
(defun find-sibling-concept-for-instance(instance scenario)
  (let ((concept (car (quasi-get-concept-for-kb-instance instance scenario))))
    (find-sibling-concept concept)))

;;Returns list of applicable sibling concepts for instance in scenario
(defun find-applicable-sibling-concept-for-instance (instance scenario)
  (let ((concept (car (quasi-get-concept-for-kb-instance instance scenario))))
    (find-applicable-sibling-concept concept)))

;;Placeholder for more discerning approach to select sibling concepts as sibling match candidate.
;;Perhaps look at degree of match, i.e. whether defn of sibling concept includes referenced slots in original instance.
;;For now, we only consider user-defined concepts as candidates for sibling match.
(defun find-applicable-sibling-concept(concept)
  (intersection
   *all-user-defined-concepts*
   (find-sibling-concept concept)))

;;Creates sibling instance that mirrors the relations for target-instance
(defun create-sibling-graph-for-instance(target-instance sibling-concept scenario)
  (let ((existing-concept      (car (quasi-get-concept-for-kb-instance target-instance scenario))))
    (cond ((member sibling-concept (find-applicable-sibling-concept existing-concept))
	   (create-sibling-graph-for-instance0 sibling-concept
					       target-instance
					       scenario)))))

(defun create-sibling-graph-for-instance0 (sibling-concept target-instance scenario)
  (let* ((target-triples      (get-target-triples-for-sibling-graph-creation target-instance scenario))
	 (sibling-instance    (ps-instantiate-concept sibling-concept))
	 (replace-map         (list (cons target-instance sibling-instance))))
    (cons (list sibling-instance '|instance-of| sibling-concept)
	  (replace-elements-in-list target-triples replace-map))))

;;Get all triples related to target-instance for sibling-graph creation.
(defun get-target-triples-for-sibling-graph-creation (target-instance scenario)
  (extract-non-instance-triples 
   (sieve-triple-list-having-head target-instance 
				  (make-triple-proper scenario))))
    
;;Uncovers all relations for instance and augments scenario
(defun make-explicit-all-relations-for-instance(target-instance scenario)
  (let ((target-instance-graph (gather-graph target-instance nil 1)))
    (union target-instance-graph scenario :test 'ps-triple-equal)))

;;Generates sibling match viewpoints
(defun create-sibling-match-viewpoint(viewpoint-parent target-instance sibling-concept)
  (let* ((query-matched t)
	 (scenario (strip-triple-prefix 
		    (make-explicit-all-relations-for-instance
		     target-instance
		     (get-simplified-context-for-viewpoint viewpoint-parent)))))
    (multiple-value-bind 
	(cloned-scenario orig->clone clone->orig)
	(ps-clone-triple-list scenario)
      (let* ((query-slot-value   (update-query-slot-value 
				  (get-viewpoint-query viewpoint-parent) 
				  clone->orig))
	     (cloned-target-instance (cdr (assoc target-instance
						 orig->clone)))
	     (sibling-model-graph (create-sibling-graph-for-instance 
				   cloned-target-instance
				   sibling-concept cloned-scenario))
	     (key-val-pair 
	      (list `(|viewpoint-source|     
		      (,(cons ':|set| (extract-all-instances-from-triple-list cloned-scenario))))
		`(|viewpoint-scenario|   
		  (,(cons ':|set| (affix-triple-prefix 
				   (deaggregate-instance-types cloned-scenario)))))
		`(|viewpoint-query|
		  (,(cons ':|seq| query-slot-value)))
		`(|viewpoint-filter|  
		  ,(ps-slot-lookup viewpoint-parent '|viewpoint-filter|))
		`(|viewpoint-target|  
		  (,(cons ':|seq| (cons SIBLING-CONCEPT
								   (cdr (car (ps-slot-lookup viewpoint-parent '|viewpoint-target|)))))))
		`(|viewpoint-model-graph|    ,(affix-triple-prefix sibling-model-graph))
		`(|viewpoint-score|          (0))
		`(|viewpoint-query-matched|  (,query-matched))
		`(|viewpoint-parent|         (,viewpoint-parent))
		`(|viewpoint-correspondence| ()))))
    (assert-km-instance '|Slot-Value-Viewpoint| key-val-pair)))))

;;Creates all applicable sibling match viewpoints for given viewpoint
(defun create-all-sibling-match-viewpoints-for-viewpoint(viewpoint-parent)
  (let* ((scenario         (get-simplified-context-for-viewpoint viewpoint-parent))
	(target-instances (find-sibling-match-candidate-instances scenario)))
    (mappend #'(lambda(instance)
		 (create-all-sibling-match-viewpoints-for-target-instance viewpoint-parent instance))
	     target-instances)))

;;Creates all applicable sibling match viewpoints for target-instance of scenario
(defun create-all-sibling-match-viewpoints-for-target-instance(viewpoint-parent target-instance)
  (let* ((scenario         (get-simplified-context-for-viewpoint viewpoint-parent))
	 (sibling-concepts (find-applicable-sibling-concept-for-instance target-instance scenario)))
    (mapcar #'(lambda(concept)
		(create-sibling-match-viewpoint viewpoint-parent target-instance concept))
	    sibling-concepts)))
