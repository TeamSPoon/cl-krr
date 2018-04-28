;;
;; $Id: viewpoint-matcher.lisp,v 1.85 2008/12/21 20:17:03 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;----------------------------------------------------------------------
;; Relations to always lookup during viewpoint matching.
;;----------------------------------------------------------------------
(defvar *vp-match-default-lookups* 
  '(
    |component| |component-of| |equation| |equation-uses| 
    |equation-expression| |equation-symbol|
))

;;----------------------------------------------------------------------
;; Relations we want to ignore during matching. This applies mainly to
;; equations.
;;----------------------------------------------------------------------
(defvar *triples-to-ignore-for-matching* '(|equation-symbol| |equation-uses| |equation-expression| |equation|))

(defun viewpoint-match (scenario-graph 
			candidate-concepts 
			cpl-question-list
			viewpoint-parent 
			&optional (extraction-depth 3))
  (let* ((result 
	  (mapcar #'(lambda (concept-selector-result)
		      (let ((vp-defn 
			     (generate-slot-value-viewpoint concept-selector-result 
							    viewpoint-parent 
							    cpl-question-list)))
			vp-defn))
		  (get-all-selectable-concepts scenario-graph 
					       candidate-concepts
					       'weak))))
    (format t "BPS: Found ~a candidate(s)~%" (length result))
    (remove nil
	    (sort-viewpoint-candidate-list 
	     result))))

(defun generate-slot-value-viewpoint(concept-selector-result 
				     viewpoint-parent 
				     cpl-question-list)
  (multiple-value-bind 
      (candidate-concept candidate-root scenario-graph orig->clone model-graph 
       assumption-triples instance-map triple-map simple-score taxonomic-score
       add-valid-abl-match-detail add-invalid-abl-match-detail)
      (parse-concept-selector-result concept-selector-result)
    (let* ((query-matched t)
	   (updated-cpl-question-list (update-cpl-question-list cpl-question-list 
								orig->clone))
	   (key-val-pair 	    
	    (list 
	     `(|viewpoint-concept|        (,CANDIDATE-CONCEPT))
	     `(|viewpoint-source|         (,(cons ':|set| (extract-all-instances-from-triple-list scenario-graph))))
		  `(|viewpoint-scenario|       (,(cons ':|set| (affix-triple-prefix 
								(deaggregate-instance-types scenario-graph)))))
		  `(|viewpoint-query|          (,(cons ':|seq| updated-cpl-question-list)))
		  `(|viewpoint-filter|         ,(ps-slot-lookup viewpoint-parent '|viewpoint-filter|))
		  `(|viewpoint-target|         (,(cons ':|seq| (remove-duplicates
								(cons CANDIDATE-CONCEPT 
								      (cdr (car (ps-slot-lookup viewpoint-parent '|viewpoint-target|))))))))
		  `(|viewpoint-model-root|     (,candidate-root))
		  `(|viewpoint-model-graph|    ,(affix-triple-prefix model-graph))
		  `(|viewpoint-assumption-triples| ,(affix-triple-prefix assumption-triples))
		  `(|viewpoint-query-matched|  (,query-matched))
		  `(|viewpoint-parent|         (,viewpoint-parent))
		  `(|viewpoint-triple-map|     ,(format-triple-map triple-map))
		  `(|viewpoint-correspondence| ,(affix-pair-prefix 
						 (make-list-elements-into-string
						  instance-map)))
		  `(|viewpoint-correspondence-for-most-general-ablation|
		    ,(affix-pair-prefix 
		      (make-list-elements-into-string
		       (cadr add-valid-abl-match-detail))))
		  `(|viewpoint-correspondence-for-ablation-at-depth-0| 
		    ,(affix-pair-prefix 
		      (make-list-elements-into-string
		       (cadr add-invalid-abl-match-detail))))
;; 	          `(|viewpoint-correspondence-with-parent-viewpoint| 
;; 		    ,(affix-pair-prefix 
;; 		      (make-list-elements-into-string (mapcar #'(lambda(x) (list (car x)(cdr x))) orig->clone))))
		  
		  )))
      (multiple-value-bind
	  (temp-vp-inst asserted-vp-defn)
	  (assert-km-instance '|Slot-Value-Viewpoint| key-val-pair)
	(push asserted-vp-defn *viewpoint-defn-lst*)
	(ps-score-viewpoint-instance temp-vp-inst
				     simple-score
				     taxonomic-score
				     (ps-compute-coverage-score (get-scenario-for-root-viewpoint)
								(GET-SIMPLIFIED-CONTEXT-FOR-VIEWPOINT temp-vp-inst))
				     (ps-compute-tax-dist-for-matched-concept candidate-concept candidate-root scenario-graph instance-map)
				     (length assumption-triples))))))

(defun get-viewpoint-root-instance-count()
  (length (get-all-instances-in-root-viewpoint)))

(defun get-scenario-for-root-viewpoint()
  (get-simplified-context-for-viewpoint (root-viewpoint)))

(defun ps-update-triple-list(triple-list)
  triple-list)

#|
(defun ps-update-triple-list(triple-list)
  (progn 
    (init-get-specialized-triples-for-instance)
    (properly-terminate-triple-list
     (remove-duplicates (append
			 triple-list 
			 (mappend #'(lambda(triple)
				      (ps-update-triple triple))
				  (extract-non-instance-triples triple-list)))
			:test 'equal))))
|#

(defun ps-update-triple(triple &optional(verbose t))
  (let ((frame    (triple-head triple))
	(relation (triple-relation triple))
	(filler   (triple-tail triple)))
    (let ((actual-filler (ps-km-query `(|the| ,relation |of| ,frame) verbose)))
      (if (not (consp (car actual-filler)))
	  (mappend 'get-specialized-triples-for-instance0
		   actual-filler)
	  (list (list frame relation (car actual-filler)))))))
	      
(defun get-all-instances-in-root-viewpoint()
  (get-all-instances-in-viewpoint (get-root-viewpoint)))

(defun get-all-instances-in-viewpoint(vp-inst)
  (let ((scenario (get-simplified-context-for-viewpoint vp-inst)))
    (remove-duplicates (extract-all-instances-from-triple-list scenario))))

(defun get-instances-introduced-by-problem-solver(ancestor-vp-inst descendant-vp-inst)
  (let ((ancestor-instances   (remove-duplicates (ps-unclone (get-all-instances-in-viewpoint ancestor-vp-inst))))
	(descendant-instances (remove-duplicates (ps-unclone (get-all-instances-in-viewpoint descendant-vp-inst)))))
    (set-difference descendant-instances ancestor-instances)))

(defun get-instances-specialized-by-problem-solver(ancestor-vp-inst descendant-vp-inst)
  (let ((ancestor-triple-lst   (get-simplified-context-for-viewpoint ancestor-vp-inst))
	(descendant-triple-lst (get-simplified-context-for-viewpoint descendant-vp-inst))
	(ancestor-instances    (remove-duplicates (ps-unclone (get-all-instances-in-viewpoint ancestor-vp-inst)))))
    (remove nil
	    (mapcar #'(lambda(instance)
			(if (is-instance-specialized instance ancestor-triple-lst descendant-triple-lst)
			    instance))
		    ancestor-instances))))
	      
(defun is-instance-specialized(instance ancestor-triple-lst descendant-triple-lst &optional(verbose nil))
  (let ((ancestor-concept-type 
	 (quasi-get-concept-for-kb-instance instance 
					    (unclone-triple-list-naively ancestor-triple-lst)))
	(descendant-concept-type 
	 (quasi-get-concept-for-kb-instance instance 
					    (unclone-triple-list-naively descendant-triple-lst))))
    (let ((result (cond ((equal ancestor-concept-type descendant-concept-type)               nil)
			((> (length descendant-concept-type) (length ancestor-concept-type)) t)
			(t (> (max-abs-tax-dist descendant-concept-type) 
			      (max-abs-tax-dist ancestor-concept-type))))))
      (if verbose (format t "specialized? ~a => ~a [~a]~%" ancestor-concept-type descendant-concept-type result))
    result)))
    
(defun max-abs-tax-dist(concept-type-list)
  (apply 'max (mapcar #'(lambda(x)
			  (abs-tax-dist '|Thing| x))
		      concept-type-list)))

(defun format-triple-map (triple-map)
  (let ((result (mapcar #'(lambda (entry)
			    (let ((mapping       (car  entry))
				  (mapping-score (cdr  entry)))
			      (let ((lhs-triple  (car  mapping))
				    (rhs-triple  (cadr mapping)))
				(list ':|pair|
				      (list ':|pair| 
					    (cons ':|triple| (stringify-triple lhs-triple))
					    (cons ':|triple| (stringify-triple rhs-triple)))
				      mapping-score))))
			triple-map)))
result))
	
(defun unformat-triple-map (triple-map)
  (mapcar #'(lambda (entry)
	      (let ((mapping       (nth 1 entry))
		    (mapping-score (nth 2 entry)))
		(let ((lhs-triple  (nth 1 mapping))
		      (rhs-triple  (nth 2 mapping)))
		  (cons 
		   (list (unstringify-triple (cdr lhs-triple))
			 (unstringify-triple (cdr rhs-triple)))
		   mapping-score))))
	  triple-map))

(defun stringify-triple-list (triple-list)
  (mapcar #'(lambda(triple)
	      (stringify-triple triple))
	  triple-list))

(defun stringify-triple(triple)
  (list (format nil "~a" (triple-head     triple))
	(format nil "~a" (triple-relation triple))
	(format nil "~a" (triple-tail     triple))))

(defun unstringify-triple(triple)
  (list (intern (nth 0 triple) :km)
	(intern (nth 1 triple) :km)
	(intern (nth 2 triple) :km)))

(defun assert-km-instance (concept key-val-pair)
  (let* ((instance (ps-instantiate-concept concept))
	 (km-expr  (append (list instance '|has|)
			   key-val-pair))
	 (start    (get-internal-real-time)))
    (let ((result (car (ps-km-query km-expr))))
      ;(format t "BPS: Asserting (~a has ...) (~a ms)~%" instance (- (get-internal-real-time) start))
      (values result km-expr))))

(defun generate-correspondence-pairs(match-result)
  (let (g1-km-node g2-km-node)
    (mapcar #'(lambda(graph-node-pair)
		(setf g1-km-node (gethash (first graph-node-pair) *node-to-km-mappings*))
		(setf g2-km-node (gethash (second graph-node-pair) *node-to-km-mappings*))
		(list G1-KM-NODE G2-KM-NODE))
	    (get-all-node-bindings-for-mappings match-result))))

;;----------------------------------------------------------------------
;; DESC: Determine whether the query (i.e. the question being asked)
;;	 was matched.
;; INPUT: query-triples = a list of triples.
;;	  mappings = The match under consideration.
;; OUTPUT: T if all the triples in query-triples show up in mappings.
;;	   NIL otherwise.
;;----------------------------------------------------------------------
(defun query-triples-match-p (query-triples mappings)
  (let ((match-p t) src-triples)
    ;; 1) Get all the source triples!
    (dolist (mapping mappings)
      (cond
        ((path-p (get-mapping-source mapping)) (setf src-triples (append (get-mapping-source mapping) src-triples)))
        (t                                     (setf src-triples (cons   (get-mapping-source mapping) src-triples)))))
    ;; 2) Check whether all the query triples are matched!
    (dolist (query-triple query-triples)
      (if (not (member query-triple src-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y))))))
          (progn (setf match-p nil) (return))))
    match-p))

;;----------------------------------------------------------------------
;; DESC: Given a CPL query, find all triples in the scenario that 
;;	 correspond to it.
;; INPUT: cpl-query = a list of the form:
;;	    	(:pair <QUERY-TYPE>
;;		       (:triple <KM Instance> <relation> *))
;;	  scenario = a list of triples.
;; OUTPUT: A list of triples that match the query.
;;----------------------------------------------------------------------
(defun find-query-triples (cpl-query scenario)
  (let ((query-head (triple-head     (rest (third cpl-query))))
        (query-reln (triple-relation (rest (third cpl-query))))
        head tail reln query-triples)
    (dolist (triple scenario)
      (setf head (triple-head triple))
      (setf tail (triple-tail triple))
      (setf reln (triple-relation triple))
      (if (or (and (eql query-head head) (eql query-reln reln))
              (and (eql query-head tail) (eql query-reln (invert-slot reln))))
          (setf query-triples (cons triple query-triples))))
    query-triples))

(defun remove-irrelevant-triples-for-matching (triples &optional (triples-to-ignore *triples-to-ignore-for-matching*))
  (let (new-triples)
    (dolist (triple triples)
      (if (not (member (triple-relation triple) triples-to-ignore :test #'(lambda (x y) (or (eql x y) (eql x (invert-slot y))))))
	  (setf new-triples (cons triple new-triples))))
    new-triples))

;;----------------------------------------------------------------------
;; DESC: Given a list of slots (i.e. relations) include all their 
;;	 subslots. We use this information to gather a concept to 
;;	 match against a scenario.
;; INPUT: slots = a list of slots. 
;; OUTPUT: A list containing the original list of slots plus additional
;;	subslots.
;;----------------------------------------------------------------------
(defun include-subslots (slot-list)
  (remove-duplicates 
   (append slot-list
	   (mappend #'(lambda(slot)
			(ps-km-query `(|the| |subslots| |of| ,slot)))
		    slot-list))
   :test #'equal))

;;----------------------------------------------------------------------
;; DESC: Given a query (i.e. the question being answered by the problem
;;	 solver), update it to reflect cloning.
;; INPUT: cpl-question = The query that the problem solver is trying
;;		to answer.
;;	  cloned-bindings = The bindings between the new instances
;;		and the old ones they map to.
;; OUTPUT: An updated query.
;;----------------------------------------------------------------------

(defun update-cpl-question-list (cpl-question-list cloned-bindings)
  (mapcar #'(lambda (cpl-question)
	      (update-cpl-question cpl-question cloned-bindings))
	  cpl-question-list))

(defun update-cpl-question (cpl-question cloned-bindings)
  (replace-elements-in-list cpl-question cloned-bindings))

(defun degenerate-encoding-p (cpl-graph)
  (and (eql (length cpl-graph) 1) (eql (triple-relation (first cpl-graph)) '|instance-of|)))

