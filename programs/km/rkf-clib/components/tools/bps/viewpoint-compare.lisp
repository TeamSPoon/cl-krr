;;
;; $Id: viewpoint-compare.lisp,v 1.31 2008/10/14 22:16:03 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;----------------------------------------------------------------------
;; DESC: This function interfaces with the controller to answer a 
;;	 comparison type question.
;; INPUT: cpl-scenario = A list of triples. We use this information to 
;;		extract the concepts being compared.
;;	  cpl-question = The KM instance being queried.
;;	  comparison-dimensions (optional) = The dimension along with
;;		to compare against. This is a list of the slot groups
;;		-- e.g. Meronymic-Relation, Causal-Relation, etc..
;;	  extraction-depth (optional) = The depth to compare to. 
;; OUTPUT: A viewpoint frame containing the result of the comparison.
;;	   See output-view-match-result for more info.
;;----------------------------------------------------------------------
(defun viewpoint-compare (cpl-scenario 
			  cpl-question 
			  &optional 
			  (comparison-dimensions '(|Spatial-Relation| |Participant-Relation| |Meronymic-Relation| |Causal-Relation|))
			  (extraction-depth 3))
  (multiple-value-bind
    (comparison-type x-class y-class)
    (parse-comparison-question cpl-scenario (car cpl-question))
    (let* ((*on-error* 'continue)     ;hack.To workaround potential bug in semantic-matcher/CLib.
	   (relations-to-query 
	    (append 
	     '(|solubility| |electrolyte-status| |conductivity|)
	     (mappend #'(lambda(comparison-dimension)
			  (ps-km-query `(|the| |instances| |of| ,comparison-dimension)))
		      comparison-dimensions))))
      (let ((x+y+mappings+g1+g2-unmatched (compare-x-and-y x-class y-class relations-to-query extraction-depth)))
	(output-view-match-result cpl-scenario 
				  (first  x+y+mappings+g1+g2-unmatched)
				  (second x+y+mappings+g1+g2-unmatched)
				  (third  x+y+mappings+g1+g2-unmatched)
				  (fourth x+y+mappings+g1+g2-unmatched)
				  (fifth  x+y+mappings+g1+g2-unmatched))))))

;;Extracts a triple, (<Comparison-Type> <Concept1> <Concept2>)
;;depicting comparison requirements.
(defun parse-comparison-question(cpl-scenario cpl-question)
  (let* ((comparison-type   (car    (get-concept-for-kb-instance cpl-question)))
	 (instance1         (first  (ps-slot-lookup cpl-question '|object|)))
	 (instance2         (second (ps-slot-lookup cpl-question '|object|))))
    (format t "BPS: ~a comparison between ~a and ~a.~%" comparison-type instance1 instance2)
    (let ((concept1 (find-strongly-similar-concept (sieve-triple-list-not-having-instance instance2 cpl-scenario)
						   instance1))
	  (concept2 (find-strongly-similar-concept (sieve-triple-list-not-having-instance instance1 cpl-scenario)
						   instance2)))
      (if (null concept1) (setf concept1 (car (get-concept-for-kb-instance instance1))))  ;;stopgap measure, FIXME for handling "What is the similarity between Move and Reaction?"
      (if (null concept2) (setf concept2 (car (get-concept-for-kb-instance instance2))))  ;;stopgap measure
      (values comparison-type
	      concept1
	      concept2))))

;;----------------------------------------------------------------------
;; DESC: Given a cpl-scenario, this function will determine whether
;;	 the scenario is asking a comparison type question.
;; INPUT: cpl-scenario = A list of triples making up the scenario.
;; OUTPUT: T if the question is asking a comparison question. NIL
;;	   otherwise.
;;----------------------------------------------------------------------
(defun is-comparison-question-p (cpl-scenario) 
  (let (result)
    (dolist (cpl-triple cpl-scenario)
      (if (and (eql (triple-relation cpl-triple) '|instance-of|) 
	       (member (triple-tail cpl-triple) '(|Be-Similar| |Be-Different|) :test #'eql))
	  (progn (setf result t) (return))))
    result))

;;----------------------------------------------------------------------
;; DESC: This function is a wrapper around the matcher to answer 
;;	 similarity type questions. It takes the two classes X and 
;;	 Y being compared plus the dimensions to compare along, and 
;;	 returns the similarities between X and Y.
;; INPUT: x-class = One of two the KM classes being compared.
;;	  y-class = The other KM class being compared.
;;	  relations-to-compare = A list of relations to compare 
;;		x-class with y-class.
;;	  depth (optional) = How deep to make the comparison.
;; OUTPUT: An output of the form:
;;
;;		(<x-instance> <y-instance> <bindings>)
;;	   where
;;		<x-instance> = a KM instance of x-class.
;;		<y-instance> = a KM instance of y-class.
;;		<bindings> = A list of mappings between <x-instance>
;;			and <y-instance>.
;;----------------------------------------------------------------------
(defun compare-x-and-y (x-class y-class relations-to-compare &optional (depth 3))
  (let ((*allow-sibling-match* nil)
	(*slots-to-ignore-for-conversion* (cons '|clone-built-from| *slots-to-ignore-for-conversion*))
	all-mappings temp-mappings root+graph x-instance y-instance g1 g2 
	g1-prime g2-prime g1+g2-unmatched-triples min-common-ancestor final-result)
    ;; 0) Set the stages for this question type.
    (set-BPSCurStage 0)
    (set-BPSMaxStage 2)
    (set-BPSCurPosition 0)
    (set-BPSMaxPosition 0)
    ;; Turn off sibling matching.
    ;; 05/12/06 -- TEMP FIX! We'll remove this after we get an updated version of
    ;;		   the matcher to Pete!
    (if (not (member '|has-clones| *slots-to-ignore-for-conversion* :test #'equal))
	(setf *slots-to-ignore-for-conversion* (cons '|has-clones| *slots-to-ignore-for-conversion*)))
    ;; 1) Create an instance of the two things being compared, and gather information about them.
    ;;    We will prefer using information stored as prototypes because these information are 
    ;;	  user-defined (at least in AURA). 
    (set-BPSCurStage 1)
    (set-BPSCurPosition 0)
    (set-BPSMaxPosition 3)
    (cond
     ((get-vals x-class '|prototypes| :facet 'own-properties :situation *global-situation*)
      (setf root+graph (get-km-prototype-def x-class))
      (setf x-instance (first root+graph))
      (setf g1 (second root+graph)))
     (t
      (setf x-instance (ps-instantiate-concept x-class))
      (setf g1 (gather-graph x-instance relations-to-compare depth))))
    (set-BPSCurPosition 1)
    (cond
     ((get-vals y-class '|prototypes| :facet 'own-properties :situation *global-situation*)
      (setf root+graph (get-km-prototype-def y-class))
      (setf y-instance (first root+graph))
      (setf g2 (second root+graph)))
     (t	
      (setf y-instance (ps-instantiate-concept y-class))
      (setf g2 (gather-graph y-instance relations-to-compare depth))))
    (set-BPSCurPosition 2)
    ;; 2) Generalize the concepts being compared.
    (setf g1 (filter-generic-concepts g1))
    (setf g2 (filter-generic-concepts g2))
    (setf min-common-ancestor (find-min-common-ancestor x-class y-class))
    (setf g1-prime (gen-concepts-compared x-instance min-common-ancestor g1))
    (setf g2-prime (gen-concepts-compared y-instance min-common-ancestor g2))
    (set-BPSCurPosition 3)
    ;; 3) Compare X with Y.
    (set-BPSCurStage 2)
    (set-BPSCurPosition 0)
    (set-BPSMaxPosition 6)
    (clrhash *km-to-node-mappings*)
    (clrhash *node-to-km-mappings*)
    (setf g1-prime (convert-to-peters-form g1-prime))
    (setf g2-prime (convert-to-peters-form g2-prime))
    (set-BPSCurPosition 1)
    (setf all-mappings (kb-match g1-prime g2-prime 2 'weak nil t t))
    (set-BPSCurPosition 3)
    ;; 4) Get rid of any incosistent matches!
    (setf temp-mappings nil)
    (dolist (mappings all-mappings)
      (hash-match-mappings mappings *htable-mappings-for-g1* *htable-mappings-for-g2*)
      (if (not (check-for-inconsistent-mappings mappings *htable-mappings-for-g1* *htable-mappings-for-g2*))
          (setf temp-mappings (cons mappings temp-mappings))))
    (setf all-mappings temp-mappings)
    ;; 5) Find the best match!
    (cond
      ;; If there are more than one match result, then check whether any of
      ;; the match results are consistent but disjoint.
      ((> (length all-mappings) 1)
       (setf all-mappings (find-most (combine-disjoint-mappings all-mappings) :test #'> :form #'number-of-matched-triples)))
      (t
       (setf all-mappings (first all-mappings))))
    (set-BPSCurPosition 4)
    ;; 6) Remove matches that are too distant. These matches should not be returned
    ;;    to the user. 
    (setf temp-mappings nil)
    (dolist (mapping all-mappings)
      (if (not (filter-distant-matches mapping))
	  (setf temp-mappings (cons mapping temp-mappings))))
    (setf all-mappings temp-mappings)
    (set-BPSCurPosition 5)
    ;; 7) Remove the matched triples (for difference) questions. 
    (setf g1+g2-unmatched-triples (get-complement-for-graphs g1-prime g2-prime all-mappings))
    ;; 8) Convert these information into the appropriate format.
    (setf g1-prime (convert-to-petes-form (first  g1+g2-unmatched-triples)))
    (setf g2-prime (convert-to-petes-form (second g1+g2-unmatched-triples)))
    (dolist (graph-node-pair (get-all-node-bindings-for-mappings all-mappings))
      (setf final-result (cons (list (gethash (first  graph-node-pair) *node-to-km-mappings*)
				     (gethash (second graph-node-pair) *node-to-km-mappings*))
			       final-result)))
    (set-BPSCurPosition 6)
    ;; 9) Return the result.
    (list x-instance y-instance final-result g1-prime g2-prime)))

;;----------------------------------------------------------------------
;; DESC: Generalize the type of an instance to improve matching. 
;; INPUT: compared-instance = The instance whose type we want to 
;;		generalize.
;;	  genl-class = The class we want to generalize the type of
;;		compared-instance to.
;;	  triples = A list of triples containing compared-instance.
;; OUTPUT: A list of triples with the generalization.
;;----------------------------------------------------------------------
(defun gen-concepts-compared (compared-instance genl-class triples)
  (let (new-triples)
    (dolist (triple triples)
      (cond
	((and (eql (triple-relation triple) '|instance-of|) (eql (triple-head triple) compared-instance))
	 (setf new-triples (cons (list (triple-head triple) (triple-relation triple) genl-class) new-triples)))
	(t
	 (setf new-triples (cons triple new-triples)))))
    new-triples))

(defun get-complement-for-graphs (g1 g2 mappings)
  (let (g1-matched-triples g1-unmatched-triples g2-matched-triples g2-unmatched-triples)
    ;; 1) Get the matched triples for G1 and G2.
    (dolist (mapping mappings)
      ;; Get the matched triples for G1.
      (if (path-p (get-mapping-source mapping))
          (setf g1-matched-triples (append (get-mapping-source mapping) g1-matched-triples))
          (setf g1-matched-triples (cons   (get-mapping-source mapping) g1-matched-triples)))
      ;; Get the matched triples for G2.
      (if (path-p (get-mapping-target mapping))
          (setf g2-matched-triples (append (get-mapping-target mapping) g2-matched-triples))
          (setf g2-matched-triples (cons   (get-mapping-target mapping) g2-matched-triples))))
    ;; 2) Once we have all the matched triples, get all the unmatched triples. 
    (dolist (triple g1)
      (if (not (member triple g1-matched-triples :test 'ps-triple-equal))
          (setf g1-unmatched-triples (cons triple g1-unmatched-triples))))
    (dolist (triple g2)
      (if (not (member triple g2-matched-triples :test 'ps-triple-equal))
          (setf g2-unmatched-triples (cons triple g2-unmatched-triples))))
    ;; 3) Return the unmatched triples.
    (list g1-unmatched-triples g2-unmatched-triples)))

;;----------------------------------------------------------------------
;; DESC: Given two classes, find the minimum common ancestor.
;; INPUT: class1 = A KM class.
;;	  class2 = Another KM class.
;; OUTPUT: The KM class that is the minimum common ancestor of class1
;;	   and class2.
;;----------------------------------------------------------------------
(defun find-min-common-ancestor (class1 class2)
  (let ((class1-supclasses (all-superclasses class1))
	(class2-supclasses (all-superclasses class2))
	(min-tdist 1000) 
	temp-tdist common-ancestors result)
    (setf common-ancestors (intersection class1-supclasses class2-supclasses :test #'eql))
    (dolist (ancestor common-ancestors)
      (setf temp-tdist (+ (tax-dist class1 ancestor) (tax-dist class2 ancestor)))
      (cond
	((< temp-tdist min-tdist) (setf min-tdist temp-tdist) (setf result ancestor))))
    result))

;;----------------------------------------------------------------------
;; DESC: Output the result of matching in viewpoint format for 
;;	 explanation generation.
;; INPUT: vp-scenario = A list of triples making up the scenario.
;;	  source = One of the two instances being compared.
;;	  target = The other instance being compared.
;;	  bindings = A list of mappings between the two concepts.
;; OUTPUT: A viewpoint frame containing the output.
;;----------------------------------------------------------------------
(defun output-view-match-result (vp-scenario source target bindings source-unmatched target-unmatched)
  (let (compare-instance viewpoint-query)
    ;; Assemble the query.
    (dolist (triple vp-scenario)
      (cond 
	((and (eql (triple-relation triple) '|instance-of|) (eql (triple-tail triple) '|Be-Similar|))
	 (setf compare-instance (triple-head triple)) 
	 (setf viewpoint-query `(:|pair| |*match-result| (:|triple| ,compare-instance |similarity| *)))
	 (return))
	((and (eql (triple-relation triple) '|instance-of|) (eql (triple-tail triple) '|Be-Different|))
	 (setf compare-instance (triple-head triple)) 
	 (setf viewpoint-query `(:|pair| |*match-result| (:|triple| ,compare-instance |difference| *)))
	 (return))))
    ;; If the mappings are NULL, then include the source and target, so 
    ;; we'll have something at least.
    (if (null bindings) (setf bindings (list (list source target))))
    ;; Put all the results in the output format.
    (let ((result (ps-instantiate-concept '|Match-Result-Viewpoint|)))
      (if (not (null SOURCE))           (ps-km-query `(,result |now-has| (|viewpoint-source|        (,SOURCE)))))
      (if (not (null TARGET))           (ps-km-query `(,result |now-has| (|viewpoint-target|        ((:|seq| ,TARGET))))))
      (if (not (null viewpoint-query))  (ps-km-query `(,result |now-has| (|viewpoint-query|         ((:|seq| ,viewpoint-query))))))
      (if (not (null source-unmatched)) (ps-km-query `(,result |now-has| (|viewpoint-scenario|      (,(cons ':|set| (mapcar #'(lambda (a) (cons ':|triple| a)) (deaggregate-triple-list source-unmatched))))))))
      (if (not (null target-unmatched)) (ps-km-query `(,result |now-has| (|viewpoint-model-graph|   (,(cons ':|set| (mapcar #'(lambda (b) (cons ':|triple| b)) (deaggregate-triple-list target-unmatched))))))))
      (if (not (null bindings))         (ps-km-query `(,result |now-has| (|viewpoint-correspondence| ,(mapcar #'(lambda (x) (cons ':|pair| x)) bindings))) ))
      result)))

(defun filter-distant-matches (mapping &optional (maximum-taxonomic-distance-threshold 3))
  (let ((node-bindings (get-node-bindings-for-mapping mapping)) 
	tdist filter-p)
    (dolist (node-pair node-bindings)
      (setf tdist (or (tax-dist (first (first node-pair)) (first (second node-pair)))
		      (tax-dist (first (second node-pair)) (first (first node-pair)))))
      ;; Nothing fancy. Just a straight distance. If necessary, we will use something more sophisticated
      (if (and (numberp tdist)
	       (numberp maximum-taxonomic-distance-threshold)
	       (> tdist maximum-taxonomic-distance-threshold))
	  (progn (setf filter-p t) (return))))
    filter-p))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defun filter-generic-concepts (triples)
  (let (instance-triples content-triples non-generic-classes final-result)
    ;; 1) Split up the instance-of and content triples, and for each instance-of triple
    ;;    clean its class information to remove generic concepts.
    (dolist (triple triples)
      (cond
       ((equal (triple-relation triple) '|instance-of|)
	(setf non-generic-classes (filter-generic-concepts0 (triple-tail triple)))
	(if non-generic-classes 
	    (setf instance-triples 
		  (cons (list (triple-head triple) (triple-relation triple) non-generic-classes)
			instance-triples))))
       (t 
	(setf content-triples (cons triple content-triples)))))
    ;; 2) Now remove any content triples that are no longer defined.
    (dolist (triple content-triples)
      (cond
       ((and (member (triple-head triple) instance-triples :test #'(lambda (x y) (equal x (triple-head y))))
	     (member (triple-tail triple) instance-triples :test #'(lambda (x y) (equal x (triple-head y)))))
	(setf final-result (cons triple final-result)))))
    ;; 3) Tack on the instance of triples.
    (append instance-triples final-result)))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defun filter-generic-concepts0 (classes)
  (let ((top-level-concepts 
	 '(|Thing| |String| |Variable| |Entity| |Property-Group| |Class| |Role| |Event| |Number|
	   |Property-Node| |Temporal-Entity| |Structure| |System| |Big-Node| |Spatial-Entity| |Sequence| 
	   |Intangible-Entity| |Aggregate| |Tangible-Entity| |Place| |Substance| |Physical-Object| 
	   |Region| |Territory| |Chemical-Entity|  |Living-Entity| |Suborganismal-Entity| |Organ| 
	   |Tissue| |Organism| |Organelle| |Organic-Structure|))
	result)
    (dolist (c (if (atom classes) (list classes) classes))
      (cond
       ((not (member c top-level-concepts :test #'equal))
	(push c result))))
    (if (eql (length result) 1) (first result) result)))

#|

;;
;;Test cases
;;

;;Example 1 - Be-Similar between Metal-Nonmetal-Combination-Reaction and Combustion-Reaction using Tecuci's KB
;;            Will perform necessary analogical match to find most similar concept in KB for both instances.

(let ((scenario '((|_Metal| |instance-of| |Metal|)
		  (|_Non-Metal| |instance-of| |Non-Metal|)
		  (|_Reaction1| |instance-of| |Reaction|)
		  (|_Reaction1| |raw-material| |_Metal|)
		  (|_Reaction1| |raw-material| |_Non-Metal|)

		  (|_O2-Substance| |instance-of| |O2-Substance|)
		  (|_H2O-Substance| |instance-of| |H2O-Substance|)
		  (|_Reaction2| |instance-of| |Combustion-Reaction|)
		  (|_Reaction2| |raw-material| |_O2-Substance|)
		  (|_Reaction2| |result| |_H2O-Substance|)

		  (|_Be-Similar| |instance-of| |Be-Similar|)
		  (|_Be-Similar| |object| |_Reaction1|)
		  (|_Be-Similar| |object| |_Reaction2|)))
      (question '(|_Be-Similar|))
      (*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
      (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
      (kb (bps-get-kb)))
  (ps-assert-triples scenario)
  (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
  (put-kb kb))

;;Example 2 - Be-Different between Metal-Nonmetal-Combination-Reaction and Combustion-Reaction using Tecuci's KB
;;            Will perform necessary analogical match to find most similar concept in KB for both instances.

(let ((scenario '((|_Metal| |instance-of| |Metal|)
		  (|_Non-Metal| |instance-of| |Non-Metal|)
		  (|_Reaction1| |instance-of| |Reaction|)
		  (|_Reaction1| |raw-material| |_Metal|)
		  (|_Reaction1| |raw-material| |_Non-Metal|)

		  (|_O2-Substance| |instance-of| |O2-Substance|)
		  (|_H2O-Substance| |instance-of| |H2O-Substance|)
		  (|_Reaction2| |instance-of| |Combustion-Reaction|)
		  (|_Reaction2| |raw-material| |_O2-Substance|)
		  (|_Reaction2| |result| |_H2O-Substance|)

		  (|_Be-Different| |instance-of| |Be-Different|)
		  (|_Be-Different| |object| |_Reaction1|)
		  (|_Be-Different| |object| |_Reaction2|)))
      (question '(|_Be-Different|))
      (*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
      (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
      (kb (bps-get-kb)))
  (ps-assert-triples scenario)
  (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
  (put-kb kb))
|#
