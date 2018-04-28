;;
;; $Id: alpha-works.lisp,v 1.18 2009/08/29 21:41:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;-----------------
;;12Oct08, code for ppsm that tries to stich prototypes to improve match.
;;-----------------

(defun pretty-print-ppsm-result(result)
  (format t "~a~%" result)
)

;;Ideally, we will check the triple mappings and
;;instance mappings.
;;But (ps-stitch) does not do "join" correctly as it
;;does not attempt unification.
;;(ps-stitch) currently do "join" by append slot values.
;;Thus we (ppsm-result-equal ...) returns t if the number
;;of instance mappings are the same.
;;Ideally, we should ask the matcher to improve a match
;;instead of attempting match from scratch.
(defun ppsm-result-equal(ppsm-result1 ppsm-result2)
  (let ((debug nil))
    (if debug
	(progn
	  (pretty-print-ppsm-result ppsm-result1)
	  (pretty-print-ppsm-result ppsm-result2)))
  (let ((ppsm-same? 
	  (= (length (nth 1 ppsm-result1)) 
	     (length (nth 1 ppsm-result2)))))
    (if debug (format t "ppsm-result-equal ==> ~A~%" ppsm-same?))
    ppsm-same?
)))

;;Returns ppsm result and concept-graph. We return the concept-graph because it may have been
;;expanded by stitching together additional prototypes.
;;Ideally, we should ask the matcher to improve a match
;;instead of attempting match from scratch.
(defun my-ps-perform-semantic-match
  (src-graph concept-graph 
	     &optional (prev-ppsm-result nil) (previously-stitched-instances nil))
  (reset-semantic-matcher)
  ;(format t "previously-stitched-instances ~a ~%" previously-stitched-instances)
  (let* ((ppsm-result
	  (ps-perform-semantic-match
	   src-graph
	   concept-graph))
	 (triple-map-lst   (nth 0 ppsm-result))
	 (instance-map-lst (nth 1 ppsm-result))
	 (avg-score        (nth 2 ppsm-result))
	 (instances-for-stitching
	  (set-difference 
	   (ppsm-result-indicate-stitch-graph?
	    src-graph concept-graph 
	    triple-map-lst
	    instance-map-lst)
	   previously-stitched-instances)))
    (cond ((ppsm-result-equal ppsm-result prev-ppsm-result)
    	   (values ppsm-result concept-graph))
	  ((not (null instances-for-stitching))
	   (progn
	     (format t "Retrying ppsm after stitching concept graph with additional triples for ~a~%" instances-for-stitching)
	     (my-ps-perform-semantic-match src-graph 
					   (ps-stitch concept-graph 
						      instances-for-stitching)
					   ppsm-result
					   (union previously-stitched-instances instances-for-stitching))
	     ))
	  (t (values ppsm-result concept-graph)))))

(defun ppsm-result-indicate-stitch-graph?
  (src-graph 
   concept-graph 
   triple-map-lst
   instance-map-lst)
  (let ((result nil))
    (dolist (instance-map instance-map-lst)
      (if (ppsm-result-entry-indicate-stitch-graph?
	   src-graph concept-graph 
	   triple-map-lst
	   instance-map)
	  (push (cadr instance-map) result)))
    result))

(defun ppsm-result-entry-indicate-stitch-graph?
  (src-graph 
   concept-graph 
   triple-map-lst
   instance-map)
  (let* ((debug nil)
	 (src-instance     (car instance-map))
	 (concept-instance (cadr instance-map))
	 (mapped-src-subgraph 
	  (determine-matched-scenario-triples
	   triple-map-lst
	   src-graph))
	 (mapped-concept-subgraph
	  (determine-matched-candidate-triples
	   triple-map-lst
	   concept-graph))
	 (unmapped-src-subgraph 
	   (ps-get-subgraph
	    src-instance 
	    (set-difference
	     src-graph
	     (extract-non-instance-triples
	      mapped-src-subgraph)
	     :test 'ps-triple-equal))))
    (if debug
	(format t "src-subgraph[~a], stitch? == ~a ~%~a~%"
		src-instance
		(not (null (extract-non-instance-triples unmapped-src-subgraph)))
		unmapped-src-subgraph
		))
    (not (null (extract-non-instance-triples unmapped-src-subgraph)))
))

;; (defun testme()
;;   (multiple-value-bind
;;       (mitosis-root mitosis-graph)
;;       (ps-get-new-graph '|Mitosis|)
;;     (my-ps-perform-semantic-match
;;      *scenario* 
;;      mitosis-graph)
;; ))

(defun ps-all-nodes-matched?(triple-lst 
			     instance-map-lst)
   (= 
    (length 
     (extract-all-km-instances triple-lst))
    (length instance-map-lst)))

(defun make-is-it-true-that-questionp(triple)
  (list triple
	'(|is-it-true-that-questionp| |query-varp| |t|)))


;;moves necessary semantic triples from scenario to yn-query, to provide context for yn-query.
;;at this point, the code does not handle the case where original yn-query contain
;;non-semantic yn-query, e.g., isa, same-as, greater-than...
;;This thing triggers whenever there is one or more semantic triples in the yn-query.
(defun move-semantic-triples-from-scenario-to-yn-query(input-scenario compute-question yn-query)
  (let ((query-triples (ps-get-query-triples input-scenario compute-question yn-query)))
    (multiple-value-bind 
	(non-semantic-triples-to-verify semantic-triples-to-verify lookup-triples)
	(collate-triples-to-verify query-triples)
      (let ((triples-to-be-moved
	     (extract-non-instance-triples
	      (get-all-triples-related-to-instance-list 
	       (extract-all-km-instances semantic-triples-to-verify)
	       input-scenario))))
      (values (set-difference input-scenario
			      triples-to-be-moved
			      :test 'ps-triple-equal)
	      compute-question	      
	      (append yn-query 
		      (mapcar 'make-is-it-true-that-questionp		      
			      triples-to-be-moved)))))))

;;returns set of '|Be-Related| instances
(defun get-Be-Related-instances(scenario)
  (let ((result ()))
    (dolist (x (extract-instance-triples scenario))
      (if (equal (triple-tail x) '|Be-Related|)
	  (push (triple-head x) result)))
    result
))

(defun remove-triple-containing-instance(triple-lst instance)
  (let ((result ()))
    (cond ((null instance) triple-lst) ;;nothing to remove
	  ((atom instance)             ;;remove triples containing instance atom
	   (dolist (triple triple-lst)
	     (if (not (member instance triple))
		 (push triple result)))
	   result)
	  (t (remove-triple-containing-instance 
	      (remove-triple-containing-instance triple-lst 
						 (car instance))
	      (cdr instance))))))

;;includes additional ((_X related-to_Y) (is-it-true-that-questionp t)) yn triples.
(defun rewrite-cpl-for-find-relationship(scenario compute-question yn-query)
  (let ((additional-yn-triple ())
	(find-rel-instances (get-Be-Related-instances scenario)))
    (dolist (rel-head find-rel-instances)
      (let ((rel-pair-lst (all-pairs 
			   (quasi-ps-slot-lookup rel-head '|object| scenario))))
	(dolist (rel-pair rel-pair-lst)
	  (if (not (equal (car rel-pair) (cadr rel-pair)))
	      (push (list (car rel-pair) '|related-to| (cadr rel-pair))
		    additional-yn-triple)))))
    (values 
     (remove-triple-containing-instance scenario find-rel-instances)
	    (set-difference compute-question find-rel-instances)
	    (append yn-query 
		    (mapcar 'make-is-it-true-that-questionp
			    (remove-duplicates
			     additional-yn-triple
			     :test 'ps-triple-equal))))
	))

(defun identify-skolem-query-instances-in-scenario 
  (scenario ps-query-triples)
  (let ((km-instances-in-queries
	 (remove-duplicates 
	  (extract-all-km-instances 
	   (flatten ps-query-triples)))))
    (multiple-value-bind
	(skolem-frames non-skolem-frames)
	(ps-get-frames-for-triples scenario)
      (mappend #'(lambda(x)
		   (sieve-triple-list-having-head 
		    x
		    (extract-instance-triples scenario)))
       (mapcar 'car skolem-frames)))))

;for each viewpoint, we look at the instances in the query triples.
;Any query instance that is a skolem, we will install additional AE triples
;to seed the scenario graph for semantic matching.
(defun install-ae-triples-for-skolem-query-instances-in-scenario
  (scenario ps-query-triples
	    &key(assert? t))
  (let ((km-instances-in-queries
	 (remove-duplicates 
	  (extract-all-km-instances 
	   (flatten ps-query-triples)))))
    (multiple-value-bind
	(skolem-frames non-skolem-frames)
	(ps-get-frames-for-triples scenario)
      (let ((new-scenario
	     (ps-stitch 
	      scenario
	      (intersection 
	       (mapcar 'car skolem-frames)
	       km-instances-in-queries))))
	(if assert? (ps-assert-triples new-scenario))
	new-scenario
	))))

;;--------------------------------------------------------
;;03Nov08, code to install AE triples for skolem instances
;;inside both yn-query and compute-question
;;This is to seed graph for BPS elaboration
;;--------------------------------------------------------
;; (defun install-ae-triples-for-skolem-instances-in-yn-query(input-scenario 
;; 							   compute-question 
;; 							   yn-query)
;; 	(scenario 
;; 	 (remove-irrelevant-km-triples input-scenario))
;; 	(km-instances-in-queries
;; 	 (remove-duplicates 
;; 	  (extract-all-km-instances 
;; 	   (flatten (append compute-question yn-query))))))
;;     (multiple-value-bind
;; 	(skolem-frames non-skolem-frames)
;; 	(ps-get-frames-for-triples scenario)
;;       (values
;;        (ps-stitch scenario 
;; 		  (intersection (mapcar 'car skolem-frames)
;; 				km-instances-in-queries))
;;        compute-question
;;        yn-query))))

;; (defun testme()
;;   (let ((scenario
;; 	 '((|_Adenine2062_c0| |instance-of| |Adenine|)
;; 	   (|_Cytosine2061_c0| |instance-of| |Cytosine|)))
;; 	(compute-question nil)
;; 	(yn-question
;; 	 '(((|_Adenine2062_c0| |complement| |_Cytosine2061_c0|)
;; 	    (|is-it-true-that-questionp| |query-varp| |t|)))))
;;     (reset-problem-solver)
;;     (search-problem-solver
;;      scenario
;;      (ps-get-query-triples 
;;       scenario
;;       compute-question
;;       yn-question)
;;      yn-question)))

;; (defun testme()
;;   (let ((scenario
;; 	 '((|_Adenine| |instance-of| |Adenine|)
;; 	   (|_Thymine| |instance-of| |Thymine|)))
;; 	(compute-question nil)
;; 	(yn-question
;; 	 '(((|_Adenine| |complement| |_Thymine|)
;; 	    (|is-it-true-that-questionp| |query-varp| |t|)))))
;;     (reset-problem-solver)
;;     (search-problem-solver
;;      scenario
;;      (ps-get-query-triples 
;;       scenario
;;       compute-question
;;       yn-question)
;;      yn-question)))

;; (defun testme()
;;   (ps-attempt-question 
;;    "Is it true that adenine is the complement of thymine?"))

;; 1) tweak ps-tree to pretty-print goal-viewpoint predicates.
;; 2) Turn on heuristic-rules globally.
;; 3) Create test-case for flexibly matching (X related-to Y) for (X r Y) and (X r1 M r2 Y)


