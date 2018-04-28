;;
;; $Id: ps-semantic-matcher.lisp,v 1.22 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;;====================================================================
;;;;====================================================================
;;;; FILE: apply-rules.lisp
;;;;
;;;; Dependencies: This file references functions from these files:
;;;;			- km.lisp
;;;;			- simple-match.lisp
;;;;			- check-match.lisp
;;;;			- transformation-rules.lisp
;;;;====================================================================
;;;;====================================================================

;;;=====================================================================
;;;=====================================================================
;;; Global parameters and definitions.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; The relations need not be considered for rewrites. 
;;
;; NOTE: For this list to have a real significant impact wrt efficiency
;;       this list must be kept small. Include only those relations
;;       that are non-rewriteable and have a high frequency.
;;======================================================================
(defparameter *non-rewrite-relations*
  '(
    |value| |value-of|
#|
    |troop-strength| |allegiance| |duration| |agent-attrition-rate| 
    |available-force-ratio| |default-combat-power| |combat-power-ratio| 
    |enemy-attrition-rate| |relative-combat-power| |remaining-strength| 
    |required-force-ratio| |start-time| |end-time|
|#
   ))

;;======================================================================
;; Hash table that indexes the transformations. Each transformation is
;; indexed on the antecedent.
;;======================================================================
(defvar *transformation-hash* (make-hash-table :test #'equal))

;;======================================================================
;; Hash tables to hash the graphs to make lookups of triples more
;; efficient.
;;======================================================================
(defvar *htable-g1-indexed-by-head* (make-hash-table :test #'equal))
(defvar *htable-g1-indexed-by-tail* (make-hash-table :test #'equal))
(defvar *htable-g2-indexed-by-head* (make-hash-table :test #'equal))
(defvar *htable-g2-indexed-by-tail* (make-hash-table :test #'equal))

;;======================================================================
;; Hash tables to contain the node mappings from one graph to another.
;; For example, *htable-mappings-for-g1* contains what node in g2 is
;; mapped to by the node in g1 - i.e. the g1 node is the index and
;; the g2 node is the content.
;;======================================================================
(defvar *htable-mappings-for-g1* (make-hash-table :test #'equal))
(defvar *htable-mappings-for-g2* (make-hash-table :test #'equal))

;;======================================================================
;; Hash tables to store the mappings between KM instances and graph
;; nodes for the current match.
;;
;; **NOTE**: The matcher is a consumer of these two data structures.
;;	Hence, it is defined here. The matcher, however, does not
;;	maintain these structures - i.e. it does not setup or clear 
;;	these two structures. Those applications that use the matcher 
;;	are responsible for setting up and clearing these hashes for
;;	EACH match. Other than this, they should not update these 
;;	hashes in anyway. The only function that updates these two
;;	structures is query-km-for-relation.
;;======================================================================
(defvar *km-to-node-mappings* (make-hash-table :test #'equal))
(defvar *node-to-km-mappings* (make-hash-table :test #'equal))

;;======================================================================
;; A list of pseudo KM axioms. 
;;======================================================================
(defvar *pseudo-km-axioms-for-matcher* '(
  (|plays| 	((|Entity| |Role|)))
  (|element-of|	((|Living-Entity| |Organization|)))
))

;;;=====================================================================
;;;=====================================================================
;;; Top level function to match two representations.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: This is a variant of the KB-MATCH function that handles
;;	 negated triples.
;; INPUT:  same as kb-match (see definition for kb-match).
;; OUTPUT: same as kb-match (see definition for kb-match).
;;======================================================================
(defun kb-match-neg (graph1 graph2 &optional (depth 2) (match-type? 'strong) (discard-unit-matches? nil))
  (let (g1-graph+unnegated-triples g2-graph+unnegated-triples all-match-results)
    (setf g1-graph+unnegated-triples (unnegate-graph graph1))
    (setf g2-graph+unnegated-triples (unnegate-graph graph2))
    (setf all-match-results (kb-match (first g1-graph+unnegated-triples) (first g2-graph+unnegated-triples)
			      depth match-type? discard-unit-matches?))
    (cond
      ((and (null (second g1-graph+unnegated-triples)) (null (second g2-graph+unnegated-triples)))
       all-match-results)
      ((or (second g1-graph+unnegated-triples) (second g2-graph+unnegated-triples))
       (keep-valid-negated-matches 
	 all-match-results 
	 (length graph1) 
	 (second g1-graph+unnegated-triples) 
	 (second g2-graph+unnegated-triples))))))


(defun jchaw-filter-away-imperfect-mappings (current-match)
  (let ((perfect-mappings ())
	(imperfect-mappings ()))
    (dolist (mapping current-match)
      (if (not (= (abs (cdr mapping)) 1))
	  (push mapping imperfect-mappings)
	(push mapping perfect-mappings))
      )
    (list perfect-mappings imperfect-mappings)
))

;;======================================================================
;; DESC: Given two representations, find a match between them.
;; INPUT: graph1 = a list of triples.
;;	  graph2 = a list of triples.
;;	  depth (optional) = The depth to which we want to apply 
;;		transformations.
;;	  match-type? (optional) = strong | weak. Dictates the 
;;		direction in which subsumption testing is carried
;;		out.
;;	  discard-unit-matches? (optional) = Whether or not to discard
;;		unit matches (i.e. matches where only one triple is
;;		matched).
;; OUTPUT: A list of possible matches between G1 and G2. Each element
;;	   of this list is a set of mappings between the triples in 
;;	   G1 and G2. 
;;
;; NOTE1: We require a 1-to-1 mapping between the matching triples in
;;	  graph1 and graph2. There is, however, an oversight in the 
;;	  function check-for-inconsistent-mappings (see note in the 
;;	  header of this function) which allows for triples matched 
;;	  with transformations to violate this requirement.
;;======================================================================
(defun kb-match (graph1 graph2 
		 &optional  
		 (depth 2) 
		 (match-type? 'strong)
		 (discard-unit-matches? nil)
		 (only-consider-strongest? nil)
		 (match-degenerates? nil)
		 &key 
		 (matcher-type 'flexible-semantic-pzyeh)
		 (apply-transforms? t)
		)
  (let ((debug nil)
	(semantic-match-result-lst
	 (graph-match graph1 graph2 match-type? discard-unit-matches?))
	(graph1-length (length graph1))
	current-match new-match new-matches new-xform-matches all-match-results
	current-matches 
	structural-match-result-lst 
	strongest-structural-match-result-lst
	strongest-semantic-match-result-lst
	result
	)
    (if (and (boundp '*halo2-tester-global-matcher-type*)
	     (not (null *halo2-tester-global-matcher-type*)))
	(setf matcher-type *halo2-tester-global-matcher-type*))
    (setf current-matches semantic-match-result-lst)
    (setf strongest-semantic-match-result-lst 
	  (filter-match-result semantic-match-result-lst))
    (setf structural-match-result-lst
	  (remove nil (mapcar #'jchaw-filter-away-imperfect-mappings 
			      semantic-match-result-lst)))
    (setf strongest-structural-match-result-lst
	  (mapcar #'(lambda(structural-match)
		      (assoc structural-match
			     structural-match-result-lst
			     :test 'equal))
		  (filter-match-result 
		   (mapcar 'car structural-match-result-lst))))
    ;(format t "~%strongest-structural-match-result-lst: ~A~%" strongest-structural-match-result-lst)
    ;; First thing is to hash the two graphs, so lookup of triples can
    ;; be done more efficiently.
    (hash-graph-on-head graph1 *htable-g1-indexed-by-head*)
    (hash-graph-on-tail graph1 *htable-g1-indexed-by-tail*)
    (hash-graph-on-head graph2 *htable-g2-indexed-by-head*)
    (hash-graph-on-tail graph2 *htable-g2-indexed-by-tail*)
    ;; If transformations have not been hashed, then hash them.
    ;;(if (eql (hash-table-count *transformation-hash*) 0) (hash-transformation-rules))
    ;; Consider only the strongest matches if asked to do so.
    (if only-consider-strongest? (setf current-matches (filter-match-result current-matches)))
    ;(format t "~%(matcher-type ~A)~%"		  matcher-type )
    (if (or
	 (equal matcher-type 'semantic)
	 (equal matcher-type 'flexible-semantic-pzyeh)
        )
	(progn
	  (setf current-match   (first current-matches))
	  (setf current-matches (rest  current-matches))
	)
    )
    ;; Try to apply transformations to improve the match.
    ;;
    (if (or 
	 (equal matcher-type 'structural)
	 (equal matcher-type 'flexible-semantic-jchaw)
	)
	(progn
	  ;;okay... at this point, we pick the best structural match, i.e., most number of 
	  ;;mappings with score 1.
	  ;;begin jchaw patch, to ignore imperfect mappings at the beginning.
	  (setf current-match   (car (first strongest-structural-match-result-lst)))
	  (setf current-matches (mapcar 'car (rest  strongest-structural-match-result-lst)))
	  ;(format t "current-match ~A~%" current-match)
	  ;(format t "current-match-lst ~A~%" current-matches)
	  )
      )
    
    ;; 09/15/05 -- We finally bit the bullet and implemented a mechanism to look for degenerate
    ;;		matches if there are no regular. We will look for degenerate matches if there 
    ;;		are no regular matches and we are asked to do so.
    (if (and (null current-match) match-degenerates?)
	(progn 
	  ;(format t "Matching degenerates...~%")
	  (setf current-match   (first (find-best-degenerate-match graph1 graph2)))
	  (setf current-matches (rest (find-best-degenerate-match graph1 graph2))))
      ;(format t "Not matching degenerates...~%")
    )
    (setf all-match-results (cons current-match current-matches))
    ;;end jchaw patch
    (if (or
	 (equal matcher-type 'flexible-semantic-pzyeh)
	 (equal matcher-type 'flexible-semantic-jchaw)
	)
	(progn
	  ;(format t "~%~%Improving match using transform, ~a~%~%" matcher-type)
	  ;(format t "current-match ~A~%" current-match)
	  ;(format t "current-match-lst ~A~%" current-matches)
	  ;; Try to better align each match through the use of transformations.
	  ;; try to improve match...
	  ;; in pzyeh version, it will improve the pure semantic match. 
	  ;; in jchaw version, it will improve the structural match. 
	(loop 
      (if (and (null current-match) (null current-matches)) (return))
      (cond 
	((>= (number-of-matched-triples current-match) graph1-length)
	 (setf all-match-results (cons current-match all-match-results)))
	(t
	 (hash-match-mappings current-match *htable-mappings-for-g1* *htable-mappings-for-g2*)
	 (setf new-xform-matches 
	       (match-additional-triples-with-transformations 	
		 current-match 
		 (if (eql match-type? 'strong) 'source 'source-weak)
		 depth))
	 (cond
	   (new-xform-matches
	    (dolist (new-xform-match new-xform-matches)
	      (setf new-match (append current-match new-xform-match))
	      (hash-match-mappings new-match *htable-mappings-for-g1* *htable-mappings-for-g2*)
	      (setf new-matches (find-additional-matching-triples new-match graph1 match-type?))
	      (setf current-matches (append new-matches current-matches))))
	   ((setf new-xform-matches (match-additional-triples-with-transformations current-match 
										   'target depth))
	    ;; 08/24/05 -- Added the following code to filter matches that do not add any new information 
	    ;;		   -- i.e. do not align any unmatched triples.
	    (setf new-xform-matches (mapcar #'(lambda (x) (filter-irrelevant-xfrom-matches graph1 x current-match)) new-xform-matches))
	    (setf new-xform-matches (remove nil new-xform-matches))
	    (cond
	     (new-xform-matches
	      (dolist (new-xform-match new-xform-matches)
		(setf new-match (append current-match new-xform-match))
		(hash-match-mappings new-match *htable-mappings-for-g1* *htable-mappings-for-g2*)
		(setf new-matches (find-additional-matching-triples new-match graph1 match-type?))
		(setf current-matches (append new-matches current-matches))))
	     (t
	      (setf all-match-results (cons current-match all-match-results)))))
	   (t
	    ;; This is where we will be adding code to query the KB.
	    ;; For now we will just go ahead and save the match.
	    (setf all-match-results (cons current-match all-match-results)))
	 )))
      (setf current-match   (first current-matches))
      (setf current-matches (rest  current-matches)))
	)
      )
    ;; Some post processing. If all-match-results is null, then no additional matches 
    ;; were found using transformations, so set all-match-results to current-matches.
    ;; Also, if match-degenerates? is on, then filter for degenerate matches.
    ;;
    (if (null all-match-results) (setf all-match-results current-matches))	
    (if match-degenerates? (setf all-match-results (remove-all-degenerate-mappings all-match-results)))
    ;;
    ;; **NOTE**: 06/25/04 - Added the following call to "check-matches-make-sense" to
    ;;		 check whether a match makes sense. This is in the initial trial stages.
    ;;		 08/22/05 - We use to filter the match results, but that has been removed.
    ;;		 We let the application calling the matcher decide how to filter the 
    ;;		 match result -- i.e. the application will either provide the similarity 
    ;;		 measure or use one that we provide.
    (setf result (check-matches-make-sense all-match-results graph1 graph2))
    (if (equal matcher-type 'flexible-semantic-jchaw)
	(let ((flexible-semantic-jchaw-result (car result))  ;;best xform match is the 1st one.
	      (perfect-mappings   (car  (first strongest-structural-match-result-lst)))
	      (imperfect-mappings (cadr (first strongest-structural-match-result-lst)))
	      (imperfect-mappings-to-add ()))
	;;we need to put back semantically matched triples, 
	;;i.e., those with mapping scores <1 if xforms did not 
	;; a) match these triples
	;; b) xforms have a worse match.
	  ;(format t "put back semantically matched triples...~%")
	  ;(format t "i.e., those with mapping scores <1 if xforms did not~%")
	  ;(format t " a) match these triples~%")
	  ;(format t " b) xforms have a worse match. not necessary for now. since all xform matches are scored as 1~%")
	  ;(format t "~%")
	  ;(format t "imperfect mappings to check: ~A~%" imperfect-mappings)
	  ;(format t "result: ~a~%" result)
	  (dolist (imperfect-mapping-entry imperfect-mappings) 
	    (let ((lhs (caar imperfect-mapping-entry))
		  (rhs (cadr (car imperfect-mapping-entry)))
		  (score (cdr imperfect-mapping-entry)))
	      (if (null (jchaw-is-single-mapping-in-xform-match-result?
			 imperfect-mapping-entry
			 flexible-semantic-jchaw-result))
		  (push imperfect-mapping-entry flexible-semantic-jchaw-result))
	      )
	    )
	  (setf result (list flexible-semantic-jchaw-result)) ;;always a list of matches.
	  ))
    result
))

;;make sure it is a list of pzyeh triples...
;;often times it is a single pzyeh triple,
;;but when xforms are used, it is a list.
(defun jchaw-typesafe-pzyeh-output-into-list(x)
  (if (stringp (cdr (car x)))
      (list x) 
    x))

(defun jchaw-parse-mapping(mapping)
  (let* ((lhs-pzyeh-form   (jchaw-typesafe-pzyeh-output-into-list 
			    (caar mapping)))
	 (rhs-pzyeh-form   (jchaw-typesafe-pzyeh-output-into-list 
			    (cadr (car mapping))))
	 (lhs-content-triple-lst 
	  (extract-non-instance-triples 
	   (convert-to-petes-form lhs-pzyeh-form)))
	 (rhs-content-triple-lst 
	  (extract-non-instance-triples 
	   (convert-to-petes-form rhs-pzyeh-form)))
	 (score (cdr mapping)))
    (values lhs-content-triple-lst rhs-content-triple-lst score)
))

;;checks if single-triple match is in xform-match-result
(defun jchaw-is-single-mapping-in-xform-match-result?(mapping xform-result)
  (let ((debug nil)
	(result nil))
    (multiple-value-bind 
	(lhs-content-triple-lst rhs-content-triple-lst score)
	(jchaw-parse-mapping mapping)
      (format debug "checking if (~A/~A)[~a]~% has a better xform match.~%" 
	      (car lhs-content-triple-lst)  ;;always a single triple
	      (car rhs-content-triple-lst)   ;;always a single triple.
	      score)
    (dolist (xform-result-entry xform-result)
      (multiple-value-bind 
	  (result-lhs-content-triple-lst result-rhs-content-triple-lst result-score)
	  (jchaw-parse-mapping xform-result-entry)
	(setf result 
	      (or 
	       result
	       (or 
		(member (car lhs-content-triple-lst) result-lhs-content-triple-lst
			:test 'ps-triple-equal)
		(member (car rhs-content-triple-lst) result-rhs-content-triple-lst
			:test 'ps-triple-equal))
	      )
	)
	)
      
      )
    result
)))

;;;=====================================================================
;;;=====================================================================
;;; Functions to filter matches.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a list of possible matches, keep only the ones that 
;;	 have the largest number of matched triples in the source 
;;	 graph.
;; INPUT: matches = a list of matches. 
;; OUTPUT: The matches with the most matched triples.
;;======================================================================
(defun filter-match-result (matches)
  (let (result (max 0))
    (dolist (mappings matches result)
      (let ((size (number-of-matched-triples mappings)))
        (cond  
          ((> size max)
           (setf result (list mappings) max size))
          ((= size max)
           (setf result (cons mappings result))))))))

;;======================================================================
;; DESC: Given a list of possible matches, keep only the ones that
;;       have the highest match score.
;; INPUT: matches = a list of matches.
;;	  n (optional) = the size of the source graph.
;; OUTPUT: The matches that have the highest match scores.
;;======================================================================
(defun keep-strongest-matches (matches &optional (n 1) (testfn #'compute-match-score))
  (let (strongest-matches (max 0) score)
    (dolist (mappings matches)
      (setf score (funcall testfn mappings n))
      (cond
	((> score max)
	 (setf max score)
	 (setf strongest-matches (list mappings)))
	((= score max)
	 (setf strongest-matches (cons mappings strongest-matches)))))
    strongest-matches))

;;======================================================================
;; DESC: Given a list of matches between G1 and G2 (where one or both 
;;	 of the graphs contain a negated triple), find all the valid
;;	 matches. 
;; INPUT: 
;; OUTPUT:	 
;;======================================================================
(defun keep-valid-negated-matches (matches g1-size g1-unnegated-triples g2-unnegated-triples)
  (let ((match-threshold (- g1-size (length g1-unnegated-triples))) valid-matches)
    (dolist (mappings matches)
      (if (and (is-invalid-negated-match mappings g1-unnegated-triples g2-unnegated-triples)
	       (>= (number-of-matched-triples mappings) match-threshold))
	  (setf valid-matches (cons mappings valid-matches))))
    valid-matches))

;;======================================================================
;; DESC: Given a list of possible matches between G1 and G2, remove 
;;	 all the duplicate matches. Two matches are duplicates of 
;;	 each other if they both have the same set of node bindings.
;; INPUT: matches = (<mappings_1> ... <mappings_n>)
;; OUTPUT: A list with the unique matches between G1 and G2.
;;======================================================================
(defun remove-duplicate-matches (matches)
  (let (node-bindings node-bindings-for-all-matches result)
    (dolist (mappings matches)
      (setf node-bindings nil)	
      (dolist (mapping mappings)
	(setf node-bindings (union (get-node-bindings-for-mapping mapping) node-bindings :test #'equal)))
      (if (not (member node-bindings node-bindings-for-all-matches :test 
		 #'(lambda (x y) 
		     (and (eql (length x) (length y))
			  (not (set-difference x y :test #'equal))))))
	  (progn 
	    (setf node-bindings-for-all-matches (cons node-bindings node-bindings-for-all-matches))
	    (setf result (cons mappings result)))))
    result))


;;;=====================================================================
;;;=====================================================================
;;; Functions to match additional triples using transformations.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a match between two representations, try to improve upon 
;;	 the match by using transformations.
;; INPUT: match = a list of mappings.
;;	  source-or-target = source | source-weak | target. Specifies 
;;		whether we are trying to transform the source or target 
;;		graph. "source" means we are using subsumption testing
;;		(i.e. strong testing). "source-weak" means we are testing
;;		for subsumption in both directions.
;;	  depth (optional) = How many interations should we expand 
;;		a transformation.
;;	  htable-mappings-g1 (optional) = Hash table storing what nodes
;;		in G2 are mapped to by the nodes in G1. This hash table
;;		is indexed on the nodes in G1.
;;	  htable-g1-indx-head (optional) = Hash of the triples in G1
;;		indexed on the head node.
;;        htable-g1-indx-tail (optional) = Hash of the triples in G1
;;		indexed on the tail node.
;;	  htable-mappings-g2 (optional) = Hash table storing what nodes
;;		in G1 are mapped to by the nodes in G2. This hash table
;;		is indexed on the nodes in G2.
;;	  htable-g1-indx-head (optional) = Hash of the triples in G2
;;		indexed on the head node.
;;        htable-g1-indx-tail (optional) = Hash of the triples in G2
;;		indexed on the tail node.
;; OUTPUT: A list of additional possible matches from using transformations 
;;	   between G1 and G2 (i.e. unmatched triples aligned by applying a 
;;	   transformation). This list if of the form:
;;
;;		( (<mapping_1> ... <mapping_n>)
;;		  ...
;;		  (<mapping_1> ... <mapping_m>)
;;		)
;;
;; **NOTE** We need some way of remembering consequents that cannot be
;;	    transformed, so we don't end up processing them repeatedly.
;;======================================================================
(defun match-additional-triples-with-transformations 
       (match source-or-target 
	&optional 
	(depth 2)
	(htable-mappings-g1  *htable-mappings-for-g1*)
	(htable-g1-indx-head *htable-g1-indexed-by-head*)
	(htable-g1-indx-tail *htable-g1-indexed-by-tail*)
	(htable-mappings-g2  *htable-mappings-for-g2*)
	(htable-g2-indx-head *htable-g2-indexed-by-head*)
	(htable-g2-indx-tail *htable-g2-indexed-by-tail*))
  (let (use-htable-mappings use-htable-indx-head use-htable-indx-tail
	additional-matches all-additional-matches 
	triples-to-xform possible-rewrites 
	subsumeable-pathes starting-node)
    ;; Find all the triples that can be rewritten.
    (if (or (eql source-or-target 'source) (eql source-or-target 'source-weak))
	(progn
	  (setf use-htable-mappings htable-mappings-g1)
	  (setf use-htable-indx-head htable-g2-indx-head)
	  (setf use-htable-indx-tail htable-g2-indx-tail)
	  (setf triples-to-xform (find-triples-to-rewrite match source-or-target htable-g1-indx-head htable-g1-indx-tail)))
	(progn
	  (setf use-htable-mappings htable-mappings-g2)
	  (setf use-htable-indx-head htable-g1-indx-head)
	  (setf use-htable-indx-tail htable-g1-indx-tail)
	  (setf triples-to-xform (find-triples-to-rewrite match source-or-target htable-g2-indx-head htable-g2-indx-tail))))
    ;; Try to align these unmatched triples with rewrites.
    (dolist (consequent triples-to-xform)
      (setf possible-rewrites (back-chain-on-consequent consequent depth))
      (setf starting-node (triple-head (first consequent)))
      (setf additional-matches nil)	
      (dolist (possible-rewrite possible-rewrites)
	;; Find all pathes that subsume the rewrite.
	(setf subsumeable-pathes
	      (find-subsumeable-pathes-for 
		(gethash starting-node use-htable-mappings) possible-rewrite use-htable-indx-head use-htable-indx-tail
		(if (eql source-or-target 'source) 'strong 'weak)))
	;; Create new mappings for subsumeable pathes
	(dolist (subsumeable-path subsumeable-pathes)
	  (setf additional-matches 
		(cons (cons (if (or (eql source-or-target 'source) (eql source-or-target 'source-weak))
				(list consequent subsumeable-path)
				(list subsumeable-path consequent))
			    1)
		      additional-matches))))
      (if additional-matches 
	  (setf all-additional-matches (cons additional-matches all-additional-matches))))
    ;; The last step is to return all the possible combinations of triples 
    ;; matched through the use of rewrites.
    (combine-all-consistent-new-matches all-additional-matches htable-mappings-g1 htable-mappings-g2)))    


;;======================================================================
;; DESC: For all the triples matched by applying transformations, find
;;	 all the possible (and consistent) combinations.
;; INPUT: new-matches = A list of the possible matches. This list is 
;;		of the form:
;;		(  (((<consequent_1> <triples_i>) . <score>)
;;		    ...
;;		    ((<consequent_1> <triples_n>) . <score>))
;;		   (((<consequent_2> <triples_j>) . <score>)
;;		    ...
;;		    ((<consequent_2> <triples_m>) . <score>))
;;		   ...
;;		)
;;	  htable-mappings-g1 = Hash table storing what nodes in G2 
;;		are mapped to by the nodes in G1. This hash table is 
;;		indexed on the nodes in G1.
;;	  htable-mappings-g2 = Hash table storing what nodes in G1 
;;		are mapped to by the nodes in G2. This hash table is 
;;		indexed on the nodes in G2.
;; 	  limit (optional) = Since this function has the potential to
;;		create a huge list of possibilities, this parameters
;;		sets the limit on the upper bound.
;; OUTPUT: A list of consistent matches. Each element of this list is
;;	   a set of mappings.
;;======================================================================
(defun combine-all-consistent-new-matches (new-matches htable-mappings-g1 htable-mappings-g2 &optional (limit 1000))
  (let ((curr-new-matches (mapcar #'(lambda (x) (list x)) (first new-matches)))
	(remaining-new-matches (rest new-matches))
	(count 0)
	temp-result
	final-result)
    ;; Generate all possible combinations of the matches. 
    ;; **NOTE**: This may become very inefficient, so we definately 
    ;;		 need to consider some alternative options.
    (loop
      (if (null remaining-new-matches) (return))
      (dolist (curr-new-match curr-new-matches)
	(dolist (remaining-new-match (first remaining-new-matches))
	  (setf temp-result (cons (cons remaining-new-match curr-new-match) temp-result))))
      (setf curr-new-matches temp-result)
      (setf temp-result nil)
      (setf remaining-new-matches (rest remaining-new-matches)))
    ;; Get rid of all the mappings that are inconsistent.
    (dolist (match curr-new-matches)
      (cond
	;; We've reached the limit, so just return what we've got.
	((> count limit) 
	 (return))
	;; The mappings are consistent.
	((not (check-for-inconsistent-mappings match htable-mappings-g1 htable-mappings-g2))
	 (setf final-result (cons match final-result))
	 (setf count (+ count 1)))))
    final-result))

;;======================================================================
;; DESC: The purpose of this function is to filter any matches 
;;	 resulting from transformations that do not add any new 
;;	 information. We define this condition as not aligning
;;	 any unmatched triple in the specified graph.
;; INPUT: g1-graph = A list of triples. This should be the source graph.
;;	  new-mappings = A list of mappings resulting from the use of
;;		transformations.
;;	  curr-mappings = A list of matches so far.
;; OUTPUT: new-mappings with irrelevant matches removed.
;;======================================================================
(defun filter-irrelevant-xfrom-matches (g1-graph new-mappings curr-mappings)       
  (let (g1-matched-triples g1-unmatched-triples temp-result final-result)
    ;; 1) Get all the match and unmatched triples for G1.
    (dolist (curr-mapping curr-mappings)
      ;; Get the matched triples for G1. We are interested in G1 only because
      ;; it is the "query".
      (if (path-p (get-mapping-source curr-mapping))
          (setf g1-matched-triples (append (get-mapping-source curr-mapping) g1-matched-triples))
          (setf g1-matched-triples (cons   (get-mapping-source curr-mapping) g1-matched-triples))))
    (dolist (triple g1-graph)
      (if (not (member triple g1-matched-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y))))))
          (setf g1-unmatched-triples (cons triple g1-unmatched-triples))))  
    ;; 2) Now filter new matches. We only want to keep xforms that align an unmatched triple in G1.
    (dolist (new-mapping new-mappings)
      (setf temp-result nil)
      (dolist (source-triple (get-mapping-source new-mapping))
        (if (member source-triple g1-unmatched-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))
            (progn (setf temp-result t) (return))))
      (if temp-result (setf final-result (cons new-mapping final-result))))
    final-result))


;;;=====================================================================
;;;=====================================================================
;;; Functions to match additional triples after rewrites have been 
;;; applied.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a match and a source graph, try to match additional
;;	 triples in the source graph based on what has already been 
;;	 matched.
;; INPUT: match = a list of mappings.
;;	  source-graph = a list of triples. 
;;	  match-type (optional) = strong | weak.
;;	  htable-mappings-g1 (optional) = Hash table storing what nodes
;;		in G2 are mapped to by the nodes in G1. This hash table
;;		is indexed on the nodes in G1.
;;	  htable-g1-indx-head (optional) = Hash of the triples in G1
;;		indexed on the head node.
;;        htable-g1-indx-tail (optional) = Hash of the triples in G1
;;		indexed on the tail node.
;;	  htable-mappings-g2 (optional) = Hash table storing what nodes
;;		in G1 are mapped to by the nodes in G2. This hash table
;;		is indexed on the nodes in G2.
;;	  htable-g1-indx-head (optional) = Hash of the triples in G2
;;		indexed on the head node.
;;        htable-g1-indx-tail (optional) = Hash of the triples in G2
;;		indexed on the tail node.
;;	  source-graph-matched-triples (optional) = A list of the matched
;;		triples in the source graph.
;;	  target-graph-matched-triples (optional) = A list of the matched
;;		triples in the target graph.
;;	  source-graph-unmatched-triples (optional) = A list of the 
;;		unmatched triples in the source graph.
;; OUTPUT: A list containing the original set of mappings between the
;;	   source and the target plus any additional matches that can
;;	   be found. 
;;======================================================================
(defun find-additional-matching-triples 
       (match source-graph 
	&optional
	(match-type? 'strong)
	(htable-mappings-g1  *htable-mappings-for-g1*)
	(htable-g1-indx-head *htable-g1-indexed-by-head*)
	(htable-g1-indx-tail *htable-g1-indexed-by-tail*)
	(htable-mappings-g2  *htable-mappings-for-g2*)
	(htable-g2-indx-head *htable-g2-indexed-by-head*)
	(htable-g2-indx-tail *htable-g2-indexed-by-tail*)
	source-graph-matched-triples
	target-graph-matched-triples
	source-graph-unmatched-triples)
  (let (g1-matched-triples g2-matched-triples g1-unmatched-triples 
	possible-matches new-mapping new-mappings all-new-mappings
	curr count g1-number-of-unmatched branch-flag)
    ;; Get all the matched triples for both G1 and G2, and all the unmatched 
    ;; triples for G1.
    (cond
      ((and source-graph-matched-triples target-graph-matched-triples source-graph-unmatched-triples)
       (setf g1-matched-triples source-graph-matched-triples)
       (setf g2-matched-triples target-graph-matched-triples)
       (setf g1-unmatched-triples source-graph-unmatched-triples))
      (t
       (dolist (mapping match)
         ;; Get the matched triples for G1.
	 (if (path-p (get-mapping-source mapping))
	     (setf g1-matched-triples (append (get-mapping-source mapping) g1-matched-triples))
             (setf g1-matched-triples (cons   (get-mapping-source mapping) g1-matched-triples)))
      	 ;; Get the matched triples for G2.
      	 (if (path-p (get-mapping-target mapping))
             (setf g2-matched-triples (append (get-mapping-target mapping) g2-matched-triples))
             (setf g2-matched-triples (cons   (get-mapping-target mapping) g2-matched-triples))))
       ;; Once we have all the matched triples for G1, get all the unmatched triples for the 
       ;; source graph. We are focusing on the source graph because it is smaller and therefore 
       ;; more efficient to process.
       (dolist (triple source-graph)
	 (if (not (member triple g1-matched-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y))))))
    	     (setf g1-unmatched-triples (cons triple g1-unmatched-triples))))))
    ;;
    ;; (format t "==== STARTING: UNMATCHED ====~%~s~%" g1-unmatched-triples)
    ;;
    ;; For each unmatched triple in the source graph find a corresponding triple in the 
    ;; target graph that subsumes it and is consistent with the current bindings.
    (setf g1-number-of-unmatched (length g1-unmatched-triples))
    (setf curr (first g1-unmatched-triples))
    (setf count 0)
    (loop
      ;; Check and make sure that there are unmatched triples to begin with.
      ;; This condition is kind of redundant because we are testing it again
      ;; at the end of the loop. We need this however to make sure that there
      ;; are indeed unmatched triples on the first pass.
      (if (null g1-unmatched-triples) (return))
      ;; (format t "==== UNMATCHED ====~%~s~%" g1-unmatched-triples)
      ;; (format t "==== CURR ====~%~s~%" curr)
      ;; (format t "==== COUNT ~a/~a ====~%" count g1-number-of-unmatched)
      (cond
	((setf possible-matches (find-unmatched-triples-extending-from-node 
				  (gethash (triple-head curr) htable-mappings-g1)
				  (triple-relation curr) 
				  g2-matched-triples htable-g2-indx-head htable-g2-indx-tail)))
	((setf possible-matches (find-unmatched-triples-extending-from-node 
				  (gethash (triple-tail curr) htable-mappings-g1)
				  (invert-slot (triple-relation curr)) 
				  g2-matched-triples htable-g2-indx-head htable-g2-indx-tail))
	 (setf curr (invert-triple curr))))
      ;;
      ;; (format t "==== POSSIBLE MATCHES ====~%~s~%" possible-matches)
      ;;
      ;; Find the "true" matches. 
      (dolist (possible-match possible-matches)
        (setf new-mapping (cons (list curr possible-match) 1))
	(if (and (not (check-for-inconsistent-mappings (list new-mapping) htable-mappings-g1 htable-mappings-g2))
		 (subsume-p curr possible-match match-type?))
	    (setf new-mappings (cons new-mapping new-mappings))))
      ;;
      ;; (format t "==== NEW MAPPINGS ====~%~s~%" new-mappings)
      ;;
      ;; Check if there is a branch.
      (cond 
	((> (length new-mappings) 1) 
	 (setf branch-flag t) 
	 (setf g1-unmatched-triples (rest g1-unmatched-triples))
	 (return))
	(new-mappings
      	 (setf all-new-mappings (append new-mappings all-new-mappings))
      	 (update-match-mappings (first new-mappings) htable-mappings-g1 htable-mappings-g2)
	 (setf g1-unmatched-triples (rest g1-unmatched-triples)))
	(t
	 (setf g1-unmatched-triples (append (rest g1-unmatched-triples) (list (first g1-unmatched-triples))))))
      ;; Reset the parameters, and get the next unmatched triple.
      (setf curr (first g1-unmatched-triples))
      (setf new-mappings nil)
      (setf count (+ count 1))
      ;; Check for terminating conditions.
      (cond 
	((or (null curr) (null g1-unmatched-triples))
	 (return))
        ;; We finished one iteration through the list of unmatched triples, so we
        ;; need to check whether the list of unmatched triples has changed.
	((= count g1-number-of-unmatched) 
	 (if (= (length g1-unmatched-triples) g1-number-of-unmatched) 
	     (return) 
	     (progn (setf count 0) (setf g1-number-of-unmatched (length g1-unmatched-triples))) ))))
    (cond
      (branch-flag 
       (find-additional-matching-triples-branch 
	 (mapcar #'(lambda (m) (cons m all-new-mappings)) new-mappings)
	 match source-graph match-type?
	 htable-mappings-g1 htable-g1-indx-head htable-g1-indx-tail
	 htable-mappings-g2 htable-g2-indx-head htable-g2-indx-tail
	 source-graph-matched-triples target-graph-matched-triples source-graph-unmatched-triples))
      (t
       (list (append match all-new-mappings))))))


;;======================================================================
;; DESC: This function handles cases where there are alternative 
;;	 match possibilities (i.e. branches).
;; INPUT: branching-mappings = a list of alternative matches.
;;	  match = the original set of mappings between G1 and G2.
;;	  source-graph = a list of triples. 
;;	  htable-mappings-g1 = Hash table storing what nodes in G2 are 
;;		mapped to by the nodes in G1. This hash table is indexed 
;;		on the nodes in G1.
;;	  htable-g1-indx-head = Hash of the triples in G1 indexed on the 
;;		head node.
;;        htable-g1-indx-tail = Hash of the triples in G1 indexed on the 
;;		tail node.
;;	  htable-mappings-g2 = Hash table storing what nodes in G1 are 
;;		mapped to by the nodes in G2. This hash table is indexed 
;;		on the nodes in G2.
;;	  htable-g1-indx-head = Hash of the triples in G2 indexed on the 
;;		head node.
;;        htable-g1-indx-tail = Hash of the triples in G2 indexed on the 
;;		tail node.
;;	  source-graph-matched-triples = A list of the matched triples in 
;;		the source graph.
;;	  target-graph-matched-triples = A list of the matched triples in 
;;		the target graph.
;;	  source-graph-unmatched-triples = A list of the unmatched triples 
;;		in the source graph.
;; OUTPUT: A list containing the original set of mappings between the
;;         source and the target plus any additional matches that can
;;         be found.
;;======================================================================
(defun find-additional-matching-triples-branch 
       (branching-mappings match source-graph match-type?
	htable-mappings-g1 htable-g1-indx-head htable-g1-indx-tail
	htable-mappings-g2 htable-g2-indx-head htable-g2-indx-tail
	source-graph-matched-triples target-graph-matched-triples source-graph-unmatched-triples)
  (let (new-match final-result)
    ;; Investigate each of the branches.
    (dolist (branching-mapping branching-mappings)
      (setf new-match (append match branching-mapping))	
      ;; hash the node mappings for this match.
      (hash-match-mappings new-match htable-mappings-g1 htable-mappings-g2)
      (setf final-result 
	    (append (find-additional-matching-triples 
		      new-match source-graph match-type?
		      htable-mappings-g1 htable-g1-indx-head htable-g1-indx-tail
		      htable-mappings-g2 htable-g2-indx-head htable-g2-indx-tail
		      source-graph-matched-triples target-graph-matched-triples source-graph-unmatched-triples)
		    final-result)))
    final-result))


;;;=====================================================================
;;;=====================================================================
;;; Functions to find 1) triples to rewrite and 2) other triples that 
;;; match the triples being rewritten.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a match between two graphs, find all the triples that 
;;	 can be rewritten. These are unmatched triples connected to 
;;	 a mismatch point.
;; INPUT: match = a list of mappings.
;;	  source-or-target = source | source-weak | target. Specifies 
;;		whether to get the triples that are to be rewritten 
;;		from the source or target graph.
;;	  htable-graph-on-head = Hash of the triples in the specified
;;		graph indexed on the head node.
;;	  htable-graph-on-tail = Hash of the triples in the specified
;;		graph indexed on the tail node.
;; OUTPUT: A list of the possible rewrites. This list is of the form
;;	
;;		( (<triple_1> ... <triple_n>)
;;		  ...
;;		)
;;
;; **NOTE**: The current version of this function only finds paths that 
;;	are of length 1 to rewrite. We need to extend this function to 
;;	find paths of length N. All other functions are currently set 
;;	up to handle pathes of arbitrary length.
;;======================================================================
(defun find-triples-to-rewrite (match source-or-target htable-graph-on-head htable-graph-on-tail)
  (let (possible-rewrites bindings matched-triple matched-triples result)
    ;; Get all the unique bindings and the matched triples.
    (dolist (mapping match)
      (setf bindings (union (get-node-bindings-for-mapping mapping) bindings :test #'equal))
      (if (or (eql source-or-target 'source) (eql source-or-target 'source-weak))
	  (setf matched-triple (get-mapping-source mapping))
	  (setf matched-triple (get-mapping-target mapping)))
      ;; If the matched triple is a path, then append the path to the list
      ;; of matched-triples.
      (if (path-p matched-triple)
	  (setf matched-triples (append matched-triple matched-triples))
	  (setf matched-triples (cons matched-triple matched-triples))))
    ;; Now process the individual bindings to get unmatched triples that
    ;; intersect with the bindings
    (dolist (binding bindings)
      (setf possible-rewrites
	    ;; This mapcar is temporary. When we extend this function to handle pathes
	    ;; of arbitrary length (and this is the section of code to extend for this
	    ;; purpose), we will no longer need the mapcar. For now, possible-rewrites 
	    ;; is a list of triples. We want a list of consequents, which are lists of 
	    ;; triples, so we simply make each triple into a list.
	    (mapcar #'(lambda (trpl) (list trpl))	;; <- [1].
		    (if (or (eql source-or-target 'source) (eql source-or-target 'source-weak))
			(append (lookup-triples-with-node (first  binding) htable-graph-on-head) 
				(lookup-triples-with-node (first  binding) htable-graph-on-tail))
			(append (lookup-triples-with-node (second binding) htable-graph-on-head)
				(lookup-triples-with-node (second binding) htable-graph-on-tail)))))
      (dolist (possible-rewrite possible-rewrites)
	;; This is a rewrite if 1) it is not a triple that cannot be rewritten, 2) it is not 
	;; a matched triple, and 3) it is not a duplicate in the result, 
	;;
	;; **NOTE**: We need to somehow check whether we have tried rewriting this
	;; 	     triple before.
	;; **NOTE**: We can move the first two conditions to the [1]. This will make
	;;	     the code more efficient.
	(if (not (or (member (triple-relation (first possible-rewrite)) *non-rewrite-relations* :test #'equal)
		     (member (first possible-rewrite) matched-triples :test 
		       #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))
		     (member possible-rewrite result :test 
		       #'(lambda (x y) (or (equal x y) (equal x (invert-triple-path y))))) ))
	    (setf result (cons possible-rewrite result)))))
    result))


;;====================================================================== 
;; DESC: Given a starting point (i.e. a node) and a rewrite path, 
;;	 find a path starting at the start point that subsumes (or 
;;	 is subsumed by) the rewrite path.
;; INPUT: starting-node = a node. 
;;	  rewrite-path = a path that is a list of triples. We are trying
;;		to find a path like this one starting from starting-node.
;;	  htable-graph-on-head = a hash table indexing the triples that 
;;		belong to the same graph as starting-node. Each triple
;;		is indexed on the head node.
;;	  htable-graph-on-tail = a hash table indexing the triples that
;;		belong to the same graph as starting-node. Each triple
;;		is indexed on the tail node.
;;	  match-type? (optional) = strong | weak. If match-type? is set
;;		to 'strong, then each part of the rewrite-path must be 
;;		more general than it's corresponding part in the path
;;		starting at starting-node. If match-type? is set to 
;;		weak, then the subsumption can be carried out in either
;;		direction.
;; OUTPUT: A list of possible pathes found by starting from starting-node.
;;	   This list is of the form:
;;
;;		( (<triple_1> ... <triple_n>)
;;		  ...	
;;		  (<triple_i> ... <triple_n>))
;;
;;======================================================================
(defun find-subsumeable-pathes-for (starting-node rewrite-path htable-graph-on-head htable-graph-on-tail
				    &optional (match-type? 'strong))
  (let ((curr-triple-in-path (first rewrite-path))
	(path-pos 0)
	curr-pathes
        temp-result)
    ;; Get the first piece of the path.
    (dolist (first-triple 
	     (find-triples-extending-from-node starting-node (triple-relation curr-triple-in-path)
	       htable-graph-on-head htable-graph-on-tail))
      (if (subsume-p curr-triple-in-path first-triple match-type?)
	  (setf curr-pathes (cons (list first-triple) curr-pathes))))
    (setf path-pos (+ path-pos 1))
    ;; Now iterate on the remaining pieces.
    (loop
      ;; (setf curr-triple-in-path (elt rewrite-path path-pos))
      ;; *PZY*: Added this condition, so we won't get an out-of-bounds error
      ;; 	for flavors of lisp like openmcl.
      (setf curr-triple-in-path 
	    (if (and (listp rewrite-path) (< path-pos (length rewrite-path))) (elt rewrite-path path-pos) nil))
      (if (or (null curr-triple-in-path) (null curr-pathes)) (return))
      (dolist (curr-path curr-pathes)
	(setf temp-result (append (find-subsumeable-pathes-for0 curr-path curr-triple-in-path 
				    htable-graph-on-head htable-graph-on-tail match-type?)
				  temp-result)))
      (setf curr-pathes temp-result)
      (setf temp-result nil)
      (setf path-pos (+ path-pos 1)))
    curr-pathes))


;;======================================================================
;; DESC: This is a helper function for find-subsumeable-pathes-for. This
;;	 function takes a path/chain and the candidate triple we want
;;	 to extend the path by, and looks for a triple in the graph
;;	 that path is a part of with the following properties:
;;		1) extends the path.
;;		2) Subsumes (or is subsumed by) extending-triple.
;; INPUT: chain = a list of triples that make up a path.
;;	  extending-triple = A triple that we would like to extend
;;		chain by.
;;	  htable-graph-on-head = Hash table indexing the triples of 
;;		the graph that chain belong to. Each triple is indexed
;;		on the head node.
;;	  htable-graph-on-tail = Hash table indexing the triples of 
;;		the graph that chain belong to. Each triple is indexed
;;		on the tail node.
;;	  match-type? (optional) = strong | weak. See description given
;;		for the function find-subsumeable-path.
;; OUTPUT: A list of paths (where each path = chain + extension). NIL,
;;	   if chain cannot be extended in the specified manner.
;;
;; **NOTE** - 04/19/04, We need to add an additional condition that 
;;	checks to make sure that the head node of the extending triple 
;;	found is the same as the tail node of the last triple in the
;;	current path (i.e. chain). We need this for two reasons:
;;	     1) We are working with paths, and this condition will
;;	 	ensure that we are indeed working with paths.
;;	     2) The new class of transformations will require this
;;		check to remove non-sensical transformations.
;;======================================================================
(defun find-subsumeable-pathes-for0 (chain extending-triple htable-graph-on-head htable-graph-on-tail
				     &optional (match-type? 'strong))
  (let ((last-triple (first (last chain))) candidate-extensions result)
    (setf candidate-extensions 
	  (find-triples-extending-from-node (triple-tail last-triple) (triple-relation extending-triple)
	    htable-graph-on-head htable-graph-on-tail))
    (dolist (candidate-extension candidate-extensions)
      ;; ** See Note above **
      (if (and (subsume-p extending-triple candidate-extension match-type?)
	       (not (member candidate-extension chain :test 
		      #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))))
          (setf result (cons (append chain (list candidate-extension)) result))))
    result))


;;======================================================================
;; DESC: Given a node and a relation, find all the triples with this 
;;	 node whose relation is the same as the given relation.
;; INPUT: node = a concept node - e.g. (|Dog| . 1)
;;	  relation = a relation
;;	  htable-graph-on-head = Hash table for all the triples in 
;;		G index on the head.
;;	  htable-graph-on-tail = Hash table for all the triples in 
;;		G index on the tail.
;; OUTPUT: A list of triples where each triple contains node and its 
;;	   relation is the same as the given relation.
;;
;; NOTE: 06/03/04 - The optional parameters were a temporary hack to 
;;	 allow this function to query KM for triples that extend from 
;;	 the given node. The problem with this temp solution is the 
;;	 hashes may be corrupted by other functions that access them
;;	 - i.e. there maybe functions will write to and update these 
;;	 hashes in an unanticipated manner.
;;	 
;;	 Currently, the only function that updates these two structures 
;;	 is query-km-for-relation, and its updates are permitted. I am 
;;	 not sure if there are functions outside the matche (e.g. 
;;	 functions in RA) that will update these hashes. Hence, I need 
;;	 to look into this integration issue. Also, see the documentation 
;;	 for these two hash structures at the start of this file.
;;======================================================================
(defun find-triples-extending-from-node (node relation htable-graph-on-head htable-graph-on-tail
					 &optional (km-to-node-mappings *km-to-node-mappings*)	;; <- temporary see 
						   (node-to-km-mappings *node-to-km-mappings*)) ;; <- NOTE above.
  (let ((candidate-triples (append (lookup-triples-with-node node htable-graph-on-head)
				   (lookup-triples-with-node node htable-graph-on-tail)))
	final-result)
    (dolist (candidate-triple candidate-triples)
      (if (eql relation (triple-relation candidate-triple))
	  (setf final-result (cons candidate-triple final-result))))
    (if (null final-result)
	(setf final-result (query-km-for-relation node relation km-to-node-mappings node-to-km-mappings)))
    final-result))	
#|-------------------------------------------- 
  06/24/04 - I moved the following chuck of code to a function called "query-km-for-relation".
	     We'll keep this around for the time being to serve as reference. We should remove
	     it in the near.

    ;; If there are no triples that satisfy the path, then try to
    ;; query the KB for triples that satisfy the path.
    (if (null final-result) (setf km-instance (if (is-instance-p node) (first node) (gethash node node-to-km-mappings))))
    (cond
      ;; Check whether or not the KM instance is a variable (e.g. ?OBJ). Don't 
      ;; query the KB if it is.
      ((and km-instance (not (eql (elt (string km-instance) 0) '#\?)))
       (dolist (instance-val (km0 `(|the| ,relation |of| ,km-instance))) ;; <- get-vals misses too much info.
	 (setf instance-type (if (atom instance-val) (get-class-for-km-instance instance-val)))
	 (cond
	   ((and instance-type (anonymous-instancep instance-val))
	    (setf new-node (or (gethash instance-val km-to-node-mappings)
			       (cons instance-type (string (gensym "Node")))))
	    (setf final-result (cons (list node relation new-node) final-result))
	    (if (not (gethash instance-val km-to-node-mappings))
		(progn
		  (setf (gethash instance-val km-to-node-mappings) new-node)
		  (setf (gethash new-node node-to-km-mappings) instance-val))))
	   (instance-type
	    (setf final-result (cons (list node relation (list instance-val)) final-result)))))
       final-result)
      (t final-result))))
-------------------------------------------------|#


;;======================================================================
;; DESC: Given a node and a relation, find all the unmatched triples 
;;	 with this node whose relation is the same as the given 
;;	 relation.
;; INPUT: node = a concept node - e.g. (|Dog| . 1)
;;	  relation = a relation
;;	  matched-triples = a list of triples that have been matched.
;;	  htable-graph-on-head = Hash table for all the triples in 
;;		G index on the head.
;;	  htable-graph-on-tail = Hash table for all the triples in 
;;		G index on the tail.
;; OUTPUT: A list of unmatched triples where each triple contains node 
;;	   and its relation is the same as the given relation.
;;======================================================================
(defun find-unmatched-triples-extending-from-node (node relation matched-triples htable-graph-on-head htable-graph-on-tail)
  (let ((candidate-triples (append (lookup-triples-with-node node htable-graph-on-head)
				   (lookup-triples-with-node node htable-graph-on-tail)))
	final-result)
    (dolist (candidate-triple candidate-triples)
      (if (and (eql relation (triple-relation candidate-triple))
	       (not (member candidate-triple matched-triples :test
                      #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))))
	  (setf final-result (cons candidate-triple final-result))))
    final-result))


;;======================================================================
;; DESC: Given a graph node and a relation, query the underlying KM KB
;;	 for any values related to the given node via the given relation. 
;; INPUT: node = the graph node we want to query - e.g. (|Dog| . 1234).
;;	  relation = the relation we are interested in querying for - 
;;		e.g. |has-part|.
;;	  km-to-node-mappings (optional) = A hash table storing the 
;;		mappings from the KM node to the graph node - e.g.
;;		m(|_Dog123|) = (|Dog| . X3123).
;;	  node-to-km-mappings (optional) = A hash table storing the
;;		mappings from the graph node to the KM node - e.g.
;;		m((|Dog| . X3123)) = |_Dog123|.
;; OUTPUT: The output is a list of triples of the form:
;;	
;;		((node relation <val_1>)
;;		 ...
;;		 (node relation <val_n>))
;;	where 
;;		<val_i> is a concept related to node via relation.
;; 
;;      Further, this function will update both km-to-node-mappings and
;;	node-to-km-mappings to account for new instances introduced via
;;	the query process.
;;
;; **NOTE**: If the hashes are not given, then this function will 
;;	not query the underlying KB because we will not know what
;;	the given graph node maps to in the underlying KB. Also, 
;;	SEE the note in the header of find-triples-extending-from-node
;;	for related issues concerning the two hash structures.
;;======================================================================
(defun query-km-for-relation (node relation &optional (km-to-node-mappings *km-to-node-mappings*)
						      (node-to-km-mappings *node-to-km-mappings*)) 
  (let ((km-instance (if (is-instance-p node) (first node) (gethash node node-to-km-mappings)))
	new-node instance-vals instance-type final-result)
    ;; Check whether or not the KM instance is a variable (e.g. ?OBJ). Don't
    ;; query the KB if it is.
    (if (and km-instance (not (eql (elt (string km-instance) 0) '#\?)))
	(setf instance-vals (km0 `(|the| ,relation |of| ,km-instance)))) ;; <- get-vals misses too much info.
    ;; **NOTE**: 05/31/06 -- If we don't get anything answers, then try 
    ;;		 applying pseudo axioms.
    (cond
     ((and (null instance-vals) km-instance)
      (setf instance-vals (apply-pseudo-km-axiom km-instance relation))))
    ;; Process our results.
    (dolist (instance-val instance-vals)
      (setf instance-type (if (atom instance-val) (get-class-for-km-instance instance-val)))
      (cond
	;; We know the type for this instance and it's an anonymous instance.
        ((and instance-type (anonymous-instancep instance-val))
         (setf new-node (or (gethash instance-val km-to-node-mappings)
                            (cons instance-type (string (gensym "Node")))))
         (setf final-result (cons (list node relation new-node) final-result))
         (if (not (gethash instance-val km-to-node-mappings))
	     (progn
	       (setf (gethash instance-val km-to-node-mappings) new-node)
               (setf (gethash new-node node-to-km-mappings) instance-val))))
	(instance-type
	 (setf final-result (cons (list node relation (list instance-val)) final-result)))
	((is-value-p instance-val)
	 (setf final-result (cons (list node relation instance-val) final-result))) ))
    final-result))

;;======================================================================
;; DESC:
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun apply-pseudo-km-axiom (km-instance relation)
  (let (pseudo-axioms new-instances)
    (setf pseudo-axioms (first (member relation *pseudo-km-axioms-for-matcher* :test #'(lambda (x y) (equal x (first y))))))
    (dolist (domain+range (second pseudo-axioms))
      (if (isa km-instance (first domain+range))
	  (setf new-instances (cons (first (km0 `(|a| ,(second domain+range)))) new-instances))))
    new-instances))


;;;=====================================================================
;;;=====================================================================
;;; Functions to back chain on the unmatched triples.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a consequent, backchain on it to find possible rewrites
;;	 that could have generated it.
;; INPUT: consequent = the consequent we are back chaining on. This
;;		is a list of triples.
;;	  depth = how far to back chain.
;; OUTPUT: A list of all possible starting points that could have 
;;	   lead to the consequent. This list is of the form:
;;
;;		((<triple_1> ... <triple_n>) ... )
;;
;;	   NIL is returned if the consequent cannot be backchained on.
;;
;; **NOTE**: This function uses a hash table to maintain duplicate
;;	     starting points (i.e. pathes) that lead to the consequent.
;;	     *back-chain-on-consequent-duplicate-paths*
;;======================================================================
(defvar *back-chain-on-consequent-duplicate-paths* (make-hash-table :test #'equal))
(defun back-chain-on-consequent (consequent depth)
  (let ((curr-depth 0) (curr-triple-paths (list consequent)) 
	expansions hashkey result final-result)
    (clrhash *back-chain-on-consequent-duplicate-paths*)
    (loop
      (if (or (>= curr-depth depth) (null curr-triple-paths))
	  (return))
      (dolist (curr-triple-path curr-triple-paths)
	;; Do all possible expansions.
	(dolist (key (return-subpathes-for-expansion curr-triple-path))
	  (setf expansions 
		(append (expand-chain-on key curr-triple-path)
			(expand-chain-on-inverse key curr-triple-path)))
	  ;; Get rid of duplicates
	  (dolist (expansion expansions)
	    (setf hashkey (flatten-path expansion))
	    (if (not (gethash hashkey *back-chain-on-consequent-duplicate-paths*))
		(progn
		  (setf (gethash hashkey *back-chain-on-consequent-duplicate-paths*) t)
		  (setf result (cons expansion result)))))))
      (setf final-result (append result final-result))
      (setf curr-triple-paths result)
      (setf result nil)
      (setf curr-depth (+ curr-depth 1)))
    (clrhash *back-chain-on-consequent-duplicate-paths*)
    final-result))


;;======================================================================
;; DESC: Given a path and one of its subsequence, expand the subpath
;;	 based on applicable transformations.
;; INPUT: key = a list of triples that is a subsequence of chain.
;;	  chain = a list of triples that is the supersequence of key.
;; OUTPUT: This function returns NIL if the key cannot be expanded.
;;	   Otherwise it returns all the possible expansions of the 
;;	   key in chain. This list is of the form:
;;	
;;		((<triple_1> ... <triple_n>) ... )
;;		 
;;	   For example, given key = ((X r Y)) and 
;;			      chain = ((W r1 X) (X r Y) (Y r2 Z))
;;	   return
;;		( ((W r1 X) (X r X1) (X1 r  Y) (Y r2 Z))
;;		  ((W r1 X) (X r X2) (X2 r3 Y) (Y r2 Z)) )
;;
;; **NOTE**: How we find the starting position can be problematic in 
;;	the future. For example, imagine a path that has gone through
;;	several iterations of expansion:
;;	  original:	
;;			((A has-part B))
;;	  several interations later:
;;		((X has-part Y) (Y has-part Z) (X has-part Z))
;;	If the key was (X has-part Y), then we will be referring to
;;	its first occurance. A solution to this problem is to create
;;	unique IDs for each node with a path is expanded.
;;======================================================================
(defun expand-chain-on (key chain)
  (let* ((xforms (lookup-transformations-no-inverse key)) 
	 (start (position (first key) chain :test #'equal))  ;; <- see **NOTE**
	 (end (if start (+ start (length key))))
	 lhs rhs result bindings)
    (cond 
      ((or (not start) (not xforms)) nil)
      (t
       (dolist (xform xforms)
	 (setf lhs (get-xform-lhs xform))
	 (setf rhs (get-xform-rhs xform))
	 (setf bindings (transformation-applies? key rhs))
	 (if bindings
	     (setf result (cons (inline-list (instantiate-transformation lhs bindings) chain start end) result))))))
    result))

;;======================================================================
;; DESC: Same as expand-chain-on, except it expands the inverse of 
;;	 the key.
;; INPUT: Same as expand-chain-on.
;; OUTPUT: Same as expand-chain-on.
;;======================================================================
(defun expand-chain-on-inverse (key chain)
  (let* ((inverted-key (invert-triple-path key))
	 (xforms (lookup-transformations-no-inverse inverted-key))
	 (start (position (first key) chain :test #'equal))	;; <- see **NOTE**
	 (end (if start (+ start (length key))))
	 lhs rhs result bindings)
    (cond 
      ((or (not start) (not xforms)) nil)
      (t
       (dolist (xform xforms)
	 (setf lhs (get-xform-lhs xform))
	 (setf rhs (get-xform-rhs xform))
	 (setf bindings (transformation-applies? inverted-key rhs))
	 (if bindings
	     (setf result (cons (inline-list (instantiate-transformation (invert-triple-path lhs) bindings)
				  chain start end) result))))))
    result))


;;======================================================================
;; DESC: Given a list of triples and the antecedent (or consequent) of 
;;	 a transformation, determine if the transformation applies.
;; INPUT: triples = a list of triples.
;;	  rhs-or-lhs = a list of triples
;;	  match-type? (optional) = strong | weak. This should always 
;;		be strong. Setting this to weak can lead to unsound 
;;		inferences. There is only one special case where this
;;		can be set to weak, and that for anaphora resolution.
;; OUTPUT: NIL if the transformation does NOT applies. A list of the
;;	   bindings between triples and rhs-or-lhs if the transformation
;;	   does apply.
;;======================================================================
(defun transformation-applies? (triples rhs-or-lhs &optional (match-type? 'strong))
  (let (does-apply? match?)
    (cond 
      ((/= (length triples) (length rhs-or-lhs)))
      (t
       (dotimes (n (length triples))
	 (setf match? (subsume-p (elt rhs-or-lhs n) (elt triples n) match-type?))
	 (if (or (not match?) (and match? (< (rest match?) 0)))
	     (progn (setf does-apply? nil) (return))
	     (setf does-apply? (append (list (list (triple-head (elt rhs-or-lhs n)) (triple-head (elt triples n)))
					     (list (triple-tail (elt rhs-or-lhs n)) (triple-tail (elt triples n))))
				       does-apply?))))))
    does-apply?))


;;======================================================================
;; DESC: Given a transformation, instantiate it.
;; INPUT: xform = a list of triples that make up either the LHS or 
;;		RHS of a transformation.
;;	  bindings = a list of node bindings of the form
;; 			( (<node_1> <node_2>) 
;;			  ...
;;			)
;;		     where <node_1> is a node that belongs to xform
;;		     and <node_2> is the node we will use to replace
;;		     <node_1> in xform.
;; OUTPUT: The instantiated version of xform.
;;======================================================================
(defun instantiate-transformation (xform bindings)
  (let ((all-bindings bindings) instantiated-xform)
    (dolist (triple xform)
      (let ((head (triple-head triple)) (tail (triple-tail triple)) head-binding tail-binding)
	(setf head-binding (second (first (member head all-bindings :test #'(lambda (x y) (equal x (first y)))))))
	(setf tail-binding (second (first (member tail all-bindings :test #'(lambda (x y) (equal x (first y)))))))
	(if (not head-binding)
	    (progn 
	      (setf head-binding (cons (first head) (+ (random 100) 10)))
	      (setf all-bindings (cons (list head head-binding) all-bindings))))
	(if (not tail-binding)
	    (progn 
	      (setf tail-binding (cons (first tail) (+ (random 100) 10)))
	      (setf all-bindings (cons (list tail tail-binding) all-bindings))))
	(setf instantiated-xform (cons (list head-binding (triple-relation triple) tail-binding) instantiated-xform))))
    (reverse instantiated-xform)))


;;======================================================================
;; DESC: Given a path, return all subsequences of the path.
;; INPUT: triple-path = a list of triples.
;; OUTPUT: A list of pathes each of which is a subsequence of triple-path.
;;
;; **NOTE**: This function needs to be update so it will return all
;;	possible subsequences of the path. 
;;======================================================================
(defun return-subpathes-for-expansion (triple-path)
  (append (list triple-path) (mapcar #'(lambda (x) (list x)) triple-path)))


;;======================================================================
;; DESC: Given a path, return just the symbols in it.
;; INPUT: path = a list of triples.
;; OUTPUT: A list with just the symbols (i.e. types), with order 
;;	   preserved.
;;======================================================================
(defun flatten-path (path)
  (mapcar #'(lambda (x) (list (first (triple-head x)) (triple-relation x) (first (triple-tail x)))) path))



;;;=====================================================================
;;;=====================================================================
;;; Functions to maintain and lookup the various hashes.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: This function hashes all the rewrites, so they can be accessed
;;	 more efficiently.
;; INPUT: transformations (optional) = A list of transformation rules.
;;		Each element is a list of the form
;;			((<triples>) (<triples>))
;;	  htable (optional) = The hash table to store these transformations.
;; OUTPUT: All transformations are indexed on the relation path of their 
;;	   consequent (rhs).
;;======================================================================
(defun hash-transformation-rules (&optional (transformations *domain-neutral-transformations*) (htable *transformation-hash*))
  (let (xforms key)
    (dolist (transformation transformations)
      (setf key (get-relation-path (get-xform-rhs transformation)))
      (setf xforms (gethash key htable))
      (setf (gethash key htable) (cons transformation xforms)))))


;;======================================================================
;; DESC: Given a consequent, look up the possible rewrites that could
;;	 have lead to it. The inverse of the consequent is also 
;;	 considered during the lookup process.
;; INPUT: rhs = The consequent. This is a list of triples.
;;	  htable (optional) = The hash table used to look up the
;;		consequent.
;; OUTPUT: A list of possible rules - this list is of the form:
;;
;;		((<triples> <triples>) ... )
;;
;;	   or NIL if there are no rewrites that could have generated
;;	   the rhs.
;;
;; **NOTE**: 11/18/2004: Modified this function to return possible
;;	transforms for both the rhs and its inverse. 
;;======================================================================
(defun lookup-transformations (rhs &optional (htable *transformation-hash*))
  (let ((rpath (get-relation-path rhs)))
    (append (gethash rpath htable) (gethash (invert-relation-path rpath) htable))))


;;======================================================================
;; DESC: This function is a variant of lookup-transformations. The 
;;	 only difference is the inverse of the rhs is not considered
;;	 during the lookup process. 
;; INPUT: same as lookup-transformations.
;; OUTPUT: same as lookup-transformations.
;;======================================================================
(defun lookup-transformations-no-inverse (rhs &optional (htable *transformation-hash*))
  (let ((rpath (get-relation-path rhs)))
    (gethash rpath htable)))


;;======================================================================
;; DESC: Given a graph (i.e. a list of triples), this function will
;;	 index each triple based on its head. This will make lookup
;;	 of triples (especially in large graphs) much more efficient.
;; INPUT: graph = a list of triples.
;; 	  htable = Hash table to store the triples.
;; OUTPUT: All the triples in graph are indexed into htable based on
;;	   their head.
;;======================================================================
(defun hash-graph-on-head (graph htable)
  (let (key tempval)
    (clrhash htable)
    (dolist (triple graph)
      (setf key (triple-head triple))
      (setf tempval (gethash key htable))
      (setf (gethash key htable) (cons triple tempval)))))
      

;;======================================================================
;; DESC: Given a graph (i.e. a list of triples), this function will
;;	 index each triple based on its tail. This will make lookup
;;	 of triples (especially in large graphs) much more efficient.
;; INPUT: graph = a list of triples.
;; 	  htable = Hash table to store the triples.
;; OUTPUT: All the triples in graph are indexed into htable based on
;;	   their tail.
;;======================================================================
(defun hash-graph-on-tail (graph htable)
  (let (key tempval)
    (clrhash htable)
    (dolist (triple graph)
      (setf key (triple-tail triple))
      (setf tempval (gethash key htable))
      (setf (gethash key htable) (cons (invert-triple triple) tempval)))))


;;======================================================================
;; DESC: Given a node, look up the triples in the graph that contain
;;	 this node.
;; INPUT: node = a graph node - e.g. (|Dog| . 1)
;;	  htable = hash table that contains the triples of a graph 
;;		hashed on the nodes in the triple.
;; OUPUT: A list of triples containing node.
;;======================================================================
(defun lookup-triples-with-node (node htable)
  (gethash node htable))


;;======================================================================
;; DESC: This function will extract the mappings/bindings between the 
;;	 nodes of a match, and hash those bindings in the hash tables 
;;	 *htable-mappings-for-g1* and *htable-mappings-for-g2*. 
;;	 Duplicate bindings are removed.
;; INPUT: matches = ( ((<g1-triple> <g2-triple>) . <score>) ... )
;;        htable-g1 (optional) = specifies the hash table to store the
;;              mappings for G1 - i.e. the nodes in G1 are used as the
;;		index and the nodes they map to in G2 are stored as the
;;		values of this hash table.
;;	  htable-g2 (optional) = hash table to store the mappings for G2.
;; OUTPUT: This function does not return any output, but as a side effect 
;;	the hash tables *htable-mappings-for-g1* and *htable-mappings-for-g2*
;;	will store the mappings for the specified match.
;;======================================================================
(defun hash-match-mappings (match &optional (htable-g1 *htable-mappings-for-g1*) (htable-g2 *htable-mappings-for-g2*))
  (let (node-pairs)
    (clrhash htable-g1)
    (clrhash htable-g2)
    (dolist (mapping match)
      ;; Get the node mappings (i.e. ((X A) (Y B)) ) for this pair of triples. 
      (setf node-pairs (get-node-bindings-for-mapping mapping))
      ;; Hash the node mappings.
      (setf (gethash (first  (first  node-pairs)) htable-g1) (second (first  node-pairs)))
      (setf (gethash (first  (second node-pairs)) htable-g1) (second (second node-pairs)))
      (setf (gethash (second (first  node-pairs)) htable-g2) (first  (first  node-pairs)))
      (setf (gethash (second (second node-pairs)) htable-g2) (first  (second node-pairs)))) ))


;;======================================================================
;; DESC: Given a mapping, update the hash tables with the node bindings
;;	 of this mapping.
;; INPUT: mapping = ((<triple_1> <triple_2>) . <score>)
;; 	  htable-g1 (optional) = hash table containing the nodes in 
;;		G2 that are mapped to by the nodes in G1.
;;	  htable-g2 (optional) = hash table containing the nodes in
;;		G1 that are mapped to by the nodes in G2.
;; OUTPUT: No explicit output, but the hash tables for G1 and G2 are
;;	   updated as a side effect.
;;======================================================================
(defun update-match-mappings (mapping &optional (htable-g1 *htable-mappings-for-g1*) (htable-g2 *htable-mappings-for-g2*))
  (let ((bindings (get-node-bindings-for-mapping mapping)))
    (dolist (binding bindings)
      (setf (gethash (first binding) htable-g1) (second binding))
      (setf (gethash (second binding) htable-g2) (first binding)))))


;;======================================================================
;; DESC: Given a list of mappings between G1 and G2, check whether they 
;;	 are consistent with the current mappings.
;; INPUT: mappings = A list of the form:
;;		( ((<source-triple> <target-triple>) . <score>) ... )
;;        htable-g1-mappings = hash table containing the nodes in
;;              G2 that are mapped to by the nodes in G1.
;;        htable-g2-mappings = hash table containing the nodes in
;;              G1 that are mapped to by the nodes in G2.
;; OUTPUT: NIL if mappings are consistent. T if mappings are
;;	   inconsistent.
;; 
;; **NOTE**: 05/11/04 - Problem with this function. This function 
;;		fails to check whether new mappings are consistent 
;;		with each other. It only check whether new mappings 
;;		are consistent with existing mappings. Do we need to 
;;		fix this? This oversight allows for 1-to-N mappings. 
;;		Do we want this? 
;;======================================================================
(defun check-for-inconsistent-mappings (mappings htable-g1-mappings htable-g2-mappings)
  (let (node-pairs g1-node g2-node is-inconsistent)
    (dolist (mapping mappings)
      (setf node-pairs (get-node-bindings-for-mapping mapping))
      ;; Since there's only two node pairs, just inline it instead of iterating
      ;; through them. This probably will be more efficient.
      (setf g1-node (gethash (first  (first node-pairs)) htable-g1-mappings))
      (setf g2-node (gethash (second (first node-pairs)) htable-g2-mappings))
      (if (and (or g1-node g2-node)
	       (or (not (equal g1-node (second (first node-pairs))))
		   (not (equal g2-node (first  (first node-pairs))))))
	  (progn (setf is-inconsistent t) (return)))
      (setf g1-node (gethash (first  (second node-pairs)) htable-g1-mappings))
      (setf g2-node (gethash (second (second node-pairs)) htable-g2-mappings))
      (if (and (or g1-node g2-node)
	       (or (not (equal g1-node (second (second node-pairs))))
		   (not (equal g2-node (first  (second node-pairs))))))
	  (progn (setf is-inconsistent t) (return)))
      ;; **NOTE***: 12/13/04 - Check that property values are consistent. Since property 
      ;;		values are always sinks, we only need to check for them here.
      ;;		This may not be the best place to do this!!!
      (if (and (not g1-node) (not g2-node)
	       (is-value-p (first  (second node-pairs)))
	       (is-value-p (second (second node-pairs)))
	       (not (equal (first (second node-pairs)) (second (second node-pairs)))))
	  (progn (setf is-inconsistent t) (return))))
    is-inconsistent))

#|----------------------------------------------------------------------
*PZY* 05/23/06 -- NEED TO FIX THESE TWO FUNCTIONS!
;;======================================================================
;; DESC:
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun remove-dangling-xforms (mappings graph &optional (fn #'get-mapping-source))
  (let ((graph-nodes (extract-nodes-in-graph graph)) bindings new-mappings)
    (dolist (mapping mappings)
      (cond
        ((path-p (funcall fn mapping))
         (setf bindings (get-node-bindings-for-mapping mapping))
         (if (and (member (first (first  bindings)) graph-nodes :test #'equal)
                  (member (first (second bindings)) graph-nodes :test #'equal))
             (setf new-mappings (cons mapping new-mappings))))
        (t
         (setf new-mappings (cons mapping new-mappings)))))
    new-mappings))
----------------------------------------------------------------------|#

;;======================================================================
;;======================================================================
(defun has-dangling-xforms (mappings graph &optional (fn #'get-mapping-source))
  (let ((graph-nodes (extract-nodes-in-graph graph)) bindings b1-exist b2-exist result)
    (dolist (mapping mappings)
      (cond
        ((path-p (funcall fn mapping))
         (setf bindings (get-node-bindings-for-mapping mapping))
	 (setf b1-exist (member (first (first  bindings)) graph-nodes :test #'equal))
  	 (setf b2-exist (member (first (second bindings)) graph-nodes :test #'equal))
         (cond 
	   ((and b1-exist b2-exist))
	   ((and (not b1-exist) (equal (first (first (first  bindings))) (first (second (first  bindings))))))
	   ((and (not b2-exist) (equal (first (first (second bindings))) (first (second (second bindings))))))
	   (t (setf result t) (return))))))
    result))


;;;=====================================================================
;;;=====================================================================
;;; Functions for handling negated triples.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given the mappings between two graphs (one of which contains 
;;	 at least one negated triple), determine if this match is a 
;;	 valid one.
;; INPUT: mappings = the mappings between G1 and G2.
;; 	  g1-unnegated-triples = A list containing the unnegated forms 
;;		of the negated triples in G1.
;;	  g2-unnegated-triples = A list containing the unnegated forms 
;;		of the negated triples in G2.
;; OUTPUT: T if the match is invalid, NIL otherwise.
;;======================================================================
(defun is-invalid-negated-match (mappings g1-unnegated-triples g2-unnegated-triples)
  (let (source target result)
    (dolist (mapping mappings)
      (setf source (check-if-triple-was-unnegated (get-mapping-source mapping) g1-unnegated-triples))
      (setf target (check-if-triple-was-unnegated (get-mapping-target mapping) g2-unnegated-triples))
      (if (or (and (not source) target) (and source (not target)))
	  (progn (setf result t) (return))))
    result))

;;======================================================================
;; DESC: 
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun check-if-triple-was-unnegated (triples unnegated-triples)
  (let (result)
    (dolist (triple (if (path-p triples) triples (list triples)))
      (if (member triple unnegated-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))
	  (progn (setf result t) (return))))
    result))

;;======================================================================
;; DESC: Given a graph, return the unnegated version of the graph plus
;;	 all the negated triples that were unnegated.
;; INPUT: graph = a list of triples.
;; OUTPUT: A list of the form
;;			(<graph'> <unnegated-triples>)
;;	   where
;;		<graph'> is the unnegated version of graph
;;		<unnegated-triples> the unnegated form of all
;;		  the negated triples in graph.
;;======================================================================
(defun unnegate-graph (graph)
  (let (unnegated-triples unnegated-graph)
    (dolist (triple graph)
      (cond 
	((is-negated-triple triple) 
	 (setf unnegated-triples (cons (unnegate-triple triple) unnegated-triples))
	 (setf unnegated-graph   (cons (unnegate-triple triple) unnegated-graph)))
	(t
	 (setf unnegated-graph 	 (cons triple unnegated-graph)))))
    (list unnegated-graph unnegated-triples)))


;;======================================================================
;; DESC: Given a triple, determine if it is negated.
;; INPUT: triple = a triple.
;; OUTPUT: T if the triple is negated, NIL otherwise.
;;======================================================================
(defun is-negated-triple (triple)
  (and (consp (triple-relation triple)) (eql (first (triple-relation triple)) '|not|)))


;;======================================================================
;; DESC: Given a negated triple, unnegate it. This function assumes
;;	 that the triple passed to it is a negated triple.
;; INPUT: negated-triple = a negated triple.
;; OUTPUT: The unnegated form of negated-triple.
;;======================================================================
(defun unnegate-triple (negated-triple)
  (list (triple-head negated-triple) (second (triple-relation negated-triple)) (triple-tail negated-triple)))


;;;=====================================================================
;;;=====================================================================
;;; Utility functions.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a list of mappings, compute its match score.
;; INPUT: match = a list of mappings.
;;	  n = the size of the graph.
;; OUTPUT: The match score.
;;======================================================================
(defun compute-match-score (match n)
  (let ((score 0))
    (dolist (mapping match)
      (setf score (+ score (abs (get-mapping-score mapping)))))
    (/ score n)))	

;;======================================================================
;; DESC: 
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun recompute-match-score (match n)
  (let ((score 0.0) mapping-source mapping-target mapping-score)
    (dolist (mapping match)
      (setf mapping-source (get-mapping-source mapping))
      (setf mapping-target (get-mapping-target mapping))
      (setf mapping-score  (get-mapping-score  mapping))
      (if (< mapping-score 0)
	  (if (path-p mapping-source) 
	      (setf mapping-source (invert-triple-path mapping-source))
	      (setf mapping-source (invert-triple mapping-source))))
      (setf score (+ (compute-score-for-mapping mapping-source mapping-target) score)))
    (/ score n)))

;;======================================================================
;; DESC: Given a list of mappings, count the number of triples in the
;;	 source graph that was matched.
;; INPUT: match = a list of mappings.
;; OUTPUT: The number of matched triples in the source graph.
;;======================================================================
(defun number-of-matched-triples (match)
  (let ((count 0))
    (dolist (mapping match)
      (cond 
	((path-p (get-mapping-source mapping))
	 (setf count (+ count (length (get-mapping-source mapping)))))
	((not (eql (triple-relation (get-mapping-source mapping)) '|vacuous-relation|))
	 (setf count (+ count 1)))))
    count))

;;======================================================================
;; DESC:
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun compute-score-for-mapping (mapping-source mapping-target)
  (let (head1 tail1 head2 tail2)
    (if (path-p mapping-source)
	(progn (setf head1 (triple-head (first mapping-source)))
	       (setf tail1 (triple-tail (first (last mapping-source)))))
	(progn (setf head1 (triple-head mapping-source))
	       (setf tail1 (triple-tail mapping-source))))
    (if (path-p mapping-target)
	(progn (setf head2 (triple-head (first mapping-target)))
	       (setf tail2 (triple-tail (first (last mapping-target)))))
	(progn (setf head2 (triple-head mapping-target))
	       (setf tail2 (triple-tail mapping-target))))
    (rest (subsume-p (list head1 '|relation| tail1) (list head2 '|relation| tail2) 'weak))))


;;======================================================================
;; DESC: Given a graph, remove all the triples that cannot be rewritten.
;; INPUT: graph = a list of triples.
;; OUTPUT: A list of triples with all the triples that cannot be 
;;	   rewritten removed.	 
;;======================================================================
(defun remove-non-xform-triples-from-graph (graph &optional (remove-values *non-rewrite-relations*))
  (let (rel result)
    (dolist (triple graph)
      (setf rel (triple-relation triple))
      (if (and (atom rel)
	       (not (member rel remove-values 
		      :test #'(lambda (x y) (or (eql x y) (eql x (invert-slot y)))))))
          (setf result (cons triple result))))
    result))

;;======================================================================
;; DESC: 
;; INPUT: 
;; OUTPUT:
;;======================================================================
(defun lookup-node-bindings (node bindings &optional (source-or-target 'source))
  (let (result)
    (dolist (binding bindings)
      (cond
	((and (member source-or-target '(source source-weak) :test #'eql) 
	      (equal (first binding) node))
	 (setf result (cons binding result)))
	((and (eql source-or-target 'target) (equal (second binding) node))
	 (setf result (cons binding result)))))
    result))

;;======================================================================
;; DESC: Given the mappings between two graphs, return all the node
;;	 bindings.
;; INPUT: mappings = A list of mappings between G1 and G2.
;; OUTPUT: A list of the form:
;;		((<node_1a> <node_2a>) ... (<node_1n> <node_2n>))
;;======================================================================
(defun get-all-node-bindings-for-mappings (mappings)
  (let (all-node-mappings)
    (dolist (mapping mappings)
      (setf all-node-mappings (union (get-node-bindings-for-mapping mapping) all-node-mappings :test #'equal)))
    all-node-mappings))

;;======================================================================
;; DESC: Given a mapping between two triples, get the nodes that are
;;	 mapped.
;; INPUT: mapping = ((<triple_1> <triple_2>) . <score>)
;; OUTPUT: A list of the node bindings/pairings between <triple_1> and
;;	   <triple_2>. This list is of the form: 
;;		((<node_1a> <node_2a>) (<node_1b> <node_2b>))
;;======================================================================
(defun get-node-bindings-for-mapping (mapping)
  (let ((t1 (get-mapping-source mapping))
	(t2 (get-mapping-target mapping))
	t1-head t1-tail t2-head t2-tail)
    (setf t1-head (if (path-p t1) (triple-head (first t1)) (triple-head t1)))
    (setf t1-tail (if (path-p t1) (triple-tail (first (last t1))) (triple-tail t1)))
    (setf t2-head (if (path-p t2) (triple-head (first t2)) (triple-head t2)))
    (setf t2-tail (if (path-p t2) (triple-tail (first (last t2))) (triple-tail t2)))
    (if (>= (get-mapping-score mapping) 0)
	(list (list t1-head t2-head) (list t1-tail t2-tail))
	(list (list t1-head t2-tail) (list t1-tail t2-head)))))


;;======================================================================
;; DESC: The following set of functions extract the various parts of
;;	 a transformation rule. Each transform is of the form 
;;
;;		(<lhs> <rhs>)
;;		where
;;		<lhs> = (<triple_1> ... <triple_n>)
;;		<rhs> = (<triple_1> ... <triple_m>)
;; INPUT: For both functions, the input is a transformation.
;; OUTPUT: Returns the component of the transformation specified by
;;	   the function.
;;======================================================================
(defun get-xform-lhs (xform) (first  xform))
(defun get-xform-rhs (xform) (second xform))


;;======================================================================
;; DESC: Given a path, return all the relations in this path. For 
;;	 example, given 
;;		(((|Tail| . 1) |is-part-of| (|Dog| . 2))
;;		 ((|Dog| . 2) |owned-by| (|Person| . 3)))
;;	 return
;;		(|is-part-of| |owned-by|)
;; INPUT: triples = a list of triples that make up the path.
;; OUTPUT: A list consisting of the relations in the path.
;;======================================================================
(defun get-relation-path (triples)
  (mapcar #'(lambda (x) (triple-relation x)) triples))


;;======================================================================
;; DESC: Given a path of relations, invert the path (and the relations
;;	 in the path). For example, given the path
;;		(|is-part-of| |owned-by|)
;;	 return
;;		(|owns| |has-part|)
;; INPUT: rel-path = (<relation_1> ... <relation_n>)
;; OUTPUT: A list of the form:
;;		(<relation_n>-1 ... <relation_1>-1)
;;======================================================================
(defun invert-relation-path (rel-path)
  (let (result)
    (dolist (rel rel-path) 
      (setf result (cons (invert-slot rel) result)))
    result))


;;======================================================================
;; DESC: Given a path of triples, invert the path.
;; INPUT: triples = (<triple_1> ... <triple_n>)
;;		    where the tail of <triple_i> is the same node as 
;;			the head of <triple_i+1>.
;; OUTPUT: The inversion of the given path. For example, given
;;		
;;		(((|Dog|  . 1) |has-part| (|Tail| . 2)) 
;;		 ((|Tail| . 2) |has-part| (|Fur|  . 3)))
;;	   return
;;		(((|Fur|  . 3) |is-part-of| (|Tail| . 2))
;;		 ((|Tail| . 2) |is-part-of| (|Dog|  . 1)))
;;======================================================================
(defun invert-triple-path (triples)
  (let (result)
    (dolist (triple triples)
      (setf result (cons (invert-triple triple) result)))
    result))


;;======================================================================
;; DESC: Given a triple, invert the triple.
;; INPUT: x = a triple
;; OUTPUT: Return the inverse of x.
;;======================================================================
(defun invert-triple (x)
  (let ((relation (triple-relation x)))
    (list (triple-tail x)
          (if (atom relation) (invert-slot relation) (list (first relation) (invert-slot (second relation))))
          (triple-head x))))


;;======================================================================
;;======================================================================
(defun get-class-for-km-instance (instance)
  (let ((class (if instance (immediate-classes instance))))
    (cond 
      ((eql (length class) 1) (first class))
      (t class))))


;;======================================================================
;; DESC:
;; INPUT: 
;; OUTPUT:
;;======================================================================
(defun inline-list (key vals start &optional (end (+ start 1)))
  (let ((end-of-list vals) (n 0) start-of-list)
    (cond 
      ((>= start (length vals)) 
       nil)
      (t
       (dolist (val vals)
	 (cond 
	   ((< n start) 
	    (setf start-of-list (cons val start-of-list))
	    (setf end-of-list (rest end-of-list)))
	   ((>= n end)
	    (return))
	   (t
	    (setf end-of-list (rest end-of-list))))
	 (setf n (+ n 1)))
       (append (reverse start-of-list) key end-of-list)))))
;;;;====================================================================
;;;;====================================================================
;;;; FILE: simple-match.lisp
;;;;
;;;; Dependencies: km.lisp, graph-util.lisp
;;;;====================================================================
;;;;====================================================================

;;;=====================================================================
;;;=====================================================================
;;;=====================================================================
;;; Function for finding overlaps/matches between two representations
;;; that are encoded as graphs. These functions perform a "regular"
;;; match that does not involve any transformations.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: This function takes two graphs and returns the mappings 
;;	 between them. 
;; INPUT: graph1 = a list of triples.
;; 	  graph2 = a list of triples.
;;	  match-type? (optional) = 'weak or 'strong. If set to weak
;;              then subsumption is tested both ways.
;;	  discard-unit-matches? (optional) = T | NIL. 
;; OUTPUT: A list of the form:
;;		( ((<g1-triple> <g2-triple>) . <score>)
;;		  ...
;;		)
;;
;; **NOTE** - 07/02/04: I replaced this version of the code with the one below.
;;		I'm keeping it here as reference, but I will remove it after 
;;		I test the latest version thoroughly.
;; (defun graph-match (graph1 graph2 &optional (match-type? 'weak) (discard-unit-matches? t))
;;   (let ((result (find-all-matching-candidates (graph-match0 graph1 graph2 match-type?) discard-unit-matches?))
;;	   temp-match 
;;	   final-result)
;;     ;; Convert the internal format to the external format.
;;     (dolist (r result)
;;       (setf temp-match nil)
;;       (dolist (match r)
;;	   (setf temp-match 
;;	         (cons (cons (list (match-head match) (match-triple (first (match-body match))))
;;			     (rest (rest (first (match-body match)))))
;;		       temp-match)))
;;         (if temp-match (setf final-result (cons temp-match final-result))))
;;     final-result))
;;----------------------------------------------------------------------

#|
;;Original unpatched version
(defun graph-match (graph1 graph2 &optional (match-type? 'weak) (discard-unit-matches? t))
  (let ((initial-matches (graph-match0 graph1 graph2 match-type?))
	result
	temp-match 
	final-result)
    ;; Gather all the disjoint matches and iterate through each one to contruct the 
    ;; candidate matches.
    (dolist (matches (gather-all-disjoint-matches initial-matches))
      (setf result (append (find-all-matching-candidates 
			    (jchaw-patch-sort-input-to-find-all-matching-candidates matches)  ;;jchaw-patch
			    discard-unit-matches?) result)))

    ;; Convert the internal format to the external format.
    (dolist (r result)
      (setf temp-match nil)
      (dolist (match r)
	(setf temp-match 
	      (cons (cons (list (match-head match) (match-triple (first (match-body match))))
			  (rest (rest (first (match-body match)))))
		    temp-match)))
      (if temp-match 
	    (setf final-result (cons temp-match final-result))))
    final-result))
|#

;;jchaw-patched version
(defun graph-match (graph1 graph2 &optional (match-type? 'weak) (discard-unit-matches? t))
  (let ((initial-matches (graph-match0 graph1 graph2 match-type?))
	result
	temp-match 
	final-result)
    ;; Gather all the disjoint matches and iterate through each one to contruct the 
    ;; candidate matches.
    (dolist (matches (gather-all-disjoint-matches initial-matches))
      (setf result (append (find-all-matching-candidates 
			    (jchaw-patch-sort-input-to-find-all-matching-candidates matches)  ;;jchaw-patch
			    discard-unit-matches?) result)))

    ;; Convert the internal format to the external format.
    (dolist (r result)
      (setf temp-match nil)
      (dolist (match r)
	(setf temp-match 
	      (cons (cons (list (match-head match) (match-triple (first (match-body match))))
			  (rest (rest (first (match-body match)))))
		    temp-match)))
      (if temp-match 
	    (setf final-result (cons temp-match final-result))))
    final-result))

;;Sorts input to find-all-matching-candidates by their taxonomic score
(defun jchaw-patch-sort-input-to-find-all-matching-candidates(input)
  (sort input 
	#'(lambda(x y)
	    (let ((x-score (abs (cddr (caadr x))))
		  (y-score (abs (cddr (caadr y)))))
	      (or 
	       (> x-score y-score)
	       (and (= x-score 1) ;; 1 to 1 mapping. 
		    (= y-score 1) ;; But mapped relation may be a subslot, e.g., has-part vs has-functional-part
		    (let ((x-lhs   (car x))     ;;exactly 1 triple
			  ;(y-lhs   (car y))     ;;exactly 1 triple
			  (x-rhs   (caaadr x))  ;;exactly 1 triple
					;(y-rhs   (caaadr y))) ;;exactly 1 triple
			  )
		      (or (equal (triple-relation x-lhs)
				 (triple-relation x-rhs))
			  (equal (triple-relation x-lhs)
				 (invert-slot (triple-relation x-rhs)))))))))))
		    

;;;=====================================================================
;;;=====================================================================
;;; Top-Level helper functions to match two graphs.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: This is a helper function for graph-match. It takes two graphs
;;       and returns a list of possible matches between them.
;; INPUT: g1 = a list of triples.
;;        g2 = a list of triples.
;;        match-type? (optional) = the type of match we are conducting.
;;              If this variable is set to weak, then subsumption is
;;              tested both ways, otherwise subsumption is tested in
;;              only one direction.
;; OUTPUT: The output is a list of the form:
;;                
;;              ( (<g1-triple> 
;;		   ((<g2-triple_1>) <spec> . <score>)
;;                  ...  
;;                  (<g2-triple_n>) <spec> . <score>))
;;                )
;;                ...
;;              )
;;----------------------------------------------------------------------
(defun graph-match0 (g1 g2 &optional (match-type? 'weak))
  (let (result matches)
    (dolist (triple g1 result)
      (setf matches (triple-match triple g2 nil match-type?))
      (setf result  (append (if matches (list (list triple matches))) result)))))


;;----------------------------------------------------------------------
;; DESC: Given a triple, find all the other triples in the graph that 
;;       matches it.
;; INPUT: triple = ((<concept> . <ID>) <relation> (<concept> . <ID>))  
;;                 where
;;                      <concept> = <class> | (<class_1> ...)
;;                      <ID> = a number, character, symbol or string.
;;        graph = a list of triples.
;;        result (optional) = the triples in graph that match triple.
;;        match-type? (optional) = 'weak or 'strong. If set to weak 
;;              then subsumption is tested both ways.
;; OUTPUT: Returns a list of matches of the form
;;                
;;                      ( ((triple <triple>) . score) ... )
;;                      
;;         where <triple> is a triple from graph.
;;----------------------------------------------------------------------
(defun triple-match (triple graph &optional (result nil) (match-type? 'weak))
  (let (score detect+match-fn)
    (cond
      ;; Done. Nothing left to match.
      ((null graph) 
       result)

      ;; See if the triple contains a structured slot filler we know about. If so, then call
      ;; the corresponding match function to match it.
      ((setf detect+match-fn (contains-structured-slot-value-p triple))
       (setf score (apply-structured-slot-value-handler triple (car graph) detect+match-fn))
       (if score
	   (triple-match triple (cdr graph) (cons score result) match-type?)
	   (triple-match triple (cdr graph) result match-type?)))

      ;; The two triples both involve scalar values. This is handled 
      ;; differently from the other triples being matched. 
      ;; Note: This function does not handle ratios, comparisons, etc.. 
      ;;       This will be be handled by a different subsystem in the 
      ;;       future.
      ((and (or (is-value-p (triple-head triple))
		(is-value-p (triple-tail triple)))
	    (or (is-value-p (triple-head (car graph)))
		(is-value-p (triple-tail (car graph)))))
       (if (setf score (scalar-value-match-p triple (car graph)))
       	   (triple-match triple (cdr graph) (cons (cons (car graph) score) result) match-type?)
	   (triple-match triple (cdr graph) result match-type?)))

      ;; At this point, we know that one of the triples has a scalar and the other doesn't. 
      ;; If this condition is satisfied, then there is no need to try to match them. Move on 
      ;; to the next one.
      ((or (is-value-p (triple-head triple))
	   (is-value-p (triple-tail triple))
	   (is-value-p (triple-head (car graph)))
	   (is-value-p (triple-tail (car graph))))
       (triple-match triple (cdr graph) result match-type?))	

      ;; The two triples match exactly.
      ((equal (flatten-triple triple) (flatten-triple (car graph)))
       (triple-match triple (cdr graph) (cons (cons (car graph) (cons '(first . first) 1)) result) match-type?))

      ;; See if one triple subsumes the other.
      ((and (is-well-formed-triple-p triple)
	    (is-well-formed-triple-p (car graph))
	    (setf score (subsume-p triple (car graph) match-type?)))
       (triple-match triple (cdr graph) (cons (cons (car graph) score) result) match-type?))

      ;; No match. Go on to the next triple.
      (t
       (triple-match triple (cdr graph) result match-type?)))))

;;;=====================================================================
;;;=====================================================================
;;; Function to determine if two triples subsume and auxiliary 
;;; functions used in the process.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: This function takes two triples and determines whether one
;;       subsumes the other. Subsumption in this case is with respect
;;       to one triple being the superclass of another.
;; INPUT: t1 = a triple.
;;        t2 = a triple
;;        match-type? (optional) = the type of the match (i.e. strong
;;              or weak).
;; OUTPUT: NIL is returned if the two triples do not subsume each other.
;;         A list of the form 
;;			((<spec> . <spec>) . <score>)  
;;	   where <spec> = first | second 
;;		 <score> = indicating the degree of match based on 
;;			the taxonomic distance is returned otherwise.
;;
;; NOTE: It is important to realize that a negative score does not denote
;;       a negative score! A negative score means that the triples matched
;;       inversely. The absolute value of the score will be used to
;;       calculate the final score of the match.
;;----------------------------------------------------------------------
(defun subsume-p (t1 t2 &optional (match-type? 'weak))
  (let (head1 tail1 head2 tail2 relation-match? 	
	(rel1 (triple-relation t1)) (rel2 (triple-relation t2)) rel1-inv 
	inst-h1 inst-h2 inst-t1 inst-t2			; <- For determining instances.
	tdist1 tdist1a tdist1b 				; <- For taxonomic distance of head.
	tdist2 tdist2a tdist2b  			; <- For taxonomic distance of tail.
	(head-spec 'first) (tail-spec 'first)) 
    ;; Throw away the not if there is one.
    (if (consp rel1) (setf rel1 (second rel1)))
    (if (consp rel2) (setf rel2 (second rel2)))
    (setf rel1-inv (invert-slot rel1))
    ;; Line up the relation, and also check for subslots.
    (cond
      ((or (eql rel1 rel2)
	   (member rel1 (get-all-subslots rel2) :test #'eql)
	   (member rel2 (get-all-subslots rel1) :test #'eql))
       (setf head1 (triple-head t1) tail1 (triple-tail t1)
	     head2 (triple-head t2) tail2 (triple-tail t2)
	     relation-match? 1))
      ((or (eql rel1-inv rel2)
	   (member rel1-inv (get-all-subslots rel2) :test #'eql)
	   (member rel2 (get-all-subslots rel1-inv) :test #'eql))
       (setf head1 (triple-tail t1) tail1 (triple-head t1)
             head2 (triple-head t2) tail2 (triple-tail t2)
             relation-match? -1)))
    ;; Get the immediate classes of all the instances.
    (if relation-match?
	(progn
	  (if (is-instance-p head1) 
	      (setf inst-h1 head1 head1 (immediate-classes (first head1)))
	      (setf head1 (first head1)))
	  (if (is-instance-p tail1) 
	      (setf inst-t1 tail1 tail1 (immediate-classes (first tail1)))
	      (setf tail1 (first tail1)))
	  (if (is-instance-p head2) 
	      (setf inst-h2 head2 head2 (immediate-classes (first head2)))
	      (setf head2 (first head2)))
	  (if (is-instance-p tail2) 
	      (setf inst-t2 tail2 tail2 (immediate-classes (first tail2)))
	      (setf tail2 (first tail2)))))
    ;; Determine if the two triples subsume each other.
    (cond
      ;; No match! 
      ((not relation-match?) nil) 
      ;; Determine if the two things compared are both instances.
      ((and inst-h1 inst-h2 inst-t1 inst-t2)
       (if (and (equal inst-h1 inst-h2) (equal inst-t1 inst-t2))
	   (cons (cons head-spec tail-spec) (* relation-match? 1))))
      ;; The matching is done in the context of auto-classification/
      ;; intensional subsumption. Thus, inorder for there to be a 
      ;; match t1, the subsumee, has to be more specific than t2 
      ;; (i.e. t1 is generalized by t2).
      ((eql match-type? 'strong)
       (cond 
	 ((and inst-h1 inst-h2)
	  (if (equal inst-h1 inst-h2) 
	      (progn
		(setf tdist2 (tax-dist tail2 tail1))
	  	(setf tdist1 0))))
	 ((and inst-t1 inst-t2) 
	  (if (equal inst-t1 inst-t2) 
	      (progn
		(setf tdist1 (tax-dist head2 head1))
		(setf tdist2 0))))
         (t 
	  (setf tdist1 (tax-dist head2 head1))
	  (setf tdist2 (tax-dist tail2 tail1))))
       (if (and tdist1 tdist2)
	   (cons (cons 'second 'second)
	     	 (* relation-match? (/ (+ (/ 1 (+ tdist1 1)) (/ 1 (+ tdist2 1))) 2)))))
      ;; The matching is done in the context of some like
      ;; Literal Similarity.
      ((eql match-type? 'weak)
       (cond
	 ((and inst-h1 inst-h2)
	  (if (equal inst-h1 inst-h2)
	      (progn	
		(or (setf tdist2a (tax-dist tail1 tail2)) (setf tdist2b (tax-dist tail2 tail1)))
		(setf tdist1a 0))))
	 ((and inst-t1 inst-t2)
	  (if (equal inst-t1 inst-t2)
	      (progn 
		(or (setf tdist1a (tax-dist head1 head2)) (setf tdist1b (tax-dist head2 head1)))
		(setf tdist2a 0))))
	 (t
	  (or (setf tdist1a (tax-dist head1 head2)) (setf tdist1b (tax-dist head2 head1)))
	  (or (setf tdist2a (tax-dist tail1 tail2)) (setf tdist2b (tax-dist tail2 tail1)))))
       (setf tdist1 (or tdist1a tdist1b))	
       (if tdist1b (setf head-spec 'second))
       (setf tdist2 (or tdist2a tdist2b))	
       (if tdist2b (setf tail-spec 'second))
       (if (and tdist1 tdist2)
	   (cons (cons head-spec tail-spec)	
		 (* relation-match? (/ (+ (/ 1 (+ tdist1 1)) (/ 1 (+ tdist2 1))) 2)))))
      (t nil))))

;;----------------------------------------------------------------------
;; DESC: This function takes two concept(s) and determines if one
;;       concept is a subclass of the other.
;; INPUT: subconcept = a concept. This input can be either a list
;;              or an atom. For example either '(|Dog| |Pet|) or
;;              '|Dog| is fine.  
;;        supconcept = the concept we are testing subconcept against to
;;              see if subconcept is one of its subclasses.
;; OUTPUT: The result is a number denoting the taxonomic distance between
;;      the two concepts if subconcept is a subclass of supconcept. NIL
;;      is returned otherwise. If the inputs are lists of concepts, then
;;      this function is not minimal. The function tax-dist0 is called
;;      to compute the taxonomic distance.
;;----------------------------------------------------------------------
#|
;;Unsafe version. 
;;The function subsume-p
;;sometimes calls tax-dist with a cons whose cdr is not listp (like
;;this: '(|Gas| . "Node18812"). It's obviously a bug that such a
;;cons get through, and it's very rare. But the following guard I
;;added on elems1 and elems2 in tax-dist seems to at least prevent
;;the matcher from crashing.
(defun tax-dist (subconcept supconcept)
  (let ((elems1 (if (consp subconcept) subconcept (list subconcept)))
        (elems2 (if (consp supconcept) supconcept (list supconcept)))
        dist result)
    (dolist (elem2 elems2)
      (dolist (elem1 elems1)
        (if (setf dist (tax-dist0 (list elem1) elem2)) (return dist)))
      (if (null dist)
	  (progn (setf result nil) (return))
	  (setf result (cons dist result))))
    (if result (apply #'min result) result)))
|#

;;New version contributed by Ken Barker [03Feb08]
(defun tax-dist (subconcept supconcept)
  (let ((elems1 (if (consp subconcept) 
		    (if (listp (cdr subconcept)) 
			subconcept 
		      (list (car subconcept))) (list subconcept)))
	(elems2 (if (consp supconcept) 
		    (if (listp (cdr supconcept)) 
			supconcept 
		      (list (car supconcept))) (list supconcept)))
       dist result)
   (dolist (elem2 elems2)
     (dolist (elem1 elems1)
       (if (setf dist (tax-dist0 (list elem1) elem2)) (return dist)))
     (if (null dist)
         (progn (setf result nil) (return))
         (setf result (cons dist result))))
   (if result (apply #'min result) result)))


;;----------------------------------------------------------------------
;; DESC: This is the helper function of tax-dist, which computes the
;;       number of taxonomic edges between two concepts.
;; INPUT: start = the starting point (i.e. subclass). This is a list
;;              (e.g. '(|Enter|)).
;;        end = This is the end point (i.e. superclass) we are trying
;;              to reach from start. It is an constant (e.g. '|Move|).
;; RESULT: A number if we can reach end from start. NIL otherwise.
;;----------------------------------------------------------------------
(defun tax-dist0 (start end &optional (dist 0))
  (let (result)
    (cond 
      ((null start) nil)
      ((member end start :test #'eql) dist) 
      (t
       (dolist (sc start)
         (setf result (tax-dist0 (immediate-superclasses sc) end (+ dist 1)))
	 (if result (return result)))))))

;;----------------------------------------------------------------------
;; DESC: This function gets all the subslots of a slot. The function
;;       calls KM's all-subslot function. It augments KM's function
;;       by also returning all the subslots of an inverse which KM
;;       won't do unless that information is explicitly defined
;;      somewhere.
;; INPUT: slot = a slot.
;; OUTPUT: a list of all the subslots of slot.
;;----------------------------------------------------------------------
(defun get-all-subslots (slot)
  (let ((subslots (all-subslots slot)))
    (if subslots
	subslots
	(mapcar #'(lambda (x) (invert-slot x)) (all-subslots (invert-slot slot))))))

;;----------------------------------------------------------------------
;; DESC: This function determines if a given concept is an instance.
;; INPUT: X = (<concept> . X) or (<concept>).
;; OUTPUT: T if X is an instance. NIL otherwise.
;;----------------------------------------------------------------------
(defun is-instance-p (x) (and (consp x) (atom (first x)) (null (rest x))))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defun is-well-formed-triple-p (triple)
  (let ((head-type (first (triple-head triple))) 
	(tail-type (first (triple-tail triple))))
    (if (and (or (consp head-type) (kb-objectp head-type)) (or (consp tail-type) (kb-objectp tail-type))) t nil)))

;;----------------------------------------------------------------------
;; DESC: This function determines if a given concept is a value.
;; INPUT: X = (<concept> . X) or (<concept>).
;; OUTPUT: T if X is a value. NIL otherwise.
;;----------------------------------------------------------------------
(defun is-value-p (x) (and (consp x) (eql ':|pair| (first x))))

;;----------------------------------------------------------------------
;; DESC: Given two triples with scalar values, determine if the two 
;;       triples match. Two triples match if the following conditions
;;       are satisfied:
;;              1. <Value-class> are equivalent
;;              2. <scalar> are equivalent
;;              3. <ref-class> are subsumeable.
;; INPUT: t1 = a triple of the form:
;;              ((<Value-class> . x) |value| (:pair <scalar> <ref-class>))
;;        t2 = a triple of the same form as t1.
;; OUTPUT: A score indicating the degree of match between t1 and t2
;;         (if they match). NIL otherwise.
;; 
;; NOTE: Based on the current interface in SHAKEN it is not possible
;;       to specify a NOT for the value. Furthermore, because
;;       (:pair <<scalar> <ref-class>) is never a prototype participant,
;;       it is always the case that the triples are lined up.
;;----------------------------------------------------------------------
(defvar *property-value-relations* '(|value| |time-value|))
(defun scalar-value-match-p (t1 t2)
  (let (tdist)
    (cond
      ;; Make sure that the relation is value. Stop if not.  
      ((or (not (member (triple-relation t1) *property-value-relations*)) 
	   (not (member (triple-relation t2) *property-value-relations*))) 
       nil)
      ;; See if the two triples satisfy the match criteria. This will take care of 
      ;; the property values with instances instead of classes.
      ((and (equal (first  (triple-head t1)) (first  (triple-head t2)))
            (equal (second (triple-tail t1)) (second (triple-tail t2)))
	    (equal (third (triple-tail t1))  (third (triple-tail t2))))	
       (cons '(first . first) 1))	
      ;; Check again, but this time we are dealing with reference
      ;; classes.	
      ((and (equal (first  (triple-head t1)) (first  (triple-head t2)))
	    (equal (second (triple-tail t1)) (second (triple-tail t2)))
	    (setf tdist (or (tax-dist (third (triple-tail t1)) (third (triple-tail t2)))
			    (tax-dist (third (triple-tail t2)) (third (triple-tail t1))))))
       (cons '(first . first) (/ (+ 1 (/ 1 (+ tdist 1))) 2)))
      ;; There was no match.
      (t nil))))

;;----------------------------------------------------------------------
;; DESC: Given two value pairs, determine if the values are equal.
;; INPUT: v1 = (:|pair| <value> <ref-class>)
;;        v2 = (:|pair| <value> <ref-class>)
;; OUTPUT: T if v1 and v2 are equal, NIL otherwise.
;;----------------------------------------------------------------------
(defun value-pair-equal (v1 v2)
  (if (equal (second v1) (second v2))
      (if (equal (third v1) (third v2))
	  t
	  (if (and (third v1) (third v2))
	      (or (tax-dist (third v1) (third v2))
		  (tax-dist (third v2) (third v1)))))))


;;;=====================================================================
;;;=====================================================================
;;; Functions for performing morphing of concepts. In other words,
;;; sibling match.
;;;=====================================================================
;;;=====================================================================

;;
;; Given two concepts in graphical format, find any triples that can
;; be morphed (i.e. two triples match due to them being siblings where 
;; they would otherwise not match).
;;
(defun morph-triples (g1 g2)
  (let (result matches)
    (dolist (triple g1 result)
      (setf matches (morph-triples0 triple g2))
      (setf result  (append (if matches (list (list triple matches))) result)))))
   
;;     
;; Takes a unflattened triple and finds other triples that it can be 
;; morphed with in graph which is also a list of unflattened triples 
;; of the form
;;
;;      ( ((o1 . x) r (o2 . y)) ... )
;;
;; The result is in the same format as that returned by triple-match.
;;
;; NOTE TO SELF: When you come back to this function, there is no
;;	need to check for scalar values. This is because scalars will 
;;	be eliminated before the morphing process begins.
;;
(defun morph-triples0 (triple graph &optional (result nil))
  (let (score)
    (cond
      ((null graph)
       result)
      ;; Do not try to match if the triple contains information regarding
      ;; scalar property values. This will be handled by a different subsystem.
      ((or (is-value-p (triple-head triple))
           (is-value-p (triple-tail triple))
           (is-value-p (triple-head (car graph)))
           (is-value-p (triple-tail (car graph))))
       (morph-triples0 triple (cdr graph) result))
      ((setf score (morph-p triple (car graph)))
       (morph-triples0 triple (cdr graph) (cons (cons (car graph) score) result)))
      (t
       (morph-triples0 triple (cdr graph) result)))))

;;
;; Takes two unflattened triples and determines whether one can be morphed 
;; into the other. A score indicating the cost of the morph based on the 
;; taxonomic distance is returned. NIL is returned if t1 and t2 cannot be 
;; morphed. 
;;
;; NOTE: It is important to realize that a negative score does not denote
;; 	 a negative score! A negative score means that the triples merged
;;	 inversely. The absolute value of the score will be used to 
;;	 calculate the final score.
;;
(defun morph-p (t1 t2)
  (let (head1 tail1 head2 tail2 relation-match?
        (rel1 (triple-relation t1)) (rel2 (triple-relation t2))
        rel1-inv inst-h1 inst-h2 inst-t1 inst-t2 tdist1 tdist2)
    ;; Throw away the not if there is one.
    (if (consp rel1) (setf rel1 (second rel1)))
    (if (consp rel2) (setf rel2 (second rel2)))
    (setf rel1-inv (invert-slot rel1))
    ;; Line up the relation.
    (cond
      ((or (eql rel1 rel2)
           (member rel1 (get-all-subslots rel2) :test #'eql)
           (member rel2 (get-all-subslots rel1) :test #'eql))
       (setf head1 (triple-head t1) tail1 (triple-tail t1)
             head2 (triple-head t2) tail2 (triple-tail t2)
             relation-match? 1)) 
      ((or (eql rel1-inv rel2)
           (member rel1-inv (get-all-subslots rel2) :test #'eql)
           (member rel2 (get-all-subslots rel1-inv) :test #'eql))
       (setf head1 (triple-tail t1) tail1 (triple-head t1)
             head2 (triple-head t2) tail2 (triple-tail t2)
             relation-match? -1)))
    ;; Get the immediate classes of all the instances.
    (if relation-match?
        (progn
          (if (is-instance-p head1)
              (setf inst-h1 head1 head1 (immediate-classes (first head1)))
              (setf head1 (first head1)))
          (if (is-instance-p tail1)
              (setf inst-t1 tail1 tail1 (immediate-classes (first tail1)))
              (setf tail1 (first tail1)))
          (if (is-instance-p head2)
              (setf inst-h2 head2 head2 (immediate-classes (first head2)))
              (setf head2 (first head2)))
          (if (is-instance-p tail2)
              (setf inst-t2 tail2 tail2 (immediate-classes (first tail2)))
              (setf tail2 (first tail2)))
	  ;; Determine if the triples are siblings.
          (cond
	    ((and inst-h1 inst-h2)
	     (if (equal inst-h1 inst-h2)
		 (progn
		   (setf tdist2 (find-common-ancestor tail1 tail2))
		   (setf tdist1 0))))
	    ((and inst-t1 inst-t2)
	     (if (equal inst-t1 inst-t2)
		 (progn
		   (setf tdist1 (find-common-ancestor head1 head2))
		   (setf tdist2 0))))
	    (t
	     (setf tdist1 (find-common-ancestor head1 head2))
	     (setf tdist2 (find-common-ancestor tail1 tail2))))))
    (if (and tdist1 tdist2)
	(cons '(second . second) (* relation-match? (/ (+ (/ 1 tdist1) (/ 1 tdist2)) 2)))
	nil)))

;;
;; Given two concepts find their common ancestor. The inputs are
;; the starting concepts in list form. The input can be either a 
;; list or an atom. For example either '(|Dog| |Pet|) or '|Dog| 
;; is fine.
;; 
;; This function will return nil if they do not share a common 
;; ancestor and a number if they do.
;; 
(defun find-common-ancestor (concept1 concept2)
  (let ((c1 (if (consp concept1) concept1 (list concept1)))
        (c2 (if (consp concept2) concept2 (list concept2)))
	common-ancestors ca1 ca2)
    (if (eql c1 c2)
	1
	(progn
	  (dolist (x c1) (setf ca1 (append (all-superclasses x) ca1)))
	  (dolist (y c2) (setf ca2 (append (all-superclasses y) ca2))) 
	  (setf common-ancestors (intersection ca1 ca2 :test #'eql))
	  (setf common-ancestors (set-difference common-ancestors *top-level-concepts* :test #'eql))
	  (if (null common-ancestors)
	      nil
	      (+ (tax-dist c1 (first common-ancestors)) (tax-dist c2 (first common-ancestors))))))))


;;;=====================================================================
;;;=====================================================================
;;; Functions used by by graph-match to determine the BEST match
;;; should multiple matches occur.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: Given a list of potential matches, construct all the possible
;;	 "interpretations". NOTE, we are looking only for 1-to-1 
;;	 mappings.
;; INPUT: matches = a list of the form:
;;
;;		((<head_1> <body_2>) ... (<head_n> <body_n>))
;;
;;	    where
;;		<head_i> = a triple in the source graph.
;;		<body_i> = all the triples in the target graph that
;;			<head_i> matched. This list is of the form:
;;				
;;			((<triple_trg1> <spec> . <score>) ... )
;;	  discard-unit-matches (optional) = Whether or not to discard
;;		unit matches.
;;	  start-matches+index (optional) = If specified, we will use
;;		this as the start point. The format of this input is
;;			((<head> <body>) . <indx>)
;; OUTPUT: This function returns all possible 1-to-1 "interpretations".
;; 
;; NOTE: There was originally a problem with duplicate answers. This 
;;	problem appears to have been fixed. However, we need to do some 
;;	more rigorous testing to ensure this. An alternative solution 
;;	to this problem is to filter the output for duplicates. This 
;;	can be costly, so we'll try to avoid this approach.
;;
;; NOTE2: We need to revert back to the method described in our KCAP 
;;	paper for selecting the starting point. We switched to this 
;;	method for efficiency reasons, but the gains were not signi-
;;	ficant. Furthermore, this current method will fail for the 
;;	following example:
;;
;;	(setf *g1 '(
;;	  ((|Aqueous-solution| . 1)     |pOH|           (|POH-Value| . 2))
;;	  ((|Question| . 0)             |context|       (|Aqueous-Solution| . 1))
;;	  ((|Question| . 0)             |context|       (|Aqueous-Solution| . 2))
;;	  ((|Question| . 0)             |context|       (|Aqueous-Solution| . 3))))
;;
;;	(setf *g2 '(
;;	  ((|Aqueous-solution| . 1)     |pOH|           (|POH-Value| . 2))
;;	  ((|Question| . 0)             |context|       (|Aqueous-Solution| . 1))))
;;
;; NOTE3: 10/19/04 - We fixed a bug in this function. Originally, 
;;	  this function was overlooking branches in the inverse 
;;	  direction (i.e. two or more triples in the source graph 
;;	  map to the same triple in the target graph). Look in the 
;;	  file simple-match.lisp.bkp101904 to reference the original
;;	  function.
;;	  08/22/05 - The previous fix was generating 2 * (choose N M)
;;	  possible match permutations when there are N-to-M mappings.
;;	  This produces too many permutations -- especially when the
;;	  graph is large -- which is slowing things down. We address
;;	  this issue by considering only N-to-1 inverse mappings.
;;----------------------------------------------------------------------
(defun find-all-matching-candidates (matches &optional (discard-unit-matches t) start-matches+index)
  (let ((indx 0) search-stack start-point final-result result remaining-matches
        new-matches new-match curr mhead temp)
    ;; Find an initial starting point to construct all possible candidates.
    ;; **NOTE2**: 04/13/04 - See comment (above) regarding this note.
    (cond
      (start-matches+index
       (setf start-point start-matches+index))
      (t
       (dolist (match matches)
         (if (> (length (match-body match)) (length (match-body (first start-point))))
             (setf start-point (cons match indx)))
         (setf indx (+ indx 1)))))
    ;; Construct all the possible interpretations.
    (loop
      ;; We are done. Return the final result.
      (if (and (null (match-body (first start-point))) (null search-stack)) (return final-result))
      ;; Check to see if there were alternative interpretations on the previous pass.
      (cond
        ;; Yes. Restore the state and examine these alternatives.
        (search-stack
         (setf result            (first  (first search-stack)))
         (setf new-matches       (second (first search-stack)))
         (setf remaining-matches (third  (first search-stack)))
         (setf search-stack      (rest search-stack)))
        ;; No. Move on to the next element in the start-point.
        (t
         (setf result nil)
         (setf new-matches (list (cons (list (match-head (first start-point))
                                             (list (first (match-body (first start-point)))))
                                       (rest start-point))))
         (setf remaining-matches matches)
         (setf start-point (cons (list (match-head (first start-point))
                                       (rest (match-body (first start-point))))
                                 (rest start-point)))))
      ;; Begin the construction process. 
      (loop
        ;; The second part of the conditional ensures that we do not end up with
        ;; any duplicate answers in the final result. It checks to make sure that
        ;; there are no longer any additional matches to add to the curr match
        ;; that is being constructed.
        (if (or (null new-matches) (not (find-if #'(lambda (x) (not (null x))) remaining-matches))) (return)) ;; <- Done.
        (setf curr        (first new-matches))   
        (setf new-matches (rest  new-matches))
           
        ;; Check for multiple branches - i.e. the current triple in source graph matches
        ;; more than one triple in the target graph or vice versa. If so, then save the
        ;; alternatives to process latter. Also, make sure that this match has not been
        ;; processed already (this is handled by the 1st condition).
        (cond
          ;; Check for multiple branches by examining whether the source triple mapped to 
          ;; more than one target triple.
	  ;; **NOTE**: 08/22/05 - This condition use to come second, but we switched the
	  ;;		order. This change was done reduce the search space. See NOTE3 
	  ;;		above.
          ((and (elt remaining-matches (rest curr)) (> (length (match-body (first curr))) 1))
           (setf mhead (match-head (first curr)))
           (dolist (m (rest (match-body (first curr))))
             (setf search-stack
                   (cons (list result
                               (cons (cons (list mhead (list m)) (rest curr)) new-matches)
                               remaining-matches)
                         search-stack)))
           (setf curr (cons (list mhead (list (first (match-body (first curr))))) (rest curr))))
          ;; Check for multiple branches in the inverse direction - i.e. two (or more)
          ;; triples in the source graph map to the same triple in the target graph. 
	  ;; **NOTE**: 08/22/05 - We are looking for N-to-1 inverse mappings so we
	  ;;		can reduce the search space. See NOTE3 above and the note
	  ;;		associated with inverse-branches-exist?
          ((and (elt remaining-matches (rest curr)) (inverse-branches-exist? (first curr) new-matches remaining-matches))
           (setf mhead (match-head (first curr)))
           (dolist (m (rest (match-body (first curr))))
             (setf search-stack
                   (cons (list result
                               (cons (cons (list mhead (list m)) (rest curr))
                                     (remove-triple-from-search m new-matches))
                               remaining-matches)
                         search-stack)))
           (setf search-stack (cons (list result new-matches (rep nil remaining-matches (rest curr))) search-stack))
           (setf curr (cons (list mhead (list (first (match-body (first curr))))) (rest curr)))))
        ;; We're all set up to traverse the graph.
        (cond
	 ((elt remaining-matches (rest curr))
	  (setf temp (elt remaining-matches (rest curr)))
	  (setf remaining-matches (rep nil remaining-matches (rest curr)))
	  (setf remaining-matches (remove-triple-from-matches (first (match-body (first curr))) remaining-matches))
	  (setf new-match (list (match-head temp) (list (first (match-body (first curr))))))
	  (setf new-match (remove-inconsistent-matches new-match result))
	  (cond               
	   ;; If this new match is consistent, then add all the overlapping matches
	   ;; to the new-matches (i.e. the current search space).
	   ((match-body new-match)
	    (setf result (cons new-match result))
	    (setf new-matches
		  (append (find-all-overlapping-matches
			   (extract-mapping-pair (match-head new-match)   
						 (match-triple (first (match-body new-match)))
						 (cddr (first (match-body new-match))))
			   remaining-matches)
			  new-matches)))) )))
      (if (or (> (length result) 1) (not discard-unit-matches)) (setf final-result (cons result final-result))))))


;;;=====================================================================
;;;=====================================================================
;;; Supporting functions used by the find-all-matching-candidates to
;;; determine and remove inconsistent matches.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: This function takes as input a new match of the form
;; 
;;			(<head> <body>)
;;
;; 	 and a list of current matches of the form:
;;
;;		((<head_1> <body_1>) ... (<head_n> <body_n>))
;;
;;	 and removes all the elements in <body> that is inconsistent 
;;	 with the current set of matches. 
;; INPUT: new-match = (<head> <body>)
;;	  matches = ((<head_1> <body_1>) ... (<head_n> <body_n>))
;; OUTPUT: The result is a tuple of the form: 
;;
;;				(<head> <body'>)
;;----------------------------------------------------------------------
(defun remove-inconsistent-matches (new-match matches)
  (let ((head1 (match-head new-match)) (count 0) head2 mpair1 mpair2 flag new-body)
    ;; **NOTE**: 04/13/04 - This function now is only being called by find-all-matching-candidates.
    ;;		 As a result, the match body of new-match and match contain only one element. Hence, 
    ;;		 the loops (i.e. dolist) are not necessary, and we should rewrite this code to remove 
    ;;		 them in the future.
    (dolist (m (match-body new-match))	
      (setf mpair1 (extract-mapping-pair head1 (match-triple m) (cddr m)))
      (dolist (match matches)
	(setf head2 (match-head match) count 0)
	(dolist (mb (match-body match))
	  (setf mpair2 (extract-mapping-pair head2 (match-triple mb) (cddr mb)))
	  ;; See if there are any inconsistent mappings.
	  (cond
	    ((inconsistent-mpair (first mpair1) (first mpair2))
	     (setf flag (list (second mpair1) (second mpair2)) count (+ count 1)))
	    ((inconsistent-mpair (first mpair1) (second mpair2))
	     (setf flag (list (second mpair1) (first mpair2)) count (+ count 1)))
	    ((inconsistent-mpair (second mpair1) (first mpair2))
	     (setf flag (list (first mpair1) (second mpair2)) count (+ count 1)))
	    ((inconsistent-mpair (second mpair1) (second mpair2))
	     (setf flag (list (first mpair1) (first mpair2)) count (+ count 1)))))
	;; Make sure the inconsistency is not due to 1-to-N mappings in 
	;; the list of current matches.
	;;
	;; *NOTE*: 04/13/04 - This condition was to allow for 1-to-N mappings. We have since
	;;	   disallowed 1-to-N mappings and only allow for 1-to-1 mappings.
	;; (if (and flag (= count (length (match-body match)))) (return) (setf flag nil)))
	(if flag (return)))
      (if ;; (or 
	  (null flag) 
	      ;; **NOTE**: This condition allows for 1-to-N mappings, but this may not be
              ;;           suitable for the find-all-matching candidates function!	      
	      ;; **NOTE2**: 04/13/04 - This condition has been commented out to disallow 1-to-N mappings.
	      ;; (and flag (equal (triple-relation head1) (triple-relation head2)) (apply #'equal flag)))
	  (setf new-body (cons m new-body) flag nil)
	  (setf flag nil)))
    (list head1 new-body)))		

;;----------------------------------------------------------------------
;; DESC: Given two pairs of matched nodes, determine if they are 
;;	 inconsistent.
;; INPUT: p1 = (X A) 
;;	  p2 = (Y B)
;; OUTPUT: True if p1 and p2 are inconsistent -- i.e. X == Y & A != B 
;;	   or vice versa. NIL otherwise.
;;----------------------------------------------------------------------
(defun inconsistent-mpair (p1 p2)
  (or (and (equal (first p1) (first p2)) (not (equal (second p1) (second p2))))
      (and (not (equal (first p1) (first p2))) (equal (second p1) (second p2)))))

;;----------------------------------------------------------------------
;; DESC: Given two triples and a flag denoting whether or not they are
;;	 inverses of each other, extract the the mappings of between 
;;	 the concepts of the two triples.
;; INPUT: t1 = a triple.	
;; 	  t2 = a triple.
;;	  inv? (optional) = a number.
;; OUTPUT: The mappings between the nodes of t1 and t2.  For example, 
;;	   given (X r Y) and (A r B) return ((X A) (Y B)).
;;----------------------------------------------------------------------
(defun extract-mapping-pair (t1 t2 &optional (inv? 1))
  (if (> inv? 0) 
      (list (list (triple-head t1) (triple-head t2)) (list (triple-tail t1) (triple-tail t2)))
      (list (list (triple-head t1) (triple-tail t2)) (list (triple-tail t1) (triple-head t2)))))


;;======================================================================
;;======================================================================
;; Various functions for locating and removing other matches given a 
;; list of matches.
;;======================================================================
;;======================================================================

;;----------------------------------------------------------------------
;; DESC:
;; INPUT:
;; OUTPUT:
;; **NOTE**: 08/22/05 -- We changed the semantics of this function to
;;		detect N-to-1 mappings only. 
;;----------------------------------------------------------------------
(defun inverse-branches-exist? (head+body new-matches remaining-matches)
  (let ((source-triple   (match-head head+body))
        (matched-triples (match-body head+body))
        target-triple
        result)
    (dolist (matched-triple matched-triples)
      (setf target-triple (match-triple matched-triple))  
      (dolist (match+indx new-matches)
        (if (and (elt remaining-matches (rest match+indx))
                 (not (equal source-triple (match-head (first match+indx))))
		 ;; **NOTE**: 08/22/05 -- We removed this condition and added
		 ;;		the following two because we only want to catch
		 ;;		inverse branches involving N-to-1 mappings. Why?
		 ;;		To reduce the search space.
                 ;; (member target-triple (match-body (first match+indx))
                 ;;  :test (lambda (x y) (equal x (match-triple y))))
		 ;;
		 (eql (length (match-body (first match+indx))) 1)
		 (equal target-triple (match-triple (first (match-body (first match+indx))))))
            (progn (setf result t) (return))))
      (if result (return)))
    result))

;;----------------------------------------------------------------------
;; DESC: Given a pair of matched nodes, find all matches whose bindings
;;	 equal one of the given pairs.
;; INPUT: mpair    = ((X Y) (A B))
;;	  matches  = ((<head_1> <body_1>) ... (<head_n> <body_n>))
;; OUTPUT: Returns all the elements in matches that intersect mpair. 
;;	   The format of result is the same as matches.
;;----------------------------------------------------------------------
(defun find-all-overlapping-matches (mpair matches)
  (let ((count 0) result temp-result)
    (dolist (match matches) 
      (setf temp-result nil)
      (dolist (m (match-body match))
        (if (intersection (extract-mapping-pair (match-head match) (match-triple m) (cddr m)) mpair :test #'equal)
            (setf temp-result (cons m temp-result))))
      (if temp-result
          (setf result (cons (cons (list (match-head match) temp-result) count) result)))
      (setf count (+ count 1)))
    result))

;;----------------------------------------------------------------------
;; DESC: Given a key and a list of possible matches, remove all the 
;;	 elements from this list that key.
;; INPUT: key 	  = (<triple> <spec-pos> . x) 
;;	  matches = ((<head_1> <body_1>) ... (<head_n> <body_n>))
;; OUTPUT: 
;;
;; **NOTE**: Previous versions of this function allowed for 1-to-N 
;;	mappings (see simple-match.lisp.bkp101904), but we no longer 
;;	allow this.
;;----------------------------------------------------------------------
(defun remove-triple-from-matches (key matches)
  (let ((key-triple (match-triple key)) result)
    (mapcar
      #'(lambda (x)
          (cond
            (x
             (setf result (remove key-triple (match-body x) :test #'(lambda (y z) (equal y (match-triple z)))))
             (if result (list (match-head x) result) nil))
            (t nil)))
      matches)))

;;----------------------------------------------------------------------
;; DESC:
;; INPUT:
;; OUTPUT:
;;----------------------------------------------------------------------
(defun remove-triple-from-search (key matches+indx)
  (let ((key-triple (match-triple key))
        temp-result 
        final-result)         
    (dolist (match+indx matches+indx)               
      (setf temp-result (remove key-triple (match-body (first match+indx))
                          :test #'(lambda (x y) (equal x (match-triple y)))))
      (if temp-result
          (setf final-result (cons (cons (list (match-head (first match+indx)) temp-result) (rest match+indx)) final-result))))
    (reverse final-result)))


;;;=====================================================================
;;;=====================================================================
;;; Functions to count and find the best mappings.
;;; 
;;; ***NOTE***: These functions are note being used. 
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: Takes a list of matches of the form ( (<head <body>) ... ) 
;;	 and collect all the mapping and the count for that specific 
;;	 mapping. 
;; INPUT: matches = ((<head <body>) ... )
;; OUTPUT: The output is a list of the form 
;;
;;			( (((<c1> . x) (<c2> . y)) . N) ... )
;;----------------------------------------------------------------------
(defun collect-mapping-counts (matches)
  (let (result key1 key2 mhead)
    (dolist (match matches)
      (setf mhead (match-head match))
      (dolist (m (match-body match))
	(if (> (cddr m) 0)
	    (setf key1 (list (triple-head mhead) (triple-head (match-triple m)))
		  key2 (list (triple-tail mhead) (triple-tail (match-triple m))))
	    (setf key1 (list (triple-head mhead) (triple-tail (match-triple m)))
		  key2 (list (triple-tail mhead) (triple-head (match-triple m)))))
	(setf result (update-mapping-count key1 result))
	(setf result (update-mapping-count key2 result))))
    (mapcar #'(lambda (x) 
		(if (or (is-value-p (first (first x))) (is-instance-p (first (first x)))) 
		    (cons (first x) 1) 
		    x)) 
	    result)))

;;----------------------------------------------------------------------
;; DESC: Function used by collect-mapping-counts to update the count.
;;----------------------------------------------------------------------
(defun update-mapping-count (key elems &optional (examined nil))
  (cond 
    ((null elems)
     (cons (cons key 1) examined))
    ((equal key (caar elems))
     (append examined (cons (cons (caar elems) (+ (cdar elems) 1)) (rest elems))))
    (t 
     (update-mapping-count key (rest elems) (cons (first elems) examined)))))

;;----------------------------------------------------------------------
;; Takes two unflattened triples and determines if they have any nodes
;; in common.
;;
;; NOTE: Checks just the head or the tail (if inverse). This is the 
;;	 best way to go to eliminates undesirable 1-to-N mappings.
;;----------------------------------------------------------------------
(defun matching-nodes (m1 m2)
  (cond
    ((and (> (cddr m1) 0) (> (cddr m2) 0))
     (equal (triple-head (match-triple m1)) (triple-head (match-triple m2))))
    ((and (< (cddr m1) 0) (< (cddr m2) 0))
     (equal (triple-tail (match-triple m1)) (triple-tail (match-triple m2))))
    ((and (> (cddr m1) 0) (< (cddr m2) 0))
     (equal (triple-head (match-triple m1)) (triple-tail (match-triple m2))))
    ((and (< (cddr m1) 0) (> (cddr m2) 0))
     (equal (triple-tail (match-triple m1)) (triple-head (match-triple m2))))))		

;;----------------------------------------------------------------------
;; Takes a list of matches of the form
;;
;;		matches = ( (<head> <body>) ... )
;;
;; and list of mappings, return the fittest match. The format of the 
;; result is 
;;		(<multiple> <match> . <indx>)  
;; where 
;;		<multiple> = Boolean denoting if this is unique.
;;		<match>	   = The actual match. 
;;		<indx> 	   = The position of the match.
;;----------------------------------------------------------------------
(defun find-max (matches mappings)
  (let ((best-match '(() . 0.0)) (count 0) head score)
    (dolist (match matches)
      (setf head (match-head match))
      (dolist (m (match-body match))
	(setf score (find-max-compute-score head m mappings))
        (cond 
	  ((> score (rest best-match))
	   (setf best-match (cons (cons nil (cons (list head (list m)) count)) score)))
	  ((and (= score (rest best-match))
		(equal (match-triple m) (match-triple (first (match-body (cadar best-match))))))
	   (setf best-match (cons (cons t (cdar best-match)) (rest best-match))))))
      (setf count (+ count 1)))
    (first best-match)))

;;----------------------------------------------------------------------
;; Takes as input
;;
;;	head	  = <triple> that matches the elements in match-b.
;;      matches-b = ( ([<triple>|<chain>] <spec-pos> . y) ... )
;; 	mappings  = ( ((X Y) . N) ... )
;;
;; returns the match in the list with the highest score.
;;----------------------------------------------------------------------
(defun find-max-in-mbody (head matches-b mappings)
  (let ((best-match '(() . 0.0)) score)
    (dolist (m matches-b)
      (setf score (find-max-compute-score head m mappings))	
      (if (> score (rest best-match)) (setf best-match (cons m score))))
    (first best-match)))

;;----------------------------------------------------------------------
;; Auxiliary function used by the find-max functions to compute the 
;; score of a match wrt finding a candidate.
;;----------------------------------------------------------------------
(defun find-max-compute-score (head match mappings)
  (let ((score (match-score match)) 
	(pair1 (if (> (cddr match) 0)
		   (list (triple-head head) (triple-head (match-triple match))) 
		   (list (triple-head head) (triple-tail (match-triple match)))))
	(pair2 (if (> (cddr match) 0)
		   (list (triple-tail head) (triple-tail (match-triple match)))
		   (list (triple-tail head) (triple-head (match-triple match)))))
        mcount-h mcount-t)
    (setf mcount-h (cdar (member pair1 mappings :test #'(lambda (x y) (equal x (first y))))))
    (setf mcount-t (cdar (member pair2 mappings :test #'(lambda (x y) (equal x (first y))))))
    (* (+ mcount-t mcount-h) score)))


;;;=====================================================================
;;;=====================================================================
;;; Functions used to extract the elements of the output or triple.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; DESC: Given a mapping, extract the various parts of it.
;; INPUT: x = a list of the form: 
;;
;;		    ((<triple_src> <triple_trg>) .  <score>)
;; OUTPUT: The specified part of the mapping.
;;----------------------------------------------------------------------
(defun get-mapping 	  (x) (first  x))
(defun get-mapping-source (x) (first  (first x)))
(defun get-mapping-target (x) (second (first x)))
(defun get-mapping-score  (x) (rest   x))

;;----------------------------------------------------------------------
;; DESC: A individual match is internally represented in the form
;;			x = (head body)
;;	 where
;;		head = a triple.
;;		body = a list of the form:
;;			 ((<triple_1> <spec> . <score>) ... )
;;----------------------------------------------------------------------
(defun match-head (x) (car x))
(defun match-body (x) (cadr x))

;;----------------------------------------------------------------------
;; DESC: An second set of functions to extract the triple and score
;; 	of an individual match. 
;; INPUT: The internal representation (i.e. x) is of the for:
;;
;;		([<chain> | <triple>] <spec-pos> . z)
;; 		where
;;			<triple> = ((o1 . x) r (o2 . y))
;; OUTPUT: 
;;----------------------------------------------------------------------
(defun match-score 	(x) (abs (cddr x)))
(defun match-triple 	(x) (car x))
(defun match-spec	(x) (cadr x))
(defun match-spec-head 	(x) (caadr x))
(defun match-spec-tail 	(x) (cdadr x))


;;----------------------------------------------------------------------
;; DESC: The following functions are used to extract the parts of 
;;	 a triple.
;; INPUT: triple = a triple.
;; OUTPUT: The specified part of the triple is returned.
;;----------------------------------------------------------------------
(defun triple-head 	(triple) (car triple))
(defun triple-tail 	(triple) (caddr triple))
(defun triple-relation 	(triple) (cadr triple))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;[jchaw-patch, 20Aug08, Changed (node-type ...) to be (semantic-matcher-node-type)
;;This is to work-around similar function name inside cpl/system/util.lisp
;;See HLO-2369
(defun semantic-matcher-node-type (node) (if (consp node) (first node)))

;;----------------------------------------------------------------------
;; DESC: Given the mappings between the source and target graph, gather 
;;	 those triples from the source graph.
;; INPUT: mappings = a list of matched triples.
;; OUTPUT: A list of triples.
;;----------------------------------------------------------------------
(defun get-source-graph-from-mappings (mappings)
  (let (source-triple result)
    (dolist (mapping mappings)
      (setf source-triple (get-mapping-source mapping))  
      (if (path-p source-triple)
          (setf result (append source-triple result))
          (setf result (cons   source-triple result))))
    result))

;;----------------------------------------------------------------------
;; DESC: Given the mappings between the source and target graph, gather 
;;	 those triples from the target graph.
;; INPUT: mappings = a list of matched triples.
;; OUTPUT: A list of triples.
;;----------------------------------------------------------------------
(defun get-target-graph-from-mappings (mappings)
  (let (target-triple result)
    (dolist (mapping mappings)
      (setf target-triple (get-mapping-target mapping))
      (if (path-p target-triple)
          (setf result (append target-triple result))
          (setf result (cons   target-triple result))))
    result))


;;;=====================================================================
;;;=====================================================================
;;; Some misc. functions.
;;;=====================================================================
;;;=====================================================================

;;----------------------------------------------------------------------
;; Given a triple or list of triple return the number of 
;; triples.
;;----------------------------------------------------------------------
(defun path-length (p) (if (path-p p) (length p) 1))

;;----------------------------------------------------------------------
;; Function to determine whether the given element is a path.
;; A path is a sequence of triples.
;;----------------------------------------------------------------------
(defun path-p (x)
  (or (<= (length x) 2) (> (length x) 3) (and (consp (second x)) (= (length (second x)) 3))))

;;----------------------------------------------------------------------
;; DESC: This function will replace the element at the specified 
;;	 position with "needle".
;; INPUT:
;; OUTPUT:
;;----------------------------------------------------------------------
(defun rep (needle haystack pos)
  (substitute-if needle #'(lambda (x) t) haystack :start pos :end (+ pos 1)))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defun find-all (item elems &key (test #'equal))
  (let (result)
    (dolist (elem elems result)
      (if (funcall test item elem) (setf result (cons elem result))))))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(defun flatten-triple (triple)
  (let ((head (car triple)) (tail (caddr triple)))
    (list (if (atom (rest head)) (first head) head) (cadr triple) (if (atom (rest tail)) (first tail) tail))))

;;;;====================================================================
;;;;====================================================================
;;;; FILE: check-match.lisp
;;;;
;;;; This file contains a library of functions to check whether a 
;;;; match is valid. Our notion of validity is based on semantic
;;;; validity.
;;;;====================================================================
;;;;====================================================================

;;;=====================================================================
;;;=====================================================================
;;; Below is a list of semantic match constraints that must be
;;; satisfied in order for a match to make sense - i.e. be a valid
;;; match. 
;;; 
;;; Each entry in this list is of the form:
;;;
;;;	    (<class> (<relation_1> ... <relation_n>) <test-fn>)
;;;
;;; 	where
;;;	<class> = the class of concepts this match constraint 
;;;		  applies to.
;;;	<relation_1> = relations we are interested in testing.
;;;	<test-fn> = the test to perform on these relations to decide
;;;		whether they (more precisely their values) satisfy the
;;;		semantic constraint.
;;;=====================================================================
;;;=====================================================================
(defparameter *semantic-match-constraints* '(
;;  (|Property-Value| (|value|) 	prop-val-eql)
;;  (|Time-Instant|   (|time-value|)	prop-val-eql)
))

;;;=====================================================================
;;;=====================================================================
;;; Top level functions
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: This function takes the matches between two graphs (G1 and  G2), 
;;	 and checks which matches make sense according to the semantic 
;;	 match constraints specified in *semantic-match-constraints*. 
;;	 Those matches which do not make sense are removed. This
;;	 function is a wrapper that iterates through the matches 
;;	 between G1 and G2. The actual work is done by calling the 
;;	 function check-match-makes-sense.
;; INPUT: matches = a list of the matches between graph1 and graph2.
;;	  graph1 = a list of triples.
;;	  graph2 = a list of triples.
;;	  semantic-match-constraints (optional) = a list of semantic 
;;		match constraints.
;;	  htable-g1-indx-head (optional) = this hash table stores the 
;;		triples in graph1 indexed on their head node.
;;	  htable-g1-indx-tail (optional) = this hash table stores the 
;;		triples in graph1 indexed on their tail node.
;;	  htable-g2-indx-head (optional) = this hash table stores the 
;;		triples in graph2 indexed on their head node.
;;	  htable-g2-indx-tail (optional) = this hash table stores the 
;;		triples in graph2 indexed on their tail node.
;;	  htable-km-to-node (optional) = this hash table stores the
;;		graph node that a KM node maps to.
;;	  htable-node-to-km (optional) = this hash table stores the
;;		km node that a graph node maps to.
;; OUTPUT: The output is list containing those matches from the input
;;	parameter "matches" that satisfy the applicable semantic match
;;	constraints defined in semantic-match-constraints.
;;======================================================================
(defun check-matches-make-sense 
       (matches graph1 graph2 
	&optional 
	 (semantic-match-constraints *semantic-match-constraints*)
         (htable-g1-indx-head *htable-g1-indexed-by-head*)
         (htable-g1-indx-tail *htable-g1-indexed-by-tail*)
         (htable-g2-indx-head *htable-g2-indexed-by-head*)
         (htable-g2-indx-tail *htable-g2-indexed-by-tail*)
	 (htable-km-to-node   *km-to-node-mappings*)
	 (htable-node-to-km   *node-to-km-mappings*))
  (let (result)
    ;; Check to make sure that both graph1 and graph2 are indexed. Otherwise, 
    ;; index them.
    (if (= (hash-table-count htable-g1-indx-head) 0) (hash-graph-on-head graph1 htable-g1-indx-head))
    (if (= (hash-table-count htable-g1-indx-tail) 0) (hash-graph-on-tail graph1 htable-g1-indx-tail))
    (if (= (hash-table-count htable-g2-indx-head) 0) (hash-graph-on-head graph2 htable-g2-indx-head))
    (if (= (hash-table-count htable-g2-indx-tail) 0) (hash-graph-on-tail graph2 htable-g2-indx-tail))
    (dolist (match matches)
      (if (check-match-makes-sense match semantic-match-constraints
		htable-g1-indx-head htable-g1-indx-tail
		htable-g2-indx-head htable-g2-indx-tail
		htable-km-to-node   htable-node-to-km)
	  (setf result (cons match result))))
    result))


;;======================================================================
;; DESC: This function takes a single match and checks whether 
;;	 this match "makes sense". We say a match makes sense if it 
;;	 satisfies all the applicable semantic match constraints 
;;	 defined in *semantic-match-constraints*
;; INPUT: match = a list of mappings.
;;	  semantic-match-constraints (optional) = a list of semantic
;;		match constraints.
;;	  htable-g1-indx-head (optional) = see "check-matches-make-sense"
;;	  htable-g1-indx-tail (optional) = see "check-matches-make-sense"
;;	  htable-g2-indx-head (optional) = see "check-matches-make-sense"
;;	  htable-g2-indx-tail (optional) = see "check-matches-make-sense"
;;	  htable-km-to-node (optional)   = see "check-matches-make-sense"
;; 	  htable-node-to-km (optional)   = see "check-matches-make-sense"
;; OUTPUT: The output is either T or nil. T if "match" satisfies all 
;;	the applicable match constraints in semantic-match-constraints.
;; 	Nil otherwise. Note, if there are no applicable constraints, 
;;	then this function will return T by default.
;;======================================================================
(defun check-match-makes-sense (match &optional 
	 				(semantic-match-constraints *semantic-match-constraints*)
         				(htable-g1-indx-head *htable-g1-indexed-by-head*)
         				(htable-g1-indx-tail *htable-g1-indexed-by-tail*)
         				(htable-g2-indx-head *htable-g2-indexed-by-head*)
         				(htable-g2-indx-tail *htable-g2-indexed-by-tail*)
	 				(htable-km-to-node   *km-to-node-mappings*)
	 				(htable-node-to-km   *node-to-km-mappings*))
  (let ((makes-sense? t) g1-matched-triples g2-matched-triples node-mappings)
    ;; The first step is to collect three pieces of info:
    ;;	1) the node mappings.
    ;;  2) the matched triples in graph1.
    ;;  3) the matched triples in graph2.
    (dolist (mapping match)
      ;; get the node bindings for this mapping
      (setf node-mappings (union (get-node-bindings-for-mapping mapping) node-mappings :test #'equal))
      ;; Get the matched triples for G1.
      (if (path-p (get-mapping-source mapping))
	  (setf g1-matched-triples (append (get-mapping-source mapping) g1-matched-triples))
	  (setf g1-matched-triples (cons   (get-mapping-source mapping) g1-matched-triples)))
      ;; Get the matched triples for G2.
      (if (path-p (get-mapping-target mapping))
	  (setf g2-matched-triples (append (get-mapping-target mapping) g2-matched-triples))
	  (setf g2-matched-triples (cons   (get-mapping-target mapping) g2-matched-triples))))
    ;; Now iterate through each of the semantic match constraints for each pair of node mappings.
    (dolist (semantic-constraint semantic-match-constraints)
      (dolist (node-mapping node-mappings)
	(cond 
	  ((and (does-constraint-apply? semantic-constraint (first  node-mapping))
	        (does-constraint-apply? semantic-constraint (second node-mapping)))
	   (if (not (check-if-sem-match-constraint-satisfied
		      semantic-constraint node-mapping g1-matched-triples g2-matched-triples
		      htable-g1-indx-head htable-g1-indx-tail
		      htable-g2-indx-head htable-g2-indx-tail
		      htable-km-to-node   htable-node-to-km))
	       (progn (setf makes-sense? nil) (return))))))
      (if (not makes-sense?) (return)))
    makes-sense?))


;;======================================================================
;; DESC: This function checks whether a pair of node bindings satisfies
;;	 the given semantic constraint. This function assumes that the 
;;	 semantic constraint in question is applicable to the node pair.
;; INPUT: sem-constraint = the semantic constraint to check.
;;	  node-mapping = the pair of nodes to check sem-constraint 
;;		against. This pair is of the form: (<node_1> <node_2>)
;;		e.g. ((|Dog| . 1) (|Dog| . 2))
;;	  g1-matched-triples = a list of the matched triples in g1.
;;	  g2-matched-triples = a list of the matched triples in g2.
;;	  htable-g1-indx-head = this hash table stores the triples in
;;		g1 indexed on the head node.
;;	  htable-g1-indx-tail = this hash table stores the triples in
;;		g1 indexed on the tail node.
;;	  htable-g2-indx-head = this hash table stores the triples in
;;		g2 indexed on the head node.
;;	  htable-g2-indx-tail = this hash table stores the triples in
;;		g2 indexed on the tail node.
;;	  km-to-node-mappings = this hash table stores the graph node
;;		that a km node maps to.
;;	  node-to-km-mappings = this hash table stores the km node 
;;		that a graph node maps to.
;; OUTPUT: T is returned if node-mapping satisfies sem-constraint. 
;;	   NIL is returned otherwise.
;;======================================================================
(defun check-if-sem-match-constraint-satisfied 
       (sem-constraint node-mapping g1-matched-triples g2-matched-triples 
	htable-g1-indx-head htable-g1-indx-tail 
	htable-g2-indx-head htable-g2-indx-tail
	km-to-node-mappings node-to-km-mappings)
  (let ((test-fn (semantic-match-constraint-test sem-constraint))
	(g1-node (first  node-mapping)) 
	(g2-node (second node-mapping)) 
	(satisfied? t)
	g1-triples-to-check 
	g2-triples-to-check 
	g1-val)
    ;; Iterate through each relation in the semantic constraint.
    (dolist (relation (semantic-match-constraint-relation sem-constraint))
      ;; Get all the unmatched triples extending from the matched nodes
      (setf g1-triples-to-check 
	    (find-unmatched-triples-extending-from-node
 	      g1-node relation g1-matched-triples htable-g1-indx-head htable-g1-indx-tail))
      (setf g2-triples-to-check 
	    (find-unmatched-triples-extending-from-node 
	      g2-node relation g2-matched-triples htable-g2-indx-head htable-g2-indx-tail))
      ;; If either g1 or g2 is null, then query KM to see if the relation in question exists.
      (if (null g1-triples-to-check)
	  (setf g1-triples-to-check (query-km-for-relation g1-node relation km-to-node-mappings node-to-km-mappings)))
      (if (null g2-triples-to-check)
	  (setf g2-triples-to-check (query-km-for-relation g2-node relation km-to-node-mappings node-to-km-mappings)))
      ;; Test the tail node of each triple in g1 with the tail node of each triple in g2.
      (dolist (g1-triple g1-triples-to-check)
	(setf g1-val (triple-tail g1-triple))
	(dolist (g2-triple g2-triples-to-check)
	  (if (not (funcall test-fn g1-val (triple-tail g2-triple))) (progn (setf satisfied? nil) (return))))
	(if (not satisfied?) (return)))
      (if (not satisfied?) (return))) 
    satisfied?))


;;======================================================================
;; DESC: This function tests whether a semantic match constraint applies 
;;	 to a graph node. A constraint applies to a node if the node
;;	 is subsumed by the class field of the constraint.
;; INPUT: sem-constraint = a semantic constraint which is of the form:
;;		(<class> (<relation_1> ... <relation_n>) <test-fn>)
;;	  node = a graph node - e.g. (|Dog| . 123)
;; OUTPUT: T if sem-constraint applies to node. NIL otherwise.
;;======================================================================
;;[jchaw-patch, 20Aug08, Changed (node-type ...) to be (semantic-matcher-node-type)
(defun does-constraint-apply? (sem-constraint node)
  (let ((sem-class (semantic-match-constraint-class sem-constraint)) class-type)
    (cond
      ((is-value-p node)
       nil)
      ((is-instance-p node)
       (if (isa (first node) sem-class) t nil))
      (t
       (setf class-type (semantic-matcher-node-type node))
       (dolist (ctype (if (consp class-type) class-type (list class-type)))
	 (if (is-subclass-of ctype sem-class)
	     (return t)))))))


;;;=====================================================================
;;;=====================================================================
;;; Auxiliary functions to extract the various fields of a semantic
;;; constraint - See *semantic-match-constraints* for a description
;;; on the structure of these semantic match constraints.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: 
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun semantic-match-constraint-class 	  (semantic-constraint) (first  semantic-constraint))
(defun semantic-match-constraint-relation (semantic-constraint) (second semantic-constraint))
(defun semantic-match-constraint-test 	  (semantic-constraint) (third  semantic-constraint))
  

;;;=====================================================================
;;;=====================================================================
;;; Definitions of those test functions referenced in 
;;; *semantic-match-constraints*.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Takes two properties values and tests whether they are equal.
;; INPUT: pval1 = a property value - e.g. (:pair 270.95 *usdollar)
;;	  pval2 = a property value. 
;; OUTPUT: T if pval1 and pval2 are equivalent. NIL otherwise.
;;
;; **NOTE**: Currently, this function does a straightforward test of
;;	equality between the values given to it. We need, however, to 
;;	extend this function to perform unit conversion.
;;======================================================================
(defun prop-val-eql (pval1 pval2) (equal pval1 pval2))
;;;;====================================================================
;;;;====================================================================
;;;; FILE: transformation-rules.lisp
;;;;====================================================================
;;;;====================================================================

;;;=====================================================================
;;;=====================================================================
;;; Domain neutral transformations we've enumerated from our upper 
;;; ontology.
;;;=====================================================================
;;;=====================================================================
(defparameter *domain-neutral-transformations*  '(

	;;===============================================================
	;;===============================================================
	;; Transformations for Events.
	;;===============================================================
	;;=============================================================== 
 	((((|Event| . 1)  |causes| 		(|Event| . 2))
   	  ((|Event| . 2)  |causes| 		(|Event| . 3)))
  	 (((|Event| . 1)  |causes| 		(|Event| . 3))))

	;;jchaw
 	((((|Thing| . 1)  |related-to| 		(|Thing| . 2))
   	  ((|Thing| . 2)  |related-to| 		(|Thing| . 3)))
  	 (((|Thing| . 1)  |related-to| 		(|Thing| . 3))))

 	((((|Event| . 1)  |causes| 		(|Event| . 2))
   	  ((|Event| . 2)  |subevent| 		(|Event| . 3)))
	 (((|Event| . 1)  |causes| 		(|Event| . 3))))
 	((((|Event| . 1)  |causes| 		(|Event| . 2))
	  ((|Event| . 2)  |resulting-state| 	(|State| . 3)))
	 (((|Event| . 1)  |causes| 		(|State| . 3))))
 	((((|Event| . 1)  |caused-by| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |caused-by| 		(|Event| . 3))))
 	((((|Event| . 1)  |caused-by| 		(|Event| . 2))
 	  ((|Event| . 2)  |resulting-state-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |caused-by| 		(|Event| . 3))))
 	((((|State| . 1)  |defeated-by| 	(|Action| . 2))
 	  ((|Action| . 2) |caused-by| 		(|Event| . 3)))
 	 (((|State| . 1)  |defeated-by| 	(|Event| . 3))))
	((((|State| . 1)  |defeated-by| 	(|Action| . 2))
 	  ((|Action| . 2) |subevent-of| 	(|Event| . 3)))
 	 (((|State| . 1)  |defeated-by| 	(|Event| . 3))))

	((((|Event| . 1)  |enables| 		(|Event| . 2))
 	  ((|Event| . 2)  |enables| 		(|Event| . 3)))
 	 (((|Event| . 1)  |enables| 		(|Event| . 3))))
 	((((|Event| . 1)  |enables| 		(|Event| . 2))
 	  ((|Event| . 2)  |causes| 		(|Event| . 3)))
 	 (((|Event| . 1)  |enables| 		(|Event| . 3))))
 	((((|Event| . 1)  |enables| 		(|Event| . 2))
 	  ((|Event| . 2)  |resulting-state| 	(|State| . 3)))
 	 (((|Event| . 1)  |enables| 		(|State| . 3))))
 	((((|Event| . 1)  |enables| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent| 		(|Event| . 3)))
 	 (((|Event| . 1)  |enables| 		(|Event| . 3))))
 	((((|Event| . 1)  |enables| 		(|Event| . 2))		;; <- ??
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |enables| 		(|Event| . 3))))
 	((((|Event| . 1)  |enabled-by| 		(|Event| . 2))
 	  ((|Event| . 2)  |caused-by| 		(|Event| . 3)))
 	 (((|Event| . 1)  |enabled-by| 		(|Event| . 3))))
 	((((|Event| . 1)  |enabled-by| 		(|Event| . 2))
 	  ((|Event| . 2)  |resulting-state-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |enabled-by| 		(|Event| . 3))))
 	((((|Event| . 1)  |enabled-by| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |enabled-by| 		(|Event| . 3))))

 	((((|Event| . 1)  |inhibits| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |inhibits| 		(|Event| . 3))))
 	((((|Event| . 1)  |inhibits| 		(|Event| . 2))
 	  ((|Event| . 2)  |resulting-state| 	(|State| . 3)))
 	 (((|Event| . 1)  |inhibits| 		(|State| . 3))))
 	((((|Event| . 1)  |inhibited-by| 	(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |inhibited-by|	(|Event| . 3))))
 	((((|Event| . 1)  |inhibited-by| 	(|Event| . 2))
 	  ((|Event| . 2)  |caused-by| 		(|Event| . 3)))
 	 (((|Event| . 1)  |inhibited-by| 	(|Event| . 3))))
 	((((|Event| . 1)  |inhibited-by| 	(|Event| . 2))
  	  ((|Event| . 2)  |resulting-state-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |inhibited-by| 	(|Event| . 3))))

 	((((|Event| . 1)  |by-means-of| 	(|Event| . 2))
 	  ((|Event| . 2)  |by-means-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |by-means-of| 	(|Event| . 3))))

 	((((|Event| . 1)  |prevents| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |prevents| 		(|Event| . 3))))
 	((((|Event| . 1)  |prevented-by| 	(|Event| . 2))
 	  ((|Event| . 2)  |subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |prevented-by| 	(|Event| . 3))))
 	((((|Event| . 1)  |prevented-by| 	(|Event| . 2))
 	  ((|Event| . 2)  |caused-by| 		(|Event| . 3)))
 	 (((|Event| . 1)  |prevented-by| 	(|Event| . 3))))
 	((((|Event| . 1)  |prevented-by| 	(|Event| . 2))
 	  ((|Event| . 2)  |resulting-state-of| 	(|Event| . 3)))
 	 (((|Event| . 1)  |prevented-by| 	(|Event| . 3))))
 
	((((|Event| . 1)  |subevent| 		(|Event| . 2))
 	  ((|Event| . 2)  |subevent| 		(|Event| . 3)))
 	 (((|Event| . 1)  |subevent| 		(|Event| . 3))))

	;;===============================================================
	;;===============================================================
	;; Transformations for case roles.
	;;===============================================================
	;;===============================================================
 	((((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-part| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|away-from| 	(|Entity| . 3))))
 	((((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-region| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|away-from| 	(|Place| . 3))))
 	((((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 3))))
	((((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|away-from| 	(|Spatial-Entity| . 3))))

 	((((|Event| . 1) 		|base| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|base| 		(|Entity| . 3))))
 	((((|Event| . 1) 		|base| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|base| 		(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|base| 		(|Event| . 2))
 	  ((|Event| . 2) 		|subevent-of| 	(|Event| . 3)))
 	 (((|Event| . 1) 		|base| 		(|Event| . 3))))

 	((((|Event| . 1) 		|beneficiary| 	(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|beneficiary| 	(|Entity| . 3))))
 	((((|Event| . 1) 		|beneficiary| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|beneficiary| 	(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|beneficiary| 	(|Aggregate| . 2))
 	  ((|Aggregate| . 2) 		|element| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|beneficiary| 	(|Entity| . 3))))

 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Place| . 3))))
 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-part-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-inside| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|destination| 	(|Spatial-Entity| . 3))))

 	((((|Event| . 1) 		|instrument| 	(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|instrument| 	(|Entity| . 3))))

 	((((|Thing| . 1) 		|location| 	(|Place| . 2))
 	  ((|Place| . 2) 		|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Thing| . 1) 		|location| 	(|Tangible-Entity| . 3))))
 	((((|Place| . 1) 		|location-of| 	(|Event| . 2))
 	  ((|Event| . 2) 		|subevent| 	(|Event| . 3)))
 	 (((|Place| . 1) 		|location-of| 	(|Event| . 3))))

 	((((|Event| . 1) 		|object| 	(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|object| 	(|Entity| . 3))))
 	((((|Event| . 1) 		|object| 	(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|object| 	(|Tangible-Entity| . 3))))

 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Place| . 3))))
 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Entity| . 3))))
 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-inside| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|origin| 	(|Spatial-Entity| . 3))))

 	((((|Event| . 1) 		|path| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|path| 		(|Place| . 3))))
 	((((|Event| . 1) 		|path| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|path| 		(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|path| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|path| 		(|Entity| . 3))))
	((((|Event| . 1) 		|path| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|path| 		(|Tangible-Entity| . 3))))

 	((((|Event| . 1) 		|raw-material| 	(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|has-part| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|raw-material| 	(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|raw-material| 	(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|material| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|raw-material| 	(|Tangible-Entity| . 3))))
 	((((|Event| . 1) 		|raw-material| 	(|Aggregate| . 2))
 	  ((|Aggregate| . 2) 		|element| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|raw-material| 	(|Tangible-Entity| . 3))))

 	((((|Entity| . 1) 		|result-of| 	(|Event| . 2))
 	  ((|Event| . 2) 		|subevent-of| 	(|Event| . 3)))
 	 (((|Entity| . 1) 		|result-of| 	(|Event| . 3))))

 	((((|Event| . 1) 		|site| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|site| 		(|Place| . 3))))
 	((((|Event| . 1) 		|site| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-part-of| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|site| 		(|Entity| . 3))))
 	((((|Event| . 1) 		|site| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-region-of| 	(|Tangible-Entity| . 3)))
 	 (((|Event| . 1) 		|site| 		(|Tangible-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|site-of| 	(|Event| . 2))
 	  ((|Event| . 2) 		|subevent| 	(|Event| . 3)))
 	 (((|Spatial-Entity| . 1) 	|site-of| 	(|Event| . 3))))

 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Place| . 3))))
 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-part| 	(|Entity| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Entity| . 3))))
 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-region| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 3))))
 	((((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-near| 	(|Spatial-Entity| . 3)))
 	 (((|Event| . 1) 		|toward| 	(|Spatial-Entity| . 3))))

        ((((|Event|  . 0) 		|site|      	(|Place| . 1)))
         (((|Event|  . 0) 		|location|   	(|Place| . 1))))
        ((((|Event|  . 0) 		|location|   	(|Place| . 1)))
         (((|Event|  . 0) 		|site|      	(|Place| . 1))))


	;;===============================================================
	;;===============================================================
	;; Roles.
	;;===============================================================
	;;===============================================================
 	((((|Role| . 1) 		|played-by| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-part-of| 		(|Entity| . 3)))
 	 (((|Role| . 1) 		|played-by| 		(|Entity| . 3))))
 	((((|Role| . 1) 		|played-by| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|is-region-of| 		(|Tangible-Entity| . 3)))
 	 (((|Role| . 1) 		|played-by| 		(|Tangible-Entity| . 3))))

 	((((|Role| . 1) 		|in-event| 		(|Event| . 2))
 	  ((|Event| . 2) 		|subevent-of| 		(|Event| . 3)))
 	 (((|Role| . 1) 		|in-event| 		(|Event| . 3))))

 	((((|Container| . 1) 		|content| 		(|Entity| . 2))		;; <- ??
 	  ((|Entity| . 2) 		|content| 		(|Entity| . 3)))
 	 (((|Container| . 1) 		|content| 		(|Entity| . 3))))
 	((((|Container| . 1) 		|content| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|has-part| 		(|Entity| . 3)))
 	 (((|Container| . 1) 		|content| 		(|Entity| . 3))))
 	((((|Container| . 1) 		|content| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|has-region| 		(|Spatial-Entity| . 3)))
 	 (((|Container| . 1) 		|content| 		(|Spatial-Entity| . 3))))
 	((((|Container| . 1) 		|content| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|material| 		(|Tangible-Entity| . 3)))
 	 (((|Container| . 1) 		|content| 		(|Tangible-Entity| . 3))))
 	((((|Container| . 1) 		|content| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 		(|Spatial-Entity| . 3)))
 	 (((|Container| . 1) 		|content| 		(|Spatial-Entity| . 3))))

	;;===============================================================
        ;;===============================================================
        ;; Rewrites for Entity to Entity relations.
        ;;===============================================================
        ;;===============================================================
	;; **NOTE**: Need to fix these!
 	((((|Entity| . 1) 		|has-part| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|has-part| 		(|Entity| . 3)))
 	 (((|Entity| . 1) 		|has-part| 		(|Entity| . 3))))
	;; Was Tangible-Entity, but has been changed to Entity.
	;;
 	;; ((((|Entity| . 1) 		|has-functional-part| 	(|Entity| . 2)))
 	;;  (((|Entity| . 1) 		|has-part| 		(|Entity| . 2))))
 	;; ((((|Entity| . 1) 		|has-structural-part|	(|Entity| . 2)))
 	;;  (((|Entity| . 1) 		|has-part| 		(|Entity| . 2))))
	;;
 	((((|Entity| . 1) 		|has-basic-structural-unit| (|Entity| . 2)))
 	 (((|Entity| . 1) 		|has-part| 		(|Entity| . 2))))
 	((((|Entity| . 1) 		|has-basic-functional-unit| (|Entity| . 2)))
  	 (((|Entity| . 1) 		|has-part| 		(|Entity| . 2))))

 	((((|Tangible-Entity| . 1) 	|has-region| 		(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-region| 		(|Spatial-Entity| . 3)))
 	 (((|Tangible-Entity| . 1) 	|has-region| 		(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-region-of| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|is-part-of| 		(|Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-region-of| 		(|Entity| . 3))))

 	((((|Thing| . 1) 		|location| 		(|Place| . 2))
  	  ((|Place| . 2) 		|location| 		(|Place| . 3)))
  	 (((|Thing| . 1) 		|location| 		(|Place| . 3))))
#| *PZY*: 07/26/05 -- causing problems.
 	((((|Place| . 1) 		|location-of| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|has-part| 		(|Entity| . 3)))
 	 (((|Place| . 1) 		|location-of| 		(|Entity| . 3))))
|#
 	((((|Place| . 1) 		|location-of| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|has-region| 		(|Spatial-Entity| . 3)))
 	 (((|Place| . 1) 		|location-of| 		(|Spatial-Entity| . 3))))
 	((((|Place| . 1) 		|location-of| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|material| 		(|Tangible-Entity| . 3)))
 	 (((|Place| . 1) 		|location-of| 		(|Tangible-Entity| . 3))))

 	((((|Tangible-Entity| . 1) 	|material| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|material| 		(|Tangible-Entity| . 3)))
 	 (((|Tangible-Entity| . 1) 	|material| 		(|Tangible-Entity| . 3))))
 	((((|Tangible-Entity| . 1) 	|material-of| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|is-part-of| 		(|Entity| . 3)))
 	 (((|Tangible-Entity| . 1) 	|material-of| 		(|Entity| . 3))))

 	((((|Aggregate| . 1) 		|element| 		(|Thing| . 2))
 	  ((|Thing| . 2) 		|element| 		(|Thing| . 3)))
 	 (((|Aggregate| . 1) 		|element| 		(|Thing| . 3))))
 	((((|Aggregate| . 1) 		|first-element| 	(|Entity| . 2))
 	  ((|Entity| . 2) 		|first-element| 	(|Entity| . 3)))
 	 (((|Aggregate| . 1) 		|first-element| 	(|Entity| . 3))))

 	((((|Entity| . 1) 		|possesses| 		(|Entity| . 2))
 	  ((|Entity| . 2) 		|has-part| 		(|Entity| . 3)))
 	 (((|Entity| . 1) 		|possesses| 		(|Entity| . 3))))
 	((((|Entity| . 1) 		|possesses| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|has-region|	 	(|Spatial-Entity| . 3)))
 	 (((|Entity| . 1) 		|possesses| 		(|Spatial-Entity| . 3))))
 	((((|Entity| . 1) 		|possesses| 		(|Tangible-Entity| . 2))
 	  ((|Tangible-Entity| . 2) 	|material| 		(|Tangible-Entity| . 3)))
 	 (((|Entity| . 1) 		|possesses| 		(|Tangible-Entity| . 3))))
 	((((|Entity| . 1) 		|possesses| 		(|Aggregate| . 2))
 	  ((|Aggregate| . 2) 		|element| 		(|Thing| . 3)))
 	 (((|Entity| . 1) 		|possesses| 		(|Thing| . 3))))

	;;===============================================================
        ;;===============================================================
        ;; Transformations for space.
        ;;===============================================================
        ;;===============================================================
 	((((|Spatial-Entity| . 1) 	|abuts| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|abuts| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|abuts| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|abuts| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-above| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-on| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above|	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-over| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-above| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-under| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-below| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-along| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-along| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-inside| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at-of| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 2))
  	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
  	 (((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-at-of| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-beside| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-beside| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-between| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-between| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-between| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-between| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-behind| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-behind| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	 (|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-along| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 2))	;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	 (|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-in-front-of| (|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-inside| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-inside| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element-of| 	(|Aggregate| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-inside| 	(|Aggregate| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-part-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-inside| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|abuts| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-along| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-on| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|encloses| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-near| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|abuts| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-along| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-beside| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-between| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
  	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
  	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-on| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-near| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-on| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-on| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-on| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-on| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-on| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-on| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|has-on-it| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|has-on-it| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|has-on-it| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|has-on-it| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|has-on-it| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|has-on-it| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-opposite| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-opposite| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-opposite| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-opposite| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-opposite| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-opposite| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-outside| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-at| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-outside| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) |location| 		(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) |location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) |element| 		(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) |element-of| 		(|Aggregate| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Aggregate| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) |abuts| 		(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) |encloses| 		(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) |is-at| 		(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) |does-not-enclose| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-over| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|is-on| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-over| 	(|Spatial-Entity| . 3))))

 	((((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location| 	(|Place| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-under| 	(|Place| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|location-of| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 2))		;; <- ??
 	  ((|Spatial-Entity| . 2) 	|element| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|encloses| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 3))))
 	((((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 2))
 	  ((|Spatial-Entity| . 2) 	|has-on-it| 	(|Spatial-Entity| . 3)))
 	 (((|Spatial-Entity| . 1) 	|is-under| 	(|Spatial-Entity| . 3))))

        ((((|Spatial-Entity|  . 0) 	|is-at|      	(|Place| . 1)))
         (((|Spatial-Entity|  . 0) 	|location|   	(|Place| . 1))))
        ((((|Spatial-Entity|  . 0) 	|location|   	(|Place| . 1)))
         (((|Spatial-Entity|  . 0) 	|is-at|      	(|Place| . 1))))

	;;===============================================================  
        ;;===============================================================  
        ;; Temporal transformations.
        ;;===============================================================  
        ;;===============================================================
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |before| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Event| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-starts| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-started-by| (|Time-Interval| . 3)))
	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-meets| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |before| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-contains| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |before| 		(|Time-Interval| . 3))))

 	((((|Time-Interval| . 1) |after| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |after| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |after| 		(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |after| 		(|Event| . 3))))
 	((((|Time-Interval| . 1) |after| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-met-by| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |after| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |after| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-started-by| (|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |after| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |after| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-finished-by| (|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |after| 		(|Time-Interval| . 3))))

 	((((|Time-Interval| . 1) |during| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |during| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |during| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |during| 		(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |during| 		(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |during| 		(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |during| 		(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-contains| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-contains| 	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-contains| 	(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-contains| 	(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-overlaps| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-overlaps|	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-overlaps| 	(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-overlaps| 	(|Event| . 3))))
 	((((|Time-Interval| . 1) |temporally-overlaps| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-starts| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-overlaps|	(|Time-Interval| . 3))))

 	((((|Time-Interval| . 1) |temporally-overlapped-by|	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 			(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-overlapped-by|	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-overlapped-by|	(|Time-Interval| . 2))	;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 			(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-overlapped-by| 	(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-meets| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-meets| 	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-meets| 	(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-meets| 	(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-starts| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-starts| 	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-starts| 	(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-starts| 	(|Event| . 3))))
 	((((|Time-Interval| . 1) |temporally-starts| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |temporally-starts| 	(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-starts| 	(|Time-Interval| . 3))))
 	
	((((|Time-Interval| . 1) |temporally-started-by| 	(|Time-Interval| . 2))
   	  ((|Time-Interval| . 2) |time| 			(|Time-Interval| . 3)))
	 (((|Time-Interval| . 1) |temporally-started-by|	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-started-by|	(|Time-Interval| . 2))	;; <- ??
	  ((|Time-Interval| . 2) |time-of| 			(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-started-by| 	(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-finishes| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 		(|Time-Interval| . 3)))
 	 (((|Time-Interval| . 1) |temporally-finishes| 	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-finishes| 	(|Time-Interval| . 2))		;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 		(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-finishes| 	(|Event| . 3))))

 	((((|Time-Interval| . 1) |temporally-finished-by| 	(|Time-Interval| . 2))
 	  ((|Time-Interval| . 2) |time| 			(|Time-Interval| . 3)))
  	 (((|Time-Interval| . 1) |temporally-finished-by|	(|Time-Interval| . 3))))
 	((((|Time-Interval| . 1) |temporally-finished-by|	(|Time-Interval| . 2))	;; <- ??
 	  ((|Time-Interval| . 2) |time-of| 			(|Event| . 3)))
 	 (((|Time-Interval| . 1) |temporally-finished-by| 	(|Event| . 3))))

	((((|Time-Interval| . 1)    |end-time|      	(|End-Time-Value| . 2))
          ((|End-Time-Value| . 2)   |before|        	(|Start-Time-Value| . 3))
          ((|Start-Time-Value| . 3) |start-time-of| 	(|Time-Interval| . 4)))
         (((|Time-Interval| . 1)    |before|        	(|Time-Interval| . 4))))

	;;===============================================================  
	;; Misc. xforms. We may get rid of these in the future.
        ;;===============================================================  
 	((((|Event| . x) 		|result| 		    (|Tangible-Entity| . y))
;;   	  ((|Tangible-Entity| . y) 	|has-basic-structural-unit| (|Tangible-Entity| . z)))
   	  ((|Tangible-Entity| . y) 	|has-part| 		    (|Tangible-Entity| . z)))
  	 (((|Event| . x) 		|result| 		    (|Tangible-Entity| . z))))

        ((((|Event| . 1)                |object|        	    (|Information| . 2))
          ((|Information| . 2)          |information-content-of|    (|Message| 	   . 3)))
         (((|Event| . 1)                |object|        	    (|Message| 	   . 3))))

        ((((|Event|  . 1)               |object|        	    (|Entity| . 2))
          ((|Entity| . 2)          	|encloses|    		    (|Entity| . 3)))
         (((|Event|  . 1)               |object|        	    (|Entity| . 3))))

        ((((|Create|  . 0) 		|result|		(|Entity| . 1)))
         (((|Create|  . 0) 		|object|		(|Entity| . 1))))
        ((((|Create|  . 0) 		|object|		(|Entity| . 1)))
         (((|Create|  . 0) 		|result|		(|Entity| . 1))))

	;; *PZY*: 05/31/06 -- These rules are obselete.
        ;; ((((|Event|  . 1)            |object|        	(|Role|   . 2))
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |object|        	(|Entity| . 3))))
        ;; ((((|Event|  . 1)            |agent|        		(|Role|   . 2))
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |agent|        		(|Entity| . 3))))
        ;; ((((|Event|  . 1)            |recipient|        	(|Role|   . 2))
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |recipient|        	(|Entity| . 3))))
        ;; ((((|Event|  . 1)            |donor|        		(|Role|   . 2))
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |donor|        		(|Entity| . 3))))
        ;; ((((|Event|  . 1)            |beneficiary|        	(|Role|   . 2))		;; <-- *PZY*: 10/21/05
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |beneficiary|        	(|Entity| . 3))))
	;; *PZY*: Before 05/31/06
        ;; ((((|Event|  . 1)            |instrument|        	(|Role|   . 2))
        ;;   ((|Role|   . 2)          	|played-by|    		(|Entity| . 3)))
        ;;  (((|Event|  . 1)            |instrument|        	(|Entity| . 3))))

        ((((|Event|  . 1)               |object|        	(|Entity| . 2))
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |object|        	(|Role|   . 3))))
        ((((|Event|  . 1)               |agent|        		(|Entity| . 2))
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |agent|        		(|Role|   . 3))))
        ((((|Event|  . 1)               |recipient|        	(|Entity| . 2))
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |recipient|        	(|Role|   . 3))))
        ((((|Event|  . 1)               |donor|        		(|Entity| . 2))
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |donor|        		(|Role|   . 3))))
        ((((|Event|  . 1)               |beneficiary|        	(|Entity| . 2))		;; <-- *PZY*: 10/21/05
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |beneficiary|        	(|Role|   . 3))))
        ((((|Event|  . 1)               |instrument|        	(|Entity| . 2))
          ((|Entity| . 2)          	|plays|    		(|Role|   . 3)))
         (((|Event|  . 1)               |instrument|        	(|Role|   . 3))))
        ((((|Event|  . 1)               |experiencer|           (|Entity| . 2))  	;; <-- *PZY*: 06/07/06
          ((|Entity| . 2)               |plays|                 (|Role|   . 3))) 
         (((|Event|  . 1)               |experiencer|           (|Role|   . 3))))

))

;;;=====================================================================
;;;=====================================================================
;;; Functions to check transformations for consistency.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;;======================================================================
(defun check-transformations (transformations)
  (let (result)
    (dolist (transformation transformations)
      (dolist (lhs-triple (first transformation))
	(if (or (not (known-frame (first (triple-head lhs-triple))))
		(not (known-frame (triple-relation lhs-triple)))
		(not (known-frame (first (triple-tail lhs-triple)))))
	    (setf result (cons lhs-triple result))))
      (dolist (rhs-triple (second transformation))
	(if (or (not (known-frame (first (triple-head rhs-triple))))
		(not (known-frame (triple-relation rhs-triple)))
		(not (known-frame (first (triple-tail rhs-triple)))))
	    (setf result (cons rhs-triple result)))))
    result))

;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------
;;;; FILE: graph-util-degen-matches.lisp
;;;;
;;;; This file contains functions to process degenerate matches.
;;;;
;;;; FUNCTION INVENTORY:
;;;;    - (defun find-best-degenerate-match (graph1 graph2)
;;;;    - (defun remove-all-degenerate-mappings (all-mappings)
;;;;    - (defun remove-degenerate-mappings (mappings)
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Function to find a degenerate match between two graphs.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given two graphs, look for degenerate matches. The number of
;;	 degenerate matches can be enormous, so we only consider nodes
;;	 that have the same type.
;;	 **NOTE**: This function should be called only if graph-match
;;		fails to find any matches.
;; INPUT: graph1 = a list of triples.
;;	  graph2 = a list of triples.
;; OUTPUT: A degenerate match between graph1 and graph2. 
;;	   **NOTE**: Even with our heuristic, the number of degenerate
;;		matches can be huge, so we consider only one degenerate
;;		match for now. 
;;-----------------------------------------------------------------------
;;[jchaw-patch, 20Aug08, Changed (node-type ...) to be (semantic-matcher-node-type)
(defun find-best-degenerate-match (graph1 graph2)
  (let (result)
    ;; As our heuristic, we will look for nodes with the exact same type.
    (dolist (node1 (extract-nodes-in-graph graph1))
      (dolist (node2 (extract-nodes-in-graph graph2))
        ;; If we find such a node, then we will package it up as a match.
        (if (equal (semantic-matcher-node-type node1) (semantic-matcher-node-type node2))
            (setf result (cons (list (cons (list (list node1 '|vacuous-relation| '(|*Empty-Node|))
                                                 (list node2 '|vacuous-relation| '(|*Empty-Node|)))
                                           0))
                               result)))))
    ;; For now we consider the first degenerate match only.
    (list (first result))))

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions to remove degenerate matches from the final result.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given all matches between two graphs, remove any occurrence of
;;	 degenerate matches.
;; INPUT: all-mappings = A list of all mappings between two graphs.
;; OUTPUT: The list with degenerate matches removed. 
;;-----------------------------------------------------------------------
(defun remove-all-degenerate-mappings (all-mappings)
  (let (new-mappings result)
    (dolist (mappings all-mappings)
      (setf new-mappings (remove-degenerate-mappings mappings))  
      (if new-mappings (setf result (cons new-mappings result))))
    result))

;;-----------------------------------------------------------------------
;; DESC: Given a list of mappings, remove those mappings resulting
;;	 from a degenerate matches. Degenerate matches do not add any
;;	 useful information. We only permit them to see if they can
;;	 enable any matches using transformations. Hence, we should
;;	 remove them when matching is finished.
;; INPUT: mappings = the mappings between two graphs.
;; OUTPUT: The original mappings with degenerate matches removed.
;;-----------------------------------------------------------------------
(defun remove-degenerate-mappings (mappings)
  (let (result)
    (dolist (mapping mappings)
      ;; If the relation is not a vacuous-relation, then save the mapping. Otherwise,
      ;; we have a degenerate match.
      (cond
        ((not (eql (triple-relation (get-mapping-source mapping)) '|vacuous-relation|))
         (setf result (cons mapping result)))))
    result))


;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------
;;;; FILE: graph-util-structured-values.lisp 
;;;;
;;;; Functions to handle matching of structured slot values.
;;;;
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Global parameters and variables.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; Look up table of functions to detect and handle structured slot
;; values. Each entry in this table is of the form:
;;
;;		(<detection-fn> <match-fn>)
;;
;; where <detection-fn> is the name of the function used to detect
;;	 a structured slot value (e.g. :bag) of interest and
;;	 <match-fn> is the name of the function used to match the
;;	 structured slot value once it has been detected.
;;
;; **NOTE**: Entries towards the top of this list has precedence over
;;	entries at the bottom.
;;-----------------------------------------------------------------------
(defvar *structured-slot-value-handlers* '(
 	(#'is-seq-value-p  #'match-seq-value)
	(#'is-set-value-p  #'match-set-value)
	(#'is-bag-value-p  #'match-bag-value)
))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Top-Level functions
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Top-level function called by the matcher to detect structured
;;	 values that it should try to match and to determine the 
;;	 appropriate function to call.
;; INPUT:
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun contains-structured-slot-value-p (triple)
  (let ((head (triple-head triple)) 
	(tail (triple-tail triple)) 
	result)
    (dolist (detect-fn+handler-fn *structured-slot-value-handlers*)
      (cond
       ((or (funcall (eval (first detect-fn+handler-fn)) head) (funcall (eval (first detect-fn+handler-fn)) tail))
	(setf result detect-fn+handler-fn)
	(return))))
    result))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun apply-structured-slot-value-handler (triple1 triple2 detect+handle-fn)
  (let ((detect-fn (eval (first  detect+handle-fn)))
	(handle-fn (eval (second detect+handle-fn)))
	score head1 head2 tail1 tail2 result)
    ;; 1) Align the triples for matching. 
    (cond
     ;; 1.1) The triples are already aligned.
     ((equal (triple-relation triple1) (triple-relation triple2))
      (setf score 1)
      (setf head1 (triple-head triple1))
      (setf head2 (triple-head triple2))
      (setf tail1 (triple-tail triple1))
      (setf tail2 (triple-tail triple2)))
     ;; 1.2) The triples are inverted.
     ((equal (triple-relation triple1) (invert-slot (triple-relation triple2)))
      (setf score -1)
      (setf head1 (triple-head triple1))
      (setf head2 (triple-tail triple2))
      (setf tail1 (triple-tail triple1))
      (setf tail2 (triple-head triple2))))
    ;; 2) Now match the triples.
    (cond
     ((and score (funcall detect-fn head1) (funcall detect-fn head2))
      (setf result (and (tax-dist (if (is-instance-p tail1) (immediate-classes (first tail1)) (first tail1))
				  (if (is-instance-p tail2) (immediate-classes (first tail2)) (first tail2)))
			(funcall handle-fn head1 head2))))
     ((and score (funcall detect-fn tail1) (funcall detect-fn tail2))
      (setf result (and (tax-dist (if (is-instance-p head1) (immediate-classes (first head1)) (first head1))
				  (if (is-instance-p head2) (immediate-classes (first head2)) (first head2)))
			(funcall handle-fn tail1 tail2)))))
    ;; 3) Now construct the match result.
    (cond (result (cons triple2 (cons '(first . first) score))))))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Generic detection and match functions to handle structured slot
;;; values.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Detection and match functions to handle :seq
;; INPUT: 
;; OUTPUT: 
;;-----------------------------------------------------------------------
(defun is-seq-value-p  (val) (and (consp val) (equal (first val) ':|seq|)))

(defun match-seq-value (val1 val2) (equal val1 val2))


;;-----------------------------------------------------------------------
;; DESC: Detection and match functions to handle :set
;; INPUT:
;; OUTPUT: 
;;-----------------------------------------------------------------------
(defun is-set-value-p  (val) (and (consp val) (equal (first val) ':|set|)))

(defun match-set-value (val1 val2)
  (and (equal (length val1) (length val2))
       (not (set-difference val1 val2 :test #'equal))))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun is-bag-value-p (val) (and (consp val) (equal (first val) ':|bag|)))

(defun match-bag-value (val1 val2)
  (let ((v1 (remove-duplicates val1 :test #'equal))
	(v2 (remove-duplicates val2 :test #'equal)))
    (and (equal (length v1) (length v2))
	 (not (set-difference v1 v2 :test #'equal)))))
;;;;====================================================================
;;;;====================================================================
;;;; FILE: graph-util-disjoint.lisp
;;;;
;;;; Functions that test and combine disjoint matches.
;;;;
;;;; FUNCTION INVENTORY:
;;;;	- (defun combine-disjoint-mappings (all-mappings)
;;;;	- (defun combine-disjoint-graphs (disjoint-mappings+graph)
;;;;	- (defun gather-all-disjoint-matches (initial-matches)
;;;;	- (defun is-consistent-disjoint-match (mappings+graph)
;;;;	- (defun is-consistent-disjoint-match0 (mappings)
;;;;	- (defun remove-if-var-node-binding (node-bindings &optional (node-to-km-hash *node-to-km-mappings*))
;;;;	- (defun get-all-disjoint-graphs (graph)
;;;;	- (defun is-graph-disjoint (graph)
;;;;	- (defun find-connected-subgraph (key graph)
;;;;	- (defun find-adjacent-triples (key graph)
;;;;====================================================================
;;;;====================================================================

;;;=====================================================================
;;;=====================================================================
;;; Functions to combine and gather disjoint matches.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a list of matches, combine all those matches that 
;;	 are consistent.
;; INPUT: all-mappings = a list of matches between two graphs.
;; OUTPUT: a list of matches.
;;======================================================================
(defun combine-disjoint-mappings (all-mappings)
  (let (remaining-mappings temp-mappings keys temp-result curr-result final-result)
    ;; 1. Initial set up.
    (setf keys (list (first all-mappings)))
    (setf remaining-mappings (rest all-mappings))
    ;; 2. Find all consistent, disjoint matches.
    (loop
      ;; Stop if there are no more starting points.
      (if (null keys) (return))
      ;; Set things for the current starting point.
      (setf curr-result (first keys))
      (setf keys        (rest  keys))
      ;; Iterate through each of the remaining mappings to determine
      ;; whether it is consistent with the current matches. 
      (dolist (mapping remaining-mappings)
	;; Union because two sets of mappings may contain overlapping features.
        (setf temp-result (union mapping curr-result :test #'equal))
        (hash-match-mappings temp-result *htable-mappings-for-g1* *htable-mappings-for-g2*)
        (cond
          ((not (check-for-inconsistent-mappings temp-result *htable-mappings-for-g1* *htable-mappings-for-g2*))
           (setf temp-mappings (cons mapping temp-mappings))
           (setf curr-result temp-result))
          (t
           (setf keys (cons mapping keys)))))
      ;; Set things up for the next round.
      (setf remaining-mappings temp-mappings)
      (setf temp-mappings nil)
      (setf final-result (cons curr-result final-result)))
    final-result))

;;======================================================================
;; DESC:
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun combine-disjoint-graphs (disjoint-mappings+graph)
  (let ((curr-list-mappings+graph (first disjoint-mappings+graph))
        (remaining-list-mappings+graph (rest disjoint-mappings+graph))
        temp-result
        final-result)
    ;; Generate all possible combinations of the matches.  
    ;; **NOTE**: This may become very inefficient, so we definately
    ;;           need to consider some alternative options.
    (loop
      (if (null remaining-list-mappings+graph) (return))
      (dolist (curr-mappings+graph curr-list-mappings+graph)
        (dolist (remaining-mappings+graph (first remaining-list-mappings+graph))
          (setf temp-result 
		(cons (list (append (first  curr-mappings+graph) (first  remaining-mappings+graph))
			    (append (second curr-mappings+graph) (second remaining-mappings+graph)))
		      temp-result))))	
      (setf curr-list-mappings+graph temp-result)
      (setf temp-result nil)
      (setf remaining-list-mappings+graph (rest remaining-list-mappings+graph)))
    ;; Get rid of all combinations that are inconsistent.
    (dolist (mappings+graph curr-list-mappings+graph)
      (if (is-consistent-disjoint-match mappings+graph)
	  (setf final-result (cons mappings+graph final-result))))
    final-result))

;;======================================================================
;; DESC:
;; INPUT:
;; OUTPUT:
;;======================================================================
(defun gather-all-disjoint-matches (initial-matches)
  (let ((all-subgraphs (get-all-disjoint-graphs (mapcar #'(lambda (m) (match-head m)) initial-matches)))
        (remaining-initial-matches initial-matches)
        disjoint-initial-matches all-initial-matches temp-matches)
    (cond
      ((= (length all-subgraphs) 1)
       (list initial-matches))
      (t
       (dolist (subgraph all-subgraphs)
         (setf temp-matches nil)
         (setf disjoint-initial-matches nil)
         (dolist (single-match remaining-initial-matches)
           (if (member (match-head single-match) subgraph :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))
               (setf temp-matches (cons single-match temp-matches))
               (setf disjoint-initial-matches (cons single-match disjoint-initial-matches))))
         (setf remaining-initial-matches disjoint-initial-matches)
         (setf all-initial-matches (cons temp-matches all-initial-matches)))
       all-initial-matches))))


;;;=====================================================================
;;;=====================================================================
;;; Functions to test whether a disjoint match is consistent.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;;======================================================================
(defun is-consistent-disjoint-match (mappings+graph)
  (let ((separate-binding-types (is-consistent-disjoint-match0 (first mappings+graph)))
	(is-consistent? t)
	node-bindings temp)
    (setf node-bindings (remove-if-var-node-binding
			  (append (first separate-binding-types)
				  (get-all-node-bindings-for-mappings (second separate-binding-types)))))
    (dolist (node (extract-nodes-in-graph (second mappings+graph)))
      (setf temp (lookup-node-bindings node node-bindings 'target))
      (if (and temp (> (length temp) 1))
	  (progn (setf is-consistent? nil) (return))))
    is-consistent?))

(defun is-consistent-disjoint-match0 (mappings)
  (let (node-pairs triple-mappings)
    (dolist (mapping mappings)
      (cond
	((numberp (rest mapping))
	 (setf triple-mappings (cons mapping triple-mappings)))
	(t
	 (setf node-pairs (cons mapping node-pairs)))))
    (list node-pairs triple-mappings)))

(defun remove-if-var-node-binding (node-bindings &optional (node-to-km-hash *node-to-km-mappings*))
  (let (result)
    (dolist (node-binding node-bindings)
       (if (not (eql (elt (string (gethash (first node-binding) node-to-km-hash)) 0) '#\?))
	   (setf result (cons node-binding result))))
    result))


;;;=====================================================================
;;;=====================================================================
;;; Functions to test for and find disconnected subgraphs.
;;;=====================================================================
;;;=====================================================================

;;======================================================================
;; DESC: Given a graph, return all of its connected subgraphs.
;; INPUT: graph = a list of triples.
;; OUTPUT: A list of connected subgraphs in graph.
;;======================================================================
(defun get-all-disjoint-graphs (graph)
  (let (all-graphs remaining)
    (setf remaining (find-connected-subgraph (first graph) (rest graph)))
    (setf all-graphs (cons (first remaining) all-graphs))
    (loop
      (if (null (second remaining)) (return))
      (setf remaining (find-connected-subgraph (first (second remaining)) (rest (second remaining))))
      (setf all-graphs (cons (first remaining) all-graphs)))
    all-graphs))

;;======================================================================
;; DESC: Given a graph, determine if this graph is disjoint.
;; INPUT: graph = a list of triples.
;; OUTPUT: T is graph is disjoint. NIL otherwise.
;;======================================================================
(defun is-graph-disjoint (graph)
  (let ((is-disjoint? (find-connected-subgraph (first graph) (rest graph))))
    (if (null (second is-disjoint?)) nil t)))

;;======================================================================
;; DESC: Given a graph and a random starting point, this function will
;;       find a connected subgraph of graph.
;; INPUT: key = a triple in graph.
;;        graph = a list of triples.
;; OUTPUT: A list of triples that make up a connected subgraph in graph.
;;======================================================================
(defun find-connected-subgraph (key graph)
  (let (result needles new-needles haystack split-result)
    (setf split-result (find-adjacent-triples key graph))
    (setf needles  (first  split-result))
    (setf haystack (second split-result))
    (setf result (cons key needles))
    (loop
      (if (null needles) (return))
      (dolist (needle needles)  
        (setf split-result (find-adjacent-triples needle haystack))
        (setf new-needles (append (first split-result) new-needles))
        (setf haystack (second split-result)))
      (setf needles new-needles)
      (setf result (append new-needles result))
      (setf new-needles nil))
    (list result haystack)))


;;======================================================================
;; DESC: This function takes a triple key and a graph G = {t1, ...,tn}
;;       and returns two lists:
;;
;;              (<adjacent triples> <non-adjacent triples>)
;; 
;;       The first list contains triples that intersect (i.e. adjacent
;;       to) the key only once. The second list contains all the triples
;;       in G that do not satisfy this constraint.
;; INPUT: key = a triple.
;;        graph = a list of triples.
;; OUTPUT: A list of the form:
;;
;;              (<adjacent triples> <non-adjacent triples>)
;;
;;         where both <adjacent triples> and <non-adjacent triples> are
;;         lists of triples.
;;======================================================================
(defun find-adjacent-triples (key graph)
  (let ((head (triple-head key))
        (tail (if (not (eql (triple-relation key) '|value|)) (triple-tail key) 'NOMATCH-TAG))
        adj-triples
        non-adj-triples)
    (dolist (triple graph)
      (cond
        ;; Don't extend the key with itself (If it so happens that the
        ;; key is not removed from the graph already).
        ((equal key triple))
        ((eql (triple-relation triple) '|value|)
         (if (or (equal (triple-head triple) head) (equal (triple-head triple) tail))
             (setf adj-triples (cons triple adj-triples))
             (setf non-adj-triples (cons triple non-adj-triples))))   
        ((or (equal (triple-head triple) head)
             (equal (triple-head triple) tail))   
         (setf adj-triples (cons triple adj-triples)))
        ((or (equal (triple-tail triple) head)
             (equal (triple-tail triple) tail))
         (setf adj-triples (cons triple adj-triples)))
        (t
         (setf non-adj-triples (cons triple non-adj-triples)))))
    (list adj-triples non-adj-triples)))
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------
;;;; FILE: graph-util-gather.lisp
;;;; 
;;;; Functions to gather/extract a graph from KM.
;;;;
;;;; FUNCTION INVENTORY:
;;;;	- (defun gather-graph (instance &optional relations-to-query (depth 2))   
;;;;	- (defun clone-graph (graph)
;;;;	- (defun singleton-class (class)
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Global parameters
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
(defvar *relations-to-always-lookup* '(
        |has-part| |is-part-of| |subevent| |subevent-of| |agent| |agent-of|
        |object| |object-of| |base| |base-of| |next-event| |objective| |by-means-of|
        |enables| |enabled-by|
;;      |defeats| |defeated-by| |preparatory-event| |preparatory-event-of|              ;; <-- **PZY**: 12/20/04
))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Function to extract graphs from KM.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Variant of Pete Clark's original function. I'm using this 
;;	version because the original version was overlooking too much 
;;	information. This version performs a bounded walk of the graph 
;;	starting at the specified root. This will suffice for most
;;	purposes, but I will DEFINATELY return to this issue of how 
;;	we can better extract the graph directly from the KM KB.
;; INPUT: instance = a KM instance.
;;	  relations-to-query (optional) = a list of relations to query.
;;	  depth (optional) = the depth to which we should extract the
;;		graph.
;; OUTPUT: A list of triples in Pete Clark's format.
;; 
;; **NOTE**:
;; [1] - Do we need to dereference other concepts that we come across
;;       when doing a bounded walk of the graph? I believe the answer
;;       is no because KM will dereference these new concepts for us.
;;	
;;	 08/15/05 -- This doesn't appear to be the case anymore. I
;;	 	found a counter example. If an existing concept,
;;		say _Chromosome35, gets bound to a newly created 
;;		instance, then _Chromosome35 will not be dereferenced. 
;;		We'll have to do the dereferencing at the very end.
;;-----------------------------------------------------------------------
(defun gather-graph (instance &optional relations-to-query (depth 2) (query-interface-slots t))
  (let ((duplicate-instances (make-hash-table :test #'eql))
        (duplicate-triples (make-hash-table :test #'equal))
        (instances-to-do (list (dereference instance))) ;; <- Dereference the root to make sure it
                                                        ;;    is not bounded to another concept. [1]
        new-instances done-instances instance-type new-triple triples-gathered)
    (if (null relations-to-query)
        (setf relations-to-query (append (all-instances '|Relation|) (all-instances '|Property|)))
        (setf relations-to-query (union relations-to-query *relations-to-always-lookup* :test #'eql)))
    (loop
      (if (or (null instances-to-do) (= depth 0)) (return))
      (dolist (instance-to-do instances-to-do)
        (cond
          (;; (not (gethash instance-to-do duplicate-instances))
	   (and (not (gethash instance-to-do duplicate-instances)) (kb-objectp instance-to-do))
           (setf (gethash instance-to-do duplicate-instances) t)
           (setf done-instances (cons instance-to-do done-instances))
           (setf instance-type (first (immediate-classes instance-to-do)))
           (dolist (slot relations-to-query)
             (if (multi-is-subclass-of instance-type (domains-of slot))
                 (dolist (ival (km0 `(|the| ,slot |of| ,instance-to-do)))
                   (setf new-triple (list instance-to-do slot ival))
                   (cond
                     ((or (gethash new-triple duplicate-triples)
                          (gethash (invert-triple new-triple) duplicate-triples)))
                     ((is-an-instance ival)
                      (if (atom ival) (setf new-instances (cons ival new-instances)))
                      (setf (gethash new-triple duplicate-triples) t)
                      (setf triples-gathered (cons new-triple triples-gathered)))))))
	   (cond
             ;; Mirrors the above portion, but gets the interface values.
	     (query-interface-slots
	      (dolist (slot (km0 `(|the| |feature-slot| |of| ,instance-type)))
                (dolist (ival (km0 `(|the| ,slot |of| ,instance-to-do)))
		  (setf new-triple (list instance-to-do slot ival))
		  (cond
                    ((or (gethash new-triple duplicate-triples)
                         (gethash (invert-triple new-triple) duplicate-triples)))
                    ((is-an-instance ival)
                     (if (atom ival) (setf new-instances (cons ival new-instances)))
                     (setf (gethash new-triple duplicate-triples) t)
                     (setf triples-gathered (cons new-triple triples-gathered)))))))) 	)))
      (setf depth (- depth 1))
      ;; **NOTE**: If the answer to [1] is YES, then we'll need to dereference each
      ;;           new instance before we cycle through the loop again.
      (setf instances-to-do new-instances)
      (setf new-instances nil))
    (append triples-gathered
            (mapcar #'(lambda (x) (list x '|instance-of| (singleton-class (get-vals x '|instance-of|))))
                    ;; (append done-instances instances-to-do)
                    done-instances))))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Function to clone a graph.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given a graph, make a clone of it.
;; INPUT: graph = a list of triple.
;; OUTPUT: A clone of the input.
;;-----------------------------------------------------------------------
(defun clone-graph (graph)
  (let ((nodes (extract-nodes-in-graph graph))
        new-head new-tail new-nodes result)
    ;; 1) Rename the nodes in the graph.
    (dolist (node nodes)
      (if (and (rest node) (atom (rest node)))
          (setf new-nodes (cons (list node (cons (first node) (string (gensym "N")))) new-nodes))))
    ;; 2) Replace all the "old" nodes in the graph with the new ones.
    (dolist (triple graph)
      (setf new-head (second (first (member (triple-head triple) new-nodes :test #'(lambda (x y) (equal x (first y)))))))
      (setf new-tail (second (first (member (triple-tail triple) new-nodes :test #'(lambda (x y) (equal x (first y)))))))
      (setf result (cons (list (or new-head (triple-head triple))
                               (triple-relation triple)
                               (or new-tail (triple-tail triple)))
                         result)))
    ;; 3) Reverse the graph, so we can maintain the original order. Normally,
    ;;    order is not important, but we want to maintain the order in case
    ;;    for some reason rely on the order to be the same.
    (reverse result)))

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Auxiliary functions used by the top-level ones.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC:
;; INPUT:
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun singleton-class (class)
  (cond
    ((atom class) class)
    ((= (length class) 1) (first class))
    (t class)))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun multi-is-subclass-of (subtype suptypes)
  (let (result)
    (dolist (suptype suptypes)
      (if (is-subclass-of subtype suptype)
          (progn (setf result t) (return))))
    result))

;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------
;;;; FILE: graph-util-join.lisp
;;;;
;;;; Contains utility functions for joining two graphs.
;;;;
;;;; FUNCTION INVENTORY:
;;;; 	- (defun join-graphs (g1 g2 mappings &optional special-nodes)
;;;;	- (defun multi-graph-join (all-graph1+graph2+mappings)
;;;;	- (defun join-graphs-at-point (g1 g2)
;;;;	- (defun multi-join-hash-node-binding (all-node-bindings &optional (binding-hash *multi-join-node-bindings*))
;;;;	- (defun multi-join-node-lookup (node &optional (binding-hash *multi-join-node-bindings*))
;;;;	- (defun join-graphs-at-point0 (nodes)
;;;;	- (defun more-specific? (c1 c2 &optional special-nodes)
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Global variables and parameters.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
(defvar *multi-join-node-bindings* (make-hash-table :test #'equal))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Top-Level functions to join two or more graphs.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given two graphs, join them based on their mappings. When two
;;       graphs are joined, the more specific node replaces the more
;;       general one.
;; INPUT: g1 = a list of triples.
;;        g2 = a list of triples.
;;        mappings = the mappings between g1 and g2.
;;        special-nodes (optional) = "Special nodes" are nodes that should
;;              remain in the graph resulting from the join regardless of
;;              how specific or general they are. This list contains such
;;              nodes, and these nodes can be from either g1 or g2.
;; OUTPUT: A list of triples representing the graph resulting from the
;;         join of g1 and g2.
;;-----------------------------------------------------------------------
(defun join-graphs (g1 g2 mappings &optional special-nodes)
  (let (g1-triple g1-matched-triples node-bindings node-pair join-result)
    (dolist (mapping mappings)
      (setf g1-triple (get-mapping-source mapping))
      (setf node-bindings (append (get-node-bindings-for-mapping mapping) node-bindings))
      (if (path-p g1-triple)
          (setf g1-matched-triples (append g1-triple g1-matched-triples))
          (setf g1-matched-triples (cons   g1-triple g1-matched-triples))))
    ;; Contrary to what I thought previously, we want to remove G1's matched triples
    ;; because G1 may possess discrepancies and ambiguities. G2, on the other hand,
    ;; is the "correct" encoding that we want G1 to align with.
    (setf join-result (append (set-difference g1 g1-matched-triples
                                :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))
                              g2))
    ;; Finally, replace all the mapped node pairs with the most specific node.
    (dolist (node-binding node-bindings)
      (setf node-pair (more-specific? (first node-binding) (second node-binding) special-nodes))
      (setf join-result (replace-concept-in-graph (second node-pair) (first node-pair) join-result)))
    join-result))

;;-----------------------------------------------------------------------
;; DESC: Given a list of graphs and how they match each other, combine   
;;       these graphs into one representation based on the mappings.
;; INPUT: all-graph1+graph2+mappings = a list of the form:
;;              ((<graph_1> <graph_2> <mappings_1>)
;;               (<graph_2> <graph_3> <mappings_2>)
;;               ...
;;               (<graph_i> <graph_j> <mappings_j>))
;; OUTPUT: The result is a list of the form:
;;                      (<resulting-graph> <score>)
;;         where
;;              <resulting-graph> = a list of triples.
;;              <score> = the match score.
;;
;; **NOTE**: There is a peculiarity of this function that occurs under 
;;	the following circumstance. Some triples can be matched only 
;;	with transformations, and sometimes this requires querying KM
;;	for additional information not in the graphs. When this occurs
;;	additional information brought in from KM is not included in
;;	the final result! Is this a problem? Should this be addressed?
;;-----------------------------------------------------------------------
(defun multi-graph-join (all-graph1+graph2+mappings)
  (let (all-mappings node-bindings temp-triple mapping-source mapping-target 
	triples-to-remove resulting-graph final-result)
    ;; 0. clear the hash!
    (clrhash *multi-join-node-bindings*)
    ;; 1. Append together all the graphs and all the mappings.
    (dolist (graph1+graph2+mappings all-graph1+graph2+mappings)
      (setf resulting-graph (append (first  graph1+graph2+mappings) resulting-graph))
      (setf resulting-graph (append (second graph1+graph2+mappings) resulting-graph))
      (setf all-mappings    (append (third  graph1+graph2+mappings) all-mappings)))
    ;; 2. Process the mappings.
    (dolist (mapping all-mappings)
      ;; Get the node bindings for each mapping.
      (setf node-bindings  (append (get-node-bindings-for-mapping mapping) node-bindings))
      (setf mapping-source (get-mapping-source mapping))
      (setf mapping-target (get-mapping-target mapping))
      (cond
	;; The source is not a path (which indicates that not transforms were applied), 
	;; so we should remove it.
	((not (path-p mapping-source))
	 (setf triples-to-remove (cons mapping-source triples-to-remove)))
	;; The source is a path of length 1, so the target must be greater than 1.
	((and (path-p mapping-source) (= (length mapping-source) 1))
	 (setf triples-to-remove (append mapping-source triples-to-remove)))
	;; The target is not a path, so we should remove it.
	((not (path-p mapping-target))
	 (setf triples-to-remove (cons mapping-target triples-to-remove)))
	;; The target is a path of length 1, so the source must be greater than 1.
	((and (path-p mapping-target) (= (length mapping-target) 1))
	 (setf triples-to-remove (append mapping-target triples-to-remove)))
	;; At this point, both the source and target are paths, so we can remove
	;; either one. We'll chose the source.
	(t
	 (setf triples-to-remove (append (get-mapping-source mapping) triples-to-remove)))))
    ;; 3. Hash the node bindings.
    (multi-join-hash-node-binding node-bindings)
    ;; 4. Now, process each triple to 1) remove duplicates, 2) remove mapped triples, and
    ;;    3) resolve node bindings.
    (dolist (triple resulting-graph)
      ;; Deference the triple!
      (setf temp-triple (list (multi-join-node-lookup (triple-head triple))
                              (triple-relation triple)
                              (multi-join-node-lookup (triple-tail triple))))
      ;; Check if: 1) the curr triple is NOT one of the triples to remove.
      ;;           2) the curr triple (in its dereferenced form) has NOT been added
      ;;              to the final results already.
      ;; We will add this triple to the final result if both 1 and 2 are satisfied.
      (if (and (not (member triple triples-to-remove :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y))))))
               (not (member temp-triple final-result :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))))
          (setf final-result (cons temp-triple final-result))))
    (list final-result (/ (number-of-matched-triples all-mappings) (length final-result)))))


;;-----------------------------------------------------------------------
;; Given two graphs try to join them at the node level.
;;-----------------------------------------------------------------------
(defun join-graphs-at-point (g1 g2)
  (let ((g1-nodes (extract-nodes-in-graph g1))
        (g2-nodes (extract-nodes-in-graph g2))
        dist matched-nodes all-mappings
        join-result mapping-result
        final-result)
    ;; Find all the mappings between the nodes in g1 with the nodes in g2.
    (dolist (g1-node g1-nodes) 
      (setf matched-nodes nil)
      (dolist (g2-node g2-nodes)
        (cond
          ((or (is-value-p g1-node) (member (first g1-node) *top-level-concepts* :test #'eql))
           (return))
          ((setf dist (tax-dist (first g1-node) (first g2-node)))
           (if (<= dist 2) (setf matched-nodes (cons (cons (list g1-node g2-node) dist) matched-nodes))))
          ((setf dist (tax-dist (first g2-node) (first g1-node)))
           (if (<= dist 2) (setf matched-nodes (cons (cons (list g1-node g2-node) (- 0 dist)) matched-nodes))))))
      (if matched-nodes (setf all-mappings (cons matched-nodes all-mappings))))
    ;; Call the helper function join-graphs-at-point0 to contruct all the
    ;; possible mappings between the matched nodes in G1 and G2.
    (setf all-mappings (join-graphs-at-point0 all-mappings))
    (cond
      ;; There are no connecting points (i.e. mapped nodes), so the two graphs
      ;; cannot be joined.
      ((null all-mappings))
      ;; Join the graphs!
      (t
       (dolist (mappings all-mappings)
         (setf join-result (append g1 g2))
         (setf mapping-result nil)
         (dolist (mapping mappings)
           ;; Replace all the mapped node pairs with the most specific node.
           (setf join-result
                 (if (> (rest mapping) 0)
                     (replace-concept-in-graph (second (first mapping)) (first (first mapping)) join-result)
                     (replace-concept-in-graph (first (first mapping)) (second (first mapping)) join-result)))
           (setf mapping-result (cons (first mapping) mapping-result)))
         (setf final-result (cons (list join-result mapping-result) final-result)))))
    final-result))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions to hash and lookup nodes during multi-join.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given a list of node bindings, hash and store them in a hash
;;       table.
;; INPUT: all-node-bindings = a list of node pairs -- each pair is
;;              two matched concepts.
;;        binding-hash (optional) = hash table storing the node bindings.
;; OUTPUT: This function does not return any values, but the values in   
;;         all-node-bindings are hashed as a result of calling this      
;;         function.
;;-----------------------------------------------------------------------
(defun multi-join-hash-node-binding (all-node-bindings &optional (binding-hash *multi-join-node-bindings*))
  (let (concept1 concept2 key val hash-result)
    (dolist (concept1+concept2 (remove-duplicates all-node-bindings :test #'equal))
      (setf concept1 (first  concept1+concept2))
      (setf concept2 (second concept1+concept2))
      ;; Order the concepts so that the more specific one is first in the list.
      (cond
        ((equal concept1 concept2)                    (setf key nil)      (setf val nil))
        ((is-instance-p concept1)                     (setf key concept2) (setf val concept1))
        ((is-instance-p concept2)                     (setf key concept1) (setf val concept2))
        ((tax-dist (first concept1) (first concept2)) (setf key concept2) (setf val concept1))
        (t                                            (setf key concept1) (setf val concept2)))
      (cond
        ((and key val)
         (setf hash-result (gethash key binding-hash))
         (cond
           (hash-result
            (if (not (equal val hash-result)) (setf (gethash val binding-hash) hash-result)))
           (t
            (setf (gethash key binding-hash) val))))))))

;;-----------------------------------------------------------------------
;; DESC: Given a concept, look up what concept it is bound to.
;; INPUT: node = a concept node -- e.g. (|Dog| . 1)
;;        binding-hash (optional) = hash table storing the bindings where
;;              "key" is bounded to "val". NOTE, "val" can be the "key"
;;              in another entry.
;; OUTPUT: A concept node.
;;-----------------------------------------------------------------------
(defun multi-join-node-lookup (node &optional (binding-hash *multi-join-node-bindings*))
  (let ((curr-node node) result)
    (loop
      (setf result (gethash curr-node binding-hash))
      (if (null result) (return))
      (setf curr-node result))
    curr-node))

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Some other auxiliary functions used by the top-level ones.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: 
;; INPUT:
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun join-graphs-at-point0 (nodes)
  (let* ((most-mapped-node (find-most nodes :test #'> :form #'(lambda (x) (length x))))
         (remaining-mapped-nodes (remove most-mapped-node nodes :test #'equal))
         most-specific-mapping
         result
         final-result)
    (dolist (mapping most-mapped-node)
      (setf result (list mapping))
      (dolist (remaining-mappings remaining-mapped-nodes)
        (setf most-specific-mapping (find-most remaining-mappings :test #'< :form #'(lambda (x) (abs (rest x)))))
        (if (not (equal (second (first most-specific-mapping)) (second (first mapping))))
            (setf result (cons most-specific-mapping result))))
      (setf final-result (cons result final-result)))
    final-result))

;;-----------------------------------------------------------------------
;; DESC: Given two concepts return them in a list with the most 
;;	 specific element of the two as the first element of this 
;;	 list.
;; INPUT:
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun more-specific? (c1 c2 &optional special-nodes)
  (cond
    ((member c1 special-nodes :test #'equal) (list c1 c2))
    ((member c2 special-nodes :test #'equal) (list c2 c1))
    ((is-instance-p c1) (list c1 c2))
    ((is-instance-p c2) (list c2 c1))
    ((tax-dist (first c1) (first c2)) (list c1 c2))
    (t (list c2 c1))))
 
;;-----------------------------------------------------------------------
;; DESC: Find the item that is the "most" according to some criteria.
;; INPUT: 
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun find-most (items &key (test #'>) (form #'length))
  (let (val max)
    (dolist (item items)
      (cond
        ((null max)  
         (setf val item)
         (setf max (funcall form item)))
        ((funcall test (funcall form item) max)
         (setf val item)
         (setf max (funcall form item)))))
    val))

;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------
;;;; FILE: graph-util-misc.lisp
;;;; 
;;;; This file contains misc. utility functions for extract and
;;;; manipulating node, relations, and triples in graphs.
;;;;
;;;; FUNCTION INVENTORY:
;;;; 	- (defun replace-concept-in-graph (old-concept new-concept graph)
;;;;	- (defun remove-value-triples (triples)
;;;;	- (defun find-triple-with-head (head triples)
;;;;	- (defun find-all-triples-with-head (head triples)
;;;;	- (defun find-triple-with-tail (tail triples)
;;;;	- (defun find-all-triples-with-tail (tail triples)
;;;;	- (defun find-all-triples-containing (concept-node triples)
;;;;	- (defun extract-nodes-in-graph (graph)
;;;;	- (defun extract-types-in-graph (graph)
;;;;	- (defun extract-relations-in-graph (triples)
;;;;	- (defun extract-relations-in-graph-inv (triples)
;;;;---------------------------------------------------------------------
;;;;---------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions for replacing/removing concepts from a graph.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Replace all the old-concept in graph with new-concept.
;; INPUT: old-concept = a concept node -- e.g. (|Dog| . 1)
;;	  new-concept = a concept node
;;	  graph = a list of triples.
;; OUTPUT: A list of triples where old-concept is replaced with new 
;;	   concept.
;;-----------------------------------------------------------------------
(defun replace-concept-in-graph (old-concept new-concept graph)
  (let (result)
    (dolist (triple graph)
      (cond
        ((equal (triple-head triple) old-concept)
         (setf result (cons (list new-concept (triple-relation triple) (triple-tail triple)) result)))
        ((equal (triple-tail triple) old-concept)
         (setf result (cons (list (triple-head triple) (triple-relation triple) new-concept) result)))
        (t
         (setf result (cons triple result)))))
    result))

;;-----------------------------------------------------------------------  
;; DESC: Given a list of triples, remove those that are values.
;; INPUT: triples = a list of triples.
;; OUTPUT: A list of triples with value triples removed.
;;-----------------------------------------------------------------------
(defun remove-value-triples (triples)
  (let (result)
    (dolist (triple triples)
      (if (not (eql (triple-relation triple) '|value|))
          (setf result (cons triple result))))
    result))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions for finding triples in a graph.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, find the first triple in this list 
;;	 whose head is equal to the specified head.
;; INPUT: head = a concept node.
;;	  triples = a list of triples.
;; OUTPUT: The first triple satisfying the specified condition.
;;-----------------------------------------------------------------------
(defun find-triple-with-head (head triples)
  (first (member head triples :test #'(lambda (x y) (equal x (triple-head y))))))


;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, find all triples in this list whose 
;;	 head is equal to the specified head.
;; INPUT: head = a concept node.
;;	  triples = a list of triples.
;; OUTPUT: A list of triples satisfying the specified condition.
;;-----------------------------------------------------------------------
(defun find-all-triples-with-head (head triples)
  (let (result)
    (dolist (triple triples)
      (if (equal head (triple-head triple)) (setf result (cons triple result))))
    result))

;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, find the first triple in this list 
;;	 whose tail is equal to the specified tail.
;; INPUT: tail = a concept node.
;;	  triples = a list of triples.
;; OUTPUT: The first triple satisfying the specified condition.
;;-----------------------------------------------------------------------
(defun find-triple-with-tail (tail triples)
  (first (member tail triples :test #'(lambda (x y) (equal x (triple-tail y))))))

;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, find all triples in this list whose 
;;	 tail is equal to the specified tail.
;; INPUT: tail = a concept node.
;;	  triples = a list of triples.
;; OUTPUT: A list of triples satisfying the specified condition.
;;-----------------------------------------------------------------------
(defun find-all-triples-with-tail (tail triples)
  (let (result)
    (dolist (triple triples)
      (if (equal tail (triple-tail triple)) (setf result (cons triple result))))
    result))

;;-----------------------------------------------------------------------
;; DESC:
;; INPUT:
;; OUTPUT:
;;-----------------------------------------------------------------------
(defun find-all-triples-containing (concept-node triples)
  (let (result)
    (dolist (triple triples)
      (if (or (equal concept-node (triple-head triple)) (equal concept-node (triple-tail triple)))
	  (setf result (cons triple result))))
    result))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions to extract info for nodes and relations from a graph.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; DESC: Given a graph, extract all the unique nodes in the graph.
;; INPUT: graph = a list of triples.
;; OUTPUT: A list of concept nodes.
;;-----------------------------------------------------------------------
(defun extract-nodes-in-graph (graph)
  (let (result)
    (dolist (triple graph)
      (cond
        ((eql (triple-relation triple) '|value|)
         (setf result (cons (triple-head triple) result)))
        (t
         (setf result (cons (triple-head triple) result))
         (setf result (cons (triple-tail triple) result)))))
    (remove-duplicates result :test #'equal)))

;;-----------------------------------------------------------------------
;; DESC: Give a graph extract all the unique types within the graph.
;; INPUT: graph = a list of triples.
;; OUTPUT: A list of the types/classes referenced in the graph. 
;;-----------------------------------------------------------------------
;;[jchaw-patch, 20Aug08, Changed (node-type ...) to be (semantic-matcher-node-type)
(defun extract-types-in-graph (graph)
  (let ((nodes (extract-nodes-in-graph graph)) types)
    (dolist (node nodes)
      (cond
        ((or (is-instance-p node) (is-value-p node)))
        ((atom (semantic-matcher-node-type node))  (setf types (cons   (semantic-matcher-node-type node) types)))
        (t			  (setf types (append (semantic-matcher-node-type node) types)))))
    types))

;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, extract all the unique relations
;;       from this list.
;; INPUT: triples = a list of triples.
;; OUTPUT: A list of the unique relations from the list of triples.
;;         **NOTE**: This function does not install the inverses nor
;;              does it check for duplicate relations that are inverses
;;              of each other.
;;-----------------------------------------------------------------------
(defun extract-relations-in-graph (triples)
  (let (done-relations relation results)
    ;; Iterate through each triple and get their relations plus the relations
    ;; of those transformations that can lead to these relations.
    (dolist (triple triples)
      (setf relation (triple-relation triple))
      (cond
        ((member relation done-relations :test #'equal))
        (t
         (dolist (xform (lookup-transformations (list triple)))
           (setf results (append (get-relation-path (get-xform-lhs xform)) results)))
         (setf done-relations (cons relation done-relations)))))
    ;; Remove any duplicates relations
    (union done-relations (remove-duplicates results :test #'eql))))

;;-----------------------------------------------------------------------
;; DESC: Given a list of triples, extract all the unique relations from
;;	 this list plus all their inverses.
;; INPUT: triples = a list of triples.
;; OUTPUT: A list of the unique relations (and their inverses) from 
;;	   the list of triples.
;;-----------------------------------------------------------------------
(defun extract-relations-in-graph-inv (triples) 
  (let (done-relations relation results)
    (dolist (triple triples)
      (setf relation (triple-relation triple))
      (cond
        ((or (eql relation '|instance-of|) (member relation done-relations :test #'eql)))
        (t
         (dolist (xform (lookup-transformations (list triple)))
           (setf results (append (get-relation-path (get-xform-lhs xform)) results)))
         (setf done-relations (cons relation done-relations)))))
    (setf results (union done-relations (remove-duplicates results :test #'eql)))
    (append results (mapcar #'(lambda (r) (invert-slot r)) results))))

;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------
;;;; FILE: graph-util-prototype.lisp
;;;; 
;;;; This file includes code to extract a KM concept represented as
;;;; a prototype.
;;;;
;;;; FUNCTION INVENTORY:
;;;;	- (defun get-km-prototype-for (km-class-name)
;;;;	- (defun get-triples (prototype-participants)
;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------

;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Global parameters.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; A list of relations we can ignore. These are house keeping slots
;; that do not carry any content.
;;----------------------------------------------------------------------
(defparameter *slots-to-ignore-for-conversion*
  '(|prototype-of| |prototype-scope| |prototype-participants| |new-objects|
    |prototypes| |superclasses| |element-type| |has-pattern| |critique-score| 
    |coa-description| |applies-to-any| |participants| |unitAssignedToTask|
    |unitAssignedToOperation| |enables-SitSitType| |number-of-elements| |rcc8-DC| 
    |rcc8-EC| |rcc8-EQ| |rcc8-PO| |rcc8-TPP| |rcc8-TPPi| |big-nodes|
    |rcc8-NTPPi| |rcc8-NTPP| |spatiallyIntersects| |is-west-of| |is-north-of| 
    |is-northwest-of| |is-southwest-of| |internally-expanded| |node-visibility|
    |cloned-from| |contains-node| |contains-edge| |big-node-is-open| |called|
    |has-clones|

    ;; Slot in the KM Slot Group. These slots should be ignored if they ever
    ;; shown up.
    |property-slot| |cardinal-unit-class| |categorical-constant-class| 
    |caused-by-class| |cmap-correspondence| |description| |defeated-by-class| 
    |edit-state| |from-value| |max-value| |min-value| |primary-slot| 
    |primitive-actions| |property-of-slot| |property| |required-slot| 
    |scalar-constant-class| |scale-class| |secondary-slot| |nuSketch-coordinate|
    |nuSketch-X-coordinate| |nuSketch-Y-coordinate| |combine-values-by-appending|

    ;; Additional slots to ignore if they shown up!
    ;; |/==| |equation-symbol| |equation-uses| |equation| |equation-expression|
   ))

;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Top-level function to get the representation of a concept.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: Function to extract the prototype definition of a concept. We 
;;	 are assuming one prototype definition for each class. In the 
;;	 event of multiple definition, only the first one will be used 
;;	 and the others will be discarded.
;; INPUT: km-class-name = The class name of the concept. 
;; OUTPUT: The output is the prototype root (which is a KM instance)
;;	   plus a list of triples in CPL format.
;;----------------------------------------------------------------------
#|
(defun get-km-prototype-def (km-class-name)
  (let ((prototypes (get-vals km-class-name '|prototypes| :facet 'own-properties :situation *global-situation*)) result)
    ;; 1) Always change to a local situation if we are not in one!
    (if (eql (curr-situation) '|*Global|) (new-situation))
    ;; 2) Extract the representation only if there is a prototype.
    (cond 
     (prototypes
      ;; Make a copy of the prototype, so we don't mess it up.
      (multiple-value-bind (proto-root alist)
	(clone (first prototypes))
	(setf result (list proto-root (get-km-prototype-triples (mapcar #'(lambda (x) (rest x)) alist)))))))
    ;; 3) Return the representation. This representation is in CPL format.
    result))
|#
(defun get-km-prototype-def (km-class-name)
  (let ((prototypes (get-vals km-class-name '|prototypes| :facet 'own-properties :situation *global-situation*))
        prototype-scope target-protoype result)
    ;; 1) Always change to a local situation if we are not in one!
    (if (eql (curr-situation) '|*Global|) (new-situation))
    ;; 2) Extract the representation only if there is a prototype.
    (cond
     (prototypes
      (dolist (prototype prototypes)
        (setf prototype-scope (get-vals prototype '|prototype-scope| :facet 'own-properties :situation *global-situation*))
        (if (member km-class-name prototype-scope :test #'equal)
            (progn (setf target-protoype prototype) (return))))
      (if (null target-protoype) (setf target-protoype (first prototypes)))
      ;; Make a copy of the prototype, so we don't mess it up.
      (multiple-value-bind (proto-root alist)
        (clone target-protoype)
        (setf result (list proto-root (get-km-prototype-triples (mapcar #'(lambda (x) (rest x)) alist)))))))
    ;; 3) Return the representation. This representation is in CPL format.
    result))


;;----------------------------------------------------------------------
;; DESC: Function that does the actual work of gathering the triples 
;;	 of a prototype definition. This function will throw away
;; 	 complex structures -- e.g. (if ...), ( .. && .. ), etc..
;; INPUT: prototype-participants = A list of prototype participants 
;;		-- i.e. instances associated with the prototype.
;; OUTPUT: A list of triples in CPL format.
;;----------------------------------------------------------------------
(defun get-km-prototype-triples (prototype-participants)
  (let ((triple-hash (make-hash-table :test #'equal)) ;; This is used to maintain duplicate triples, so we can manage them more efficiently.
	all-instances-examined all-slot+vals 
	slot vals triple all-triples)
    ;; 1) Iterate through each prototype participant to collect its properties.
    (dolist (prototype-participant prototype-participants)
      ;; 1.1) Get all properties of the prototype participant.
      (setf all-slot+vals (append (get-slotsvals prototype-participant :facet 'own-properties :situation *global-situation*)
				  (get-slotsvals prototype-participant :facet 'own-properties :situation (curr-situation))))
      ;; 1.2) Now process each slot values pair.	
      (dolist (slot+vals all-slot+vals)
	(setf slot (slot-in slot+vals))
	(setf vals (remove-duplicates (vals-in slot+vals) :test #'equal))
	(cond 
	 ((eql slot '|instance-of|)
	  (if (eql (length vals) 1) 
	      (setf triple (list prototype-participant '|instance-of| (first vals)))
	      (setf triple (list prototype-participant '|instance-of| vals)))
	  (cond 
	    ((not (gethash triple triple-hash)) 
	     (setf all-triples (cons triple all-triples))
	     (setf (gethash triple triple-hash) triple))))
	 ((and (not (member slot *slots-to-ignore-for-conversion* :test #'eql))
	       (not (member (invert-slot slot) *slots-to-ignore-for-conversion* :test #'eql)))
	  (setf all-instances-examined (append vals all-instances-examined))
	  (dolist (val vals)
	    (cond 
	      ((and (consp val) 
		    (or (eql (first val) ':|pair|)
			(eql (first val) ':|set|)
			(eql (first val) ':|seq|)))
	       (setf triple (list prototype-participant slot val)))
	      ((and (consp val) (eql (first val) 'QUOTE))
	       (setf triple (list prototype-participant slot val)))
	      ;; Check if the instance is associated with a comment tag. If so, then we should 
	      ;; throw away the comment tag, because we do not use it in problem solving.
	      ((and (consp val) 
		    (eql (length val) 2)
		    (comment-tagp (second val)))
	       (setf triple (list prototype-participant slot (first val))))
	      ((atom val) 				    
	       (setf triple (list prototype-participant slot val)))
	      (t 					    
	       (setf triple nil)))
	    (cond 
	      ((and triple (not (gethash triple triple-hash)) (not (gethash (invert-triple triple) triple-hash)))
	       (setf all-triples (cons triple all-triples))
	       (setf (gethash triple triple-hash) triple))))))))
    ;; 2) Check to make sure we did not miss the types of instances.
    (setf all-instances-examined (remove-duplicates all-instances-examined :test #'equal))
    (setf all-instances-examined (remove-if #'(lambda (x) (or (consp x) (named-instancep x))) all-instances-examined))
    (dolist (instance all-instances-examined)
      (cond
	((not (member instance all-triples
		:test #'(lambda (x y) (and (eql x (triple-head y)) (eql (triple-relation y) '|instance-of|)))))
	 (setf vals (get-vals instance '|instance-of| :facet 'own-properties :situation *global-situation*))
	 (cond
	  ((and vals (eql (length vals) 1)) (setf triple (list instance '|instance-of| (first vals))))
	  ((and vals (> (length vals) 1))   (setf triple (list instance '|instance-of| vals)))
	  (t 				    (setf triple nil)))
	 (if triple (setf all-triples (cons triple all-triples))))))
    ;; 3) Return all the triple gathered.
    all-triples))
(defun load-lib (&optional (comp-path "/u/pzyeh/RKF/Working/Trunk/components/"))
  (let (result)
    (dolist (component (traverse-directory comp-path) result)
      (if (equal (pathname-type component) "km")
          (progn
	    (load-kb component)
	    (setf result (cons (pathname-name component) result)))))))

(defun traverse-directory (root-dir-path)
  (let ((dir-list (directory root-dir-path)) sub-dir-path result)
    (dolist (element dir-list result)
      (if (directory (setf sub-dir-path (concatenate 'string root-dir-path (pathname-name element) "/")))
          (setf result (append (traverse-directory sub-dir-path) result))
          (setf result (cons element result))))))
                


;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------
;;;; FILE: cpl-conversion.lisp
;;;; 
;;;; Code to convert between the format used by the matcher and CPL.
;;;;
;;;; FUNCTION INVENTORY:
;;;; - (defun convert-to-peters-form (petes-triples  &optional node-to-km-mappings)
;;;; - (defun convert-to-petes-form  (peters-triples &optional node-to-km-mappings)
;;;; - (defun convert-triple-to-peters-form (triple node-to-km-mappings km-to-node-mappings)
;;;; - (defun convert-triple-to-petes-form  (triple node-to-km-mappings km-to-node-mappings)
;;;; - (defun get-instance-of-triples    (triples node-to-km-mappings km-to-node-mappings)
;;;; - (defun create-instance-of-triples (triples node-to-km-mappings)
;;;; - (defun create-and-hash-new-node-instance (km-instance node-to-km-mappings km-to-node-mappings)
;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------

;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Functions to convert between the different formats used by CPL and
;;; the matcher.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
  
;;----------------------------------------------------------------------
;; DESC: Given a list of triples in Pete's CPL format, convert them to
;;	 a list of triples in the format used by the matcher.
;; INPUT: petes-triples = a list of triples in Pete's CPL format.
;;	  node-to-km-mappings (optional) = hash table with the mappings
;;		from a graph node to km concept
;;	  km-to-node-mappings (optional) = hash table with the mappings
;;		from a km concept to a graph node.
;; OUTPUT: A list of triples in Peter's format.
;;----------------------------------------------------------------------
(defun convert-to-peters-form (petes-triples &optional (node-to-km-mappings *node-to-km-mappings*)
						       (km-to-node-mappings *km-to-node-mappings*))
  (let (triples)
    ;; Get all instance-of triples in petes-triples and hash them.
    (get-instance-of-triples petes-triples node-to-km-mappings km-to-node-mappings)
    ;; For each triple in petes-triples convert them to the format used by the matcher.
    (dolist (triple petes-triples)
      (cond
       ((eql (triple-relation triple) '|instance-of|))
       (t
        (setf triples (cons (convert-triple-to-peters-form triple node-to-km-mappings km-to-node-mappings) triples)))))
    triples))

;;----------------------------------------------------------------------
;; DESC: Given a list of triples in the format used by the Matcher, 
;;	 convert them to the format used in CPL.
;; INPUT: peters-triples = a list of triples in the Matcher format.
;;	  node-to-km-mappings (optional) = hash table with the mappings
;;		from a graph node to km concept
;;	  km-to-node-mappings (optional) = hash table with the mappings
;;		from a km concept to a graph node.
;; OUTPUT: A list of triples in Pete's format.
;;----------------------------------------------------------------------
(defun convert-to-petes-form (peters-triples &optional (node-to-km-mappings *node-to-km-mappings*)
						       (km-to-node-mappings *km-to-node-mappings*))
  (let (triples pete-triple)
    ;; Convert each triple to Pete's form.
    (dolist (triple peters-triples)
      (setf pete-triple (convert-triple-to-petes-form triple node-to-km-mappings km-to-node-mappings))
      (if pete-triple (setf triples (cons pete-triple triples))))
    ;; Make sure to create the instance-of triples and add them to the final output.
    (append (create-instance-of-triples peters-triples node-to-km-mappings) triples)))


;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Fucntions to convert an single triple between the CPL and 
;;; Matcher format.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: Given a triple in Pete's form convert it to Peter's form.
;; INPUT: triple = a triple in Pete's (i.e. CPL) form.
;;	  node-to-km-mappings = hash with the mappings from a graph
;;		node instance to a KM instance.
;;	  km-to-node-mappings = hash with the mappings from a KM
;;		instance to a graph node instance.
;; OUTPUT: The corresponding triple in Peter's format.
;;----------------------------------------------------------------------
(defun convert-triple-to-peters-form (triple node-to-km-mappings km-to-node-mappings)
  (let ((head (or (gethash (triple-head triple) km-to-node-mappings)
                  (create-and-hash-new-node-instance (triple-head triple) node-to-km-mappings km-to-node-mappings)))
        (rel  (triple-relation triple))
        (tail (if (atom (triple-tail triple))
                  (or (gethash (triple-tail triple) km-to-node-mappings)
                      (create-and-hash-new-node-instance (triple-tail triple) node-to-km-mappings km-to-node-mappings))
                  (triple-tail triple))))
    (list head rel tail)))
    
;;----------------------------------------------------------------------
;; Given a class, create a new KM instance of that class.
;;----------------------------------------------------------------------
;;jchaw: 18May08, resurrected from old cpl-semantic-matching.lisp code. 
;;       necessary for (convert-triple-to-petes-form) to work with expanded triples introducing bridging instances.
(defun create-and-hash-new-km-instance (node node-to-km-mappings km-to-node-mappings)
  (let* ((node-type       (first node))
	 (new-km-instance (ps-instantiate-concept node-type)))
    (if (not (null new-km-instance))
	(progn
	  (setf (gethash node node-to-km-mappings) new-km-instance)
	  (setf (gethash new-km-instance km-to-node-mappings) node)))
    new-km-instance))

;;----------------------------------------------------------------------
;; DESC: Given a triple in peters form convert it to Pete's form.
;; INPUT: triple = a triple in Peter's form.
;;	  node-to-km-mappings = hash with the mappings from a graph
;;		node instance to a KM instance.
;;	  km-to-node-mappings = hash with the mappings from a KM
;;		instance to a graph node instance.
;; OUTPUT: The corresponding triple in Pete's format.
;;----------------------------------------------------------------------
;;jchaw: 18May08, uncommented calls to (create-and-hash-new-km-instance head node-to-km-mappings km-to-node-mappings).
;;       necessary for (convert-triple-to-petes-form) to work with expanded triples introducing bridging instances.
(defun convert-triple-to-petes-form (triple node-to-km-mappings km-to-node-mappings)
  (let ((head     (triple-head triple))
        (relation (triple-relation triple))
        (tail     (triple-tail triple))
        km-head km-tail)
    ;; See if the head is bounded.
    (cond
      ((is-instance-p head)
       (setf km-head (first head)))
      (t
       (setf km-head (gethash head node-to-km-mappings))
       (if (not km-head) (setf km-head (create-and-hash-new-km-instance head node-to-km-mappings km-to-node-mappings)))
      ))
    ;; Do the same thing for the tail.
    (cond
      ((is-instance-p tail)
       (setf km-tail (first tail)))
      ((is-value-p tail)
       (setf km-tail tail))
      (t
       (setf km-tail (gethash tail node-to-km-mappings)) 
       (if (not km-tail) (setf km-tail (create-and-hash-new-km-instance tail node-to-km-mappings km-to-node-mappings)))
      ))
    (if (and km-head km-tail) (list km-head relation km-tail) nil)))


;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Functions for extracting, storing, and creating new instances.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: Given a list of triples in CPL (i.e. Pete's) format: 
;;	 1) extract all the instance-of triples, 2) create a 
;;	 graph node corresponding to the KM instance, and 3)
;; 	 hash the mapping between the graph node instance and
;;	 the KM instance.
;; INPUT: triples = a list of triples in CPL format.
;;	  node-to-km-mappings = hash to store the mappings from a 
;;		graph node instance to a KM instance.
;;	  km-to-node-mappings = hash to store the mappings from a 
;;		KM instance to a graph node instance.
;; OUTPUT: This function does not return any output, but as a side 
;;	   effect behavior, it will 1) create a corresponding graph
;;	   node instance for each KM instance encountered and 2)
;;	   store the mappings between the graph and KM instances.
;;----------------------------------------------------------------------
(defun get-instance-of-triples (triples node-to-km-mappings km-to-node-mappings)
  (let (unique-id new-node)
    (dolist (triple triples)
      (if (eql (triple-relation triple) '|instance-of|)
          (progn
            (setf unique-id (string (gensym "Node")))
            (setf new-node (cons (triple-tail triple) unique-id))
            (setf (gethash new-node node-to-km-mappings) (triple-head triple))
            (setf (gethash (triple-head triple) km-to-node-mappings) new-node))) )))
#|
(defun get-instance-of-triples (triples node-to-km-mappings km-to-node-mappings)
  (let (unique-id new-node)
    (dolist (triple triples)
      (cond
	((and (eql (triple-relation triple) '|instance-of|) (not (eql (triple-tail triple) '|_WHATEVER|)))
	 (cond
	   ((not (anonymous-instancep (triple-head triple)))
	    (setf new-node (cons (triple-head triple) nil))
	    (km `(,(triple-head triple) |has| (|instance-of| (,(triple-tail triple))))))
	   (t
	    (setf unique-id (string (gensym "Node")))
            (setf new-node (cons (triple-tail triple) unique-id))))
	 (setf (gethash new-node node-to-km-mappings) (triple-head  triple))
	 (setf (gethash (triple-head triple) km-to-node-mappings) new-node)))) ))
|#
  
  
;;----------------------------------------------------------------------
;; DESC: Given graph (i.e. a list of triples in Peter's format), create 
;;	 an instance-of triple for each unique node in the graph. We
;;	 need this function to make the output compliant with the 
;;	 format expected by CPL.
;; INPUT: triples = a list of triples in Peter's format.
;;	  node-to-km-mappings = hash with the mappings from a graph
;;		node instance to a KM instance.
;; OUTPUT: A list of instance-of triples -- one for each unique node
;;	   in the input graph.
;;----------------------------------------------------------------------
(defun create-instance-of-triples (triples node-to-km-mappings)
  (let (nodes instance instance-triples)
    ;; Get all the nodes in the graph.
    (setf nodes (remove-duplicates
		 (apply #'append (mapcar #'(lambda (x) (list (triple-head x) (triple-tail x))) triples))
		 :test #'equal))
    ;; For each node, look up its correspondings KM instance.
    (dolist (node nodes)
      (setf instance (gethash node node-to-km-mappings))
      (if instance
          (setf instance-triples (cons (list instance '|instance-of| (first node)) instance-triples))))
    instance-triples))

#|
;;----------------------------------------------------------------------
;; Given a class, create a new KM instance of that class.
;;----------------------------------------------------------------------
(defun create-and-hash-new-km-instance (node node-to-km-mappings km-to-node-mappings)
  (let* ((node-type (first node))
         (new-name (if (consp node-type) (first node-type) node-type))
         (newsym (intern (string (gensym (concatenate 'string "_" (string new-name)))) :km))
         new-km-instance)
    (cond
      ((atom node-type)
       (setf new-km-instance (first (km0 `(,newsym |has| (|instance-of| (,node-type)))))))
      ((consp node-type)
       (setf new-km-instance (first (km0 `(,newsym |has| (|instance-of| ,node-type)))))))
    (if new-km-instance
        (progn
          (setf (gethash node node-to-km-mappings) new-km-instance)
          (setf (gethash new-km-instance km-to-node-mappings) node)))
    new-km-instance))
|#

;;----------------------------------------------------------------------
;; DESC: This function is called by convert-triple-to-peters-form. 
;;	 Its purpose is to create a new graph node corresponding to 
;;	 the given KM instance. This function will also hash the 
;;	 the mappings between the newly create node instance and
;;	 the existing KM instance.
;; INPUT: km-instance = a KM instance -- e.g. |_Dog12|
;;	  node-to-km-mappings = hash to store the mappings from a 
;;		graph node instance to a KM instance.
;;	  km-to-node-mappings = hash to store the mappings from a 
;;		KM instance to a graph node instance.
;; OUTPUT: The newly created graph node instance that corresponds to
;;	   the given KM instance.
;;----------------------------------------------------------------------
(defun create-and-hash-new-node-instance (km-instance node-to-km-mappings km-to-node-mappings)
  (let (new-node)
    (cond
      ;; There is no need to hash a named instance.
      ((named-instancep km-instance)
       (list km-instance))
      (t
       (setf new-node (cons (singleton-class (immediate-classes km-instance)) (string (gensym "Node"))))
       (setf (gethash new-node node-to-km-mappings) km-instance)
       (setf (gethash km-instance km-to-node-mappings) new-node)
       new-node))))

;;;-------------------------------------------------------------
;;;-------------------------------------------------------------
;;; Additional transformations for the domain of CPL.
;;;-------------------------------------------------------------
;;;-------------------------------------------------------------
								
(defparameter *additional-transformations*  '(

 	;;----------------------------------------------------------------------
 	;;----------------------------------------------------------------------
	;; Domain neutral xforms we need to migrate to the main distribution.
 	;;----------------------------------------------------------------------
 	;;----------------------------------------------------------------------
	((((|Event| . 1)	|site|			(|Place| . 2))
          ((|Place| . 2)	|location-of|   	(|Tangible-Entity| . 3)))
         (((|Event| . 1)	|site|			(|Tangible-Entity| . 3))))

	;; Part descension through Message.
	((((|Event|   . 1)	|object|		(|Message| . 2))
          ((|Message| . 2)	|information-content|   (|Entity|  . 3)))
         (((|Event|   . 1)	|object|		(|Entity|  . 3))))
	((((|Event|   . 1)	|object|		(|Message| . 2))
          ((|Message| . 2)	|information-content|   (|Event|   . 3)))
         (((|Event|   . 1)	|object|		(|Event|   . 3))))
	;; Part descension through Information.
	((((|Event|   	  . 1)	|object|		(|Information| . 2))
          ((|Information| . 2)	|information-content|   (|Entity|      . 3)))
         (((|Event|   	  . 1)	|object|		(|Entity|      . 3))))
	((((|Event|   	  . 1)	|object|		(|Information| . 2))
          ((|Information| . 2)	|information-content|   (|Role|        . 3)))
         (((|Event|   	  . 1)	|object|		(|Role|        . 3))))
	((((|Event|   	  . 1)	|object|		(|Information| . 2))
          ((|Information| . 2)	|information-content|   (|Event|       . 3)))
         (((|Event|   	  . 1)	|object|		(|Event|       . 3))))


	;; Part ascension/descension through elements.
	((((|Event|     . 1)	|object|                (|Aggregate| . 2))
	  ((|Aggregate| . 2)	|element|               (|Entity|    . 3)))
	 (((|Event|     . 1)	|object|                (|Entity|    . 3))))
	((((|Event|     . 1)	|object|                (|Entity|    . 2))
	  ((|Entity|    . 2)	|element-of|            (|Aggregate| . 3)))
	 (((|Event|     . 1) 	|object|                (|Aggregate| . 3))))
        ((((|Event|  . 1)	|object|                (|Entity| . 2))
          ((|Entity| . 2)	|has-part|              (|Entity| . 3)))
         (((|Event|  . 1)	|object|                (|Entity| . 3))))
        ((((|Event|  . 1)	|instrument|            (|Entity|    . 2))
          ((|Entity| . 2)	|element-of|            (|Aggregate| . 3)))
         (((|Event|  . 1)	|instrument|            (|Aggregate| . 3))))

        ((((|Event|  	   . 1)	|agent|			(|Person| . 2))		;; <-- 11/03/05 New rule.
          ((|Person| 	   . 2)	|element-of| 		(|Community| . 3)))
         (((|Event|  	   . 1)	|agent|			(|Community| . 3))))
        ((((|Event|  	   . 1)	|agent|			(|Person| . 2))		;; <-- 11/03/05 was Entity.
          ((|Person| 	   . 2)	|element-of| 		(|Organization| . 3)))
         (((|Event|  	   . 1)	|agent|			(|Organization| . 3))))
        ((((|Event|  	   . 1)	|agent|			(|Organization| . 2))
          ((|Organization| . 2)	|element| 		(|Person| . 3)))
         (((|Event|  	   . 1)	|agent|			(|Person| . 3))))
        ((((|Event|  	   . 1)	|recipient|		(|Person| . 2))		;; <-- 11/03/05 New rule.
          ((|Person| 	   . 2)	|element-of| 		(|Organization| . 3)))
         (((|Event|  	   . 1)	|recipient|		(|Organization| . 3))))
        ((((|Event|  	   . 1)	|recipient|		(|Organization| . 2))	;; <-- Event was Express.
          ((|Organization| . 2)	|element| 		(|Person| . 3)))
         (((|Event|  	   . 1)	|recipient|		(|Person| . 3))))
        ((((|State|  	   . 1)	|experiencer|		(|Person| . 2))		;; <-- 11/08/05 New rule.
          ((|Person| 	   . 2)	|element-of| 		(|Organization| . 3)))
         (((|State|  	   . 1)	|experiencer|		(|Organization| . 3))))

	;; New rule -- 10/26/05
	((((|Tangible-Entity| . 1) |material|           (|Tangible-Entity| . 2))
          ((|Tangible-Entity| . 2) |has-part|           (|Tangible-Entity| . 3)))
         (((|Tangible-Entity| . 1) |material|           (|Tangible-Entity| . 3))))
	((((|Organization|    . 1) |element|		(|Role|   . 2))
	  ((|Role|	      . 2) |played-by|		(|Person| . 3)))
	 (((|Organization|    . 1) |element|		(|Person| . 3))))
	((((|Tangible-Entity| . 1) |is-part-of|		(|Animal| . 2))
	  ((|Animal|	      . 2) |plays|		(|Participant| . 3)))
 	 (((|Tangible-Entity| . 1) |is-part-of|		(|Participant| . 3))))
	((((|Entity| 	      . 1) |is-possessed-by|	(|Animal| . 2))		;; <-- 11/07/05 -- Changes T-E to Entity.
	  ((|Animal|	      . 2) |plays|		(|Participant| . 3)))	;; <--
 	 (((|Entity| 	      . 1) |is-possessed-by|	(|Participant| . 3))))
        ((((|Tangible-Entity| . 1) |has-part|    	(|Aggregate| 	   . 2))   ;; <-- 11/14/05 -- New rule.
          ((|Aggregate|       . 2) |element|            (|Tangible-Entity| . 3)))  
         (((|Tangible-Entity| . 1) |has-part|    	(|Tangible-Entity| . 3))))
	;; New rule -- 06/02/06
        ((((|Tangible-Entity| . 1) |has-part|         	(|Tangible-Entity| . 2))
          ((|Tangible-Entity| . 2) |plays|              (|Role| . 3)))
         (((|Tangible-Entity| . 1) |has-part|         	(|Role| . 3))))

	;; New rules -- 10/21/05
	((((|Tangible-Entity| . 1) |plays|		(|Container| . 2))
	  ((|Container|	      . 2) |content|		(|Tangible-Entity| . 3)))
	 (((|Tangible-Entity| . 1) |content|		(|Tangible-Entity| . 3))))
        ((((|Move|  	      . 1) |destination|	(|Spatial-Entity| . 2))
          ((|Spatial-Entity|  . 2) |plays|		(|Role|   	  . 3)))
         (((|Move|  	      . 1) |destination|	(|Role|   	  . 3))))
	;; *PZY*: 05/31/06
        ;; ((((|Move|  	      . 1) |destination|	(|Role|   	  . 2))
        ;;   ((|Role|         . 2) |played-by|  	(|Spatial-Entity| . 3)))
        ;;  (((|Move|  	      . 1) |destination|	(|Spatial-Entity| . 3))))
        ((((|Move|  	      . 1) |origin|		(|Spatial-Entity| . 2))
          ((|Spatial-Entity|  . 2) |plays|		(|Role|   	  . 3)))
         (((|Move|  	      . 1) |origin|		(|Role|   	  . 3))))
	;; *PZY*: 05/31/06
        ;; ((((|Move|  	      . 1) |origin|		(|Role|   	  . 2))
        ;;   ((|Role|         . 2) |played-by|  	(|Spatial-Entity| . 3)))
        ;;  (((|Move|  	      . 1) |origin|		(|Spatial-Entity| . 3))))
	((((|Move|	      . 1) |path|		(|Spatial-Entity| . 2))
	  ((|Spatial-Entity|  . 2) |plays|		(|Conduit|	  . 3)))
	 (((|Move|	      . 1) |path|		(|Conduit|	  . 3)))) 
	((((|Physical-Object| . 1) |object-of|		(|State| . 2)))
	 (((|Physical-Object| . 1) |equal|		(|State| . 2))))

 	;;----------------------------------------------------------------------
 	;;----------------------------------------------------------------------
	;; Rules to resolve alternations in CPL. These should not be considered
	;; as xforms.
 	;;----------------------------------------------------------------------
 	;;----------------------------------------------------------------------
	((((|See|   . 0)	|object|	(|Light| . 1))			;; <-- 10/20/05
	  ((|Light| . 1)	|object-of|	(|Emit|  . 2))
	  ((|Emit|  . 2)	|origin|	(|Tangible-Entity| . 3)))
	 (((|See|   . 0)	|object|	(|Tangible-Entity| . 3)))) 

	;; Xforms for Chemistry.
	((((|Mix| . 0)		|raw-material|	(|Chemical-Entity| . 1))) 
	 (((|Mix| . 0)		|object|	(|Chemical-Entity| . 1))))
	((((|Mix| . 0)		|raw-material|	(|Chemical| . 1))) 
	 (((|Mix| . 0)		|object|	(|Chemical| . 1))))
	((((|Mix| . 0)		|raw-material|	(|Chemical-Entity| . 1))) 
	 (((|Mix| . 0)		|instrument|	(|Chemical-Entity| . 1))))
	((((|Mix| . 0)		|raw-material|	(|Chemical| . 1))) 
	 (((|Mix| . 0)		|instrument|	(|Chemical| . 1))))
	((((|Reaction| . 0)	|raw-material|	(|Chemical-Entity| . 1))) 
	 (((|Reaction| . 0)	|object|	(|Chemical-Entity| . 1))))
	((((|Reaction| . 0)	|raw-material|	(|Chemical| . 1))) 
	 (((|Reaction| . 0)	|object|	(|Chemical| . 1))))
	((((|Reaction| . 0)	|raw-material|	(|Chemical-Entity| . 1))) 
	 (((|Reaction| . 0)	|agent|		(|Chemical-Entity| . 1))))	;; <-- 06/02/06: Was |instrument|
	((((|Reaction| . 0)	|raw-material|	(|Chemical| . 1))) 
	 (((|Reaction| . 0)	|agent|		(|Chemical| . 1))))		;; <-- 06/02/06: Was |instrument|

        ((((|Mix|  . 1)       	|raw-material|              (|Chemical| . 2))
          ((|Chemical| . 2)     |has-basic-structural-unit| (|Chemical-Entity| . 3)))
         (((|Mix|  . 1)       	|raw-material|              (|Chemical-Entity| . 3))))
        ((((|Reaction| . 1)     |raw-material|              (|Chemical| . 2))
          ((|Chemical| . 2)     |has-basic-structural-unit| (|Chemical-Entity| . 3)))
         (((|Reaction| . 1)     |raw-material|              (|Chemical-Entity| . 3))))
	((((|Dissolve| . 0)	|object|		    (|Chemical| . 1)))
	 (((|Dissolve| . 0)	|agent|			    (|Chemical| . 1))))
	((((|Dissolve| . 0)	|object|		    (|Chemical-Entity| . 1)))
	 (((|Dissolve| . 0)	|agent|			    (|Chemical-Entity| . 1))))
	((((|Dissolve| . 0)	|object|		    (|Chemical| . 1))
	  ((|Chemical| . 1)	|has-basic-structural-unit| (|Chemical-Entity| . 2)))
	 (((|Dissolve| . 0)	|object|		    (|Chemical-Entity| . 2))))
	((((|Dissolve| . 0)	|base|		    	    (|Chemical| . 1))		;; <-- 06/01/06
	  ((|Chemical| . 1)	|has-basic-structural-unit| (|Chemical-Entity| . 2)))
	 (((|Dissolve| . 0)	|base|		    	    (|Chemical-Entity| . 2))))

	;; A couple of very domain specific rules to handle some weird CPL cases.
	((((|Be-Touching|  . 0)	|object|	(|Entity| . 1)))		;; <-- 11/09/05
	 (((|Be-Touching|  . 0)	|agent|		(|Entity| . 1))))
	((((|Be-Contained| . 0)	|base|		(|Entity| . 1)))		;; <-- 11/15/05
	 (((|Be-Contained| . 0)	|agent|		(|Entity| . 1))))
	((((|Slide| . 0)	|object|	(|Inanimate-Object| . 1)))
	 (((|Slide| . 0)	|experiencer|	(|Inanimate-Object| . 1))))
	((((|Fall| . 0)		|object|	(|Tangible-Entity| . 1)))	;; <-- 10/20/05
	 (((|Fall| . 0)		|experiencer|	(|Tangible-Entity| . 1))))
        ((((|Fall| . 0)         |object|        (|Role| . 1)))       		;; <-- 06/07/06
         (((|Fall| . 0)         |experiencer|   (|Role| . 1))))
	((((|Increase| . 0)	|from-value|	(|Property-Value| . 1)))
	 (((|Increase| . 0)	|agent|		(|Property-Value| . 1))))
	((((|Increase| . 0)	|from-value|	(|Property-Value| . 1)))
	 (((|Increase| . 0)	|object|	(|Property-Value| . 1))))
	((((|Decrease| . 0)	|from-value|	(|Property-Value| . 1)))
	 (((|Decrease| . 0)	|agent|		(|Property-Value| . 1))))
	((((|Decrease| . 0)	|from-value|	(|Property-Value| . 1)))
	 (((|Decrease| . 0)	|object|	(|Property-Value| . 1))))
	((((|Increase| . 0)	|object|	(|Inanimate-Object| . 1)))
	 (((|Increase| . 0)	|agent|		(|Inanimate-Object| . 1))))
	((((|Decrease| . 0)	|object|	(|Inanimate-Object| . 1)))
	 (((|Decrease| . 0)	|agent|		(|Inanimate-Object| . 1))))

	;; For Physics question.
	((((|Length-Value|    . 0) |height-of|	(|Physical-Object| . 1))
	  ((|Physical-Object| . 1) |has-region| (|Spatial-Entity|  . 3)))
	 (((|Length-Value|    . 0) |height-of|	(|Spatial-Entity|  . 3))))

	;; 03/29/06 -- Temporary rules for has-solute. 
        ((((|Solution|  . 1)    |has-solute|              	(|Chemical| . 2))
	  ((|Chemical| 	. 2)	|has-basic-structural-unit| 	(|Chemical-Entity| . 3)))
	 (((|Solution|  . 1)	|has-solute|              	(|Chemical-Entity| . 3))))
        ((((|Solution|  . 1)    |has-part|              	(|Chemical| . 2)))
	 (((|Solution|  . 1)	|has-solute|              	(|Chemical| . 3))))
	
 	;;----------------------------------------------------------------------
	;; Domain specific rules for property.
 	;;----------------------------------------------------------------------
	;; ((((|Event| . 0)		|distance|	(|Length-Value|   . 1)))
	;;  (((|Event| . 0)		|val-property|	(|Length-Value|   . 1))))
	;; ((((|Spatial-Entity| . 0)	|length|	(|Length-Value|   . 1)))
	;;  (((|Spatial-Entity| . 0)	|val-property|	(|Length-Value|   . 1))))
	;; ((((|Move| . 0)		|velocity|	(|Velocity-Value| . 1)))
	;;  (((|Move| . 0)		|val-property|	(|Velocity-Value| . 1))))
	;;
))
(setf *domain-neutral-transformations* (append *additional-transformations* *domain-neutral-transformations*))

;;Includes Dan Tecuci's structural matching code.

;; how far we search for siblings
(defvar *max-sibling-search-radius* 6)

;; whether to allow sibling matches at all
(defvar *allow-sibling-match* nil)

;; classes that are not considered when doing sibling matches
(defvar *excluded-clib-concepts* nil)


;;----------------------------------------------------------------------
;; DESC: Given a triple, find all the other triples in the graph that 
;;       matches it.
;; INPUT: triple = ((<concept> . <ID>) <relation> (<concept> . <ID>))  
;;                 where
;;                      <concept> = <class> | (<class_1> ...)
;;                      <ID> = a number, character, symbol or string.
;;        graph = a list of triples.
;;        result (optional) = the triples in graph that match triple.
;;        match-type? (optional) = 'weak or 'strong. If set to weak 
;;              then subsumption is tested both ways.
;; OUTPUT: Returns a list of matches of the form
;;                
;;                      ( ((triple <triple>) . score) ... )
;;                      
;;         where <triple> is a triple from graph.
;;
;; dgt: duplicate file, overwrites pzy's original to include siblings check
;;----------------------------------------------------------------------
(defun triple-match (triple graph &optional (result nil) (match-type? 'weak))
  (let (score)
    (cond
      ;; Done. Nothing left to match.
      ((null graph) 
       result)
      ;; The two triples both involve scalar values. This is handled 
      ;; differently from the other triples being matched. 
      ;; Note: This function does not handle ratios, comparisons, etc.. 
      ;;       This will be be handled by a different subsystem in the 
      ;;       future.
      ((and (or (is-value-p (triple-head triple))
		(is-value-p (triple-tail triple)))
	    (or (is-value-p (triple-head (car graph)))
		(is-value-p (triple-tail (car graph)))))
       (if (setf score (scalar-value-match-p triple (car graph)))
       	   (triple-match triple (cdr graph) (cons (cons (car graph) score) result) match-type?)
	   (triple-match triple (cdr graph) result match-type?)))
      ;; At this point, we know that one of the triples has a scalar and the other doesn't. 
      ;; If this condition is satisfied, then there is no need to try to match them. Move on 
      ;; to the next one.
      ((or (is-value-p (triple-head triple))
	   (is-value-p (triple-tail triple))
	   (is-value-p (triple-head (car graph)))
	   (is-value-p (triple-tail (car graph))))
       (triple-match triple (cdr graph) result match-type?))	

      ;; The two triples match exactly.
      ((equal (flatten-triple triple) (flatten-triple (car graph)))
       (triple-match triple (cdr graph) (cons (cons (car graph) (cons '(first . first) 1)) result) match-type?))

      ;; See if one triple subsumes the other.
      ((setf score (subsume-p triple (car graph) match-type?))
       (triple-match triple (cdr graph) (cons (cons (car graph) score) result) match-type?))
      
      ;; See if one triple subsumes the other.
      ((setf score (siblings-p triple (car graph) match-type?))
       (triple-match triple (cdr graph) (cons (cons (car graph) score) result) match-type?))
      ;; No match. Go on to the next triple.
      (t
       (triple-match triple (cdr graph) result match-type?)))))

;; 


;;----------------------------------------------------------------------
;; DESC: This function takes two triples and determines whether one
;;       subsumes the other. Subsumption in this case is with respect
;;       to one triple being the superclass of another.
;; INPUT: t1 = a triple.
;;        t2 = a triple
;;        match-type? (optional) = the type of the match (i.e. strong
;;              or weak).
;; OUTPUT: NIL is returned if the two triples do not subsume each other.
;;         A list of the form 
;;			((<spec> . <spec>) . <score>)  
;;	   where <spec> = first | second 
;;		 <score> = indicating the degree of match based on 
;;			the taxonomic distance is returned otherwise.
;;
;; NOTE: It is important to realize that a negative score does not denote
;;       a negative score! A negative score means that the triples matched
;;       inversely. The absolute value of the score will be used to
;;       calculate the final score of the match.
;;----------------------------------------------------------------------
(defun siblings-p (t1 t2 &optional (match-type? 'weak))
  (if (not *allow-sibling-match*) (return-from siblings-p nil))
  (let (head1 tail1 head2 tail2 relation-match? 	
	(rel1 (triple-relation t1)) (rel2 (triple-relation t2)) rel1-inv 
	inst-h1 inst-h2 inst-t1 inst-t2			; <- For determining instances.
	tdist1 tdist1a tdist1b 				; <- For taxonomic distance of head.
	tdist2 tdist2a tdist2b  			; <- For taxonomic distance of tail.
	(head-spec 'first) (tail-spec 'first)) 
    ;; Throw away the not if there is one.
    (if (consp rel1) (setf rel1 (second rel1)))
    (if (consp rel2) (setf rel2 (second rel2)))
    (setf rel1-inv (invert-slot rel1))
    ;; Line up the relation, and also check for subslots.
    (cond
      ((or (eql rel1 rel2)
	   (member rel1 (get-all-subslots rel2) :test #'eql)
	   (member rel2 (get-all-subslots rel1) :test #'eql))
       (setf head1 (triple-head t1) tail1 (triple-tail t1)
	     head2 (triple-head t2) tail2 (triple-tail t2)
	     relation-match? 1))
      ((or (eql rel1-inv rel2)
	   (member rel1-inv (get-all-subslots rel2) :test #'eql)
	   (member rel2 (get-all-subslots rel1-inv) :test #'eql))
       (setf head1 (triple-tail t1) tail1 (triple-head t1)
             head2 (triple-head t2) tail2 (triple-tail t2)
             relation-match? -1)))
    ;; Get the immediate classes of all the instances.
    (if relation-match?
	(progn
	  (if (is-instance-p head1) 
	      (setf inst-h1 head1 head1 (immediate-classes (first head1)))
	      (setf head1 (first head1)))
	  (if (is-instance-p tail1) 
	      (setf inst-t1 tail1 tail1 (immediate-classes (first tail1)))
	      (setf tail1 (first tail1)))
	  (if (is-instance-p head2) 
	      (setf inst-h2 head2 head2 (immediate-classes (first head2)))
	      (setf head2 (first head2)))
	  (if (is-instance-p tail2) 
	      (setf inst-t2 tail2 tail2 (immediate-classes (first tail2)))
	      (setf tail2 (first tail2)))))
    ;; Determine if the two triples match structurally
    (if relation-match?
	(progn
	  (cond
	   ((and inst-h1 inst-h2)
	    (if (equal inst-h1 inst-h2)
		(progn	
		  (or (setf tdist2a (sibling-dist tail1 tail2)) 
		      (setf tdist2b (sibling-dist tail2 tail1)))
		  (setf tdist1a 0))))
	   ((and inst-t1 inst-t2)
	    (if (equal inst-t1 inst-t2)
		(progn 
		  (or (setf tdist1a (sibling-dist head1 head2)) 
		      (setf tdist1b (sibling-dist head2 head1)))
		  (setf tdist2a 0))))
	   (t
	    (or (setf tdist1a (sibling-dist head1 head2)) 
		(setf tdist1b (sibling-dist head2 head1)))
	    (or (setf tdist2a (sibling-dist tail1 tail2)) 
		(setf tdist2b (sibling-dist tail2 tail1)))))
	  (setf tdist1 (or tdist1a tdist1b))	
	  (if tdist1b (setf head-spec 'second))
	  (setf tdist2 (or tdist2a tdist2b))	
	  (if tdist2b (setf tail-spec 'second))
	  (if (and tdist1 tdist2)
	      (cons (cons head-spec tail-spec)	
		    (* relation-match? (/ (+ (/ 1 (+ tdist1 1)) (/ 1 (+ tdist2 1))) 2))))))))

(defun sibling-dist (concept1 concept2)
  (if (some #'(lambda (x) (or (tax-dist x concept1) (tax-dist x concept2))) *excluded-clib-concepts*)
      (return-from sibling-dist nil))
  (if (eq concept1 concept2) 
      0
    (let ((tax-dist (tax-dist concept1 concept2)))
      (if tax-dist 
	  tax-dist 
	(let ((supers1 (immediate-superclasses concept1))
	      (supers2 (immediate-superclasses concept2)))
	  (if (or (intersection supers1 *excluded-clib-concepts*)
		  (intersection supers2 *excluded-clib-concepts*))
	      (return-from sibling-dist nil))
	  (let ((min1 (remove-if #'null (mapcar #'(lambda(s) (sibling-dist concept1 s)) supers2)))
		(min2 (remove-if #'null (mapcar #'(lambda(s) (sibling-dist concept2 s)) supers1))))
	    (cond ((and min1 min2)
		   (+ 1 (apply 'min (append min1 min2))))
		  (min1
		   (+ 1 (apply 'min min1)))
		  (min2
		   (+ 1 (apply 'min min2)))
		  (t nil))))))))
   
(defun all-common-ancestors (c1 c2)
  (km0 `(|allof| (|the| |all-superclasses| |of| ,C1) |where| ((|the| |all-superclasses| |of| ,C2) |includes| |It|))))
  
(defun km-all-superclasses (class)
  (km0 `(|the| |superclasses| |of| ,CLASS)))

#|
;;Turn on structural matching
;(setf *allow-sibling-match* t)
;(setf *max-sibling-search-radius* 4)
;(setf *excluded-clib-concepts* '(|Action|))
|#
