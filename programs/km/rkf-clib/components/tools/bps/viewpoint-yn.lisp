;;
;; $Id: viewpoint-yn.lisp,v 1.110 2009/08/31 19:13:56 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;converts yn-query-triples into elaboration triples
;; 1) change (_X is-a Class) yn-query triples into (_X instance-of *) elaboration triple
;; 2) includes all elaboration triples for _X and _Y instances in (_X relation _Y) yn-query triple.
;;    except for Property-Relations, e.g., greater-than.      
(defun get-question-triples-for-yn-question(scenario 
					    triples-to-verify)
  (let ((debug nil))
    (remove-duplicates
     (append
     (mappend #'(lambda(triple)
		  (cond ((member (triple-relation triple) '(|is-a| |instance-of| |isa|))
			 (list (list (triple-head triple) '|instance-of| '*)))))
	      triples-to-verify)
     (mappend 'cadr
	      (get-query-map-for-yn-question scenario
					     triples-to-verify)))
     :test 'equal)))

;;FIXME! it should not return query triples whose slots are Property-Relation.
(defun get-query-map-for-yn-question(scenario triples-to-verify)
  (let ((full-instance-map (get-query-triples-for-all-instances scenario)))
    (remove nil
	    (mapcar #'(lambda(instance)
			(assoc instance full-instance-map))
		    (extract-all-instances-from-triple-list triples-to-verify)))))

;;rewrites yn-question triples when
;; (x R <property-value>) and (the R of x) evaluates to <Property-Value>. 
;; Rewritten as (x r <new-property-value>), (<new-property-value> same-as <property-value>)
;; Is this still necessary? seem that cpl-output-rewrite.lisp code should handle this.
(defun rewrite-triple-to-verify-lst(scenario triple-to-verify-lst)
  (let (result)
    (dolist (triple-to-verify triple-to-verify-lst)
      (let ((frame (triple-head triple-to-verify))
	    (slot  (triple-relation triple-to-verify))
	    (tail  (triple-tail triple-to-verify)))
	(if (and (not (member slot (all-instances '|Property-Relation|)))
		 (quasi-property-value-instance-p tail scenario))
	    (let ((lhs (quasi-ps-slot-lookup frame slot scenario)))
	      (dolist (lhs-entry lhs)
		(push (list lhs-entry '|same-as| tail) result)))
	  (push triple-to-verify result))))
    result))

(defun verify-triple-list (scenario
			   triples-to-verify)
  (let ((debug nil)
	(*use-heuristic-rulebase* t)
	yn-triple-true yn-triple-false)
    (multiple-value-bind
	(non-semantic-triples-to-verify semantic-triples-to-verify)
	(collate-triples-to-verify triples-to-verify)
      (multiple-value-bind
	  (non-semantic-yn-triple-true non-semantic-yn-triple-false non-semantic-yn-provenance)
	  (verify-non-semantic-yn-triple-list scenario non-semantic-triples-to-verify)
	(multiple-value-bind
	    (semantic-yn-triple-true 
	     semantic-yn-triple-false-contradiction semantic-yn-triple-false-cwa
	     semantic-yn-provenance)
	    (verify-semantic-yn-triple-list 
	     scenario 
	     semantic-triples-to-verify)
	  (values (append non-semantic-yn-triple-true
			  semantic-yn-triple-true)
		  (append non-semantic-yn-triple-false
			  semantic-yn-triple-false-contradiction
			  semantic-yn-triple-false-cwa)
		  (merge-provenance-info
		   non-semantic-yn-provenance
		   semantic-yn-provenance)
		  ))))))

(defun collate-triples-to-verify(triples-to-verify)
  (let (non-semantic-triples-to-verify semantic-triples-to-verify lookup-triples)
    (dolist (triple triples-to-verify)
      (let ((rel  (triple-relation triple))
	    (tail (triple-tail triple)))
	(if (equal tail '*)
	    (push triple lookup-triples) ;lookup triple. nothing to verify.
	  (progn 
	    (cond ((is-relation-an-operator? rel)
		   (push triple non-semantic-triples-to-verify))
		  (t (push triple semantic-triples-to-verify)))))))
    (values non-semantic-triples-to-verify semantic-triples-to-verify lookup-triples)))

(defun verify-non-semantic-yn-triple-list (scenario
					   triples-to-verify)
  (let ((debug nil)
	yn-triple-true yn-triple-false overall-provenance)
    (dolist (triple triples-to-verify)
      (multiple-value-bind 
	  (result provenance)
	  (verify-non-semantic-yn-single-triple 
	   scenario triple)
	(cond (result
	       (push triple yn-triple-true))
	      (t (push triple yn-triple-false)))
	(setf overall-provenance 
	      (merge-provenance-info 
	       provenance
	       overall-provenance))))
    (values yn-triple-true yn-triple-false overall-provenance)))

(defun verify-non-semantic-yn-single-triple (scenario
					     triple-to-verify)
  (let* ((debug nil)
	 (head (triple-head     triple-to-verify))
	 (rel  (triple-relation triple-to-verify))
	 (tail (triple-tail     triple-to-verify))
	 (head-query-paths      (get-query-paths scenario (list head) debug))
	 (tail-query-paths      (get-query-paths scenario (list tail) debug)))
    (if debug
	(progn
	  (format debug "head-query-path: ~a~%" head-query-paths)
	  (format debug "tail-query-path: ~a~%" tail-query-paths)))
    (cond  ;;1) property-value equal property-value should be handled
	   ;;   inside (is-property-comparison-relation-p ...)
	   ;;2) (verify-single-triple-for-equation-relation ...) appears to 
	   ;;   only work for
	   ;;   ("Is it true that a cell is a cell? because it is based
	   ;;   on string= of their text descriptions.
	   ;;((is-equal-relation-p rel)
	   ;;(verify-single-triple-for-equal-relation triple-to-verify debug)
	  ((is-property-comparison-relation-p rel) 
	   (verify-single-triple-for-property-comparison-relation scenario triple-to-verify))
	  ((is-isa-relation-p rel)
	   (verify-single-triple-for-isa-relation scenario
						  triple-to-verify))
	  ((and (is-related-to-relation-p rel) *controller-expand-query*)
	   (verify-single-triple-for-related-to-relation 
	    scenario 
	    triple-to-verify)))))

(defun assert-and-test-property-comparison-relation(assert-expr test-expr)
  (let ((*logging* t)
	(*on-error* 'abort)) ; constraint checking on. abort and report error as part of km return vals.
    (bps-set-checkpoint 'assert-and-test-property-comparison-relation)
    (multiple-value-bind
	(assert-result assert-error-str)
	(km assert-expr)
      (multiple-value-bind
	  (test-result test-error-str)
	  (km test-expr)
	(bps-undo 'assert-and-test-property-comparison-relation)
	(values test-result 
		(or assert-error-str
		    test-error-str))))))

;;need provenance [done], positive[done], negative[done], unknown[done]
(defun verify-single-triple-for-property-comparison-relation (scenario triple-to-verify)
  (let* ((debug nil)
	 (head (triple-head     triple-to-verify))
	 (rel  (triple-relation triple-to-verify))
	 (tail (triple-tail     triple-to-verify)))
    (solve-for-property-value-instance scenario head)
    (solve-for-property-value-instance scenario tail)
    (let* ((head-val-str (car (ps-km-query `(|the| |text-gen| |of| ,head) debug)))
	   (tail-val-str (car (ps-km-query `(|the| |text-gen| |of| ,tail) debug)))
	   (assert-expr `(,head |now-has| (,rel (,tail))))
	   (test-expr   `(|the| ,rel |of| ,head))
	   (result nil)
	   (provenance nil))
      (cond ((and (not (skolem-property-value? head)) ;;ok. we have values to compare
		  (not (skolem-property-value? tail)))
	     (multiple-value-bind
		 (km-return-val km-error-str)
		 (assert-and-test-property-comparison-relation assert-expr test-expr)
	       (setf result (null km-error-str))
	       (cond (result ;;validated!
		      (setf provenance 
			    (get-property-comparison-relation-provenance triple-to-verify 1))
		      )
		     (t ;; contradiction
		      (setf provenance 
			    (get-property-comparison-relation-provenance triple-to-verify -1))
		      )
		     )
	       (push (list *current-vp-inst*
			   (list triple-to-verify
				 (list head-val-str
				       rel
				       tail-val-str)
				 result))
		     *viewpoint-query-lst*))
	     )
	    (t ;; unknown case. one or both values are unavailable. cannot do comparision
	     (progn
	       (setf provenance 
		     (get-property-comparison-relation-provenance triple-to-verify 0)
		     )
	       (if debug 
		   (format 
		    t
		    "(not testing ~a). Either a) lhs or rhs is a skolem, or b) lhs/rhs is a Vector. Default to nil.~%" 
		    triple-to-verify))
	       (push (list *current-vp-inst*
			   (list triple-to-verify
				 "Not tested. Either a) lhs or rhs is a skolem, or b) lhs/rhs is a Vector."
				 result))
		     *viewpoint-query-lst*))
	     )
	    )
      (values result provenance)
      )
    )
  )

;;need provenance [done], positive[done], negative[done], unknown[NA]
(defun verify-single-triple-for-isa-relation (scenario
					      triple-to-verify)
  (let* ((debug nil)
	 (head (triple-head     triple-to-verify))
	 (rel  (triple-relation triple-to-verify))
	 (tail (triple-tail     triple-to-verify))
	 (head-concept-lst (remove-subsumers (ps-km-query `(|the| |instance-of| |of| ,head) debug)))
	 (tail-concept-lst  (quasi-get-concept-for-kb-instance tail scenario))
	 (provenance nil))
    (let* ((result (eval (cons 'or
			       (mapcar #'(lambda(tail-concept-entry)
					   (ps-improved-isa head tail-concept-entry))
				       tail-concept-lst))))
	   (score (if result 1 -1))) ; taxonomic score should be calculated here. FIXME
      (cond ((equal score 1) ;;validation case
	     (setf provenance 
		   `(
		     (
		      (
		       ;;triple map
		       (
			(,head |is-a| ,tail-concept-lst) ;; lhs
			,(mapcar #'(lambda(concept) (list head '|is-a| concept)) head-concept-lst)
			)
		       . ,score) ;; 1 entry
		      )
		     nil                                          ;;node mappings
		     )
		   )
	     )
	    (t  ;; contradiction case
	     (setf provenance 
		   `(
		     (
		      (
		       ;;triple map
		       (
			(,head |is-a| ,tail-concept-lst) ;; lhs
			,(cons 
			  (get-contradiction-triple-for-non-semantic-yn-triple triple-to-verify
									       :scenario scenario)
			  (mapcar #'(lambda(concept) (list head '|is-a| concept)) head-concept-lst)
			  )
			)
		       . ,score) ;; 1 entry
		      )
		     nil                                          ;;node mappings
		     )
		   )
	     )
	    )
      (if debug (progn 
		  (format t "BPS: Y/N predicate expression, (~a isa ~a) => ~a ~%" head tail result)))
      (push (list *current-vp-inst*
		  (list triple-to-verify
			(format nil "(the instance-of of ~a) => ~a" head (ps-km-query `(|the| |instance-of| |of| ,head)))
			result))
	    *viewpoint-query-lst*)
      (values result provenance))))

;;previous version 20Jun09
;; (defun verify-semantic-yn-triple-list (scenario
;; 				       triples-to-verify)
;;   (let* ((debug nil)
;; 	 (source-graph (quasi-properly-terminate-triple-list 
;; 			triples-to-verify
;; 			scenario)))
;;     (multiple-value-bind
;; 	(cloned-source-graph orig->clone clone->orig)
;; 	(ps-clone-triple-list source-graph)
;;       (multiple-value-bind
;; 	  (regular-match-config 
;; 	   add-valid-abl-match-detail add-invalid-abl-match-detail 
;; 	   heuristic-match-config)
;; 	  (ps-perform-semantic-match
;; 	   cloned-source-graph
;; 	   scenario)
;; 	(let* ((regular-triple-map   (nth 0 regular-match-config))
;; 	       (regular-mapped-scenario-triples 
;; 		(determine-matched-scenario-triples
;; 		 regular-triple-map scenario))
;; 	       (heuristic-triple-map (nth 0 heuristic-match-config))
;; 	       (heuristic-mapped-scenario-triples 
;; 		(determine-matched-scenario-triples
;; 		 heuristic-triple-map scenario)))
;; 	  (let* ((yn-triple-true (remove-duplicate-triples
;; 				  (replace-elements-in-list
;; 				   (append 
;; 				    regular-mapped-scenario-triples 
;; 				    heuristic-mapped-scenario-triples)
;; 				   clone->orig)))
;; 		 (yn-triple-false-contradiction
;; 		  (set-difference triples-to-verify 
;; 				  yn-triple-true 
;; 				  :test 'ps-triple-equal))
;; 		 (yn-triple-false-cwa nil))
;; 	    (values yn-triple-true 
;; 		    yn-triple-false-contradiction
;; 		    yn-triple-false-cwa
;; 		    (get-provenance-info-from-match-config
;; 		     regular-match-config) ;;semantic-match-results
;; 		    )))))))

;;need to put in explanation.
(defun verify-semantic-yn-triple-list (scenario
				       triples-to-verify)
  (multiple-value-bind
      (validated-triples unvalidated-triples validated-provenance)
      (verify-semantic-yn-triple-list-validate scenario
					       triples-to-verify)
    (multiple-value-bind
	(contradicted-triples unvalidated-triples contradicted-provenance)
	(verify-semantic-yn-triple-list-contradict scenario             ;;check remaining
						   unvalidated-triples) ;; not validated
      (multiple-value-bind
	  (cwa-triples unvalidated-triples cwa-provenance)              ;;check remaining
	  (verify-semantic-yn-triple-list-cwa scenario                  ;;not validated
					      unvalidated-triples)      ;;or contradicted
	(values validated-triples
		contradicted-triples
		cwa-triples
		(merge-provenance-info
		 validated-provenance
		 (merge-provenance-info
		  contradicted-provenance
		  cwa-provenance))
		)
	)
      )
    )
  )

(defun verify-semantic-yn-triple-list-validate(scenario
					       triples-to-verify)
  (let* ((debug t)
	 (related-triple-lst-info (ps-include-related-triples triples-to-verify))
	 (expanded-triples-to-verify 
	  (remove-duplicates 
	   (mappend 'cadr related-triple-lst-info) 
	   :test 'ps-triple-equal)))
    ;(if debug (format t "*domain-neutral-transformations*: ~a entries~%" (length *domain-neutral-transformations*)))
    ;(if debug (format t "*heuristic-transformations*: ~a entries~%" (length *heuristic-transformations*)))
  (multiple-value-bind
      (graph-to-verify orig->clone clone->orig)
      (ps-clone-triple-list 
       (quasi-properly-terminate-triple-list
	expanded-triples-to-verify
	scenario))
    (multiple-value-bind
	(regular-match-config 
	 add-valid-abl-match-detail add-invalid-abl-match-detail 
	 heuristic-match-config)
	(ps-perform-semantic-match graph-to-verify scenario)
      (let* ((regular-triple-map   (nth 0 regular-match-config))
	     (regular-mapped-scenario-triples 
	      (determine-matched-scenario-triples
	       regular-triple-map scenario))
	     (heuristic-triple-map (nth 0 heuristic-match-config))
	     (heuristic-mapped-scenario-triples 
	      (determine-matched-scenario-triples
	       heuristic-triple-map scenario)))
	(let* ((validated-expanded-triples-to-verify
		(remove-duplicate-triples
		 (replace-elements-in-list
		  (append
		   regular-mapped-scenario-triples 
		   heuristic-mapped-scenario-triples)
		  clone->orig)))
	       (unvalidated-expanded-triples-to-verify
		(set-difference triples-to-verify 
				validated-expanded-triples-to-verify
				:test 'ps-triple-equal))
	       (validated-triples
		(mapcar #'(lambda(triple)
			    (ps-get-original-triple-from-related-triple-lst-info triple related-triple-lst-info))
			validated-expanded-triples-to-verify))
	       (unvalidated-triples
		(set-difference triples-to-verify
				validated-triples
				:test 'ps-triple-equal))
	       (provenance 
		(MAKE-POSITIVE-PROVENANCE-SCORES 
		 (get-provenance-info-from-match-config
		  (append regular-match-config 
			  heuristic-match-config)))))
	  (values validated-triples
		  unvalidated-triples 
		  provenance) ;;need to make sure all scores are >0
	  )
	)
      )
    )
  )
)

(defun ps-get-original-triple-from-related-triple-lst-info(triple related-triple-lst-info)
  (car (rassoc triple related-triple-lst-info
	       :test #'(lambda(x y)
			 (member x (car y)
				 :test 'ps-triple-equal)))))

;; contradicted triple (X r Y) is 
;;  a) (X  r  Y) is unmatched in scenario, and either
;;        (X  r  ?) has some value
;;     or (Y  r' ?) has some value
;;all other cases, (X r Y) is not contradicted and assumed false.
;;There is no application of xforms to prove contradicted triples.
(defun contradicted-triple?(scenario triple-to-verify)
  (let* ((lhs              (triple-head triple-to-verify))
	 (relation         (triple-relation triple-to-verify))
	 (inverse-relation (invert-slot relation))
	 (rhs              (triple-tail triple-to-verify)))
    (append
    (mapcar #'(lambda(x)
		(list lhs relation x))
	    (quasi-ps-slot-lookup lhs relation scenario))
    (mapcar #'(lambda(x)
		(list rhs inverse-relation x))
	    (quasi-ps-slot-lookup rhs inverse-relation scenario))
    )
  )
)

;;need to put in code to handle contradiction checking [done]
;;need provenance [done]
(defun verify-semantic-yn-triple-list-contradict(scenario
						 triples-to-verify)
  (let ((contradicted-triple-lst ())
	(provenance              ()))
  (multiple-value-bind
      (graph-to-verify orig->clone clone->orig)
      (ps-clone-triple-list 
       (quasi-properly-terminate-triple-list 
	triples-to-verify scenario))
    (dolist (triple triples-to-verify)
      (if (contradicted-triple? scenario triple)
	  (progn
	    (push triple contradicted-triple-lst)
	    (push (list triple (contradicted-triple? scenario triple))
		  provenance)
	  )
	)
    )
    (values contradicted-triple-lst 
	    (set-difference triples-to-verify 
			    contradicted-triple-lst)
	  (CREATE-PROVENANCE-FOR-SEMANTIC-YN-TRIPLE-LIST-contradicted
	   contradicted-triple-lst
	   provenance) ;;need to make sure all scores are -1
	  )
    )
  )
)

(defun create-provenance-for-semantic-yn-triple-list-contradicted(triples-to-verify provenance)
  (list
   ;;triple-map... each entry has null mapping with score -1
   (mapcar #'(lambda(triple)
	       (cons (list triple 
			   (cadr (assoc triple provenance :test 'ps-triple-equal)))
		      -1))
	   triples-to-verify)
  nil ;;empty node map
  )
)

;;need to put in explanation.
(defun verify-semantic-yn-triple-list-cwa(scenario
					  triples-to-verify)
  (multiple-value-bind
      (graph-to-verify orig->clone clone->orig)
      (ps-clone-triple-list 
       (quasi-properly-terminate-triple-list 
	triples-to-verify scenario))
  (values triples-to-verify  ;;remember to remove instance triples
	  nil                ;;remember to remove instance triples
	  (CREATE-PROVENANCE-FOR-SEMANTIC-YN-TRIPLE-LIST-CWA
	   triples-to-verify) ;;need to make sure all scores are 0
  )
))


(defun create-provenance-for-semantic-yn-triple-list-cwa(triples-to-verify)
  (list
   ;;triple-map... each entry has null mapping with score 0
   (mapcar #'(lambda(triple)
	       (cons (list triple nil) 0))
	   triples-to-verify)
  nil ;;empty node map
  )
)

(defun explain-semantic-match-comparison(triple-to-verify
					 regular-match-config 
					 add-valid-abl-match-detail 
					 add-invalid-abl-match-detail 
					 heuristic-match-config
					 cloned-target-graph)
  (let ((idx 1)
	(head (triple-head     triple-to-verify))
	(reln (triple-relation triple-to-verify))
	(tail (triple-tail     triple-to-verify))
	(regular-triple-map (nth 0 regular-match-config))
	(s (make-string-output-stream)))
    (cond ((null regular-match-config)
	   (format s "Nothing matched."))
	  (t (dolist (entry regular-triple-map)
	       (let ((matched-triples 
		      (remove-duplicate-triples
		       (determine-matched-candidate-triples
			regular-triple-map
			cloned-target-graph))))
		 (format s "Mapping #~a : ~a~%" idx matched-triples)
		 (setf idx (1+ idx))))))
    (get-output-stream-string s)
))


(defun is-relation-an-operator?(rel)
  (or (is-equal-relation-p rel)
      (is-property-comparison-relation-p rel) 
      (is-isa-relation-p rel)
      (is-related-to-relation-p rel)
      )
)

(defun is-equal-relation-p(relation)
  (equal relation '|equal|))

(defun is-property-comparison-relation-p(relation)
  (not (null (isa relation '|Property-Relation|))))

(defun is-isa-relation-p(relation)
  (or 
   (equal relation '|instance-of|)
   (equal relation '|is-a|)
   (equal relation '|isa|)))

(defun is-related-to-relation-p(relation)
   (equal relation '|related-to|))

(defun text-description-comparison-p(scenario-instance inferred-instance &optional(verbose nil))
  (let ((scenario-desc (get-text-description-for-km-frame scenario-instance verbose))
	(inferred-desc (get-text-description-for-km-frame inferred-instance verbose)))
    (string= scenario-desc inferred-desc)))

(defun remove-relation-from-triple(triple)
  (let ((result (list (car triple) '|related-to| (caddr triple))))
     result                       ;; unsemantic triple
))

(defun remove-relations-from-triple-list(triple-lst)
  (let ((unsemantic-triple-lst      ())
	(add-back-relations-mapping ()))
    (dolist (triple (extract-non-instance-triples triple-lst))
      (let ((unsemantic-triple (remove-relation-from-triple triple)))
	(cond ((not (null (assoc unsemantic-triple add-back-relations-mapping :test 'ps-triple-equal)))
	       (push (list unsemantic-triple
			   (cons triple
				 (cadr (assoc unsemantic-triple 
					      add-back-relations-mapping 
					      :test 'ps-triple-equal))))
		     add-back-relations-mapping))
	      (t (push (list unsemantic-triple (list triple))
		       add-back-relations-mapping)))
	(push unsemantic-triple unsemantic-triple-lst)))
    (values 
     (append (extract-instance-triples triple-lst)
	     unsemantic-triple-lst)
     add-back-relations-mapping)
    )
  )

(defun add-back-relations-to-triple-list(unsemantic-triple-lst add-back-relations-mapping)
  (let ((result ()))
    (dolist (triple unsemantic-triple-lst)
      (let ((mapping (assoc triple add-back-relations-mapping 
			    :test 'ps-triple-equal)))
	(cond ((null mapping) 
	       (push triple result))
	      (t (setf result (append result (cadr mapping))))
	      )
	)
      )
    (remove-duplicates result :test 'ps-triple-equal)
    )
  )

(defun listify-triple(x)
  (if (triple-p x)
      (list x)
    x))

(defun add-back-relations-to-provenance(provenance add-back-relations-mapping)
  (let ((original-triple-map (nth 0 provenance))
	(node-map   (nth 1 provenance))
	(new-triple-map nil))
    (dolist (entry original-triple-map)
      (let* ((mapping (car entry))
	     (lhs (nth 0 mapping))
	     (rhs (nth 1 mapping))
	     (score (cdr entry)))
	(push (cons (list 
		     lhs
		     (add-back-relations-to-triple-list 
		      (listify-triple rhs)
		      add-back-relations-mapping)
		     )
		    score) 
	      new-triple-map)
	)
      )
    (list new-triple-map
	  node-map
	  )
    )
  )

;;need provenance [done], positive[done], negative[done], unknown[NA]
(defun verify-single-triple-for-related-to-relation (scenario
						     triple-to-verify
						     &optional (verbose nil))
  (let ((provenance nil)
	(result     nil))
    (multiple-value-bind
	(unsemantic-scenario mapping)
	(remove-relations-from-triple-list scenario) ;; get scenario without semantic relations
      (multiple-value-bind
	  (regular-match-config add-valid-abl-match-detail add-invalid-abl-match-detail heuristic-match-config)
	  (ps-perform-semantic-match 
	   (quasi-properly-terminate-triple-list ;;install instance-of info.
	    (list triple-to-verify) ;; lhs, list containing exactly 1 related-to triple to verify
	    scenario)
	   unsemantic-scenario ;; rhs, scenario without semantic relations
	   )
	(setf result (not (null regular-match-config))) ;; was a relationship found?
	(cond 
	 (result		       ;; validated, score 1
	  (let ((unsemantic-provenance ;; provenance-info with semantic relations
		 (get-provenance-info-from-match-config
		  regular-match-config))
		)
	    (setf provenance ;; add back semantic information to validate
		  (add-back-relations-to-provenance 
		   unsemantic-provenance
		   mapping
		   )
		  )
	    )
	  )
	 (t ;; provenance for contradiction case
	  (setf provenance 
		(list 
		 (list 
		  (CREATE-PROVENANCE-TRIPLE-MAP-INFO ;;exactly one entry
		   triple-to-verify		     ;;lhs
		   (get-contradiction-triple-for-non-semantic-yn-triple triple-to-verify) ;;no relationship found
		   -1) ;;contradiction case, thus -1
		  )
		 nil ;;empty node-map
		 )
		)
	  )
	 )
	(values result provenance)
	)
      )
    )
  )

#|12Jun09. Old code that depends on LS to do spreading activation.
(defun verify-single-triple-for-related-to-relation (scenario
						     triple-to-verify
						     &optional (verbose nil))
  (let* ((head (triple-head     triple-to-verify))
	 (rel  (triple-relation triple-to-verify))
	 (tail (triple-tail     triple-to-verify)))
    (let ((result (not (null (ps-relate-instances head tail)))))
      (if verbose (progn
		    (format t "BPS: related-to predicate expression, (~a related-to ~a) => ~a ~%" head tail result)))
      result)))
|#

;;Handles only Property-Value.
;;Does not handle Property-Vector-Value.
(defun skolem-property-value?(x)
  (let ((debug nil))
    (and (property-value-instance-p x)
	 (null (ps-km-query `(|the| |value| |of| ,x) debug)))
))

(defun ps-instantiate-property-value(cardinal uom)
  (car (ps-km-query
	`(,(ps-instantiate-concept 
       (car 
       (ps-km-query 
	`(|the| |range| |of| 
	  (|the| |cardinal-unit-class-of| |of|
	   (|the| |instance-of| |of| ,uom))))))
     |now-has| (|value| ((:|pair| ,cardinal ,uom)))))))

;;Have to hack to get around Slot-Value-Viewpoint and Multislot-Value-Viewpoint expl. brittleness.
;;Remember viewpoint-query expects a :seq in viewpoint-answer and viewpoint-answerable-query
(defun output-yn-viewpoint-for-lookup-viewpoint(input-vp-inst)
  (multiple-value-bind
      (vp-answerable-query-filler yn-query-lst sl-query-lst)
      (get-viewpoint-answerable-query input-vp-inst)
    (let* ((debug nil)
	   (cloned-vp-inst (ps-clone-viewpoint input-vp-inst))
	   (vp-defn  `(|a| |Yes-No-Viewpoint| |with|
		       (|viewpoint-parent| (,cloned-vp-inst))
		       (|viewpoint-scenario| ,(affix-triple-prefix (get-simplified-context-for-viewpoint cloned-vp-inst)))
		       (|viewpoint-query|    ,(ps-km-query `(|the| |viewpoint-query| |of| ,cloned-vp-inst) debug))
		       (|viewpoint-answerable-query| (,(cons ':|seq| yn-query-lst))))))
      (let ((result (car (ps-km-query vp-defn debug)))
	    (sl-query-lst (CREATE-SLOT-VALUE-QUERY-LST-FOR-YN-QUERY-LST 
			   cloned-vp-inst
			   yn-query-lst)))
	(ps-km-query `(,cloned-vp-inst |now-has| (|viewpoint-child| (,result))) )
	(ps-km-query `(,cloned-vp-inst |now-has| (|viewpoint-query| (,(cons ':|seq| sl-query-lst)))) debug)
	(if (equal (length sl-query-lst) 1)
	    (progn
	      (ps-km-query `(,cloned-vp-inst |now-has| (|instance-of| (|Slot-Value-Viewpoint|))))
	      (ps-km-query `(,cloned-vp-inst |now-has| (|viewpoint-answerable-query| ()))))
	  (progn
	      (ps-km-query `(,cloned-vp-inst |now-has| (|instance-of| (|Multislot-Value-Viewpoint|))))
	      (ps-km-query `(,cloned-vp-inst |now-has| (|viewpoint-answerable-query| (,(cons ':|seq| sl-query-lst)))))))
	(reinstall-viewpoint-answer-provenance 
	 result input-vp-inst)
	result))))

(defun ps-clone-viewpoint(vp-inst)
  (let* ((debug nil)
	 (sym (intern (format nil "~a-B" vp-inst) :km))
	 (slotsvals (get-slotsvals vp-inst))
	 (vp-defn (append (list sym '|now-has|)
			  `((|instance-of| ,(ps-km-query `(|the| |instance-of| |of| ,vp-inst))))
			  slotsvals)))
    (car (ps-km-query vp-defn t))))

(defun true-false-question-p (question)
  (and (listp question)
       (not (null (remove-if-not 'true-false-yn-query-entry-p question)))))

(defun true-false-yn-query-entry-p(entry)
  (and (listp entry)
       (or
	(eq (yn-question-type entry)
	    'is-it-true-that)
	(eq (yn-question-type entry)
	    'is-it-false-that)
	(eq (yn-question-type entry)
	    'is-it-possible-that)
  )))

(defun create-slot-value-query-lst-for-yn-query-lst(vp-inst yn-query-lst)
  (multiple-value-bind
      (simple-context full-context)
      (get-simplified-context-for-viewpoint vp-inst)
    (remove nil
    (mapcar #'(lambda(instance)
		(car   ;;only pick the 1st one.
		 (make-viewpoint-query-slot-for-question-triple-list		 
		  (ps-get-query-triples-for-compute-question
		   full-context
		   (list instance)))))
	    (extract-all-km-instances yn-query-lst)))))

