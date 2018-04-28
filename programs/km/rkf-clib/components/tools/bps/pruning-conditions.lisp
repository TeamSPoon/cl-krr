;;
;; $Id: pruning-conditions.lisp,v 1.129 2009/09/01 21:41:26 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; *NO need* to assert viewpoint triples for
;; certain questions, e.g., 
;;  a) yn-queries that solely depend on semantic matching
;;  b) spreading activation
;;  c) lookup queries without rules-for
;;
;;others can answered without doing so.
;;ideally, we should minimize asserting/querying stuff in KM.
;;only when necessary, e.g.,
;; a) explanation generation for answer viewpoint
;; b) computing small-v values when eq-sets are present.
(defun assert-viewpoint-triples-to-test-viewpoint?(vp-inst)
  (multiple-value-bind
      (lookup-query-lst yn-query-lst)
      (collate-viewpoint-query (get-viewpoint-query vp-inst))
    (or (assert-viewpoint-triples-for-lookup-query-lst lookup-query-lst)
        (assert-viewpoint-triples-for-yn-query-lst yn-query-lst)
)))

(defun assert-viewpoint-triples-for-lookup-query-lst (lookup-query-lst)
  (if (not (null lookup-query-lst))
      t ;; need to compute, so let's assert the viewpoint
    nil ;; nothing to compute
))

(defun assert-viewpoint-triples-for-yn-query-lst (yn-query-entries)
    (multiple-value-bind
	(non-semantic-triples-to-verify semantic-triples-to-verify)
	(collate-triples-to-verify 
	 (mapcar 'extract-yes-no-question-triples-in-viewpoint-query-entry
		 yn-query-entries))
       (not (null non-semantic-triples-to-verify)) ;; isa, comparison. so need to assert
))

(defun extract-yes-no-question-triples-in-viewpoint-query-entry(input)
  (let ((type (nth 1 input))
	(val  (cdr (nth 2 input))))
    (if (equal type '|*yes-no-question|)
	val)
    ))

(defun check-all-viewpoint-query(vp-inst)
    ;;there should be a test here to 
    ;;see if we need to assert the viewpoint.
    ;;certain questions, e.g., 
    ;; a) yn-queries that solely depend on semantic matching
    ;; b) spreading activation
    ;; c)lookup queries without rules-for
    ;;can answered without doing so.
    ;;ideally, we should minimize asserting/querying stuff in KM.
    ;;only when necessary, e.g.,
    ;; a) explanation generation for answer viewpoint
    ;; b) computing small-v values when eq-sets are present.
  (let ((debug nil))
    (if (assert-viewpoint-triples-to-test-viewpoint? vp-inst)
	(progn
	  (format debug "check-all-viewpoint-query: asserting ~a to test queries.~%" vp-inst)
	  (assert-viewpoint-triples vp-inst)
	  )
      (progn
	(format debug "check-all-viewpoint-query: *no need* to assert ~a to test queries.~%" vp-inst)
      )
    )
    (check-all-viewpoint-query0 vp-inst)
))

;;Determines subset of viewpoint query entries that return a meaningful answer different from skolem answers. 
(defun check-all-viewpoint-query0(vp-inst)
  (multiple-value-bind
      (lookup-query-lst yn-query-lst)
      (collate-viewpoint-query (get-viewpoint-query vp-inst))
    (multiple-value-bind
	(simplified-context full-context)
	(get-simplified-context-for-viewpoint vp-inst)
      (multiple-value-bind
	  (yn-triples-true yn-triples-false verify-triple-list-provenance)
	  (verify-triple-list
	   full-context
	   (mapcar 'extract-yes-no-question-triples-in-viewpoint-query-entry
		   yn-query-lst))
	(multiple-value-bind
	    (lookupable lookup-provenance)
	    (lookup-triple-list full-context vp-inst lookup-query-lst)
	  (values
	   (remove 
	    nil
	    (remove-duplicates
	     (append 
	      '(:|seq|)
	      (append 
	       (if (not (null yn-triples-true))
		   (list (list ':|triple| '|*yes-no-question| 
			       (seqify (affix-triple-prefix yn-triples-true))
			       '|*yes|)))
	       (if (not (null yn-triples-false))
		   (list (list ':|triple| '|*yes-no-question|
			       (seqify (affix-triple-prefix yn-triples-false))
			       '|*no|))))
	      lookupable
	      )
	     :test 'equal))
	   (merge-provenance-info
	    lookup-provenance
	    verify-triple-list-provenance ;;additional information on why things are true(*yes)
	    )))))))

(defun lookup-triple-list(scenario vp-inst lookup-query-lst)
  (let* ((overall-lookupable ())
	 (overall-provenance ()))
    (dolist (vp-query lookup-query-lst)
      (multiple-value-bind
	  (lookupable provenance)
	  (check-viewpoint-query-slot-lookup vp-inst vp-query)
	(setf overall-lookupable 
	      (append overall-lookupable
		      lookupable))
	(setf overall-provenance 
	      (merge-provenance-info
	       overall-provenance
	       provenance))))
  (values overall-lookupable overall-provenance)  
  ))

;;need to return provenance information for lookup queries.
(defun check-viewpoint-query-slot-lookup(vp-inst
					 vp-query-entry)
  (let ((debug nil)
	overall-lookupable overall-provenance)
    ;;Expand the query and return those that answer.
    ;;But how do we handle skolem answers then?
    (if (and 
	 debug
	 (> (length expanded-vp-query-lst) 1))
	(format t "Expanded ~a to be ~a queries.~%" 
		vp-query-entry 
		(length expanded-vp-query-lst)))
      (multiple-value-bind
	  (lookupable provenance)  
	  (check-viewpoint-query-aux vp-inst vp-query-entry nil)
	(if (not (null lookupable))
	    (setf overall-lookupable
		  (append lookupable overall-lookupable)))
	(setf overall-provenance     ;; always include provenance
	      (merge-provenance-info ;; for both lookupable and non-lookupable
	       overall-provenance    ;; vp-query
	       provenance))          ;; 
      )
    (values overall-lookupable overall-provenance)
    )
)

;; Determines if a vp-query-entry returns a meaningful answer different from a skolem answer.
;; Property-Vector-Value & Property-Value have to be handled differently. 
;; Property-Value contained in Property-Vector-Value may contain small-v value,
;; yet have "unknown" in text-description
(defun check-viewpoint-query-aux(vp-inst
				 vp-query-entry
				 &optional(skolem-textgen nil))
  (let* ((debug nil)
	 (query-path  (extract-query-path vp-query-entry))
         (slot-name   (nth 1 query-path)))
    (if debug (format t "BPS: query check (~a): (~a ~a ?)" 
		      (ps-km-classification-enabled-p)
		      (fourth query-path)
		      (second query-path))
    )
    (cond
     ((concept-applicable-for-slot-range? '|Property-Vector-Value| slot-name)
      (check-viewpoint-query-for-Property-Vector-Value vp-inst vp-query-entry query-path))
     
     ((concept-applicable-for-slot-range? '|Property-Value| slot-name)
      (check-viewpoint-query-for-Property-Value vp-inst vp-query-entry query-path))

     ((concept-applicable-for-slot-range? '|Chemical-Equation-Expression| slot-name)
      (check-viewpoint-query-for-chemical-equation-expression vp-inst vp-query-entry query-path))

     (t 
      (check-viewpoint-query-for-generic-fillers vp-inst vp-query-entry query-path skolem-textgen))
     )
    )
)

(defun convert-query-path-into-triples(query-path)
  (list (nth 3 query-path)   ;;only works for the form
	(nth 1 query-path)   ;;(the X of Y) query path
	'*
  )
)

;;New version. Duplicate/different answer check done at viewpoint-duplicate level.
;;This version will work because all initial answers derived from original scenario is 
;;saved, used, and can be potentially returned.
;;Cleaner and simpler.
;;need provenance [done], positive[done], negative[NA], unknown[done]
(defun check-viewpoint-query-for-generic-fillers(vp-inst 
						 original-vp-query-entry 
						 original-query-path
						 skolem-textgen)
  (let* ((debug nil)
	 (original-query-path-triple (CONVERT-QUERY-PATH-INTO-TRIPLES original-query-path)) ;;lhs
	 (expanded-vp-query-lst ;;multislot expansion done here
	  (expand-viewpoint-query-slot-entry 
	   original-vp-query-entry))
	 (provenance-rhs-info nil)
	 (provenance nil)
	 (answerable-vp-query-candidates nil))
    (dolist (vp-query-candidate expanded-vp-query-lst)
      (let* ((query-path        (extract-query-path vp-query-candidate))
	     (query-path-triple (CONVERT-QUERY-PATH-INTO-TRIPLES query-path))
	     (filler-values     (ps-km-query query-path debug))
	     (result            (not (null filler-values)))
	     (score (if result 1 0)))
      (if result
	  (progn 
	    (push vp-query-candidate answerable-vp-query-candidates)
	    (setf provenance-rhs-info
		  (append provenance-rhs-info
			  (list (list (nth 0 query-path-triple)        ;;rhs, using filler values
				      (nth 1 query-path-triple)
				      filler-values))))
	    )
      )))
    (if (not (null provenance-rhs-info)) ;; some answer was found.
	(setf provenance (list 
			  ;;1st value, triple-map
			  (list (CREATE-PROVENANCE-TRIPLE-MAP-INFO  ;;exactly one entry
			   (list original-query-path-triple)         ;;lhs
			   provenance-rhs-info
			   1
			   ))
			  ;;2nd value, node-map
			   nil ;;empty node-map
			       )
	)
	(setf provenance 
			  (list 
			  ;;1st value, triple-map
			   (list (CREATE-PROVENANCE-TRIPLE-MAP-INFO  ;;exactly one entry
			    (list original-query-path-triple)         ;;lhs
			    (list (list 
				   (nth 0 original-query-path-triple)
				   (nth 1 original-query-path-triple)
				   nil))
			    0
			    )
				 )
			   ;;2nd value, node-map
			   nil ;;empty node-map
			   )
	      )
	)
    (if (not (null provenance-rhs-info)) ;; some answer was found.
	(values answerable-vp-query-candidates provenance)
      (values nil provenance)
      )
    )
  )

;;need provenance [untested], positive[untested], negative[NA], unknown[untested]
(defun check-viewpoint-query-for-Chemical-Equation-Expression(vp-inst vp-query-entry query-path	&optional(debug nil))
  (let* ((chemical-equation-term (car (ps-km-query `(|the| |term| |of| ,query-path) debug)))
	 (result (and (not (null chemical-equation-term))
		      (not (null (car chemical-equation-term)))
		      (not (null (cadr chemical-equation-term)))))
	 (query-path-triple (CONVERT-QUERY-PATH-INTO-TRIPLES query-path)) ;;lhs
	 (score (if result 1 0))
	 (query-path-triple-answer nil))
    (if result 
	(setf query-path-triple-answer (list (nth 0 query-path-triple)        ;;rhs
					     (nth 1 query-path-triple)
					     (format nil "~s" 
						     (ps-get-text-description-for-query-path query-path debug))))
      (setf query-path-triple-answer (list (nth 0 query-path-triple)        ;;rhs
					   (nth 1 query-path-triple)
					   (format "~s" "???"))))
    (setf provenance (list 
		      (list 
		       (CREATE-PROVENANCE-TRIPLE-MAP-INFO  ;;exactly one entry
			query-path-triple         ;;lhs
			query-path-triple-answer  ;;rhs
			score)
		    )
		    nil ;;empty node-map
		   )
	  )
    (push (list vp-inst
		(list vp-query-entry
		      chemical-equation-term 
		      result))
	*viewpoint-query-lst*)
    (if result 
	(values (list vp-query-entry)
		provenance)
      (values nil provenance)
      )
    )
  )

;;need provenance [done], positive[done], negative[NA], unknown[done]
(defun check-viewpoint-query-for-Property-Value(vp-inst 
						vp-query-entry 
						query-path
						&optional(debug nil))
  (multiple-value-bind
      (solved-value instance)
      (solve-for-property-value query-path)
    (let* ((debug nil)
	   (result (not (null solved-value)))
	   (query-path-triple (CONVERT-QUERY-PATH-INTO-TRIPLES query-path)) ;;lhs
	   (query-path-triple-answer (list 
				      (nth 0 query-path-triple)        ;;rhs
				      (nth 1 query-path-triple)
				      (format nil "~s" solved-value)
				      ))
	   (score (if result 1 0))
	  )
    (push (list vp-inst 
		(list vp-query-entry
		solved-value
		result))
	  *viewpoint-query-lst*)
    (setf provenance (list 
		      (list 
		       (CREATE-PROVENANCE-TRIPLE-MAP-INFO  ;;exactly one entry
			query-path-triple         ;;lhs
			query-path-triple-answer  ;;rhs
			score)
		      )
		      nil ;;empty node-map
		   )
     )
    (if result (values 
		(list vp-query-entry) provenance)
      (values nil provenance)
    )
    )
  )
)

;;need provenance [done], positive[done], negative[NA], unknown[done]
;;if the direction is unknown, then the score should be 50%, not 100%
(defun check-viewpoint-query-for-Property-Vector-Value(vp-inst 
						       vp-query-entry 
						       query-path 
						       &optional(debug nil))
  (let* ((debug nil))
    (multiple-value-bind 
	(instance dir-value mag-value text-gen)
	(solve-for-property-vector-value query-path)
      (let* ((result (not (null mag-value)))
	     (query-path-triple (CONVERT-QUERY-PATH-INTO-TRIPLES query-path)) ;;lhs
	     (query-path-triple-answer (list 
					(nth 0 query-path-triple) ;;rhs
					(nth 1 query-path-triple)
					(format nil "~s" text-gen)))
	     (score (cond ((null result) 0)  ;;invalid answer
			  ((and (not (null mag-value))   ;;have both
				(not (null dir-value)))  ;;magnitude and direction
			   1)
			  (t 0.5)))     ;; only magnitude, no direction
	     )
	  (push (list vp-inst 
		      (list vp-query-entry
			    mag-value
			    result))
		*viewpoint-query-lst*)

	  (setf provenance (list 
			    (list 
			     (CREATE-PROVENANCE-TRIPLE-MAP-INFO  ;;exactly one entry
			      query-path-triple         ;;lhs
			      query-path-triple-answer  ;;rhs
			      score)
			     )
			    nil ;;empty node-map
			    )
		)

	  (if result (values 
		      (list vp-query-entry) provenance)
	    (values nil provenance)
	    )
	  )
      )
    )
  )

;;magnitude and direction has to be solved in isolation with each other 
;;to prevent buggy SME-authored equations from giving wrong answers.
(defun solve-for-property-vector-value(query-path)
  (let ((debug nil)
	secondary-slot)
    (if (not (null query-path))
	(multiple-value-bind
	    (instance secondary-slot dir-instance mag-instance dir-value mag-value)
	    (parse-property-vector-value-with-query-path query-path)
	  (if (null mag-value)
	      (let* ((*EQ-FLAG* t)
		     (*logging* t)) ;; very important... solve for each value in isolation
		(setq *CONTROLLER-TRIPLE-INSTANCES*
		      (insert-at-end instance *CONTROLLER-TRIPLE-INSTANCES*))
		(bps-set-checkpoint 'solving-for-direction)
		(setq dir-value (solve-for-property-value `(|the| |direction| |of| ,instance)))
		(bps-undo 'solving-for-direction)
		(bps-set-checkpoint 'solving-for-magnitude)
		(setq mag-value (solve-for-property-value `(|the| ,secondary-slot |of| ,instance)))
		(bps-undo 'solving-for-magnitude)
		(if (values-consistent dir-value)
		    (ps-km-query 
		     `(,(get-filler-for-slot-frame '|direction| instance) |now-has| 
		       (|value| ,dir-value)) debug)
		  (setq dir-value nil) ;;remove, bad value
		  )
		(if (values-consistent mag-value)
		    (ps-km-query 
		     `(,(get-filler-for-slot-frame secondary-slot instance) |now-has| 
		       (|value| ,mag-value)) debug)
		  (setq mag-value nil) ;;remove, bad value
		  )
		)
	    )
	  (values instance dir-value mag-value 
		  (car (ps-slot-lookup instance '|text-gen|)))
	  )
      )
    )
  )

(defun solve-for-property-value(query-path)
  (if (not (null query-path))
  (let ((debug nil)
	(instance (get-filler-for-slot-frame (nth 1 query-path) (nth 3 query-path))))
    (let ((*EQ-FLAG* t)
	  (val (ps-km-query `(|the| |value| |of| ,instance) debug)))
      (if (null val)
	  (setq val (equation-solve instance)))
      (let ((result (and (not (null val))
			 (values-consistent val))))
	(if (and (not (null result))
		 (listp (car val))
		 (equal (caar val) ':|pair|)
		 (numberp (cadr (car val)))
		 (not (null (car (last (car val))))))
	    (install-preferred-uom (car (last (car val))) query-path))
	(values val instance)))))
)

(defun parse-property-vector-value-with-query-path(query-path)
  (let* ((debug nil))
    (parse-property-vector-value-instance (car (ps-km-query query-path debug))) ;;should be exactly 1 value. if not, pick the 1st one.
    )
  )

(defun parse-property-vector-value-instance(instance)
  (let* ((debug nil)
	 (secondary-slot (car (ps-km-query `(|the| |secondary-slot| |of| ,instance) debug))))
    (values 
     instance
     secondary-slot ;;secondary-slot always have one value.
     (ps-km-query `(|the| |direction| |of| ,instance) debug)
     (ps-km-query `(|the| ,secondary-slot |of| ,instance) debug)
     (ps-km-query `(|the| |value| |of| (|the| |direction| |of| ,instance)) debug)
     (ps-km-query `(|the| |value| |of| (|the| ,secondary-slot |of| ,instance)) debug)
     )
    )
  )

(defun get-magnitude-for-property-vector-value(query-path)
  (let* ((debug nil)
	 (secondary-slot (car (ps-km-query `(|the| |secondary-slot| |of| ,query-path) debug))))
    (values 
     (ps-km-query `(|the| ,secondary-slot |of| ,query-path) debug)
     (ps-km-query `(|the| |value| |of| (|the| ,secondary-slot |of| ,query-path)) debug))))

;;Returns true is vp-inst answers more queries than parent-vp-inst
(defun promising-viewpoint-p(vp-inst)
  (let ((answerable-query-list (get-viewpoint-answerable-query vp-inst))
	(parent-answerable-query-list (get-viewpoint-answerable-query (get-viewpoint-parent vp-inst))))
    (< (length parent-answerable-query-list)
       (length answerable-query-list))))

(defun solve-for-property-value-instance(scenario instance)
  (let ((query-path (car (get-query-paths scenario (list instance)))))
    (cond ((isa instance '|Property-Vector-Value|)
	   (solve-for-property-vector-value query-path))
	  ((isa instance '|Property-Value|)
	   (solve-for-property-value query-path)))
))

(defun asserted-viewpoint?(vp-instance)
  (not (null (ps-slot-lookup vp-instance '|viewpoint-asserted?|)))
)

(defun test-viewpoint(vp-inst)
  (let ((*on-error* 'abort))
    (let ((result (catch 'BPS-REASONING-ERROR (test-viewpoint0 vp-inst))))
      (cond ((and (listp result)
		  (eq (car result) 'BPS-REASONING-ERROR))
	     (progn
	       (mutate-ignoredlist    (insert-at-end vp-inst *IGNOREDLIST*))
	       (push (list vp-inst 
			   (format nil "Elaboration does not make sense."))
		     *pruned-vp-reason*)
	       (format t "BPS: Skipping #~a ~a (Elaboration does not make sense).~%" 
		       (get-viewpoint-ordering vp-inst)
		       (format-mapped-concepts (get-mapped-concepts vp-inst)))
	       nil))
	    (t result)))))

(defun kmify-match-config-for-triple-match(input)
  (let ((result ()))
    (dolist (x input)
      (let ((lhs (car (car x)))
	    (rhs (cadr (car x)))
	    (score (cdr x)))
	(push 
	 (list  ':|pair| 
	  (list ':|pair| 
		    (format nil "~a" lhs)
		    (format nil "~a" rhs)
		    )
	  score)
	 result)))
    (cons '|:set| result)
))

(defun kmify-match-config-for-node-match(input)
  (let ((result ()))
    (dolist (x input)
      (let ((lhs (car x))
	    (rhs (Cadr x)))
      (push (list ':|pair| 
		  (format nil "~a" lhs)
		  (format nil "~a" rhs))
	    result)))
    (cons '|:set| result)
))

(defun kmify-match-config(input)
  (let ((triple-match (nth 0 input))
	(node-match   (nth 1 input))
	(score        (nth 2 input)))
    (list 
     ':|triple|
     (kmify-match-config-for-triple-match triple-match)
     (kmify-match-config-for-node-match node-match)
     score)
))

(defun get-scenario-triples-used (vp-inst)
  (properly-terminate-triple-list 
   (get-scenario-triples-used-from-input-provenance 
    (get-input-provenance vp-inst)))
)

(defun is-mapped-concept-a-reaction?(vp-inst)
  (not (null (member (car (get-viewpoint-target vp-inst)) (all-subclasses '|Reaction|))))
)

(defun test-viewpoint0(vp-inst)
  (let ((debug nil))
    (multiple-value-bind
	(answered-queries query-provenance)
	(check-all-viewpoint-query vp-inst)
      (fill-viewpoint-answerable-query-slot 
       vp-inst answered-queries)
      (install-viewpoint-answer-provenance vp-inst 
					   (get-input-provenance vp-inst)
					   query-provenance)
      (if (and (goal-viewpoint-p vp-inst)
	       (not (asserted-viewpoint? vp-inst)))
	  (progn
	    (if debug (format t "test-viewpoint0: goal viewpoint found, ~a... but it is not asserted... asserting it now.~%" 
		    vp-inst))
	    (assert-viewpoint-triples vp-inst) ;; assert viewpoint, if it wasn't asserted before.
	    )
	)
      (values (goal-viewpoint-p vp-inst)
	      (get-viewpoint-answerable-query vp-inst)))))

;;Still need to recognize when same viewpoint,
;;but a different expanded query is returning a different answer.
(defun duplicate-answer?(vp-inst)
  (not (null (member vp-inst (get-answer-viewpoint)
		     :test 'viewpoint-answer-equal))))

;; returns nil if
;;  a) there are yn-queries, and there are no *yes for *yes-no-question-answerable, or
;;  b) 1+ viewpoint-query, but null viewpoint-answerable-query
(defun goal-viewpoint-p (vp-inst)
  (multiple-value-bind
      (lookup-query-lst yn-query-lst)
      (collate-viewpoint-query (get-viewpoint-query vp-inst))
    (multiple-value-bind
	(answerable-lookup-query-lst answerable-yes-yn-query-lst answerable-no-yn-query-lst)
	(collate-viewpoint-answerable-query (get-viewpoint-answerable-query vp-inst))
      (cond ((and (not (null yn-query-lst))
		  (null answerable-yes-yn-query-lst))
	     nil)
	    ((and (not (null lookup-query-lst))
		  (null answerable-lookup-query-lst))
	     nil)
	    (t t)
      )
)))

;;Collates viewpoint-query contents into two parts
;;  a) slot-lookup types
;;  b) yn-query typles
(defun collate-viewpoint-query(vp-query-lst)
  (let ((lookup-query-lst nil)
	(yn-query-lst     nil))
    (dolist (vp-query vp-query-lst)
      (let ((type (nth 1 vp-query))
	    (query-triple (cdr (nth 2 vp-query))))
	(cond ((eq type '|*slot-value|)
	       (push vp-query lookup-query-lst))
	      ((eq type '|*yes-no-question|)
	       (push vp-query yn-query-lst))
	      (t (format nil "(collate-viewpoint-query ...) ignoring ~a.~%" vp-query)))))
    (values lookup-query-lst yn-query-lst)
))

;;Collates viewpoint-query contents into three parts
;;  a) slot-lookup types
;;  b) yes-yn-query typles
;;  c) no-yn-query typles
(defun collate-viewpoint-answerable-query(vp-answerable-query-lst)
  (let ((lookup-query-lst nil)
	(yes-yn-query-lst nil)
	(no-yn-query-lst  nil))
    (dolist (vp-answerable-query vp-answerable-query-lst)
      (let ((type (nth 1 vp-answerable-query))
	    (query-triple (cdr (nth 2 vp-answerable-query))))
	(cond ((eq type '|*slot-value|)
	       (push vp-answerable-query lookup-query-lst))
	      ((eq type '|*yes-no-question|)
	       (if (eq (car (last vp-answerable-query)) '|*yes|)
		   (push vp-answerable-query yes-yn-query-lst)
		   (push vp-answerable-query no-yn-query-lst))))))
    (values lookup-query-lst yes-yn-query-lst no-yn-query-lst)
))

(defun is-multislot-query-p (vp-inst &optional(debug nil))
  (not (null (ps-km-query `(|the| |viewpoint-multislot-type| |of| ,vp-inst) debug))))

(defun parse-slot-value-answerable-query-entry(entry)
  (let* ((type     (nth 1 entry))
	 (triple   (cdr (nth 2 entry)))
	 (frame    (triple-head triple))
	 (relation (triple-relation triple))
	 (filler   (triple-tail triple)))
    (values type frame relation filler)))

(defun get-text-for-all-slot-lookup-answerable-query(sl-query)
  (remove-duplicates
   (remove nil
	   (mapcar 'get-text-for-slot-lookup-answerable-query-entry 
		   sl-query))
   :test 'string=))

(defun get-text-for-slot-lookup-answerable-query-entry(entry)
  (if (eq (nth 1 entry) '|*slot-value|)
	(multiple-value-bind
	    (type frame relation filler)
	    (parse-slot-value-answerable-query-entry entry)
	  (ps-get-text-description-for-frame-slot frame relation))))

(defun parse-yes-no-question-answerable-query-entry(entry)
  (let* ((type        (nth 1 entry))
	 (triple-lst  (nth 2 entry))
	 (yn-val         (nth 3 entry)))
    (values type triple-lst yn-val)))

(defun get-text-for-all-yes-no-question-answerable-query(yn-query)
  (remove-duplicates
   (remove nil
	   (mapcar 'get-text-for-yes-no-question-answerable-query-entry
		   yn-query))
   :test 'string=))

(defun get-text-for-yes-no-question-answerable-query-entry(entry)
  (let ((debug nil))
  (if (eq (nth 1 entry) '|*yes-no-question|)
      (multiple-value-bind
	  (type triple-lst yn-val)
	  (parse-yes-no-question-answerable-query-entry entry)
	  (mappend #'(lambda(instance)
		       (ps-km-query `(|the| |text-description| |of| ,instance) debug))
		   (extract-all-km-instances triple-lst))))))

;;Checks if two viewpoints have the same answer.
;;(The (set-equal ...) call works fine for strings without
;;requiring special comparison operator.
;;(set-equal '("a" "b" "c") '("b" "c" "a" "b")) => t
;;(set-equal '("a" "b" "c") '("b" "c" "a" "d")) => nil
;;Returns true if
;; a) Have same set of slot-lookup text-strings
;; b) Have same number and same set of slot-lookup text-strings for *no  triples
;; c) Have same number and same set of slot-lookup text-strings for *yes triples
(defun viewpoint-answer-equal(vp-inst1 vp-inst2)
  (let ((debug nil))
    (multiple-value-bind
	(answerable-lookup-query-lst1 answerable-yes-yn-query-lst1 answerable-no-yn-query-lst1)
	(collate-viewpoint-answerable-query (get-viewpoint-answerable-query vp-inst1))
      (multiple-value-bind
	  (answerable-lookup-query-lst2 answerable-yes-yn-query-lst2 answerable-no-yn-query-lst2)
	  (collate-viewpoint-answerable-query (get-viewpoint-answerable-query vp-inst2))
	(and 
	 (set-equal
	  (get-text-for-all-slot-lookup-answerable-query answerable-lookup-query-lst1)
	  (get-text-for-all-slot-lookup-answerable-query answerable-lookup-query-lst2))	 
	 (and (= (length answerable-yes-yn-query-lst1)
		 (length answerable-yes-yn-query-lst2))
	      (set-equal (get-text-for-all-yes-no-question-answerable-query 
			  answerable-yes-yn-query-lst1)
			 (get-text-for-all-yes-no-question-answerable-query 
			  answerable-yes-yn-query-lst2)))
	 (and (= (length answerable-no-yn-query-lst1)
		 (length answerable-no-yn-query-lst2))
	      (set-equal (get-text-for-all-yes-no-question-answerable-query 
			  answerable-no-yn-query-lst1)
			 (get-text-for-all-yes-no-question-answerable-query 
			  answerable-no-yn-query-lst2))))
	)
      )
    )
  )
