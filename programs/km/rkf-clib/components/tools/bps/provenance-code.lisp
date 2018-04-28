;;
;; $Id: provenance-code.lisp,v 1.7 2009/08/28 04:08:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun reinstall-viewpoint-answer-provenance (new-vp-inst existing)
  (let ((answer-provenance (cadr (get-viewpoint-answer-provenance existing))))
    (if (not (null answer-provenance))
	(multiple-value-bind
	    (input-provenance query-provenance)
	    (parse-viewpoint-answer-provenance
	     answer-provenance)
	  (install-viewpoint-answer-provenance new-vp-inst
					       input-provenance
					       query-provenance)))
)
)

(defun install-viewpoint-answer-provenance (vp-inst input-provenance query-provenance)
  (let ((debug nil))
    (if (and (not (null vp-inst))
	     (not (null input-provenance))
	     (not (null query-provenance)))
	(progn 
	  (push (list vp-inst (list input-provenance query-provenance))
		*bps-answer-provenance*)
	  
	)
    )
)
)

(defun get-viewpoint-answer-provenance(vp-instance)
  (let ((tmp (assoc vp-instance *bps-answer-provenance*)))
    (if (not (null tmp))
	(list (car tmp)
	      (remove-duplicates (cadr tmp)
				 :test 'equal)))))

(defun parse-viewpoint-answer-provenance(in)
  (let ((input-provenance   (nth 0 in))
	(query-provenance  (nth 1 in)))
    (values input-provenance query-provenance)
))

;;returns info on question inputs used in concept selection
;; a) used 
;; b) not used
;; c) how it is used... [not yet]
(defun get-input-provenance(vp-inst)
  (mapcar #'(lambda(ancestor-vp-inst)
	      (if (and 
		   (not (null (get-viewpoint-triple-map ancestor-vp-inst)))
		   (not (null (get-viewpoint-correspondence ancestor-vp-inst))))
		  (list ancestor-vp-inst
			(list 
			 (get-viewpoint-model-root ancestor-vp-inst)
			 (strip-pair-prefix
			  (get-viewpoint-triple-map ancestor-vp-inst))
			 (strip-pair-prefix 
			  (get-viewpoint-correspondence ancestor-vp-inst)))
		  )
		(list ancestor-vp-inst nil)
	      )
	   )
		  (flatten (get-ancestors vp-inst)))
	  )

;;examines input provenance info to determine
;;scenario triples used.
;;There's alot of information in the input...
;;It's basically a list of ancestor viewpoints
;; a) ancestor viewpoint instance
;; b) ancestor viewpoint model root
;; c) ancestor viewpoint triple mapping
;; d) ancestor viewpoint node mapping
;;All we are interested for now, is how much of input scenario was used. 
;;So only considering the LHS of each triple-map entry, for each ancestor viewpoint instance. 
;;And we get it by doing a union of all LHS in triple maps across all ancestor viewpoints
(defun get-scenario-triples-used-from-input-provenance(input)
  (let ((uninterned-result nil)
	(interned-result   nil))
    (dolist (ancestor-info input)
      (let* ((ancestor-vp-inst    (nth 0 ancestor-info))
	     (ancestor-mapping    (nth 1 ancestor-info))
	     (ancestor-model-root (nth 0 ancestor-mapping))
	     (ancestor-triple-map (nth 1 ancestor-mapping))
	     (ancestor-node-map   (nth 2 ancestor-mapping)))
	(dolist (ancestor-triple-map-entry ancestor-triple-map)
	  (let* ((score (cadr ancestor-triple-map-entry))
		 (mapping  (strip-triple-prefix (cdr (car ancestor-triple-map-entry))))
		 (lhs (nth 0 mapping))
		 (rhs (nth 1 mapping)))
	    (push lhs uninterned-result)))))
    (dolist (triple uninterned-result)
      (push (list (intern (triple-head triple) :km)
		  (intern (triple-relation triple) :km)
		  (intern (triple-tail triple) :km))
	    interned-result))
    (properly-terminate-triple-list 
     interned-result
    )
))

(defun get-concepts-used-from-input-provenance (input-provenance)
  (let ((result nil))
    (dolist (ancestor-info input-provenance)
      (let* ((ancestor-vp-inst    (nth 0 ancestor-info))
	     (ancestor-mapping    (nth 1 ancestor-info))
	     (ancestor-model-root (nth 0 ancestor-mapping))
	     (ancestor-triple-map (nth 1 ancestor-mapping))
	     (ancestor-node-map   (nth 2 ancestor-mapping)))
	(push ancestor-model-root result)))
    (remove nil result)
    )
)

;;provenance info is a list of two values
;; a) triple mappings
;; b) node mappings
(defun get-provenance-info-from-match-config(x)
  (list 
   (car  x)   ;;triple-map information
   (cadr x)   ;;node-map information
  )
)

(defun make-positive-provenance-scores(provenance)
  (let ((triple-map (car provenance))
	(node-map   (cadr provenance)))
    (list 
     (mapcar #'(lambda(triple-map-entry)
		 (let ((map (car triple-map-entry))
		       (score (cdr triple-map-entry)))
		   (cons map (abs score))))
	     triple-map)
     node-map)
))

;;provenance info is a list of two values
;; a) triple mappings
;; b) node mappings
(defun merge-provenance-info(x y)
  (list 
   (remove-duplicates
    (append (car x)   ;;merge triple-map entries
	    (car y))
    :test 'equal)
   (remove-duplicates
    (append (cadr x)  ;;merge node-map entries
	    (cadr y))
    :test 'equal)
  )
)

(defun create-provenance-triple-map-info(lhs rhs score)
  (cons
   (list lhs rhs)
   score)
)

(defun provenance-triple-for-property-instance(instance)
  (let* ((debug nil))
    (cond ((ps-improved-isa instance '|Property-Vector-Value|)
	   (multiple-value-bind
	       (instance secondary-slot dir-instance mag-instance dir-value mag-value)
	       (parse-property-vector-value-instance instance)
	     (let ((mag-value-str (car (ps-km-query `(|the| |text-gen| |of| ,mag-instance) debug)))
		   (dir-value-str (car (ps-km-query `(|the| |text-gen| |of| ,dir-instance) debug))))
	       (list 
		(list instance '|direction| (format nil "~s" dir-value-str))
		(list instance secondary-slot (format nil "~s" mag-value-str))
		)
	       )))
	  (t 
	   (let ((val-str (car (ps-km-query `(|the| |text-gen| |of| ,instance) debug))))
	     (list 
	      (list instance '|value| (format nil "~s" val-str))
	      )
	     )
	   )
	  )
    )
  )

(defun get-contradiction-triple-for-non-semantic-yn-triple(triple-to-verify 
							   &key (scenario nil))
  (let* ((lhs (triple-head triple-to-verify))
	 (rhs (triple-tail triple-to-verify))
	 (operator (triple-relation triple-to-verify))
	 (contradictory-comparison-operator-lst
	  `((|equal| |not-equal|)
	    (|isa| |is-not-a|)
	    (|is-a| |is-not-a|)
	    (|instance-of| |is-not-a|)
	    (|greater-than| |less-than-or-equal-to|)
	    (|less-than| |greater-than-or-equal-to|)
	    (|less-than-or-equal-to| |greater-than|)
	    (|greater-than-or-equal-to| |less-than|)
	    (|related-to| |not-related-to|)))
	 (contradictory-operator 
	  (cadr (assoc operator contradictory-comparison-operator-lst))))
    (cond ((equal contradictory-operator '|is-not-a|)
	   (list lhs contradictory-operator 
		 (quasi-ps-slot-lookup rhs '|instance-of| scenario)))
	  ((null contradictory-operator) nil)
	  (t 
	   (list lhs contradictory-operator rhs))
	  )
    )
  )

(defun get-property-comparison-relation-provenance(triple-to-verify score)
  (let ((lhs (triple-head triple-to-verify))
	(rhs (triple-tail triple-to-verify))
	(comparison-operator (triple-relation triple-to-verify))
	)    
    (cond ((equal score 1)
	   `(
	     (((,triple-to-verify 
		,(cons triple-to-verify
			 (append (provenance-triple-for-property-instance lhs)
				 (provenance-triple-for-property-instance rhs))
			 ) 
		)
	       . ,score))  ;;triple mappings
	     nil           ;;node mappings
	     )
	   )
	  ((equal score -1)
	   `(
	     (((,triple-to-verify 
		,(append (list (GET-CONTRADICTION-TRIPLE-FOR-NON-SEMANTIC-YN-TRIPLE 
				triple-to-verify))
			 (append (provenance-triple-for-property-instance lhs)
				 (provenance-triple-for-property-instance rhs))
			 ) 
		)
	       . ,score))  ;;triple mappings
	     nil           ;;node mappings
	     )
	   )
	  (t
	   `(
	     (((,triple-to-verify 
		,(append (provenance-triple-for-property-instance lhs)
			 (provenance-triple-for-property-instance rhs))
		) 
	       . ,score))  ;;triple mappings
	     nil           ;;node mappings
	     )
	   )
	  )
  )
)

(defun get-provenance-info-for-viewpoint(vp-inst)
  (let ((provenance-info (cadr (assoc vp-inst
				      *bps-answer-provenance*))))
    (multiple-value-bind
	(input-provenance query-provenance)
	(parse-viewpoint-answer-provenance provenance-info)
      (values input-provenance query-provenance vp-inst)
      )
    )
)

