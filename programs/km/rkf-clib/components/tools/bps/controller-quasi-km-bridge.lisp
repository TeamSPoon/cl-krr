;;
;; $Id: controller-quasi-km-bridge.lisp,v 1.30 2009/06/22 02:09:01 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-get-subgraph(root graph)
  (cond ((null root) nil)
	((atom root)
	 (extract-all-forward-pointing-triples-for-instance root
							    graph))
	(t (remove-duplicates
	    (append (ps-get-subgraph (car root) graph)
		    (ps-get-subgraph (cdr root) graph))
	    :test 'ps-triple-equal))))

(defun ps-get-subgraph-for-frame-slot(frame slot input-graph)
  (let* ((graph       (make-triple-proper input-graph))
	 (slot-filler  (quasi-ps-slot-lookup frame slot graph)))
    (remove-duplicates
     (mappend
      #'(lambda(slot-filler-entry)
	  (ps-get-subgraph slot-filler-entry graph))
      (extract-all-km-instances slot-filler))
  :test 'ps-triple-equal)))

(defun get-query-triples-for-all-instances(scenario 
					   &optional (verbose t))
  (mapcar #'(lambda(question-instance)
	      (ps-generate-query-triples-for-instance 
	       scenario 
	       question-instance))
	  (extract-all-instances-from-triple-list scenario)))

(defun ps-get-query-triples(scenario compute-question &optional(yn-query nil))
  (union
   (ps-get-query-triples-for-compute-question scenario 
					      compute-question)
   (ps-get-query-triples-for-is-it-true-question scenario yn-query)
   :test 'ps-triple-equal))

(defun ps-get-query-triples-for-is-it-true-question (scenario yn-query)
  (let ((yn-triples (mappend 
		     #'(lambda(x)
			 (cdr (reverse x)))
		     (remove-if-not 
		      'true-false-yn-query-entry-p
		      yn-query))))
	yn-triples))

(defun ps-get-query-triples-for-compute-question (scenario compute-question)
  (cond ((null compute-question) ())
	(t (let ((target (car compute-question))
		 (rest   (cdr compute-question)))
	     (remove-duplicates
	      (append
	       (cond ((atom target)
		      (let ((result (cadr 
				     (assoc target
					    (get-query-triples-for-all-instances 
					     (extract-non-instance-triples scenario))))))
			(if (and (null result)
				 (not (quasi-property-value-instance-p compute-question scenario)))
			    (list (list compute-question '|instance-of| '*))
			  result)))
		     ((and (listp target)
			   (or 
			    (equal (car target) 'what-is-the)
			    (equal (car target) 'what-is-a)))
		      (list (list (cadr target) '|instance-of| '*))
		      ;; no longer necessary. we report input vs query provenance individually.
		      ;(cons 
		       ;(list (cadr target) '|instance-of| '*)
			    ;(extract-non-instance-triples
			    ; (get-all-triples-related-to-instance-list 
			    ;  (list (cadr target))
			    ;  scenario)
			    ; )
		      )
		     ((and (listp target)
			   (equal (car target) 'how-many))
		      (ps-get-query-triples-for-compute-question scenario (cdr target)))
		     )
	       (ps-get-query-triples-for-compute-question scenario rest))
	      :test 'equal)))))

;;Converts cpl-question instances into query-triples , i.e., (<query-frame> <relation> *)
(defun ps-generate-query-triples-for-instance (scenario 
					       question-instance
					       &optional(verbose nil))
  (let ((result 
	 (remove nil
		 ;;Only interested in query-paths whose query-frame is a KM instance.
		 (mapcar #'(lambda(query-path)
			     (let ((query-frame (fourth query-path)))
			       (if (and (km-instancep query-frame) 
					;(forward-relationp (second query-path)) ;;Necessary? 17Oct08.. If enabled, does not work correctly when slot inv and itself are the same.
					)
				   (list (fourth query-path)
					 (second query-path)
					 '*))))
			 (remove-duplicates (ps-extract-question scenario (list question-instance))
					    :test #'equal)))))
    (if verbose (format t "instance:~a, triple: ~a~%"
			question-instance
			result))
    (list question-instance result)))

(defun quasi-query-immediate-slots (instance triple-list)
  (sieve-triple-list-having-head instance triple-list))

;;Performs quasi-slot-lookup using information contained in triple-list
;;FIXME: 1) Include guards to ensure triple-relations are the same in target-triples
(defun quasi-ps-slot-lookup(target-head target-relation triple-list)
  (let ((target-triples (extract-triples-for-root-slot target-head
						       target-relation
						       triple-list)))
    (extract-triple-tail-from-triple-list target-triples)))

;;Performs get-concept-for-kb-instance using information contained in triple-list
(defun quasi-get-concept-for-kb-instance (kb-instance triple-list)
  (cond ((consp kb-instance)
	 (mapcar #'(lambda (in)
		     (car (quasi-get-concept-for-kb-instance in triple-list)))
		 kb-instance))
	(t (remove-subsumers (quasi-ps-slot-lookup kb-instance '|instance-of| triple-list)))))

;;Given triple, generalize it into a triple with class names.
(defun quasi-generalize-triple(triple triple-list)
  (let ((head (triple-head triple))
	(relation (triple-relation triple))
	(tail (triple-tail triple)))
    (list (quasi-get-concept-for-kb-instance head triple-list)
	  relation
	  (quasi-get-concept-for-kb-instance tail triple-list))))

;;Given triple, generalize it into a triple with class names.
;;FIXME: Take into account, multiple instances, e.g. Chamber1, Chamber2.
(defun quasi-generalize-triple-list(triple-list scenario)
  (mapcar #'(lambda(triple)
	      (quasi-generalize-triple triple scenario))
	  triple-list))

;;Returns the type for slot of some concept.
;;This version is unsafe, i.e. result computed using quasi-km routines.
(defun get-type-for-slot-lookup-unsafe(concept slot)
  (multiple-value-bind 
    (root graph fixpoint?)
    (procure-graph concept)
    (let ((target-instances (quasi-ps-slot-lookup root slot graph)))
      (remove-subsumers 
       (quasi-get-concept-for-kb-instance target-instances
					  graph)))))

;;Returns the cardinality for concept-slot in triple-list
(defun quasi-get-cardinality-for-instance-slot(instance slot triple-list)
  (length (quasi-ps-slot-lookup instance slot triple-list)))

;;Returns the cardinality for concept-slot
(defun quasi-get-cardinality-for-concept-slot(concept slot)
  (multiple-value-bind 
    (root graph fixpoint?)
    (procure-graph concept)
    (quasi-get-cardinality-for-instance-slot root slot graph)))

(defun get-in-edge-count(instance input-scenario &optional(verbose nil))
  (let* ((scenario (extract-non-instance-triples (make-triple-proper input-scenario)))
	 (result   (length (sieve-triple-list-having-tail instance
							  scenario))))
    (if verbose (format t "(get-in-edge-count ~a) = ~a~%" instance result))
    result))

(defun get-out-edge-count(instance input-scenario &optional(verbose nil))
  (let* ((scenario (extract-non-instance-triples (make-triple-proper input-scenario)))
	 (result   (length (sieve-triple-list-having-head instance
							 scenario))))
    (if verbose (format t "(get-out-edge-count ~a) = ~a~%" instance result))
    result))

(defun get-in-edge-count-for-root-slot(root reln input-scenario &optional(verbose nil))
  (let* ((scenario        (make-triple-proper (remove-irrelevant-km-triples input-scenario)))
	 (scenario-filler (quasi-ps-slot-lookup root reln scenario)))
    (reduce '+ (remove nil 
		       (mapcar #'(lambda(instance) (get-in-edge-count instance scenario verbose)) 
			       scenario-filler)))))

(defun quasi-property-value-instance-p (kb-instance triple-list)
  (property-value-p (quasi-get-concept-for-kb-instance kb-instance triple-list)))

(defun quasi-get-kb-instances-for-concept(concept triple-lst)
  (let ((result nil))
    (dolist (x (extract-instance-triples triple-lst))
      (if (equal concept (triple-tail x))
	  (push (triple-head x) result)))
    result))

(defun quasi-ps-slot-lookup-in-context(frame relation vp-inst)
  (let ((corresponding-frame (get-corresponding-frame-for frame vp-inst)))
    (multiple-value-bind
	(simple-context-htable full-context-htable)
	(hash-viewpoint-context vp-inst)
      (or (gethash (list frame relation) simple-context-htable)
	  (gethash (list corresponding-frame relation) full-context-htable)))))

;;Returns true if (disjunctive)
;; a) input is an atom and is mentioned in viewpoint-correspondence, or
;; b) input is a list and 1+ elements are mentioned in viewpoint-correspondence
(defun mentioned-in-viewpoint-correspondence?(input vp-inst)
  (cond ((null input) ())
	((atom input)
	 (let ((instance-str (if (not (null input))(format nil "~a" input))))
	   (and (stringp instance-str)
		(not 
		 (null 
		  (member instance-str
			  (remove-if-not 'stringp 
					 (flatten (get-viewpoint-correspondence vp-inst)))
			  :test 'string=))))))
	(t (or (mentioned-in-viewpoint-correspondence?
		(car input) vp-inst)
	       (mentioned-in-viewpoint-correspondence?
		(cdr input) vp-inst)))))

(defun get-instances-inside-viewpoint-correspondence(vp-inst)
  (remove-duplicates
   (mapcar #'(lambda(x)
	       (intern x :km))
	   (remove-if-not 'stringp 
			  (flatten (get-viewpoint-correspondence vp-inst))))
   :test 'equal))
  
(defun get-corresponding-frame-for(instance vp-inst)
  (if (mentioned-in-viewpoint-correspondence? instance vp-inst)
      (let ((instance-str (format nil "~a" instance)))
	(intern
	 (cadr (or 
		(assoc instance-str
		       (mapcar 'cdr (get-viewpoint-correspondence vp-inst))
		       :test 'string=)
		(assoc instance-str
		       (invert-map (mapcar 'cdr (get-viewpoint-correspondence vp-inst)))
		       :test 'string=)))
	 :km))))

(defun hash-viewpoint-context(vp-inst)
  (if (null (assoc vp-inst *hash-viewpoint-context-data*))
      (multiple-value-bind
	  (simple-context full-context)
	  (get-simplified-context-for-viewpoint vp-inst)
	(let ((simple-context-hash (hash-triple-list simple-context))
	      (full-context-hash (hash-triple-list full-context)))
	  (push (list vp-inst (list simple-context-hash full-context-hash))
		*hash-viewpoint-context-data*))))
  (values (car (cadr (assoc vp-inst *hash-viewpoint-context-data*)))
	  (cadr (cadr (assoc vp-inst *hash-viewpoint-context-data*)))))

(defun hash-triple-list(triple-lst)
  (let ((htable (make-hash-table :test 'equal)))
    (dolist (triple 
	     (append 
	      (aggregate-similar-relations triple-lst)
	      (aggregate-similar-relations (invert-triple-list triple-lst))))
      (let ((value (if (atom (triple-tail triple))
		       (list (triple-tail triple))
		     (triple-tail triple))))
      (setf (gethash (list (triple-head triple)
			   (triple-relation triple))
		     htable)
	    value)))
    htable))

(defun clrhash-viewpoint-context-data()
  (dolist (entry *hash-viewpoint-context-data*)
    (let ((vp-inst (car entry))
	  (simple-context-htable (car (cadr entry)))
	  (full-context-htable   (cadr (cadr entry))))
      (clrhash simple-context-htable)
      (clrhash full-context-htable)))
  (setq *hash-viewpoint-context-data* nil)
)
