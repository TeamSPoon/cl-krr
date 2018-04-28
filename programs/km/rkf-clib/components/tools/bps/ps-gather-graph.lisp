;;
;; $Id: ps-gather-graph.lisp,v 1.45 2008/10/28 20:29:26 jchaw Exp $
;;

(unless (find-package :km)     (make-package :km))
(unless (find-package :aura)   (make-package :aura))
(in-package :km)
(setq *using-km-package* t)

(defun ps-gather-graph-target-relations()
  (set-difference 
   (remove-duplicates (append (all-instances '|CLIB-Slot-Group|)
			      (all-instances '|Relation|) 
			      (all-instances '|Property|))
		      :test 'equal)
   (bps-slots-to-ignore)))

(defun bps-slots-to-ignore()
  '(|pcs-list| |dcs-list| |dcs-list-of| |wn16-synset| |wn16-synset-of| |wn20-synset| |wn20-synset-of| |computed-value| |computed-value-of| |property-slot| |property-of-slot|
    |primary-slot| |primary-slot-of|
    |secondary-slot| |secondary-slot-of| |soft-pcs-list| |soft-pcs-list-of| |test-case| |test-case-of| |description| |description-of| |doc-file| |doc-file-of|
    |output| |output-of| |cloned-from| |clone-built-from|))

(defun query-immediate-property-slots(instance &optional(verbose nil))
  (mappend #'(lambda(relation)
	       (if (and (member relation (all-instances '|EventProperty|))
			(valid-domain-for instance relation))
		   (get-triples-for-instance-query instance relation verbose)))
	   (ps-gather-graph-target-relations)))

(defun valid-domain-for(instance relation)
  (let ((concept-list       (ps-km-query `(|the| |instance-of| |of| ,instance)))
	(applicable-concept (all-domains-of relation)))
    (null (member nil
		  (mapcar #'(lambda(c)
			      (not (null (member c applicable-concept))))
			  concept-list)))))

(defun all-domains-of (relation)
  (let ((appl-concept-lst (domains-of relation)))
    (append appl-concept-lst (mappend 'all-subclasses appl-concept-lst))))

(defun all-ranges-of (relation)
  (let ((appl-concept-lst (ranges-of relation)))
    (append appl-concept-lst (mappend 'all-subclasses appl-concept-lst))))

(defun query-immediate-slots(instance &optional(verbose nil))
  (mappend #'(lambda(relation)
	       (if (valid-domain-for instance relation)
		   (get-triples-for-instance-query instance relation verbose)))
	   (ps-gather-graph-target-relations)))

(defun query-immediate-forward-slots(instance &optional(verbose nil))
  (mappend #'(lambda(relation)
	       (if (forward-relationp relation)
		   (get-triples-for-instance-query instance relation verbose)))
	   (ps-gather-graph-target-relations)))
		  
(defun get-triples-for-instance-query(instance relation &optional(verbose nil))
  (let* ((query `(|the| ,relation |of| ,instance))
	 (slot-filler (ps-km-query query verbose)))
    (mapcar #'(lambda(val)
		(list instance relation val))
	    slot-filler)))

(defun get-graph-around-instance-for-depth(scenario instance depth)
   (cond ((zerop depth) nil)
	(t (let* ((target-triples (sieve-triple-list-having-head instance 
								 scenario))
		  (tail-instances (set-difference
				   (flatten 
				    (extract-triple-tail-from-triple-list 
				     (extract-non-instance-triples target-triples)))
				   (list instance))))
	     (remove-duplicates
	      (append target-triples
		      (extract-instance-triple-for-instance
		       tail-instances scenario)
		      (mappend #'(lambda(entry)
				   (get-graph-around-instance-for-depth 
				    scenario
				    entry
				    (1- depth)))
			       tail-instances))
	      :test 'ps-triple-equal)))))

(defun old-school-ps-gather-graph(concept &optional(timeout 300))
  (sys:with-timeout
   (timeout 
    (progn
      (format t "(ps-gather-graph timed out[> ~a s]. Will attempt non-fixpoint gather graph to depth 2 for ~a.~%" timeout concept)
      (multiple-value-bind
	  (tmp-root tmp-graph tmp-fixpoint?)
	  (ps-non-fixpoint-gather-graph concept 2)
	(values tmp-root tmp-graph tmp-fixpoint?))))
   (multiple-value-bind
       (tmp-root tmp-graph tmp-fixpoint?)
       (ps-fixpoint-gather-graph concept)
     (values tmp-root 
	     tmp-graph 
	     tmp-fixpoint?
	     (COMPUTE-MD5-FOR-CONCEPT-FILE concept)
	     ))))

;; we want the original relations.
;; e.g., Motion-under-force -> caused-by -> Exert-Force, instead of Exert-Force -> causes -> Motion-under-force
;; Relations identified earlier are more important. So they should not be removed during duplicate checking.
;; The way to get this cheaply is by reversing the triple-list.
(defun ps-fixpoint-gather-graph(concept)
  (let* ((*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
	 (*built-in-remove-subsumers-slots*  '(|instance-of|))
	 (*EQ-FLAG*                          nil)
	 (root (ps-instantiate-concept concept))
	 (graph 
	  (remove-duplicates
	   (reverse  ;;Impt. See note.
	    (cons (list root '|instance-of| concept)
		  (ps-fixpoint-gather-graph-for-instance root nil nil -1)))
	   :test 'ps-triple-equal)))
    (values root (ps-update-instance-triples graph nil) t)))

(defun ps-fixpoint-gather-graph-for-instance(instance 
				    instance-depth-list
				    instance-txt-list
				    max-depth-count 
				    &optional(verbose t))
  (cond ((and (or (< (length instance-depth-list) max-depth-count)
		  (< max-depth-count 0))
	      (null (member instance instance-depth-list))
	      (km-instancep instance))
	 (let ((start-time (get-internal-run-time)))
	   (if verbose 
	       (format t "querying ~a~%" 
		       (insert-at-end instance instance-depth-list)))
	   (let* ((instance-concept (car (ps-km-query `(|the| |instance-of| |of| ,instance))))
		  (instance-skolem-txt   (get-skolem-text-description-for-concept instance-concept))
		  (instance-txt          (get-text-description-for-km-frame instance)))
	     (cond ((or (null instance-depth-list)
			(ALWAYS-EXPAND-FOR-GATHER-GRAPH-P instance-concept)
			(and (not (km-skolem-instance-p instance))
			     (not (member instance-txt instance-txt-list :test 'string=))))
		    (let ((immediate-triples (query-immediate-slots instance)))
		      (format t " [~a ms]~%" (- (get-internal-run-time) start-time))
		      (append immediate-triples
			      (mappend #'(lambda(immediate-slot-instance)
					   (ps-fixpoint-gather-graph-for-instance
					    immediate-slot-instance
					    (insert-at-end instance instance-depth-list)
					    (remove nil (insert-at-end instance-txt instance-txt-list))
					    max-depth-count))
				       (extract-all-instances-from-triple-list
					(extract-triple-tail-from-triple-list immediate-triples))))))))))))

(defun km-skolem-instance-p(instance)
  (let* ((instance-concept (car (ps-km-query `(|the| |instance-of| |of| ,instance))))
	 (instance-skolem-txt   (get-skolem-text-description-for-concept instance-concept))
	 (instance-txt          (get-text-description-for-km-frame instance)))
    (or (and (or (isa instance '|Move|)
		 (property-value-instance-p instance))
	     (not (null (search "unknown" instance-txt))))
	(string= instance-skolem-txt
		 instance-txt))))

(defun always-expand-for-gather-graph-p(concept)
  (let ((concepts-to-always-expand '(|Chemical-Formula| |Chemical-Equation-Expression|
				     |Equilibrium-Constant-Expression|)))
    (not (null (member concept concepts-to-always-expand)))))

;; we want the original relations.
;; e.g., Motion-under-force -> caused-by -> Exert-Force, instead of Exert-Force -> causes -> Motion-under-force
;; Relations identified earlier are more important. So they should not be removed during duplicate checking.
;; The way to get this cheaply is by reversing the triple-list.
(defun ps-non-fixpoint-gather-graph(concept depth)
  (let ((*classification-enabled*           nil)
	(*prototype-classification-enabled* nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|)))
    (let* ((root (ps-instantiate-concept concept))
	   (graph 
	    (remove-duplicates
	     (reverse 
	      (cons (list root '|instance-of| concept)
		    (ps-non-fixpoint-gather-graph-for-instance root '() depth)))
	      :test 'ps-triple-equal)))
      (values root (ps-update-instance-triples graph nil) nil))))

(defun ps-non-fixpoint-gather-graph-for-instance(instance 
				    instance-depth-list 
				    max-depth-count 
				    &optional(verbose t))
  (cond ((and (< (length instance-depth-list) max-depth-count)
	      (null (member instance instance-depth-list)))
	 (let ((start-time (get-internal-run-time)))
	   (if verbose 
	       (format t "querying ~a~%" 
		       (insert-at-end instance instance-depth-list)))
	   (let ((immediate-triples (query-immediate-slots instance)))
	     (format t " [~a ms]~%" (- (get-internal-run-time) start-time))
	     (append immediate-triples
		     (mappend #'(lambda(immediate-slot-instance)
				  (ps-non-fixpoint-gather-graph-for-instance
				   immediate-slot-instance
				   (insert-at-end instance instance-depth-list)
				   max-depth-count))
			      (extract-triple-tail-from-triple-list immediate-triples))))))))

(defun bps-aura-get-graph(concept)
  (handler-case 
   (let ((*prototype-classification-enabled* t)  ;temporarily turn on KM prototype classification
	 (*classification-enabled*           t)
	 (*built-in-remove-subsumers-slots*  '(|instance-of|))
	 (*recursive-classification*         nil);;turning it on causes problem with chemistry-ke-kb
	 (*on-error* 'abort)
	 )
     (aura-get-graph concept))
		(error (condition)
		       (progn
			 (if (not (null (member concept *all-user-defined-concepts*)))
			     (push concept *bad-user-defined-concepts*))
			 (format t "BPS: Bad concept? ~a~%" concept)
			 nil))))

(defun ps-gather-graph (concept)
  (if (member concept *all-user-defined-concepts*)
      (bps-aura-get-graph concept)
      (old-school-ps-gather-graph concept)))

(defun old-school-ps-gather-graph(concept &optional(timeout 300))
  (sys:with-timeout
   (timeout 
    (progn
      (format t "(ps-gather-graph timed out[> ~a s]. Will attempt non-fixpoint gather graph to depth 2 for ~a.~%" timeout concept)
      (multiple-value-bind
	  (tmp-root tmp-graph tmp-fixpoint?)
	  (ps-non-fixpoint-gather-graph concept 2)
	(values tmp-root tmp-graph tmp-fixpoint?))))
   (multiple-value-bind
       (tmp-root tmp-graph tmp-fixpoint?)
       (ps-fixpoint-gather-graph concept)
     (values tmp-root 
	     tmp-graph 
	     tmp-fixpoint?
	     (COMPUTE-MD5-FOR-CONCEPT-FILE concept)
	     ))))

;;Returns triples as described in prototype.
(defun aura-get-graph(concept)
  (let* ((prototypes (ps-km-query `(|the| |prototypes| |of| ,concept)))
	 (triple-lst 
	  (mappend #'(lambda(prototype)
		       (multiple-value-bind
			   (build-clone-assertion-lst mapping)
			   (build-clones prototype)
			 (replace-elements-in-list
			  (flatten-clone-graph-lst 
			   build-clone-assertion-lst)
			  (invert-map mapping))))
		   prototypes)))
    (values (car (quasi-get-kb-instances-for-concept concept triple-lst))
	    triple-lst)))

(defun remove-irrelevant-bps-triples (triple-lst)
  (let (result)
    (dolist (triple (remove-irrelevant-km-triples triple-lst))
      (if (and (null (member (triple-relation triple) (bps-slots-to-ignore)))
	       (null (member (invert-slot (triple-relation triple)) (bps-slots-to-ignore))))
	  (push triple result)))
    result))

(defun flatten-clone-graph-lst(clone-graph-lst)
  (remove-irrelevant-bps-triples
   (remove-duplicates (mappend 'flatten-clone-graph clone-graph-lst) :test 'ps-triple-equal)))

(defun flatten-clone-graph(clone-graph)
  (let ((head (car clone-graph)))
    (remove-duplicate-triples
     (mappend #'(lambda(slot+val-lst)
		  (let ((slot (car slot+val-lst))
			(val-lst (cadr slot+val-lst)))
		    (mappend #'(lambda(val)
				 (cond ((atom val)
					(list (list head slot val)))
					;(t (multiple-value-bind
					;	      (filler filler-triples)
					;	      (ps-instantiate-concept-as-triple-list
					;	       (ranges-of slot))
					;	    (cons (list head slot filler)
					;		  filler-triples)
				       ))
			     val-lst)))
	      (cadr clone-graph)))))

(defun ps-get-subgraph-for-matching (concept-graph)
  (let ((debug                 t)
	(non-instance-triples (extract-non-instance-triples concept-graph))
	(all-instances (extract-all-km-instances concept-graph))
	(ignored-triple-lst    nil))
    (dolist (triple non-instance-triples)
      (cond ((or (listp (triple-tail triple))
		 (listp (triple-head triple))
		 (not (member (triple-tail triple) all-instances))
		 (not (member (triple-head triple) all-instances))
		 (member (triple-relation triple)
			 '(|component|
			   |equation-uses|
			   |equation|
			   |equation-expression|)
			 :test 'same-graph-edge))				   
	     (push triple ignored-triple-lst))))
    (values 
     (quasi-properly-terminate-triple-list 
      (set-difference non-instance-triples
		      ignored-triple-lst :test 'ps-triple-equal)
      concept-graph)
     (quasi-properly-terminate-triple-list 
      ignored-triple-lst
      concept-graph))))
