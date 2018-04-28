;;
;; $Id: controller-reify-operations.lisp,v 1.114 2009/09/01 21:41:26 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Asserts a list of km-assertions
(defun ps-assert(km-assertion-lst)
  (let ((debug nil))
    (mapcar #'(lambda(km-assertion)
		(list km-assertion (ps-km-query km-assertion debug)))
	    km-assertion-lst)))

;;Need to install all applicable lhs and rhs \== entries to prevent bad unification when
;;we need to unify lhs and rhs entries that are members of multi-slot values.
(defun regular-km-unify(x y &optional(mutate-km-working-memory? t))
  (let* ((km-assertion-lst ())
	 (debug nil)
	 (lhs-\==-entries (ps-km-query `(|the| |/==| |of| ,x) debug))
	 (rhs-\==-entries (ps-km-query `(|the| |/==| |of| ,y) debug))
	 (all-\==-entries (union lhs-\==-entries rhs-\==-entries)))
    (push `(,x  |now-has| (/==  ,all-\==-entries)) km-assertion-lst)
    (push `(,y  |now-has| (/==  ,all-\==-entries)) km-assertion-lst)
    (push `(,x  == ,y)                             km-assertion-lst)
    (if mutate-km-working-memory? (ps-assert (reverse km-assertion-lst)))
    (reverse km-assertion-lst)
    ))

;;Reifies the unification of mappings for scenario and model triples.
(defun assert-viewpoint-correspondence (vp-inst 
					&optional(mutate-km-working-memory? t))
  (let ((debug nil)
	(correspondences (strip-pair-prefix (get-viewpoint-correspondence vp-inst))))
    ;; Iterate through each correspondence and unify them.
    (mappend #'(lambda(correspondence)
		 (regular-km-unify (intern (nth 0 correspondence) :km)
				   (intern (nth 1 correspondence) :km)
				   mutate-km-working-memory?))
	     correspondences)))

;;Reifies all the necessary triples in a Viewpoint to do problem-solving
;;to prevent KM from trying to unify with a more generic instance in a multi-valued-slot
;;We need to 
;;   a) explicitly relate(unify) the correspondences 
;;   b) install \== relations for all multi-valued slot-fillers
;;Also, we should not be asserting anything related to Equation-Set or System and their slots inside the concept-graph
(defun assert-viewpoint-triples (vp-instance)     
  (let* ((debug nil)
	 (km-assertion-lst  ())
	 (scenario          (get-simplified-km-triples (get-viewpoint-scenario vp-instance)))
	 (assumptions-graph (get-simplified-km-triples (get-viewpoint-assumption-triples vp-instance)))
	 (model-graph       (get-simplified-km-triples (get-viewpoint-model vp-instance)))
	 (concept-graph     (append assumptions-graph model-graph))
	 (start-time        (get-internal-real-time)))
    (if (not (asserted-viewpoint? vp-instance))
  (if (is-mapped-concept-a-reaction? vp-instance)
      (progn
      (format debug "check-all-viewpoint-query: asserting ~a differently using (reaction-identify-me) because of mapping to Reaction instance~%" 
	      vp-instance)
      (if (not (null (intersection (get-viewpoint-target vp-instance)
				   (PSEUDO-CLASSIFY-REACTION-INSTANCES scenario))))
	  (progn
	    (format t "Found! asserting ~a!~%" vp-instance)
	    (setq km-assertion-lst (append km-assertion-lst (ps-assert-triples scenario debug)))
	    (classify-reaction-instances scenario)
	)
	)
      )
    (progn
	  ;;Necessary for eq-solver to find relevant equation systems.
	  (setq *CONTROLLER-TRIPLE-LIST*      (get-simplified-context-for-viewpoint vp-instance))
	  (setq *CONTROLLER-TRIPLE-INSTANCES* (extract-all-instances-from-triple-list-ordered-by-dependency
					       *CONTROLLER-TRIPLE-LIST*))
	  (setq km-assertion-lst (append km-assertion-lst (ps-assert-triples scenario debug)))
	  (setq km-assertion-lst (append km-assertion-lst (ps-assert-triples 
							   (remove-irrelevant-km-triples 
							    concept-graph
							    (list '|equation-uses| '|equation-symbol| '|equation-expression|
								  '|component|)
							    (list '|Equation-Set| '|System|))
							   nil)))
	  (setq km-assertion-lst (append km-assertion-lst
					 (prevent-bad-unification-for-instances-inside-multi-valued-slots 
					  *CONTROLLER-TRIPLE-LIST*)))
	  (setq km-assertion-lst (append km-assertion-lst
					 (prevent-bad-unification 
					  (remove-if #'(lambda(x) (search "Equation-Set" (stringify x)))
						     (extract-all-km-instances
						      (sieve-triple-list-having-relation-list 
						       '(|equation-uses| |used-in-equation|)
						       (copy-list *controller-triple-list*)))))))
					;(ps-evaluate-all-slots-opportunistically scenario)
					;(ps-evaluate-all-slots-opportunistically concept-graph)
	  (setq km-assertion-lst (append km-assertion-lst (assert-viewpoint-correspondence vp-instance)))
					;(ps-evaluate-all-slots-opportunistically *controller-triple-list*)
	  (if debug (format t "BPS: Asserting[~a] ~a took ~a ms. ~%" 
			    (ps-km-classification-enabled-p)
			    vp-instance
			    (- (get-internal-real-time) start-time)))
	  ;;Necessary for eq-solver to find relevant equation systems.
	  (setq *CONTROLLER-TRIPLE-LIST*      (get-simplified-context-for-viewpoint vp-instance))
	  (setq *CONTROLLER-TRIPLE-INSTANCES* (extract-all-instances-from-triple-list-ordered-by-dependency
					       *CONTROLLER-TRIPLE-LIST*))
	  (eagerly-unify-in-prototypes-for-asserted-values    
	   (extract-all-km-instances *CONTROLLER-TRIPLE-LIST*)) ;;moved here
	  (ps-evaluate-all-slots-opportunistically scenario) ;;moved here
	  (ps-evaluate-all-slots-opportunistically concept-graph) ;;moved here
	  (ps-evaluate-all-slots-opportunistically *controller-triple-list*) ;;moved here.
	  (push (list vp-instance km-assertion-lst) *viewpoint-km-assertion-lst*)
	  (ps-km-query `(,vp-instance |now-has| (|viewpoint-asserted?| (t))) debug))
      )    )
    km-assertion-lst))

(defun ps-get-all-frames-for-triples(km-triples)
  (multiple-value-bind
      (skolem-assertions non-skolem-assertions)
      (ps-get-frames-for-triples km-triples)
    (append skolem-assertions non-skolem-assertions)))

(defun ps-get-frames-for-triples(km-triples)
  (let* ((target-triples        (remove-irrelevant-km-triples km-triples))
	 (km-assertion-lst      (triples-to-km-assertions target-triples))
	 (skolem-assertions     (get-skolem-assertions km-assertion-lst))
	 (non-skolem-assertions (order-km-assertions (extract-all-km-instances
						      skolem-assertions)
						     (get-non-skolem-assertions km-assertion-lst))))
    (values skolem-assertions non-skolem-assertions)))

(defun delete-frames-in-triple-lst(km-triples)
  (dolist (frame (remove-duplicates (extract-all-km-instances km-triples)))
    (if (and (kb-objectp frame)
	     (known-frame frame)) ;;make sure KM knows it.
	(delete-frame frame))))

(defun delete-user-defined-km-constants-in-triples-lst(km-triples)
  (dolist (c (remove-duplicates (extract-all-km-constants km-triples)))
    (if (and (kb-objectp  c)
	     (known-frame c)     ;;make sure KM knows it.
	     (not (ps-km-query `(,c |isa| |Constant|)))) ;; Don't delete CLib constants, e.g., *red
	(delete-frame c))))

(defun ps-get-km-assertions(input-km-triples)
  (let* ((km-assertion-lst ())
	 (km-triples 
	  (sieve-triple-list-not-having-relation-list 
	   '(|equation-uses| |used-in-equation|)
	   (copy-list
	    (ps-remove-constraint-triples input-km-triples))))
	 (equation-uses-instances
	  (remove-if #'(lambda(x) (search "Equation-Set" (stringify x)))
		     (extract-all-km-instances
		      (sieve-triple-list-having-relation-list 
		       '(|equation-uses| |used-in-equation|)
		       (copy-list input-km-triples))))))
    (multiple-value-bind
     (skolem-assertions non-skolem-assertions)
     (ps-get-frames-for-triples km-triples)
     (setq km-assertion-lst (append km-assertion-lst skolem-assertions))
     (setq km-assertion-lst (append km-assertion-lst
				    (remove-if #'(lambda(frame)
						   (or (member '|System| (flatten frame))
						       (member '|Equation-Set| (flatten frame))))
					       non-skolem-assertions)))
     (setq km-assertion-lst (append km-assertion-lst
				    (prevent-bad-unification-for-instances-inside-multi-valued-slots km-triples)))
     (setq km-assertion-lst
	   (append km-assertion-lst
		   (prevent-bad-unification equation-uses-instances)))
     km-assertion-lst)))

;;Finds necessary triples, create KM assertions, and asserts them.
(defun ps-assert-triples (input-km-triples &optional(verbose nil))
  (let* ((km-assertion-lst (ps-get-km-assertions input-km-triples))
	 (*EQ-FLAG* nil))
    (delete-user-defined-km-constants-in-triples-lst input-km-triples)
     (dolist (km-assertion km-assertion-lst)
       (let ((start-time (get-internal-real-time)))
	 (if verbose (format t "BPS: Asserting(~a) ~s "
			     (ps-km-classification-enabled-p)
			     km-assertion))
	 (ps-km-query km-assertion)
	 (if verbose (format t " ... done, (~a ms)~%" (- (get-internal-real-time) 
							 start-time)))))
     (setq *controller-triple-list* input-km-triples)
     (setq *CONTROLLER-TRIPLE-INSTANCES*
	   (extract-all-instances-from-triple-list-ordered-by-dependency
	    input-km-triples))
     ;(ps-evaluate-all-slots-opportunistically input-km-triples)
     ;;put in reaction identification tweak here. To be removed!!!
     ;(classify-reaction-instances input-km-triples) ;; classic place to put reaction-identify-me call.
     ;(eagerly-unify-in-prototypes-for-asserted-values 
     ; (extract-all-km-instances input-km-triples))
     km-assertion-lst))

;; ;;Finds necessary triples, create KM assertions, and asserts them.
;; (defun ps-assert-triples (input-km-triples &optional(verbose t))
;;   (let* ((km-assertion-lst ())
;; 	 (*EQ-FLAG* nil)
;; 	 (km-triples 
;; 	  (sieve-triple-list-not-having-relation-list 
;; 	   '(|equation-uses| |used-in-equation|)
;; 	   (copy-list
;; 	    (ps-remove-constraint-triples input-km-triples))))
;; 	 (equation-uses-instances
;; 	  (remove-if #'(lambda(x) (search "Equation-Set" (stringify x)))
;; 		     (extract-all-km-instances
;; 		      (sieve-triple-list-having-relation-list 
;; 		       '(|equation-uses| |used-in-equation|)
;; 		       (copy-list input-km-triples))))))
;;     (delete-user-defined-km-constants-in-triples-lst input-km-triples)
;;     (multiple-value-bind
;;      (skolem-assertions non-skolem-assertions)
;;      (ps-get-frames-for-triples km-triples)
;;      (if skolem-assertions (ps-assert-skolem-assertions skolem-assertions verbose)) 
;;      (setq km-assertion-lst (append km-assertion-lst skolem-assertions))
;;      (setq km-assertion-lst (append km-assertion-lst
;; 				    (remove-if #'(lambda(frame)
;; 						   (or (member '|System| (flatten frame))
;; 						       (member '|Equation-Set| (flatten frame))))
;; 					       non-skolem-assertions)))
;;      (dolist (km-assertion 
;; 	      (remove-if #'(lambda(frame)
;; 			     (or (member '|System| (flatten frame))
;; 				 (member '|Equation-Set| (flatten frame))))
;; 			 non-skolem-assertions))
;;        (let ((start-time (get-internal-real-time)))
;; 	 (if verbose (format t "BPS: Asserting(~a) ~s "
;; 			     (ps-km-classification-enabled-p)
;; 			     km-assertion))
;; 	 (ps-km-query km-assertion)
;; 	 (if verbose (format t " ... done, (~a ms)~%" (- (get-internal-real-time) 
;; 							 start-time)))))
;;      (setq *controller-triple-list* km-triples)
;;      (setq *CONTROLLER-TRIPLE-INSTANCES*
;; 	   (extract-all-instances-from-triple-list-ordered-by-dependency
;; 	    km-triples))
;;      (setq km-assertion-lst
;; 	   (append km-assertion-lst
;; 		   (prevent-bad-unification-for-instances-inside-multi-valued-slots *CONTROLLER-TRIPLE-LIST*)))
;;      (setq km-assertion-lst
;; 	   (append km-assertion-lst
;; 	    (prevent-bad-unification equation-uses-instances)))
;;      (ps-evaluate-all-slots-opportunistically km-triples)
;;      ;;put in reaction identification tweak here. To be removed!!!
;;      (classify-reaction-instances *CONTROLLER-TRIPLE-LIST*)
;;      (eagerly-unify-in-prototypes-for-asserted-values *CONTROLLER-TRIPLE-INSTANCES*)
;;      km-assertion-lst)))

;;Rather than depend on KM to lazily unify-in-prototypes... We eagerly unify them to get around incompleteness.
;;See 
(defun eagerly-unify-in-prototypes-for-asserted-values(km-instance-lst)
  (let ((debug nil))
    (dolist (km-instance km-instance-lst)
      (let ((*trace-unify-in-prototype* debug))
	(unify-in-prototypes km-instance)))))

(defun classify-reaction-instances(triple-list)
  (let ((debug nil)
	(result nil))
    (if *controller-use-reaction-identifier*
    (dolist (reaction-instance (ps-find-instances-of triple-list '|Reaction|))
      (let ((raw-mat (ps-find-slot-value reaction-instance '|raw-material| triple-list))
	    (res (ps-find-slot-value reaction-instance '|result| triple-list)))
	(if debug (format t "Identifying reaction ~A~%" reaction-instance))
	(reaction-identify-me reaction-instance) ;;; :raw-mat raw-mat :res res)
	(if debug (format t "Identified reaction ~A as ~A~%" reaction-instance
			  (remove-subsumers (get-concept-for-kb-instance reaction-instance))))
	(setf result (append (ps-slot-lookup reaction-instance '|instance-of| debug)
			     result)))))
    (remove-duplicates result)))

(defun pseudo-classify-reaction-instances(input-triple-list)
  (let ((debug nil)
	(result nil)
	(triple-list (ps-clone-triple-list input-triple-list)))
    (ps-assert-triples triple-list)
    (if *controller-use-reaction-identifier*
	(dolist (reaction-instance (ps-find-instances-of triple-list '|Reaction|))
	  (let ((raw-mat (ps-find-slot-value reaction-instance '|raw-material| triple-list))
		(res (ps-find-slot-value reaction-instance '|result| triple-list)))
	    (if debug (format t "Identifying reaction ~A~%" reaction-instance))
	    (reaction-identify-me reaction-instance) ;;; :raw-mat raw-mat :res res)
	    (if debug (format t "Identified reaction ~A as ~A~%" reaction-instance
			      (remove-subsumers (get-concept-for-kb-instance reaction-instance))))
	    (setf result (append (ps-slot-lookup reaction-instance '|instance-of| debug)
				 result)))))
    (remove-duplicates result)))

(defun ps-evaluate-all-slots-opportunistically(triple-lst)
  (mapcar 'ps-evaluate-slot-opportunistically 
	  (append 
	   (extract-non-instance-triples triple-lst)
	   (invert-triple-list 
	    (extract-non-instance-triples triple-lst)))))

(defun ps-evaluate-slot-opportunistically(triple)
  (let ((debug  nil)
	(head   (triple-head     triple))
	(reln   (triple-relation triple))
	(filler (triple-tail     triple)))
    (if (and (km-instancep filler)
	     (km-instancep head))
	(let ((rules (rules-for reln head)))
	  (if (and (member filler  (flatten rules))
		   (or (member '&  (flatten rules))
		       (member '&& (flatten rules)))
		   (not (get-binding filler)))
	      (progn		
		(if debug (format t "Op. evaluate ~a~%" rules))
		(ps-km-query `(|the| ,reln |of| ,head) debug)
		(if debug (format t "After op. evaluate ~a~%" 
				  (rules-for reln head)))))))    
    ))

;; Only install /== relations for instances inside multi-valued slots.
;; Naively install /== relations may prevent 
;; good unifications, e.g.,  (speed of Move = speed of Velocity of Move)
(defun prevent-bad-unification-for-instances-inside-multi-valued-slots  (triple-list
									 &optional(mutate-km-working-memory? t))
  (mappend #'(lambda(instance-lst)
	       (prevent-bad-unification instance-lst 
					mutate-km-working-memory?))
	   (get-instances-inside-multi-valued-slots triple-list)))

(defun prevent-bad-unification(instances
			       &optional(mutate-km-working-memory? t))
  (let ((debug nil)
	(km-assertion-lst ()))
  (dolist (pair 
	   (remove-if #'(lambda(x) (equal (car x) (cadr x))) (all-pairs instances)))
    (push `(,(car pair) /== ,(cadr pair)) km-assertion-lst))
  (if mutate-km-working-memory? (ps-assert (reverse km-assertion-lst)))
  (reverse km-assertion-lst)))

(defun ps-assert-skolem-assertions(skolem-assertions 
				   &optional(mutate-km-working-memory? t))
  (let ((debug             nil)
	(km-assertion-lst ()))
    (if (and debug
	     (not (null skolem-assertions)))
	(format t "BPS: Instantiating "))
    (mappend #'(lambda(km-assertion)
		 (if debug (format t "~a, " (car km-assertion)))
		 (push km-assertion km-assertion-lst))
	     skolem-assertions)
    (if (and debug
	     (not (null skolem-assertions)))
	(format t "~a~a~%" #\Backspace #\Backspace))
    (ps-assert (reverse km-assertion-lst))
    (reverse km-assertion-lst)))
  
;;Predicate to check that km assertion is of the form '(_xxx has (instance-of (Thing)))
(defun skolem-assertion-p(km-assertion)
  (and (= (length km-assertion) 3)
       (equal (caar (last km-assertion)) '|instance-of|)))

;;Get list of skolem-assertion triples
(defun get-skolem-assertions(km-assertion-lst)
  (remove nil
	  (mapcar #'(lambda(x)
		      (if (skolem-assertion-p x) x))
		  km-assertion-lst)))
	      
;;Get list of non skolem-assertion triples
(defun get-non-skolem-assertions(km-assertion-lst)
  (set-difference km-assertion-lst (get-skolem-assertions km-assertion-lst) :test 'equal))

(defun find-all-subsumable (frame filler)
  (mapcar #'(lambda(target)
	     (if (car (ps-km-query `(,frame &? ,target)))
		 target))
	  filler))

(defun rank-instances-by-tax-distance-relative-to-kb-instance (kb-instance target-list)
  (let* (same-lineage-lst not-same-lineage-lst)
    (dolist (target target-list)
      (if (null (get-tax-dist-for-kb-instance kb-instance target))
	  (push target not-same-lineage-lst)
	  (push target same-lineage-lst)))
    (append (sort same-lineage-lst #'(lambda (x y) (let ((dist1 (get-tax-dist-for-kb-instance kb-instance x))
							 (dist2 (get-tax-dist-for-kb-instance kb-instance y)))
						     (if (or (null dist1) (null dist2)) 
							 t  ;;Put everything that is unrelated at back of list.
						       (< dist1 dist2)))))
	    not-same-lineage-lst)))
				       			
(defun get-tax-dist-for-kb-instance (inst1 inst2)
  (let* ((superclass1 (get-most-specific-concept (get-vals inst1 '|instance-of|)))
	 (superclass2 (get-most-specific-concept (get-vals inst2 '|instance-of|))))
    (abs-tax-dist superclass1 superclass2)))

(defun get-most-specific-concept(concept-list)
  (let* ((result (car concept-list))
	 (score  (tax-dist result '|Thing|)))
    (if (null (same-lineagep concept-list))
	(format t "BPS: Warning! Getting tax-dist for instance with multiple superclasesses of disjoint lineage. Picking superclass with deepest lineage!"))
    (dolist (concept concept-list)
      (if (> (tax-dist concept '|Thing|) score)
	  (progn
	    (setf result concept)
	    (setf score (tax-dist concept '|Thing|)))))
    result))

(defun same-lineagep(concept-list)
  (cond ((< (length concept-list) 2) t)
	(t (and (numberp (abs-tax-dist (first   concept-list)
				       (second  concept-list)))
		(same-lineagep (cdr concept-list))))))

#|
;;deprecated code

;;deprecated (defun get-non-skolem-property-value-assertions (km-assertion-lst)
  (remove nil
	  (mapcar #'(lambda(km-assertion)
		      (if (member '|value| (flatten km-assertion)) km-assertion))
		  km-assertion-lst)))

;;deprecated (defun determine-viewpoint-query-frame (vp-inst)
  (let ((vp-query (get-viewpoint-query vp-inst)))
    vp-query))

;;deprecated (defun ps-assert-instance-triple(triple)
  (let ((head (triple-head     triple))
	(reln (triple-relation triple))
	(tail (triple-tail     triple)))
    (if (atom tail)
	(ps-km-query `(,HEAD |has| (|instance-of| (,TAIL))))
        (ps-km-query `(,HEAD |has| (|instance-of|  ,TAIL))))))

;;deprecated (defun ps-query-asserted-triples(non-skolem-assertion-list)
    (dolist (km-assertion non-skolem-assertion-list)
      (ps-query-asserted-triples0 km-assertion)))

;;deprecated (defun ps-query-asserted-triples0(non-skolem-km-assertion &optional(verbose nil))
  (let* ((debug nil)
	 (frame (car non-skolem-km-assertion))
	 (*show-comments* (or debug verbose)))
    (dolist (entry (cddr non-skolem-km-assertion))
      (let ((slot (car entry)))
	(ps-km-query `(|the| ,slot |of| ,frame) (or debug verbose))))))

;;deprecated (defun apply-best-subsumable(target filler)
  (cond ((or (null target) (null filler))         
	 nil)
        ((car (ps-km-query `(,target &+ ,(car filler))))  
	 (progn
	   (format t "bps: Subsuming (~a & ~a)~%" 
		   target (car filler))
	   target))
	(t (apply-best-subsumable target (cdr filler)))))

;;deprecated (defun ps-assert-content-triple(triple)
  (let* ((head            (triple-head     triple))
	 (reln            (triple-relation triple))
	 (tail            (triple-tail     triple))
	 (filler          (ps-slot-lookup head reln))
	 (subsumable-inst nil);(apply-best-subsumable tail (rank-instances-by-tax-distance-relative-to-kb-instance tail filler)))
	 )
    (if (null subsumable-inst)
        (let ((km-assertion  (if (listp TAIL)
				 `(,HEAD |has| (,RELN ,TAIL))
  			         `(,HEAD |has| (,RELN (,TAIL))))))
	  (ps-km-query `(|the| ,RELN |of| ,HEAD))
	  (format t "bps: Asserting ~a~%" km-assertion)
	  (ps-km-query km-assertion)))))

;;deprecated (defun generate-missing-instance-triples (triple-list)
  (let ((all-instances (extract-all-km-instances triple-list)))
    (remove nil
	    (mapcar #'(lambda(instance)
			(if (not (has-instance-triple-p instance triple-list))
			    (progn 
			      (ps-classify instance)
			      (generate-instance-triples instance))))
		    all-instances))))

;;Returns list of instances in triple-list
;;deprecated (defun extract-all-instances-from-triples (triple-list)
   (remove-duplicates (mapcan #'(lambda (x) (and x (anonymous-instancep x) (list x)))
      (append 
         (mapcar #'(lambda (trip) (first trip)) triple-list)
         (mapcar #'(lambda (trip) (third trip)) triple-list)))))

;;Reifies the instance-of triples in the viewpoint-model-graph slot of 
;;some viewpoint instance
;;deprecated (defun assert-model-instances-as-skolems (vp-instance &optional(verbose nil))
  (ps-assert-skolem-assertions
   (triples-to-km-assertions 
    (extract-instance-triples
     (strip-triple-prefix
      (get-viewpoint-model vp-instance)))) verbose))

;;Reifies the triples in the viewpoint-scenario slot of 
;;some viewpoint instance
;;deprecated (defun assert-scenario (vp-instance)
  (ps-assert-triples 
   (strip-triple-prefix
    (get-viewpoint-scenario vp-instance))))

;;Reifies the triples in the viewpoint-model-graph slot of 
;;some viewpoint instance
;;deprecated (defun assert-model (vp-instance) 
  (ps-assert-triples 
   (strip-triple-prefix
    (get-viewpoint-model vp-instance))))

;;deprecated (defun bps-property-value-unify (x y &optional(verbose nil))
;;  (regular-km-unify x y))

;;deprecated (defun unify-property-value-p (x y)
  (and (property-value-instance-p x)
       (property-value-instance-p y)))

;;deprecated (defun bps-unify(x y 
;;		   vp-inst
;;		   &optional(verbose t))
;;  (let* ((start (get-internal-real-time)))
;;    (if (or *CONTROLLER-SHOW-UNIFICATION* verbose)
;;	(format t "BPS: Unifying~T~a~Twith~T~a." x y))
;;    (cond ((unify-property-value-p x y) (bps-property-value-unify x y))
;;	  (t                            (regular-km-unify x y)))
;;    (if (or *CONTROLLER-SHOW-UNIFICATION* verbose)
;;	(format t " ... done, (~a ms)~%" (- (get-internal-real-time) start)))))

;;Reifies the unification of mappings for scenario and model triples.
;;deprecated (defun assert-viewpoint-correspondence (viewpoint-output &optional(verbose nil))
  ;; Query the viewpoint to get the correspondences.
  (let ((correspondences (get-viewpoint-correspondence viewpoint-output)))
    ;; Iterate through each correspondence and unify them.
    (dolist (correspondence correspondences)
      (let ((x (intern (second correspondence) :km))
	    (y (intern (third correspondence)  :km)))
	(bps-unify x y 
		   viewpoint-output
		   verbose)))))

;; deprecated code.
;; Relates the viewpoint-correspondence mappings between scenario and model graph.
;; Done by first asserting the skolem instances for in model graph and
;; then unifying the mappings in the correspondence.

;;deprecated (defun relate-model-instances(vp-instance &optional(verbose nil))
  (progn
    (assert-model-instances-as-skolems vp-instance verbose)
    (assert-viewpoint-correspondence vp-instance verbose)))

;; 
;; Deprecated code
;; Generates instance-of triples for concepts
;;deprecated (defun generate-specific-instance-triples (input existing-instance-to-concepts)
   (cond ((null input) ())
	 ((atom input)
	  (let ((classes (km0 `(|the| |instance-of| |of| ,input))))
            (mapcar #'(lambda (class)
			(list input '|instance-of| class))
		    (remove-subsumers (append 
				       (second (find-if #'(lambda (x) (eql (first x) input)) existing-instance-to-concepts)) 
				       classes)))))
	 (t (append (generate-specific-instance-triples (car input) existing-instance-to-concepts)
		    (generate-specific-instance-triples (cdr input) existing-instance-to-concepts)))))

;;deprecated (defun existing-instance-ofs (instances triple-list)
   (mapcar #'(lambda (inst) 
         (list 
            inst 
            (mapcan #'(lambda (trip) (and (eql (first trip) inst) (eql (second trip) '|instance-of|) (list (third trip))))
               triple-list)))
   instances))

;;older version that simply compute the simplified context and asserts it.
;;Reifies all the necessary triples in a Viewpoint to do problem-solving
;;deprecated (defun assert-viewpoint-triples (vp-instance 
				 &optional(verbose nil))
  (let ((debug nil)
	(start-time (get-internal-real-time))
	(km-triples (get-simplified-context-for-viewpoint vp-instance)))
    (ps-assert-triples km-triples debug)
    ;;Necessary for eq-solver to find relevant equation systems.
    (setq *CONTROLLER-TRIPLE-LIST*      (get-simplified-context-for-viewpoint vp-instance))
    (setq *CONTROLLER-TRIPLE-INSTANCES* (extract-all-instances-from-triple-list-ordered-by-dependency
					 (get-simplified-context-for-viewpoint vp-instance)))
    (if verbose	(format t "BPS: Asserting[~a] ~a took ~a ms. ~%" 
			(ps-km-classification-enabled-p)
			vp-instance
			(- (get-internal-real-time) start-time)))
    t))

;;deprecated (defun ps-assert-without-classify-triples(input-triple-list)
  (let ((*prototype-classification-enabled* nil)
	(*classification-enabled*           nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|)))
    (ps-assert-triples input-triple-list)))

|#



