;;
;; $Id: dead-code.lisp,v 1.6 2009/08/29 19:42:40 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;unused.
;; (defun generalize-instance-of-triples(triple-list)
;;   (append 
;;    (extract-non-instance-triples triple-list)
;;    (mapcar #'(lambda(x)
;; 		(list x '|instance-of| '|Thing|))
;; 	   (remove-duplicates (extract-all-km-instances triple-list)
;; 			      :test 'equal))))

;;Given scenario, model-graph, and their correspondence
;;returns triple-list resulting from the merging of scenario and model-graph.
;;merged model-graph instances are renamed with their scenario graph counterparts.
;;This is necessary as vp-query-entries continue to refer to scenario instances.
;;Older version.
;; (defun get-simplified-context(input-scenario input-model correspondence-assoc-map)
;;   (let* ((scenario (get-simplified-km-triples input-scenario))
;; 	 (model    (get-simplified-km-triples input-model))
;; 	 (aggregated-triples (remove-duplicates 
;; 			      (replace-elements-in-list 
;; 			       (append scenario model)
;; 			       (invert-map correspondence-assoc-map)) ;;necessary as vp-query-entries refer to scenario instances.
;; 			      :test #'ps-triple-equal))
;; 	 (content-triples    (extract-non-instance-triples aggregated-triples))
;; 	 (instance-triples   (extract-instance-triples aggregated-triples)))
;;     (deaggregate-triple-list
;;      (append content-triples
;; 	     (mapcar #'(lambda(triple)
;; 			 (simplify-instance-triple triple))
;; 		     (aggregate-similar-relations instance-triples))))))

;;29Oct08
;; (defun ps0 (input-cpl-scenario 
;; 	    input-compute-question 
;; 	    input-yn-questions 
;; 	    &optional (episodic nil))
;;   (multiple-value-bind
;;       (fixed-cpl-scenario fixed-compute-question fixed-yn-questions)
;;       (fix-cpl-output input-cpl-scenario input-compute-question input-yn-questions
;; 		      '(install-ae-triples-for-skolem-instances-in-yn-query))
;;     (multiple-value-bind
;; 	(km-scenario compute-question yn-questions orig->clone clone->orig)
;; 	(clone-and-assert-cpl-values fixed-cpl-scenario
;; 				     fixed-compute-question
;; 				     fixed-yn-questions)      
;;       (let ((result
;; 	     (cond ((or (null km-scenario)
;; 			(and (null compute-question)
;; 			     (null yn-questions)))
;; 		    (format t "BPS: Bad CPL input.~%")
;; 		    nil)
;; 		   ((is-comparison-question-p   km-scenario)   
;; 		    (progn 
;; 		      (format t "BPS: Comparison question type~%")
;; 		      (car (ps-km-query (viewpoint-compare  km-scenario compute-question)))))
;; 		   ((is-analogy-question-p  km-scenario compute-question)  
;; 		    (progn 
;; 		      (format t "BPS: Handling analogy question type~%")
;; 		      (viewpoint-analogy    km-scenario compute-question)))
;; 		   ((is-explanation-question-p  km-scenario compute-question)  
;; 		    (progn 
;; 		      (format t "BPS: Handling descriptive question type~%")
;; 		       (viewpoint-explain   km-scenario compute-question)))
;; 		   ((is-count-question-p        compute-question)   
;; 		    (progn 
;; 		      (format t "BPS: How many X ... type~%")
;; 		      (car (ps-km-query (viewpoint-count km-scenario compute-question)))))
;; 		   ((true-false-question-p      yn-questions)   
;; 		    (progn
;; 		      (format t "BPS: Is it true... question type~%")
;; 		      (output-yn-viewpoint-for-lookup-viewpoint
;; 		       (search-problem-solver km-scenario 
;; 					      (ps-get-query-triples
;; 					       km-scenario
;; 					       compute-question
;; 					       yn-questions)
;; 					      yn-questions))))
;; 		   ((related-to-question-p      km-scenario)   
;; 		    (progn
;; 		      (format t "BPS: Is X related to Y question type~%")
;; 		      (car (ps-km-query (viewpoint-related-to-question km-scenario compute-question)))))
;; 		   (t
;; 		    (progn
;; 		      (format t "BPS: What is the R of X question type~%")
;; 		      #|
;; 		      (multiple-value-bind
;; 		      (additional-compute-questions additional-scenario-triples)
;; 		      (identify-subgoals-in-cpl km-scenario compute-question yn-questions)
;; 		      (list (search-problem-solver km-scenario 
;; 		      (ps-get-query-triples km-scenario compute-question))))
;; 		      |#
;; 		      (search-problem-solver km-scenario 
;; 					     (ps-get-query-triples
;; 					      km-scenario
;; 					      compute-question
;; 					      yn-questions)
;; 					     yn-questions)))
;; 		   )))
;; 	result))))


;;Earlier version 12Oct08
;;No longer necessary to set viewpoint as inspection-viewpoint. 
;; deprecated (defun handle-inspection-question(initial-viewpoint)
;;   (let ((result (list initial-viewpoint)))
;;     (setf *controller-inspection-viewpoint* t)
;;     (format t "BPS: Inspection viewpoint, setting clock to 0.~%")
;;     (generate-vp-answerable-query-filler-for-inspection-viewpoint initial-viewpoint)
;;     (mutate-clock              0)
;;     (mutate-OPENLIST           nil)
;;     (mutate-CURRENT-VP-INST    initial-viewpoint)
;;     (mutate-CLOSEDLIST         result)
;;     (if (and *CONTROLLER-ENABLE-DEFAULT-ANSWER*
;; 	     (null (get-viewpoint-answerable-query initial-viewpoint)))
;;         (mutate-CLOSEDLIST         (cons (generate-what-is-viewpoint) result)))
;;     (setf *REGRESSION-REMARKS* (format nil "~a~%~a" *REGRESSION-REMARKS* "Inspection Question"))
;;     (car *CLOSEDLIST*)))

;;29Oct08
;; (defun elaborate-scenario-for-question-triple-list (scenario 
;; 						    question-triple-list
;; 						    yn-question
;; 						    &optional(verbose nil))
;;   (let* ((vp-instance         (search-problem-solver scenario question-triple-list yn-question))
;; 	 (elaborated-scenario (get-simplified-context-for-viewpoint vp-instance))
;; 	 (clone-mapping       (get-clone-mappings-for-elaborated-scenario scenario elaborated-scenario)))
;;     (values
;;      (remove-duplicates
;;      (append elaborated-scenario
;; 	     (generate-triples-for-question-triple-list 
;; 	      (update-question-triple-list question-triple-list 
;; 					   clone-mapping)
;; 	      verbose))
;;      :test 'ps-triple-equal)
;;      clone-mapping
;;      vp-instance)))

;;03Nov08. Does not work for "what is a property-value?" questions.
;; ;;Basically remove queries if it fails (invalid-query-p)
;; (defun remove-invalid-query(km-scenario question-triple-list)
;;   (remove-if #'(lambda(question-triple) (invalid-query-p  km-scenario question-triple))
;; 	     question-triple-list)
;;   )

;;03Nov08. Does not work for "what is a property-value?" questions.
;; ;;Invalid queries are
;; ;; a) (X instance-of *) when x isa Property-Value
;; (defun invalid-query-p (km-scenario question-triple)
;;   (and (equal (triple-relation question-triple)
;; 	      '|instance-of|)
;;        (quasi-property-value-instance-p (triple-head question-triple)
;; 					km-scenario)))

;;This version removes the query triples. ;
;;We no longer remove the query triples because ;
;;   a) To get force questions working. We need a skolem query to trigger eq-solver ;
;;   b) In response to HLO-1932. Which wanted the query triple back. ;
;;   c) Never had a good principled argument on why we should remove the query triple. ;
;;deprecated (defun search-problem-solver (km-scenario 
;; question-triple-list)  
;; (let* ((valid-question-triple-list (REMOVE-INVALID-QUERY km-scenario 
;; question-triple-list))
;; (initial-viewpoint-lst (get-initial-viewpoint-lst
;; (get-scenario-without-query-triples
;; (remove-irrelevant-km-triples km-scenario)
;; valid-question-triple-list)
;; valid-question-triple-list
;; (get-cpl-nl-triples km-scenario))))
;; (cond (*controller-inspection-viewpoint*
;; (insert-answer-viewpoint (handle-inspection-question (car initial-viewpoint-lst))))
;; (t (progn
;; (set-BPSMaxStage 3)
;; (setup-search (make-nodes-from-candidate-vps initial-viewpoint-lst))
;; (insert-answer-viewpoint (solve)))))
;; (first (get-answer-viewpoint))))

;;;deprecated (defun generate-vp-answerable-query-filler-for-inspection-viewpoint(vp-inst)
;; (let* ((filler 
;; (remove nil
;; (mapcar #'(lambda(vp-query-entry)
;; (let* ((query-path (extract-query-path vp-query-entry)))
;; (let ((txt-desc-for-km-frame 
;; (ps-get-text-description-for-query-path query-path)))
;; (if (and (not (null txt-desc-for-km-frame))
;; (not (string= (string-upcase txt-desc-for-km-frame)
;; "NIL")))
;; vp-query-entry))))
;; (get-viewpoint-query vp-inst)))))
;; (if (not (null filler))
;; (fill-viewpoint-answerable-query-slot vp-inst filler))))

;;old version dated 12Oct08
;;New version will always store an inspection-viewpoint
;;And use heuristic to determine if inspection-viewpoint should be returned.
;;or skipped to find a different answer.
;; (defun search-problem-solver (km-scenario 
;; 			      question-triple-list)  
;;   (let* ((valid-question-triple-list (REMOVE-INVALID-QUERY km-scenario 
;; 							   question-triple-list))
;; 	 (initial-viewpoint-lst (get-initial-viewpoint-lst
;; 				 (remove-irrelevant-km-triples km-scenario)
;; 				 valid-question-triple-list
;; 				 (get-cpl-nl-triples km-scenario))))
;;     (cond (*controller-inspection-viewpoint*
;; 	   (insert-answer-viewpoint (handle-inspection-question (car initial-viewpoint-lst))))
;; 	  (t (progn
;; 	       (set-BPSMaxStage 3)
;; 	       (setup-search (make-nodes-from-candidate-vps initial-viewpoint-lst))
;; 	       (insert-answer-viewpoint (solve)))))
;;     (first (get-answer-viewpoint))))

;;03Nov08. Does not work for "what is a property-value?" questions.
;; (defun search-problem-solver (input-km-scenario 
;; 			      question-triple-list
;; 			      yn-question)
;;   (multiple-value-bind
;;       (km-scenario)
;;       (fix-cpl-output input-km-scenario nil yn-question
;; 		      '());;for now, let's use nil for compute-question and yn-query
;;   (let* ((debug nil)
;; 	 (valid-question-triple-list 
;; 	  (REMOVE-INVALID-QUERY km-scenario 
;; 				question-triple-list))
;; 	 (initial-viewpoint-lst 
;; 	  (get-initial-viewpoint-lst
;; 	   (remove-irrelevant-km-triples km-scenario)
;; 	   valid-question-triple-list
;; 	   (get-cpl-nl-triples km-scenario))))
;;     (insert-answer-viewpoint (handle-inspection-question (car initial-viewpoint-lst)))
;;     (if (not (root-viewpoint-useful?))
;; 	(progn
;; 	  (if debug (format t "Okay. BPS believes initial answer is not useful. Will attempt problem-solving. The initial answer will be returned if it fails to find a different one.~%"))
;; 	  (set-BPSMaxStage 3)
;; 	  (setup-search (make-nodes-from-candidate-vps initial-viewpoint-lst))
;; 	  (insert-answer-viewpoint (solve)))
;;       (progn
;; 	(if debug (format t "Okay. BPS believes initial answer is useful. Returning that.~%"))
;; 	(test-viewpoint (get-root-viewpoint))))
;;     (first (get-answer-viewpoint)))))

;;29Oct08
;; deprecated (defun extract-what-is-the-instance(cpl-question)
;;   (mapcar 'cadr
;; 	  (remove-if-not #'(lambda(x) (equal (car x) 'what-is-the))
;; 			 cpl-question)))

;;29Oct08
;; deprecated (defun viewpoint-analogy(cpl-scenario cpl-question)
;;   (let* ((instance-lst (car (extract-what-is-the-instance cpl-question)))
;; 	 (vp-defn (explain-specialized-instance cpl-scenario instance-lst))
;;     	 (vp-inst (assert-viewpoint-definition vp-defn)))
;;     (assert-viewpoint-triples vp-inst)
;;     vp-inst))

;;29Oct08
;; deprecated (defun explain-specialized-instance(cpl-scenario instance-lst)
;;   (let ((*controller-max-depth* 1)
;; 	(*allow-sibling-match*  nil))
;;     (multiple-value-bind
;; 	(elaborated-scenario cloned-instance-list elaborated-vp-instance)
;; 	(elaborate-scenario-for-question-triple-list 
;; 	 cpl-scenario
;; 	 (mapcar #'(lambda(instance)
;; 		     (list instance '|instance-of| '*))
;; 		 instance-lst)
;; 	 nil)
;;       (let ((class-defn-viewpoint-target (cdr 
;; 					  (assoc (car instance-lst) 
;; 						 cloned-instance-list))))
;; 	(if (and (not (null elaborated-scenario))          ;; Some scenario returned
;; 		 (> (length (set-difference *closedlist*   ;; due to elaboration
;; 					    *ignoredlist*)) 1))  
;; 	    `(|a| |Class-Definition-Viewpoint| |with|
;; 	      (|viewpoint-scenario|  (,(cons :|set| (affix-triple-prefix 
;; 						     elaborated-scenario))))
;; 	      (|viewpoint-parent|	 ((,elaborated-vp-instance)))
;; 	      (|viewpoint-target|	 (,class-defn-viewpoint-target))
;; 	      (|viewpoint-filter|    (,(car (get-viewpoint-target elaborated-vp-instance))))
;; 	      (|viewpoint-query|	 ((:|seq| (:|pair| |*class-description| nil)))))
;; 	  (progn
;; 	    (format t "BPS: Nothing similar was found. Describing ~a as a skolem.~% " instance-lst)
;; 	    (explain-skolem-instance cpl-scenario (car instance))))))))

;; (defun ps-elaborate(km-scenario
;; 		    query)
;;   (let ((*logging* t))
;;     (reset-problem-solver)
;;     (delete-frames-in-triple-lst km-scenario)
;;     (ps-assert-triples km-scenario t)
;;     (let ((filler (ps-km-query query)))
;;       (search-problem-solver km-scenario 
;; 			     (ps-get-query-triples km-scenario filler))
;;       (multiple-value-bind
;; 	  (full-triple-lst context-triples answer-triples)
;; 	  (get-full-triple-list-for-viewpoint (car (get-answer-viewpoint)))
;; 	(multiple-value-bind
;; 	    (skolem-assertions non-skolem-assertions)
;; 	    (ps-get-frames-for-triples full-triple-lst)
;; 	  (values full-triple-lst (append skolem-assertions non-skolem-assertions) answer-triples)
;; 	  )))))

;; (defun testcase()
;;   (multiple-value-bind
;;       (full-triple-lst km-frames answer-triples)
;;       (ps-elaborate
;;        '((|_Train1238| |instance-of| |Train|)
;; 	 (|_Move7071| |instance-of| |Move|)
;; 	 (|_Move7071| |object| |_Train1238|)
;; 	 (|_Average Speed1240| |instance-of| |Speed-Value|)
;; 	 (|_Average Speed1240| |average-speed-of| |_Move7071|)
;; 	 (|_Average Speed1240| |value|
;; 	  (:|pair| 100 |*kilometer-per-hour|))
;; 	 (|_Distance1243| |instance-of| |Length-Value|)
;; 	 (|_Distance1243| |distance-of| |_Move7071|)
;; 	 (|_Distance1243| |value| (:|pair| 150 |*kilometer|))
;; 	 (|_Initial Time1246| |instance-of| |Duration-Value|)
;; 	 (|_Initial Time1246| |initial-time-of| |_Move7071|)
;; 	 (|_Initial Time1246| |value| (:|pair| 8 |*hour|))
;; 	 (|_Final Time1250| |instance-of| |Duration-Value|)
;; 	 (|_Final Time1250| |final-time-of| |_Move7071|))
;;        '(|the| |final-time| |of| |_Move7071|))
;;     (format t "KM frames:~%")
;;     (dolist (frame km-frames)
;;       (format t "~S~%" frame))
;;     (format t "Answer triples:~%")
;;     (dolist (triple answer-triples)
;;       (format t "~S~%" triple))))

;; ;;Cannot do a (remove-subsumer) because CPL may return too specific a class, e.g., Move -> Locomotion
;; ;;different problem when Throw and Two-Dimensional-Move are cousin concepts.
;; (defun get-alternative-word2concept-mappings(cpl-triples)
;;   (let ((alternative-candidate-lst ()))
;;     (dolist (entry (sieve-triple-list-having-relation '|input-word| cpl-triples))
;;       (let* ((instance (triple-head entry))
;; 	     (word     (cadr (triple-tail entry)))
;; 	     (pos      (caddr (triple-tail entry)))
;; 	     (candidate-lst (remove-if-not 'concept-p 
;; 					   (WORD2CONCEPTS word 
;; 							  :POS pos 
;; 							  :CLIMB-TAXONOMY NIL))))
;; 	(if (> (length candidate-lst) 1)
;; 	    (progn 
;; 	      (push (list instance 
;; 			  (union candidate-lst (cadr (assoc instance alternative-candidate-lst))))
;; 		    alternative-candidate-lst)
;; 	      (format t "~a, ~s(~A) => ~A~%" instance word pos candidate-lst))
;; 	  )))
;;     (mappend #'(lambda(x)
;; 		(get-candidate-w2c-substitutions x alternative-candidate-lst))
;; 	    (powerset (remove-duplicates (mapcar 'car alternative-candidate-lst))))))

;; (defun get-candidate-w2c-substitutions(target-lst alternative-candidate-lst)
;;   (mapcar 'flatten-to-triple-lst
;; 	  (cross-product
;; 	   (mapcar #'(lambda(y)
;; 		       (mapcar #'(lambda(alternative)
;; 				   (list y '|instance-of| alternative))
;; 			       (cadr (assoc y alternative-candidate-lst))))
;; 		   target-lst))))

;; (defun replace-instance-of-triples(target-lst triple-lst)
;;   (cond ((null target-lst) triple-lst)
;; 	(t (let* ((target (car target-lst))
;; 		  (frame  (triple-head target)))
;; 	     (replace-instance-of-triples 
;; 	      (cdr target-lst)
;; 	      (cons target
;; 		    (extract-triples-not-having-root-slot frame '|instance-of| triple-lst)))))))

;; (defun get-alternative-w2c-mappings-for-cpl-values(scenario compute-questions yes-no-questions)
;;   (mapcar #'(lambda(candidate-scenario)
;; 	      (multiple-value-bind 
;; 		  (alt-scenario alt-compute-questions alt-yes-no-questions orig->clone clone->orig)
;; 		  (clone-cpl-values candidate-scenario compute-questions yes-no-questions)
;; 		(list alt-scenario alt-compute-questions alt-yes-no-questions)))
;; 	  (remove-if-not 'triple-lst-violate-domain/range? 
;; 			 (mapcar #'(lambda(repl)
;; 				     (replace-instance-of-triples repl
;; 								  scenario))
;; 				 (get-alternative-word2concept-mappings scenario)))))

;; (defun get-alternative-w2c-cpl-values(formulation)
;;   (multiple-value-bind
;;       (scenario compute-questions yes-no-questions)  
;;       (ps-parse-question formulation t)
;;     (GET-ALTERNATIVE-W2C-MAPPINGS-FOR-CPL-VALUES scenario compute-questions yes-no-questions)))

;; ;;a) need code to generate variety of triple-lst give alternatives [done] [done]
;; ;;b) need code to check for domain/range violate given triple-lst or typed-triple-lst. [done]

;; (defun testcase1()
;;   (get-alternative-w2c-cpl-values
;;    "There is a throw.
;;   There is a second throw."))

;; (progn
;;   (global-situation)
;;   (km '(|Throw| |now-has| (|superclasses| (|Unobstruct|))))
;;   (km '(|initial-x-speed| |now-has| (|domain| (|Two-Dimensional-Move|))))
;;   (km '(|initial-x-speed-of| |now-has| (|range| (|Two-Dimensional-Move|))))
;;   (km '(|initial-y-speed| |now-has| (|domain| (|Two-Dimensional-Move|))))
;;   (km '(|initial-y-speed-of| |now-has| (|range| (|Two-Dimensional-Move|))))
;;   (km '(|initial-y-position| |now-has| (|domain| (|Two-Dimensional-Move|))))
;;   (km '(|initial-y-position-of| |now-has| (|range| (|Two-Dimensional-Move|))))
;;   (new-situation)
;;   )
 
;; ;;Example #1
;; (defun testcase2()
;;   (get-alternative-w2c-cpl-values
;; "A ball is thrown.
;; The mass of the ball is 100 g.
;; The initial x speed of the throw is 0 m/s.
;; The initial y speed of the throw is 20 m/s.
;; The initial y position of the throw is 1 m.
;; What is the initial speed of the throw?"))

;; ;;Example #2
;; (defun testcase3()
;;   (get-alternative-w2c-cpl-values
;;    "A child moves.
;; The initial speed of the child is 0 m/s.
;; The acceleration magnitude of the child is 2.5 m/s^2.
;; The duration of the move is 5 s.
;; What is the final speed of the child?"))

;; ;;Include alternatives in problem-solving tree. mutate openlist and not include matching for root-viewpoint. Basically don't do matching if a viewpoint has a children

;;A stub. Does not compute anything, simply assume all
;;viewpoints as unsatisfied.
;; (defun pruning-condition-stub (vp-inst)
;;   nil)

#|
;;deprecated code

;;deprecated (defun generate-all-necessary-query-paths (scenario instance-list &optional(query-path-list nil))
  (let* ((*EQ-FLAG* nil)
	 (new-query-path-list (sieve-query-path-list-not-having-instance 
			       (ps-extract-question scenario instance-list)
			       instance-list))
	 (new-instance-list   (extract-instance-for-query-path-list new-query-path-list)))
    (if (subsetp new-instance-list instance-list)
 	query-path-list
        (generate-all-necessary-query-paths scenario 
					  (remove-duplicates (append new-instance-list instance-list))
					  (append new-query-path-list query-path-list)))))

;;deprecated (defun get-all-query-paths(vp-inst)
  (mapcar #'(lambda(vp-query-entry)
	      (extract-query-path vp-query-entry))
	  (get-viewpoint-query vp-inst)))

;;deprecated (defun touch-all-dependent-query-paths-for-viewpoint(vp-inst question-instance)
  (let ((*EQ-FLAG* NIL))
    (if (not (member vp-inst *CONTROLLER-TOUCHED-LIST*))
	(let ((scenario (get-simplified-context-for-viewpoint vp-inst)))
	  (touch-all-necessary-query-paths scenario question-instance))
        (if *DEBUGGER-SHOW-TOUCH-ALL-QUERY-PATHS*
	    (format t "BPS: ~a was touched before.~%" vp-inst)))))

;;deprecated (defun touch-all-necessary-query-paths(scenario &optional (verbose nil))
  (let ((query-path-lst (remove-duplicates 
			 (ps-extract-question 
			  scenario 
			  (extract-all-instances-from-triple-list scenario)) :test 'equal)))
    (dolist (query-path query-path-lst)
      (ps-km-query query-path verbose))))

;;deprecated (defun touch-all-necessary-query-paths-for-instance(scenario instance)
  (let* ((*EQ-FLAG* NIL)
	 (necessary-query-paths (generate-all-necessary-query-paths scenario (list instance)))
	 (new-instance-lst      (extract-instance-for-query-path-list necessary-query-paths)))
    (mapcar #'(lambda(query-path)
		(progn 
		  (if *DEBUGGER-SHOW-TOUCH-ALL-QUERY-PATHS*
		      (format t "BPS: Querying ~a~%" query-path))
		  (km query-path)))
	    necessary-query-paths)))          

To group vp-queries that refer to the same KM frame.
;;deprecated (defun group-queries-by-km-frame(vp-query-list)
  (let ((*EQ-FLAG* nil))
    (mapcar #'(lambda(vp-query-entry)
		(let ((query-path (extract-query-path vp-query-entry)))
		  (list vp-query-entry (ps-km-query query-path))))
	    vp-query-list)
))
|#


;; 15Oct08
;; Gold-standard pruning condition based on text-gen
;; deprecated (defun pruning-condition(skolem-textgen-list
;; 			 skolem-slot-filler-list
;; 			 &optional(verbose nil))
;;   #'(lambda(vp-inst)   
;;       (let ((answerable-query-list (check-all-viewpoint-query vp-inst 
;; 							      skolem-textgen-list)))
;; 	(fill-viewpoint-answerable-query-slot VP-INST
;; 					      answerable-query-list
;; 					      nil)
;; 	(values (goal-viewpoint-p vp-inst answerable-query-list)
;; 		answerable-query-list))))



;;26Oct08. No longer necessary.
;; (defun ps-consistent-property-vector-valuep(query-path &optional(verbose nil))
;;   (let* ((*EQ-FLAG* t))
;;     (multiple-value-bind 
;; 	(magnitude small-v-magnitude)
;; 	(get-magnitude-for-property-vector-value query-path)
;;       (or (not (null small-v-magnitude)) ;; available magnitude value
;; 	  (progn                         ;; not available, can we solve for it?
;; 	    (solve-for-property-vector-value query-path)
;; 	    (multiple-value-bind 
;; 		(magnitude small-v-magnitude)
;; 		(get-magnitude-for-property-vector-value query-path)
;; 	      (not (null small-v-magnitude))))))))

;;Commeted out on 15Oct08 (safety version)
;; deprecated (defun check-viewpoint-query-for-generic-fillers(vp-inst 
;; 						 vp-query-entry 
;; 						 query-path
;; 						 skolem-textgen
;; 						 &optional(debug nil))
;;   (if debug (format t "BPS: [check-viewpoint-query-for-generic-fillers] ~a~%" (stringify vp-query-entry)))
;;   (let* ((returned-text-desc (ps-get-text-description-for-query-path query-path debug)))
;;   (push (list vp-inst
;; 	      (format nil "(string= (~A => ~a) ~a)"
;; 		      `(ps-get-text-description-for-query-path ,query-path)
;; 		      returned-text-desc
;; 		      skolem-textgen))
;; 	*viewpoint-query-lst*)
;;     (if (or (and (null skolem-textgen)
;; 		 (not (null returned-text-desc)))
;; 	    (and (not (null returned-text-desc))
;; 		 (not (string= returned-text-desc
;; 			       skolem-textgen))))
;; 	vp-query-entry)))

;;27Oct08. No longer necessary to install compute-questions for yn-triples.
;; (defun ps-get-query-triples-for-is-it-true-question (scenario yn-query)
;;   (let ((yn-triples (mapcar 'car 
;; 			    (remove-if-not 
;; 			     'true-false-yn-query-entry-p
;; 			     yn-query))))
;;     (append (ps-get-query-triples-for-compute-question ;;ideally, only for property-values and property-vector-values
;; 	     scenario
;; 	     (extract-all-km-instances yn-triples))
;; 	    yn-triples)))

;; (defun testme1()
;;   (let ((lhs (ps-instantiate-property-value 2 '|*meter|))
;; 	(rhs (ps-instantiate-property-value 4 '|*meter|)))
;;   (VERIFY-SINGLE-TRIPLE-FOR-PROPERTY-COMPARISON-RELATION
;;    `(,lhs |greater-than| ,rhs))
;; ))

;; (defun testme2()
;;   (let ((lhs (ps-instantiate-property-value 4 '|*meter|))
;; 	(rhs (ps-instantiate-property-value 2 '|*meter|)))
;;   (VERIFY-SINGLE-TRIPLE-FOR-PROPERTY-COMPARISON-RELATION
;;    `(,lhs |greater-than| ,rhs))
;; ))

;; (defun testme3()
;;   (let ((lhs (ps-instantiate-concept '|Length-Value|))
;; 	(rhs (ps-instantiate-property-value 2 '|*meter|)))
;;   (VERIFY-SINGLE-TRIPLE-FOR-PROPERTY-COMPARISON-RELATION
;;    `(,lhs |greater-than| ,rhs))
;; ))

;; (defun testme4()
;;   (let ((lhs (ps-instantiate-property-value 2 '|*meter|))
;; 	(rhs (ps-instantiate-concept '|Length-Value|)))
;;   (VERIFY-SINGLE-TRIPLE-FOR-PROPERTY-COMPARISON-RELATION
;;    `(,lhs |greater-than| ,rhs))
;; ))

;; (defun testme5()
;;   (let ((lhs (ps-instantiate-concept '|Length-Value|))
;; 	(rhs (ps-instantiate-concept '|Length-Value|)))
;;   (VERIFY-SINGLE-TRIPLE-FOR-PROPERTY-COMPARISON-RELATION
;;    `(,lhs |greater-than| ,rhs))
;; ))


;;testcases
;; (defun testcase1()
;;   (ps-attempt-question "Is it true that 2 meter equals 200 centimeters?"))

;; (defun testcase2()
;;   (ps-attempt-question "Is it true that 2 meter equals 210 square feet?"))

;; (defun testcase3()
;;   (ps-attempt-question "Is it true that a man is a person?"))

;; (defun testcase4()
;;   (ps-attempt-question "Is it true that a robot is a person?"))

;; (defun testcase5()
;;   (ps-attempt-question "Is it true that the sum of 10 m and 100 centimeters is 1100 cm?"))

;; (defun testcase6()
;;   (ps-attempt-question "Is it true that the sum of 10 m and 100 centimeters is 0.2 km?"))
