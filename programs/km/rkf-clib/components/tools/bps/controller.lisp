;;
;; $Id: controller.lisp,v 1.218 2009/12/16 15:42:57 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun crank-up-problem-solver()
  (progn 
    (format t "Hello, world! Cranking up the Basic Problem Solver...~%")
    (setup-chem-def-table)
    (install-CLIB-concept-cache)
    (load-user-defined-concept-cache)
    (install-user-defined-concept-cache)
    (setf *EQ-FLAG*                            t)
    (setf *CONTROLLER-PERSISTENT-HASH-MUTABLE* t)
    (setf *CONTROLLER-CRANKED-UP*              t)
    (sort-user-defined-concept-list)
    (install-cpl-hacks)
    (clean-taxonomy)
    (setf *USE-LOOSESPEAK* t)
    (setf *RECURSIVE-CLASSIFICATION* nil)
    (setf *on-error* 'continue)
    ;(setf *CONTROLLER-BACKUP-KB*               (get-kb))
    (setf *allow-sibling-match*                nil)
    ;(build-full-provenance)  ;;no longer necessary.
    (format t "Installing reaction definitions table...~%")
    (extend-reaction-definitions-table)
 ))

(defun reset-controller-parameters ()
  (progn (mutate-cpl-scenario                      nil)
	 (mutate-cpl-compute-question              nil)
	 (mutate-openlist                          nil)
	 (mutate-closedlist                        nil)
	 (mutate-ignoredlist                       nil)
	 (mutate-queue_fn                          'sorted-queue-insert)
	 (mutate-heuristic                         'neutral-heuristic)
	 ;(mutate-pruning-condition                 'PRUNING-CONDITION-STUB)
	 (mutate-clock                             *MAX-CLOCK*)
	 (mutate-current-node                      nil)
	 (mutate-current-vp-inst                   nil)
	 (mutate-current-step                      1)
	 ;(setq *controller-inspection-viewpoint*   nil) ;;no longer necessary
	 (setq *query-frame-assoc-list*            nil)
	 (setq *query-slot-list*                   nil)
	 (setq *CONTROLLER-SKOLEM-TEXTGEN-LIST*          nil)
	 (setq *CONTROLLER-ANSWER-VIEWPOINT*             nil)
;	 (setq *CONTROLLER-INPUT-TRIPLES*                nil)
	 (setq *CONTROLLER-CPL-TRIPLES*                  nil)
	 (setq *CONTROLLER-NON-CPL-TRIPLES*              nil)
	 (setq *CONTROLLER-RULE-ENGINE-TRIPLES*          nil)
	 (setq *CONTROLLER-EQ-HISTORY-EXPLANATION*       nil)
	 (setq *CONTROLLER-EQ-HISTORY-VARIABLE-BINDINGS* nil)
	 (setq *pruned-vp-reason*                        nil)	 
	 (setq *CONTROLLER-UNCLONE-MAP*                  nil)
	 (setq *CONTROLLER-KM-SITUATION*             '|*Global|)
	 (setq *bad-user-defined-concepts*          nil)
	 (setq *viewpoint-defn-lst*                  nil)
	 (setq *viewpoint-km-assertion-lst*          nil)
	 (setq *viewpoint-query-lst*                 nil)
	 (setq *eq-solver-systems* nil)
	 (setq *viewpoint-score-compute-results*     nil)
	 (clrhash-viewpoint-context-data)
	 (setq *bps-answer-provenance* nil)
))

(defun handle-inspection-question(initial-viewpoint)
  (let (;(debug nil)
       )
    (generate-vp-answerable-query-filler-for-inspection-viewpoint initial-viewpoint)
    (mutate-OPENLIST           nil)
    (mutate-CURRENT-VP-INST    initial-viewpoint)
    (mutate-closedlist         (insert-at-end initial-viewpoint *CLOSEDLIST*))
    (setf *REGRESSION-REMARKS* (format nil "~a~%~a" *REGRESSION-REMARKS* "Inspection Question"))
    initial-viewpoint))

(defun generate-vp-answerable-query-filler-for-inspection-viewpoint(vp-inst)
  (let ((answerable-query-list (check-all-viewpoint-query vp-inst)))
    (fill-viewpoint-answerable-query-slot vp-inst
					  answerable-query-list)))

(defun generate-what-is-viewpoint()
  (multiple-value-bind
      (cloned-scenario cloned-question orig->clone clone->orig)
      (clone-scenario-and-question *cpl-scenario* *cpl-compute-question*)
    (let ((query-frame (car (last (car (get-query-paths cloned-scenario cloned-question))))))
      (format t "BPS: Problem was not solved. Defaulting to descriptive answer.~%")
      (setq *REGRESSION-REMARKS* 
	    (format nil "~a~%~a" 
		    *REGRESSION-REMARKS* 
		    "Problem was not solved. Returned default descriptive answer."))
      (ps-assert-triples cloned-scenario)
      (assert-viewpoint-definition
       `(|a| |Class-Definition-Viewpoint| |with|
	 (|viewpoint-scenario| (,(cons ':|set| (affix-triple-prefix cloned-scenario))))
	 (|viewpoint-target|   ((:|seq| ,query-frame)))
	 (|viewpoint-query|    ((:|seq| (:|pair| |*class-description| nil))))
)))))

;;FIXME! Performance takes a hit if we sieve through closedlist yet again at the end of problem-solving.
;;This should not be necessary!
;;Also, the funcall on a lambda function is plain stupid! Hurts readability and debugging.
(defun sieve-for-satisfiable-viewpoints ()
  (remove nil
	  (mapcar #'(lambda (x) 
		      (multiple-value-bind
			  (goal-viewpoint? answerable-query-list)
			  (test-viewpoint x)
			(if goal-viewpoint? x)))
		  *CLOSEDLIST*)))

;;Returns answer viewpoint used to generate explanation.
(defun get-answer-viewpoint()
  *CONTROLLER-ANSWER-VIEWPOINT*)

;;Returns answer viewpoint used to generate explanation.
(defun get-current-viewpoint()
  *CURRENT-VP-INST*)

;;Inserts viewpoint into answer-viewpoint-list
;;Input must always be an atom
(defun insert-answer-viewpoint(input)
  (if (and (not (null input))
	   (not (member input (get-answer-viewpoint))))
      (push input *CONTROLLER-ANSWER-VIEWPOINT*))
  *CONTROLLER-ANSWER-VIEWPOINT*
)

(defun reset-test-bench()
  (reset-problem-solver))

(defun reset-problem-solver ()
  (progn
    (reset-bps-feedback)
    (reset-controller-parameters)
    (reset-debugger-parameters)
    ;(ps-non-global-km-situation)
    ;(rlclearrules)
    ;(rldefrules physrules)
    (clrhash *balanced-eq-hash*)  ;;Need to be removed for Mobius and other non-HALO uses.
))

(defun basic-problem-solver-register-inputs(input-cpl-scenario
					    input-compute-question 
					    input-yn-questions)
  (progn 
    (setq *CONTROLLER-INPUT-COMPUTE-QUESTION* input-compute-question)
    (setq *CONTROLLER-INPUT-YN-QUESTION*      input-yn-questions)
    (setq *CONTROLLER-INPUT-SCENARIO*         input-cpl-scenario)
    (setq *CONTROLLER-NON-CPL-TRIPLES*
	  (make-triple-proper (remove-irrelevant-km-triples input-cpl-scenario)))
    (setq *CONTROLLER-CPL-TRIPLES*
	  (set-difference input-cpl-scenario 
			  (remove-irrelevant-km-triples input-cpl-scenario)))))

(defun clone-cpl-values(input-cpl-scenario
			input-compute-question
			input-yn-questions)
  (multiple-value-bind
      (cloned-scenario orig->clone clone->orig)
      (ps-clone-triple-list input-cpl-scenario)
    (values cloned-scenario
	    (replace-elements-in-list input-compute-question orig->clone)
	    (replace-elements-in-list input-yn-questions     orig->clone)
	    orig->clone
	    clone->orig)))

(defun clone-and-assert-cpl-values(input-cpl-scenario
				   input-compute-question
				   input-yn-questions)
 ;;temporarily enable KM classification
  (let* ((*prototype-classification-enabled* t)  
	 (*classification-enabled*           t)
	 (debug nil))
    (multiple-value-bind
	(cloned-scenario cloned-compute-question cloned-yn-questions orig->clone clone->orig)
	(clone-cpl-values input-cpl-scenario input-compute-question input-yn-questions)
    (ps-assert-triples cloned-scenario debug)
    (values (ps-update-instance-triples cloned-scenario nil);;important! classification may have occurred!
	    cloned-compute-question
	    cloned-yn-questions
	    orig->clone
	    clone->orig))))

(defun ps-restart()
  (format t "Restarting basic-problem-solver~%")
  (basic-problem-solver *controller-input-scenario*
			*controller-input-compute-question*
			*controller-input-yn-question*)
)

(defun valid-bps-mode-p()
  (member *bps-mode* *bps-available-mode-list*)
)

;;Main calling card for AURA
(defun basic-problem-solver (input-cpl-scenario 
			     input-compute-question 
			     input-yn-questions 
			     &optional (episodic nil))
  (with-standard-bps-parameters()
  (let* ((bps-start-time (get-internal-run-time))
	 (*record-explanations*              nil)  ;; Turn off record explanations during problem-solving.
	 (*record-explanations-for-clones*   nil)  ;; See HLO-2225 comment by pclark@cs.
	 (*prototype-classification-enabled* nil)  ;;disable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           nil)  ;;disable KM classification for non-root viewpoints.
	 (*controller-episodic*              episodic)
	 (*LOGGING*                          t))
    (if (not (valid-bps-mode-p))
	(values nil (format nil "Unknown BPS mode ~a" *bps-mode*) bps-time)
      (let ((result (catch 'BPS-REASONING-ERROR
		      (ps (rev-coeff-trips input-cpl-scenario)
			  input-compute-question
			  input-yn-questions
			  episodic)))
	    (bps-time (- (get-internal-run-time) bps-start-time)))
	(cond ((and (listp result)
		    (eq (car result) 'BPS-REASONING-ERROR))
	       (values nil (cadr result) bps-time))
	      ((member (get-root-viewpoint) *IGNOREDLIST*)
	       (values nil "Bad CPL input" bps-time))
	      (t (values (flatten (list result)) ;;hack. To make sure it always return a flat list. For AURA.
			 nil
			 bps-time))))))))

(defun rev-coeff-trips (cpl-trips)
  (mapcar #'(lambda (tr) (if (equal (second tr) '|coefficient-of-kinetic-friction-of|)
                             (list (third tr) '|coefficient-of-kinetic-friction| (first tr))
                             tr
                         )
            )
          cpl-trips
  )
)

(defun ps (input-cpl-scenario 
	   input-compute-question 
	   input-yn-questions 
	   &optional (episodic nil))
  (let ((*prototype-classification-enabled* nil)  ;;disable KM prototype classification for non-root viewpoints.
	(*classification-enabled*           nil)  ;;disable KM classification for non-root viewpoints.
 	(*controller-episodic*              episodic)
	(*LOGGING*                          t))
    (format t "BPS: Begin problem solving (~a mode)~%" *bps-mode*)
    (reset-problem-solver)
    (basic-problem-solver-register-inputs input-cpl-scenario input-compute-question input-yn-questions)
    (let ((result (ps0 input-cpl-scenario
		       input-compute-question
		       input-yn-questions
		       episodic)))
      (format t "BPS: End problem solving (~a mode)~%" *bps-mode*)
      (insert-answer-viewpoint result)
      (car (get-answer-viewpoint)))))


(defun ps0 (input-cpl-scenario
	    input-compute-question 
	    input-yn-questions 
	    &optional (episodic nil))
  (if *purely-deductive-QA*
      (ps0-PDQA input-cpl-scenario
                input-compute-question 
                input-yn-questions)
      (ps0-BPS  input-cpl-scenario
                input-compute-question 
                input-yn-questions 
                episodic)
  )
)

(defun ps0-BPS  (input-cpl-scenario
	    input-compute-question 
	    input-yn-questions 
	    &optional (episodic nil))
  (multiple-value-bind
      (fixed-cpl-scenario fixed-compute-question fixed-yn-questions)
      (fix-cpl-output input-cpl-scenario input-compute-question input-yn-questions
		      *fix-cpl-input-procedure-list*)
    (multiple-value-bind
	(km-scenario compute-question yn-questions orig->clone clone->orig)
	(clone-and-assert-cpl-values fixed-cpl-scenario
				     fixed-compute-question
				     fixed-yn-questions)      
      (let* ((question-triples (ps-get-query-triples
				(replace-elements-in-list
				 km-scenario
				 orig->clone)
				compute-question
				yn-questions))
	     (result
	      (cond ((or (null km-scenario)
			 (and (null compute-question)
			      (null yn-questions)))
		     (format t "BPS: Bad CPL input.~%")
		     nil)
		    ((is-comparison-question-p   km-scenario)   
		     (progn 
		       (format t "BPS: Comparison question type~%")
		       (car (ps-km-query (viewpoint-compare km-scenario compute-question)))))
		    ((is-analogy-question-p  km-scenario compute-question)
		     (progn 
		       (format t "BPS: Handling analogy question type~%")
		       (output-class-definition-viewpoint-for-what-is-the-question
			(search-problem-solver km-scenario 
					       question-triples
					       yn-questions))))
		    ((is-explanation-question-p  km-scenario compute-question)  
		     (progn		      
		       (format t "BPS: Handling descriptive question type~%")
		       (output-class-definition-viewpoint-for-what-is-the-question
			(search-problem-solver km-scenario 
					       question-triples
					       yn-questions))))
		    ((is-count-question-p        compute-question)   
		     (progn 
		       (format t "BPS: How many X ... type~%")
		       (output-count-viewpoint-for-lookup-viewpoint
			(search-problem-solver km-scenario 
					       question-triples
					       yn-questions))))
		    ((true-false-question-p      yn-questions)   
		       ;;Current implementation treats multiple yn-query sets as one.
		       ;;i.e., it appends the yn-query-triples into one big set and tests it.
		       ;;This needs to be improve to treat each set of yn-query triples individually.
		       ;;Not a big deal for now as this does not occur much in current CPL formulations.
		       ;;(((|_Move Through9252| |instrument| |_Oxygen2017|)       ;;set 1
		       ;;  (|_Move Through9252| |object| |_Plasma Membrane2016|)
		       ;;  (|is-it-possible-that-questionp| |query-varp| |t|))
		       ;; ((...                                                   ;;set 2
		       ;;  (|is-it-true-that-questionp| |query-varp| |t|))
		       ;;)
		     (let ((vp-inst (search-problem-solver 
				     km-scenario 
				     question-triples
				     yn-questions)))
		       (format t "BPS: Is it true... question type~%")
		       (multiple-value-bind
			   (input-provenance query-provenance answer-viewpoint)
			   (get-provenance-info-for-viewpoint vp-inst)
			 (multiple-value-bind
			     (related-to-query related-to-result-triples)
			     (get-mapping-to-related-to-yn-query query-provenance)
			   (cond ((and (not (null related-to-query))
				       (not (null related-to-result-triples)))
				  (progn 
				    (format t "BPS: Is X related to Y question type~%")
				    (output-slot-query-viewpoint
				     (get-simplified-context-for-viewpoint vp-inst)
				     related-to-result-triples 
				     related-to-query 
				     vp-inst)  ;;includes provenance
				    )
				  )
				 (t
				  (output-yn-viewpoint-for-lookup-viewpoint vp-inst));; includes provenance
				 )))))
		    (t
		     (progn
		       (format t "BPS: What is the R of X question type~%")
		       (search-problem-solver km-scenario
					      question-triples
					      yn-questions)))
		    )))
	result))))

;;Old API. For backward compatibility.
(defun simple-problem-solver (km-scenario question extra-knowledge)
  (basic-problem-solver km-scenario question extra-knowledge))

(defun store-bps-episode (viewpoint-instance test-output)
  (format t "~% Storing episode - viewpoint: ~a outcome: ~a" viewpoint-instance test-output))

(defun continue-problem-solver()
  (let* ((bps-start-time (get-internal-run-time))
	 (result (catch 'BPS-REASONING-ERROR
		   (continue-problem-solver0)))
	 (bps-time (- (get-internal-run-time) bps-start-time)))
    (cond ((and (listp result)
		(eq (car result) 'BPS-REASONING-ERROR))
	   (values nil nil (cadr result) bps-time))
	  ((member (get-root-viewpoint) *IGNOREDLIST*)
	   (values nil nil "Bad CPL input" bps-time))
	  (t 
	   (values (car result)
		   (cadr result)
		   nil
		   bps-time)))))

;;Refills the clock value and continue problem-solving with existing values in data-structure.
;;Still need to recognize when same viewpoint but a different expanded query is returning a different answer.
(defun continue-problem-solver0()
  (with-standard-bps-parameters()
  (let* ((*record-explanations*              nil) ;; Turn off record explanations during problem-solving.
	 (*record-explanations-for-clones*   nil) ;; See HLO-2225 comment by pclark@cs.
	 (*prototype-classification-enabled* nil) ;; disable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           nil) ;; disable KM classification for non-root viewpoints.
	 (*LOGGING*                          t)
	 (prev-search-answer-viewpoints (get-answer-viewpoints-by-searching))
	 (prev-answer (first (get-answer-viewpoint)))
	 (result nil))
    (format t "Begin (continue-problem-solver)~%")
    (mutate-clock *MAX-CLOCK*)
    (solve)
    (if (not (equal (get-answer-viewpoints-by-searching)
		    prev-search-answer-viewpoints))
	(let ((new-sl-vp-inst (first (get-answer-viewpoints-by-searching))))
	  (format t "A different answer was found.~%")
	  (cond ((isa prev-answer '|Class-Definition-Viewpoint|)
		 (setq result (output-class-definition-viewpoint-for-what-is-the-question new-sl-vp-inst)))
		((isa prev-answer '|Yes-No-Viewpoint|)
		 (setq result (output-yn-viewpoint-for-lookup-viewpoint new-sl-vp-inst)))
		((isa prev-answer '|Count-Viewpoint|)
		 (setq result (output-count-viewpoint-for-lookup-viewpoint new-sl-vp-inst)))
		(t (setq result new-sl-vp-inst)))
	  (insert-answer-viewpoint result))
      (progn
	  (format t "A different was not found. Returning previous answer.~%")
	  (setq result prev-answer)))
    (format t "End (continue-problem-solver)~%")
    (list 
     (flatten (list result)) ;;hack. To make sure it always return a flat list. For AURA.
     (not (equal (get-answer-viewpoints-by-searching)
		 prev-search-answer-viewpoints))
))))

(defun get-answer-viewpoints-by-searching()
  (let ((*on-error* 'continue))
    (remove-if-not #'(lambda(x)
		       (or (isa x '|Slot-Value-Viewpoint|)
			   (isa x '|Multislot-Value-Viewpoint|)))
		   (get-answer-viewpoint))))


(defun update-question-triple-list(question-triple-list clone-mapping)
  (replace-elements-in-list question-triple-list clone-mapping))

(defun generate-triples-for-question-triple-list(question-triple-list
						 &optional (verbose nil))
  (mappend #'(lambda(question-triple)
	       (let ((slot  (triple-relation question-triple))
		     (frame (triple-head     question-triple)))
		 (let ((filler (ps-km-query `(|the| ,slot |of| ,frame) verbose)))
		   (mapcar #'(lambda (filler-entry)
			       (list frame slot filler-entry))
			   filler))))
	   question-triple-list))


(defun get-cpl-nl-triples(scenario)
  (set-difference scenario (remove-irrelevant-km-triples scenario)))


(defun root-viewpoint-useful?()
  (and (not (null (get-root-viewpoint)))
       (or (inspection-scenario? 
	    (get-simplified-context-for-viewpoint 
	     (get-root-viewpoint)))
	   (test-viewpoint (get-root-viewpoint)))))

(defun search-problem-solver (input-km-scenario 
			      question-triple-list
			      yn-question)
  (multiple-value-bind
      (km-scenario)
      (fix-cpl-output input-km-scenario nil yn-question
		      '());;for now, let's use nil for compute-question and yn-query
  (let* ((debug nil)
	 (initial-viewpoint-lst 
	  (get-initial-viewpoint-lst
	   (remove-irrelevant-km-triples km-scenario)
	   question-triple-list
	   (get-cpl-nl-triples km-scenario))))
    (insert-answer-viewpoint (handle-inspection-question (car initial-viewpoint-lst)))
    (if (not (root-viewpoint-useful?))
	(progn
	  (if debug (format t "Okay. BPS believes initial answer is not useful. Will attempt problem-solving. The initial answer will be returned if it fails to find a different one.~%"))
	  (set-BPSMaxStage 3)
	  (setup-search (make-nodes-from-candidate-vps initial-viewpoint-lst))
	  (insert-answer-viewpoint (solve)))
      (progn
	(if debug (format t "Okay. BPS believes initial answer is useful. Returning that.~%"))
	(test-viewpoint (get-root-viewpoint))))
    (let ((result (first (get-answer-viewpoint))))
      (if (not (asserted-viewpoint? result))
	  (progn
	    (if debug (format t "search-problem-solver: ~a triples were now asserted... asserting them now.~%" 
		    vp-inst))
	    (assert-viewpoint-triples result) ;; assert viewpoint, if it wasn't asserted before.
	    )
	)      
      result))))


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; PDQA implementation

(defparameter *purely-deductive-QA* nil)

;;; Allow these to be switched off
(defvar *use-newqf* t)  ; use New QF when in PDQA
(defvar *use-pdqa* t)   ; use Ken's stuff when in PDQA
(defvar *pdqa* nil)     ; in PDQA mode

(defun pdqa ()
  (eval (read-from-string "
    (defun km::ps0 (input-cpl-scenario
                    input-compute-question 
                    input-yn-questions 
                    &optional (episodic nil))
            (km::ps0-PDQA input-cpl-scenario
                          input-compute-question 
                          input-yn-questions)
    )"
  ))
  (setq *pdqa* t)
  (format t "Switched to PDQA mode!~%")
  t
)

(defun bps ()
  (eval (read-from-string "
    (defun km::ps0 (input-cpl-scenario
                    input-compute-question 
                    input-yn-questions 
                    &optional (episodic nil))
            (km::ps0-BPS input-cpl-scenario
                         input-compute-question 
                         input-yn-questions 
                         episodic)
    )"
  ))
  (setq *pdqa* nil)
  (format t "Switched to normal (BPS) mode!~%")
  t
)

(defvar *cpl-text* "")

; [1] For some reason, re-asking the question resets *current-cpl-input* to "", so use this var to protect against that.
(defun ps0-PDQA (cpl-scenario compute-vars yn-questions)
  (let* ((filtered-cpl-triples (remove-irrelevant-km-triples cpl-scenario))
         (query-type (get-query-type-from-cpl cpl-scenario compute-vars yn-questions))
         (classif-enabled? *classification-enabled*)
         (proto-classif-enabled? *prototype-classification-enabled*)
         result-stru
        )
    (setq *classification-enabled* t)
    (setq *prototype-classification-enabled* t)

    (cond ((string/= *current-cpl-input* "") (setq *cpl-text* *current-cpl-input*)))  ; [1]

    (setq result-stru
      (case query-type
        (is-it-true-that (answer-is-it-true-slot-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (is-it-true-isa-that (answer-is-it-true-isa-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (is-it-false-that (answer-is-it-false-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (is-it-possible-that (answer-is-it-possible-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (why (answer-why-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (how (answer-how-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (find-a-value (answer-slot-lookup-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (what-is-the  (answer-slot-lookup-q-PDQA filtered-cpl-triples compute-vars yn-questions :what-is-the t))
        (what-is-a (answer-what-is-a-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (how-many (answer-how-many-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (how-much (answer-much-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (what-types (answer-what-types-q-PDQA filtered-cpl-triples compute-vars yn-questions))
        (what-relationship (answer-slot-query-q-PDQA filtered-cpl-triples compute-vars yn-questions))
      )
    )

    (setq *classification-enabled* classif-enabled?)
    (setq *prototype-classification-enabled* proto-classif-enabled?)
    result-stru
  )
)

; [1] { is-it-true-that | is-it-false-that | is-it-possible-that | why | how }
; [2] { find-a-value | what-is-a | what-is-the | how-many | how-much | what-types }
; [1,2] Maybe multiple questions, just pick the first
; [4] is-it-true-isa-that isn't a CPL query-type, so introduce it here
; [5] what-relationship isn't a CPL query-type, so introduce it here
(defun get-query-type-from-cpl (cpl-scenario compute-vars yn-questions)
  (let ((query-type (or (some #'yn-question-type yn-questions) ; [1]
                        (some #'find-a-value-question-type compute-vars)) ; [2]
        )
       )
    (cond ((and (eq query-type 'is-it-true-that)                ; [4]
                (some #'(lambda (triple)
                          (eq (second triple) '|is-a|))
                      (first yn-questions))) ; just look at the first YN question, for now
           'is-it-true-isa-that
          )
          ((and (eq query-type 'find-a-value)                   ; [5]
                (isa-in-triples (car compute-vars) '|Be-Related| cpl-scenario)
           )
           'what-relationship
          )
          (t query-type)
    )
  )
)



(defun isa-in-triples (inst class triples)
  (let ((inst-trip (find-inst-trip inst triples)))
    (subsumes (list class) (list (third inst-trip)))
  )
)

(defun find-inst-trip (inst triples)
  (if (car triples)
      (if (and (equal (first (car triples)) inst)
               (equal (second (car triples)) '|instance-of|)
          )
          (car triples)
          (find-inst-trip inst (cdr triples))
      )
  )
)


(defun answer-slot-lookup-q-PDQA (cpl-triples compute-vars yn-questions &key what-is-the)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
         (cpl-frame-exprs (convert-triples-to-frame-exprs cpl-triples))
         (question-triple-list (ps-get-query-triples cpl-triples compute-vars yn-questions))
         (query-trip (car question-triple-list))
         (query-fr   (first query-trip))
         (query-sl   (second query-trip))
         (query-va   (car compute-vars))
         (query-sit  (curr-situation))
         (pdqa-sit   (car (new-situation)))
       )
    (in-situation pdqa-sit)
    (assert-km-frame-exprs cpl-frame-exprs pdqa-sit)
    (force-classification-from-triples-list cpl-triples pdqa-sit)
    (let* ((query-filt (get-class-of-inst-from-triples query-va cpl-triples))
           (query-res  (cond ((and *use-pdqa* (not what-is-the))
                              (ps-km-query `(|in-situation| ,pdqa-sit (|the| ,query-filt ,query-sl |of| ,query-fr))))))
           (res-classes (remove-duplicates (my-mapcan #'immediate-classes query-res)))
          )

      (multiple-value-bind
          (answers question-str answer-strs explanation-str)
          (cond (*use-newqf* (ask0 *cpl-text* :answer-values-to-ignore res-classes))) ; ignore res-classes
        (declare (ignore question-str explanation-str))

;       (km-format t "query-res = ~a~%" query-res)
        (let* ((deductive-answers (cond ((neq answers 'abort) (first answers))))
               (abductive-answers (cond ((neq answers 'abort) (second answers))))
               (deductive-answer-str (cond ((neq answers 'abort) (first answer-strs))))
               (abductive-answer-str (cond ((neq answers 'abort) (second answer-strs))))

               (res-disp   (list2stringnl (mapcar #'(lambda (res)
                                                      (showme res (all-situations) (all-theories) nil t))
                                                  query-res)
               ))

               (km-ans-disp (list2commastring (mapcar #'(lambda (qr)
                                                          (get-text-string-for-instance qr pdqa-sit)) query-res)))

               (ans-disp
                (with-output-to-string (s)
                  (cond ((string/= km-ans-disp "")
                         (format s "<b>")
                         (format s km-ans-disp)
                         (format s "</b>")))
                    (cond (deductive-answers
                           (cond ((string/= km-ans-disp "")(format s "<p>And I also found:<br>"))
                                 (t (format s "I found:<br>")))
                           (format s "<b>")
                           (format s (list2commastring deductive-answers))
                           (format s "</b>")))
                    (cond (abductive-answers
                           (cond ((or (string/= km-ans-disp "") deductive-answers)
                                  (format s "<p>And also sometimes (abductive reasoning):<br>"))
                                 (t (format s "Sometimes (abductive reasoning):<br>")))
                           (format s "<b>")
                           (format s (list2commastring abductive-answers))
                           (format s "</b>")))
                    (cond ((and (null *use-pdqa*) (null *use-newqf*))
                           (format s "(No answer - PDQA and NEWQF are both off!)~%")
                           (format s "Do (setq km::*use-pdqa* t) to switch PDQA on.~%")
                           (format s "Do (setq km::*use-newqf* t) to switch NEWQF on.~%"))
                          ((and (string= km-ans-disp "") (not deductive-answers) (not abductive-answers))
                           (format s "(No answer found)")))))

               (detail-str (with-output-to-string (s)
                           (cond ((and *use-pdqa* (not what-is-the))
                                  (format s "<h4>Assertions (in ~a)</h4>~%<pre>" pdqa-sit)
                                  (mapcar #'(lambda (expr) (format s "~a~%" expr))
                                          cpl-frame-exprs)
                                  (format s "</pre><h4>Query</h4>~%<pre>" pdqa-sit)
                                  (format s "~%(the ~a ~a of ~a)~%" query-filt query-sl query-fr)
                                  (format s "==> ~a~%~%<hr>" query-res)
                                  (format s "</pre><h4>KB</h4>~%<pre>")
                                  (format s "~a~%" res-disp)
                                  (format s "<hr></pre>")))
                           (cond (deductive-answer-str
                                  (format s "<h4>Additional Search of the KB</h4>~%")
                                  (format s (format-for-html deductive-answer-str))))
                           (cond (abductive-answer-str
                                  (format s "<h4>Abductive Search</h4>~%")
                                  (format s (format-for-html abductive-answer-str))))
               ))
              )

          (in-situation query-sit)
          (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,ans-disp))))
          (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,detail-str))))
        )
    ))
    viewpoint-stru
  )
)

(defun format-for-html (string)
  (replace-string " " "&nbsp;" (replace-string *newline-str* "<br>" string)))


#| Here's how to do slot-query (what is relationship) question:

1. The compute-question arg is a single instance of Be-Related 
   (guaranteed by get-query-type-from-cpl
2. source and target instances are the objects of the Be-Related inst
3. setup/call loosespeak as follows:
    (setq *use-loosespeak*    t)
    (ls-clear-cache)
    (setq *ls-cpl-mode*       t)
    (setq *ls-cpl-choices*    nil)
    (setq *ls-auto-selection* nil)
    (setq *ls-exhaustive-search* t)
    (setq *ls-stop-criteria* 'is_subclass)
    (setq *ls-search-depth-bound* 2)
    (ls-parse-triples (list (list source '|related-to| target)))
4. output of ls-parse-triples is a list
   ((
     (:|nn| source target)
     (<km frame expression>)
     (<km frame expression>)
     ...
   ))
|#

(defun answer-slot-query-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
         (cpl-frame-exprs (convert-triples-to-frame-exprs cpl-triples))
         (related-inst (car compute-question))
         (query-sit  (curr-situation))
         (pdqa-sit   (car (new-situation)))
        )
    (in-situation pdqa-sit)
    (assert-km-frame-exprs cpl-frame-exprs pdqa-sit)
    (let* ((obj-insts (ps-km-query `(|in-situation| ,pdqa-sit (|the| |object| |of| ,related-inst))))
           (source-inst (first obj-insts))
           (source-class (car (immediate-classes source-inst)))
           (source-prototype (or (car (ps-km-query `(|the| |prototypes| |of| ,source-class)))
                                 source-class
           ))
           (target-inst (second obj-insts))
           (target-class (car (immediate-classes target-inst)))
           (target-prototype (or (car (ps-km-query `(|the| |prototypes| |of| ,target-class)))
                                 target-class
           ))
          )
      (setq *use-loosespeak*    t)
      (ls-clear-cache)
      (setq *ls-cpl-mode*       t)
      (setq *ls-cpl-choices*    nil)
      (setq *ls-auto-selection* nil)
      (setq *ls-exhaustive-search* t)
      (setq *ls-stop-criteria* 'is_subclass)
      (setq *ls-search-depth-bound* 4)
      (let* ((answer-exprs (cdar (ls-parse-triples (list (list source-inst '|related-to| target-inst)))))
             (ans-disp (km-expr-path-to-string (car answer-exprs)))
             (source-disp (showme source-prototype (all-situations) (all-theories) nil t))
             (target-disp (showme target-prototype (all-situations) (all-theories) nil t))
             (detail-str (with-output-to-string (s)
                            (format s "</pre><h4>KB</h4>~%<pre>")
                            (format s "~a~%" source-disp)
                            (format s "~a~%" target-disp)
                            (format s "<hr></pre>")
             ))
            )
        (in-situation query-sit)
        (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,ans-disp))))
        (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,detail-str))))
        viewpoint-stru
      )
    )
  )
)

(defun km-expr-path-to-string (expr)
  (let* ((simple-path (remove '|a| (remove '|has| (remove '|with| (flatten expr)))))
         (anon-path (mapcar #'km-name simple-path))
        )
    (string-downcase (list2string anon-path))
  )
)

(defun answer-is-it-true-isa-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
         (cpl-frame-exprs (convert-triples-to-frame-exprs cpl-triples))
         (query-inst (first (caar yn-questions)))
         (query-class-inst (third (caar yn-questions)))
         (query-sit  (curr-situation))
         (pdqa-sit   (car (new-situation)))
         q-inst-class
        )
    (in-situation pdqa-sit)
    (assert-km-frame-exprs cpl-frame-exprs pdqa-sit)
    (setq q-inst-class (get-class-of-inst-from-triples query-inst cpl-triples))
    (force-classification-from-triples-list cpl-triples pdqa-sit)
    (let* ((query-classes (immediate-classes query-class-inst))
           (spec-answer   (if (> (length query-classes) 1)
                             (ps-km-query `(|oneof| |?c| |in| ,query-classes
                                            |where| (,query-inst |isa| |?c|)))
                             (ps-km-query `(,query-inst |isa| ,query-classes))
                          )
           )
           (skol-inst     (create-instance q-inst-class nil))
           (gen-answer    (if (> (length query-classes) 1)
                             (ps-km-query `(|oneof| |?c| |in| ,query-classes
                                            |where| (,skol-inst |isa| |?c|)))
                             (ps-km-query `(,skol-inst |isa| ,query-classes))
                          )
           )
           (ans-disp   (if gen-answer 
                           (make-sentence `("Yes,"
                                            ,(ps-km-query `(|the| |text-indef-head| |of| ,query-inst))
                                            "is"
                                            ,(ps-km-query `(|the| |text-indef-head| |of| ,query-class-inst))
                                           )
                           )
                           (if spec-answer
                               (make-sentence `("Yes, this"
                                                ,q-inst-class
                                                "is"
                                                ,(ps-km-query `(|the| |text-indef-head| |of| ,query-class-inst))
                                               )
                               )
                               (make-sentence `("No,"
                                                ,(ps-km-query `(|the| |text-indef-head| |of| ,query-inst))
                                                "is"
                                                ,(ps-km-query `(|the| |text-indef-head| |of| 
                                                                  (|an| |instance| |of| 
                                                                     (|the| |first| |of| 
                                                                       (|the| |superclasses| |of| ,q-inst-class)))))
                                               )
                               )
                           )
                       )
           )
           (test-inst-str (if gen-answer
                              (format nil "(a ~a)" q-inst-class)
                              (format nil "~a" query-inst)
                          )
           )
           (detail-str (with-output-to-string (s)
                         (format s "<h4>Assertions (in ~a)</h4>~%<pre>" pdqa-sit)
                         (mapcar #'(lambda (expr) (format s "~a~%" expr))
                                 cpl-frame-exprs
                         )
                         (format s "</pre><h4>Query</h4>~%<pre>" pdqa-sit)
                         (if (> (length query-classes) 1)
                             (format s "~%(oneof ?c in ~a where (~a isa ?c))~%" query-classes test-inst-str)
                             (format s "~%(~a isa ~a)~%" test-inst-str query-classes)
                         )
                         (format s "==> ~a~%~%<hr>" spec-answer)
                         (format s "</pre><h4>KB</h4>~%<pre>")
                         (format s "<b>(taxonomy ~a superclasses)</b>~%" q-inst-class)
                         (write-lines (make-tax q-inst-class '|superclasses| nil) s)
                         (format s "~%=====~%") 
                         (mapcar #'(lambda (class) 
                                     (format s "<b>(taxonomy ~a)</b>~%" class)
                                     (write-lines (make-tax class '|subclasses| nil) s)
                                     (format s "~%=====~%") 
                                   )
                                 query-classes
                         )
                         (format s "<hr></pre>")
                       )
           )
          )
      (in-situation query-sit)
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,ans-disp))))
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,detail-str))))
    )
    viewpoint-stru
  )
)

(defun answer-is-it-true-slot-q-PDQA (cpl-triples compute-vars yn-questions)
  (declare (ignore cpl-triples compute-vars yn-questions))
  (multiple-value-bind
      (answer question-str answer-str explanation-str)
      (cond (*use-newqf* (ask0 *cpl-text*)))
    (declare (ignore question-str answer))
    (let ((answer-disp (cond
                        ((not *use-newqf*)
                         "(No answer as newqf is switched off. Do (setq km::*use-newqf* t) to switch it on)")
                        (t (format-for-html answer-str))))
          (explanation-disp (cond (explanation-str (format-for-html explanation-str))
                                  (t "(No detail)")))
          (viewpoint-stru (create-instance '|Viewpoint| nil)))

      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,answer-disp))))
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,explanation-disp))))
      viewpoint-stru
)))

(defun answer-is-it-false-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-is-it-possible-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-why-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-how-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-what-is-a-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-what-is-the-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-how-many-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
         (cpl-frame-exprs (convert-triples-to-frame-exprs cpl-triples))
         (question-triple-list (ps-get-query-triples cpl-triples compute-question yn-questions))
         (query-trip (car question-triple-list))
         (query-fr   (first query-trip))
         (query-sl   (second query-trip))
         (query-va   (second (car compute-question)))
         (query-sit  (curr-situation))
         (pdqa-sit   (car (new-situation)))
       )
    (in-situation pdqa-sit)
    (assert-km-frame-exprs cpl-frame-exprs pdqa-sit)
    (force-classification-from-triples-list cpl-triples pdqa-sit)
    (let* ((query-filt (get-class-of-inst-from-triples query-va cpl-triples))
           (constraint-list (collect-constraints-on-instance query-fr query-sl))
           (filtered-constraints (filter-constraints-by-class constraint-list query-filt))
           (ans-disp   (with-output-to-string (s)
                         (dolist (constr filtered-constraints)
                           (format s "~a ~a ~a<br> " (first constr) (second constr) (third constr))
                         )
                       )
           )
           (detail-str (with-output-to-string (s)
                         (format s "<h4>Assertions (in ~a)</h4>~%<pre>" pdqa-sit)
                         (mapcar #'(lambda (expr) (format s "~a~%" expr))
                                 cpl-frame-exprs
                         )
                         (format s "</pre><h4>Query</h4>~%<pre>" pdqa-sit)
                         (format s "~%(constraints-for (the ~a of ~a))~%" query-sl query-fr)
                         (format s "==> ~a~%~%<hr>" constraint-list)
                         (format s "</pre>")
                       )
           )
          )
      (in-situation query-sit)
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,ans-disp))))
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,detail-str))))
    )
    viewpoint-stru
  )
)

(defun answer-how-much-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
        )
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| ("question type not supported by PDQA"))))
    (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (""))))
    viewpoint-stru
  )
)

(defun answer-what-types-q-PDQA (cpl-triples compute-question yn-questions)
  (let* ((viewpoint-stru (create-instance '|Viewpoint| nil))
         (cpl-frame-exprs (convert-triples-to-frame-exprs cpl-triples))
         (query-fr   (second (car compute-question)))
         (query-sit  (curr-situation))
         (pdqa-sit   (car (new-situation)))
       )
    (in-situation pdqa-sit)
    (assert-km-frame-exprs cpl-frame-exprs pdqa-sit)
    (force-classification-from-triples-list cpl-triples pdqa-sit)
    (let* ((frame-class (car (immediate-classes query-fr)))
           (ans-disp   (list2commastring (immediate-subclasses frame-class)))
           (detail-str (with-output-to-string (s)
                         (format s "<h4>Assertions (in ~a)</h4>~%<pre>" pdqa-sit)
                         (mapcar #'(lambda (expr) (format s "~a~%" expr))
                                 cpl-frame-exprs
                         )
                         (format s "</pre><h4>Query</h4>~%<pre>" pdqa-sit)
                         (format s "~%(the subclasses of ~a)~%" frame-class)
                         (format s "==> ~a~%~%<hr>" (immediate-subclasses frame-class))
                         (format s "</pre><h4>KB</h4>~%<pre>")
                         (format s "<b>(taxonomy ~a)</b>~%" frame-class)
                         (write-lines (make-tax frame-class '|subclasses| nil) s)
                         (format s "<hr></pre>")
                       )
           )
          )
      (in-situation query-sit)
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-answer| (,ans-disp))))
      (ps-km-query `(,viewpoint-stru |has| (|viewpoint-detail| (,detail-str))))
    )
    viewpoint-stru
  )
)


(defun convert-triples-to-frame-exprs (trips)
  (if trips
    (cons (convert-triple-to-frame-expr (car trips))
          (convert-triples-to-frame-exprs (cdr trips))
    )
  )
)

(defun convert-triple-to-frame-expr (trip)
  (if trip
      (let ((fr (first trip))
            (sl (second trip))
            (va (third trip))
           )
        `(,fr |has| (,sl (,va)))
      )
  )
)

(defun assert-km-frame-exprs (frame-exprs situation)
  (if frame-exprs
      (dolist (expr frame-exprs)
        (ps-km-query `(|in-situation| ,situation ,expr))
      )
  )
  (if frame-exprs
      (dolist (expr frame-exprs)
        (let* ((fr  (first expr))
               (sl  (first (third expr)))
               (va  (car (second (third expr))))
               (sli (invert-slot sl))
              )
          (if (not (equal sl '|instance-of|))
              (progn
                (ps-km-query `(|in-situation| ,situation (|the| ,sl |of| ,fr)))
                (ps-km-query `(|in-situation| ,situation (|the| ,sli |of| ,va)))
                (if (isa fr '|Property-Value|)
                    (ps-km-query `(|in-situation| ,situation (|the| |value| |of| ,fr)))
                )
                (if (isa va '|Property-Value|)
                    (ps-km-query `(|in-situation| ,situation (|the| |value| |of| ,va)))
                )
              )
          )
        )
      )
  )
  t
)

(defun force-classification-from-triples-list (triples-list sit)
  (let ((hold-classif-enab *classification-enabled*)
        (hold-pclassif-enab *prototype-classification-enabled*)
       )
    (enable-classification)
    (dolist (triple triples-list)
      (let ((fr (first triple))
            (sl (second triple))
           )
        (ps-km-query `(|in-situation| ,sit (|the| ,sl |of| ,fr)))
      )
    )
    (setq *classification-enabled* hold-classif-enab)
    (setq *prototype-classification-enabled* hold-pclassif-enab)
    t
  )
)


(defun get-text-string-for-instance (inst situation)
  (make-phrase (ps-km-query `(|in-situation| ,situation (|the| |text-head| |of| ,inst))))
)

(defun get-class-of-inst-from-triples (inst triples)
  (if triples
      (if (and (equal (first (car triples)) inst)
               (equal (second (car triples)) '|instance-of|)
          )
          (third (car triples))
          (get-class-of-inst-from-triples inst (cdr triples))
      )
  )
)

(defun filter-constraints-by-class (constraint-list filter-class)
  (if constraint-list
      (if (and (is-numeric-set-constraintp (car constraint-list))
               (subsumes (list filter-class) (list (third (car constraint-list))))
          )
          (cons (car constraint-list) 
                (filter-constraints-by-class (cdr constraint-list) filter-class)
          )
          (filter-constraints-by-class (cdr constraint-list) filter-class)
      )
  )
)

(defun is-numeric-set-constraintp (constraint)
  (and (listp constraint)
       (equal (length constraint) 3)
       (or (equal (car constraint) '|exactly|)
           (equal (car constraint) '|at-least|)
           (equal (car constraint) '|at-most|)
       )
  )
)


(defun unclone-instances-in-exprs (expr)
  (if expr
      (if (listp expr)
          (cons (unclone-instances-in-exprs (car expr))
                (unclone-instances-in-exprs (cdr expr))
          )
          (completely-unclone expr)
      )
      expr
  )
)

(defun completely-unclone (inst)
  (let ((uncloned-inst (ps-unclone inst)))
    (if (equal uncloned-inst inst)
        inst
        (completely-unclone uncloned-inst)
    )
  )
)

(defun list2stringnl (in-list)
  (let ((s (make-string-output-stream)))
       (mapcar #'(lambda (elem) (format s "~a~%" elem)) in-list)
       (get-output-stream-string s)
  )
)

(defun list2commastring (list)
  (if list
      (if (listp list)
          (with-output-to-string (s0)
              (format s0 "~a" (car list))
              (if (cdr list)
                  (dolist (elem (cdr list))
                      (format s0 ", ~a" elem)
                  )
              )
          )
          ""
      )
      ""
  )
)

#|
(defun dist2thing (class)
  (- (length (flatten (make-tax class '|superclasses|))) 1)
)
|#

(defun dist2thing (obj)
  (cond ((null obj) 0)
        ((listp obj) (listdist2thing obj))
        ((is-an-instance obj) (listdist2thing (immediate-classes obj)))
        ((equal obj '|Thing|) 0)
        ((classp obj) (+ 1 (listdist2thing (km0 `(|the| |superclasses| |of| ,obj)))))
        (t 0)
  )
)

(defun listdist2thing (classlist)
  (apply #'min (mapcar #'dist2thing classlist))
)

;-old stuff----------------------------------------------------------

#|
(defun ps0-PDQA (cpl-scenario compute-question yn-questions)
  (let* ((filtered-cpl-triples (remove-irrelevant-km-triples cpl-scenario))
         (query-type (get-query-type-from-cpl cpl-scenario compute-question yn-questions))
         (classif-enabled? *classification-enabled*)
         (proto-classif-enabled? *prototype-classification-enabled*)
         result-stru
        )
    (setq *classification-enabled* t)
    (setq *prototype-classification-enabled* t)
    (setq result-stru
      (case query-type
        (*is-it-true-isa-question* (answer-is-it-true-isa-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*is-it-true-slot-question* (answer-is-it-true-slot-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*is-it-false-question* (answer-is-it-false-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*is-it-possible-question* (answer-is-it-possible-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*why-question* (answer-why-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*how-question* (answer-how-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*slot-lookup-question* (answer-slot-lookup-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*slot-query-question* (answer-slot-query-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*what-is-a-question* (answer-what-is-a-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*what-is-the-question* (answer-what-is-the-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*how-many-question* (answer-how-many-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*how-much-question* (answer-how-much-q-PDQA filtered-cpl-triples compute-question yn-questions))
        (*what-types-question* (answer-what-types-q-PDQA filtered-cpl-triples compute-question yn-questions))
      )
    )
    (setq *classification-enabled* classif-enabled?)
    (setq *prototype-classification-enabled* proto-classif-enabled?)
    result-stru
  )
)

(defun get-query-type-from-cpl (cpl-scenario compute-question yn-questions)
  (if yn-questions
      (case (caadar yn-questions)
        ('|is-it-true-that-questionp| 
          (if (equal (second (caar yn-questions)) '|is-a|)
              '*is-it-true-isa-question*
              '*is-it-true-slot-question*
          )
        )
        ('|is-it-false-that-questionp| '*is-it-false-question*)
        ('|is-it-possible-that-questionp| '*is-it-possible-question*)
        ('|why-questionp| '*why-question*)
        ('|how-questionp| '*how-question*)
      )
      (if (is-an-instance (car compute-question))
          (if (isa-in-triples (car compute-question) '|Be-Related| cpl-scenario)
              '*slot-query-question*
              '*slot-lookup-question*
          )
          (if (pairp (car compute-question))
              (case (caar compute-question)
                ('WHAT-IS-A '*what-is-a-question*)
                ('WHAT-IS-THE '*what-is-the-question*)
                ('HOW-MANY '*how-many-question*)
                ('HOW-MUCH '*how-much-question*)
                ('WHAT-TYPES '*what-types-question*)
              )
              '*unknown-question-type*
          )
      )
  )
)
|#

