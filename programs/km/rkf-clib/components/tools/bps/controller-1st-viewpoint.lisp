;;
;; $Id: controller-1st-viewpoint.lisp,v 1.74 2009/06/11 20:36:09 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-query-frame-in-vp-query-entry(vp-query-entry)
  (nth 1 (car (last vp-query-entry))))

(defun get-query-slot-in-vp-query-entry(vp-query-entry)
  (nth 2 (car (last vp-query-entry))))

;;Removes all query instances and associated query triple, if the query instance supplied by CPL is a skolem.
;;But continue to leave it in if it is a specialized instance. Maybe useful for  implementing sieve functionality.
(defun get-scenario-without-query-triples(scenario query-triple-list)
  (cond ((null query-triple-list) scenario)
	(t (let* ((query-triple          (car query-triple-list))
		  (root                  (triple-head query-triple))
		  (reln                  (triple-relation query-triple))
		  (scenario-filler       (quasi-ps-slot-lookup root reln scenario))
		  (specialized-instances (get-all-specialized-instances scenario)))
	     (get-scenario-without-query-triples 
	      (cond ((equal reln '|instance-of|) scenario)
		    ((and (not (null scenario-filler))
			  (null (intersection scenario-filler specialized-instances))
			  (= (get-in-edge-count-for-root-slot root reln scenario) 1))
		     (sieve-triple-list-not-having-instance-list scenario-filler
								 scenario))
		    (t scenario))
	      (cdr query-triple-list))))))

(defun get-alt-viewpoint-lst(scenario question-triple-list cpl-nl-triples parent-vp-inst)
  (mapcar #'(lambda(alt-scenario)
	      (multiple-value-bind
		  (cloned-alt-scenario cloned-alt-question-triple-list orig->clone clone->orig)
		  (clone-scenario-and-question-triple-list alt-scenario question-triple-list)
		(ps-generate-viewpoint 
		 `((|viewpoint-source| 
		    ,(get-all-scenario-instances cloned-alt-scenario))
		   (|viewpoint-scenario|
		    ,(affix-triple-prefix cloned-alt-scenario))
		   (|viewpoint-query|
		    ,(cons ':|seq|
			   (make-viewpoint-query-slot-for-question-triple-list
			    cloned-alt-question-triple-list)))
		   (|viewpoint-parent|
		    ,parent-vp-inst)))))
	  (get-alternative-w2c-mappings-for-initial-viewpoint scenario
							      question-triple-list
							      cpl-nl-triples)))

(defun get-initial-viewpoint-lst(input-scenario 
				 input-question-triple-list
				 &optional
				 (cpl-nl-triples nil))
  (progn
    (set-BPSCurStage    1)
    (set-BPSMaxStage    3)
    (set-BPSCurPosition 1)
    (set-BPSMaxPosition 2) 
    (multiple-value-bind
	(cloned-scenario cloned-question-triple-list orig->clone clone->orig)
	(clone-scenario-and-question-triple-list input-scenario input-question-triple-list)
      (mutate-cpl-scenario         cloned-scenario)
      (mutate-cpl-compute-question cloned-question-triple-list)
      (set-BPSCurPosition 2)
      (let* ((cloned-cpl-nl-triples (replace-elements-in-list 
				     cpl-nl-triples 
				     orig->clone))
	     (final-viewpoint-query (make-viewpoint-query-slot-for-question-triple-list cloned-question-triple-list))
	     (root-vp-defn          (create-1st-viewpoint-definition cloned-scenario final-viewpoint-query))
	     (root-vp-inst          (assert-viewpoint-definition root-vp-defn))
	     (alt-vp-inst-lst       (get-alt-viewpoint-lst cloned-scenario 
							   cloned-question-triple-list 
							   cloned-cpl-nl-triples
							   root-vp-inst)))
	;;;(assert-viewpoint-triples root-vp-inst) ;; necessary??
	;(determine-pruning-condition root-vp-inst) ;;no longer necessary 15Oct08
	(cons root-vp-inst (firstn alt-vp-inst-lst 10))
	))))

;; 12Oct08. We no longer use *controller-inspection-viewpoint* parameter.
;; deprecated (defun get-initial-viewpoint-lst(input-scenario input-question-triple-list &optional(cpl-nl-triples nil))
;;   (progn
;;     (set-BPSCurStage    1)
;;     (set-BPSMaxStage    3)
;;     (set-BPSCurPosition 1)
;;     (set-BPSMaxPosition 2) 
;;   (multiple-value-bind
;;       (cloned-scenario cloned-question-triple-list orig->clone clone->orig)
;;       (clone-scenario-and-question-triple-list input-scenario input-question-triple-list)
;;       (mutate-cpl-scenario         cloned-scenario)
;;       (mutate-cpl-compute-question cloned-question-triple-list)
;;       (set-BPSCurPosition 2)
;;     (let* ((cloned-cpl-nl-triples (replace-elements-in-list 
;; 				   cpl-nl-triples 
;; 				   orig->clone))
;; 	   (final-viewpoint-query (make-viewpoint-query-slot-for-question-triple-list cloned-question-triple-list))
;; 	   (root-vp-defn          (create-1st-viewpoint-definition cloned-scenario final-viewpoint-query))
;; 	   (root-vp-inst          (assert-viewpoint-definition root-vp-defn))
;; 	   (alt-vp-inst-lst       (get-alt-viewpoint-lst cloned-scenario 
;; 							 cloned-question-triple-list 
;; 							 cloned-cpl-nl-triples
;; 							 root-vp-inst)))
;;       (setf *CONTROLLER-INSPECTION-VIEWPOINT* (or (equal *MAX-CLOCK* 1)
;; 						  (not (search-elaboration-p cloned-scenario 
;; 									     cloned-question-triple-list))))
;;       (assert-viewpoint-triples root-vp-inst)
;;       (determine-pruning-condition root-vp-inst)
;;       (cons root-vp-inst (firstn alt-vp-inst-lst 10))
;; ))))


(defun clone-scenario-and-question-triple-list (initial-scenario initial-question-triple-list)
  (multiple-value-bind
      (cloned-triple-list orig->clone clone->orig)
      (ps-clone-triple-list initial-scenario)
    (values cloned-triple-list
	    (replace-elements-in-list initial-question-triple-list orig->clone)
	    orig->clone
	    clone->orig)))

(defun remove-triples-related-to-question(scenario question)
  (sieve-triple-list-not-having-instance (car question) scenario))

(defun assert-multislot-query-type (vp-inst multislot-query-type)
  (ps-km-query `(,vp-inst |has| (|viewpoint-multislot-type| (,multislot-query-type)))))

(defun create-1st-viewpoint-definition(scenario viewpoint-query &optional(filter nil))
  (generate-viewpoint (get-all-scenario-instances scenario)
		      (affix-triple-prefix (remove-irrelevant-km-triples scenario))
		      viewpoint-query
		      filter
		      (quasi-get-concept-for-kb-instance 
		       (extract-all-root-instances-from-triple-list scenario)
		       scenario)
		      ))

;;Returns the query frame and query slot for viewpoint instance.
(defun get-query-frame-slot-pair(vp-inst)
  (let ((target (flatten (get-viewpoint-query vp-inst))))
    (list (nth 3 target)
	  (nth 4 target))))

(defun assert-viewpoint-definition(vp-defn)
  (first (ps-km-query vp-defn)))

;;Code to apply rule engine. This is part of creating the initial scenario.
(defun apply-rule-engine-on-initial-scenario(initial-scenario)
  (if *CONTROLLER-ENABLE-RULE-ENGINE*
      (let ((rl-engine-triples 	(apply-rule-engine-on-initial-scenario0 initial-scenario)))
	(setq *CONTROLLER-RULE-ENGINE-TRIPLES*   rl-engine-triples)
	rl-engine-triples)))

;;Code to apply rule engine. This is part of creating the initial scenario.
(defun apply-rule-engine-on-initial-scenario0(initial-scenario)
  (let ((rule-engine-triples     ())
	(all-scenario-instances  ()))
    (rlclearrules)
    (rldefrules physrules)   
    (setf all-scenario-instances (get-all-scenario-instances initial-scenario))
;    (ps-assert-triples initial-scenario)
	(let* (
	 (target-triples        initial-scenario)
	 (km-assertion-lst      (triples-to-km-assertions target-triples))
	 (skolem-assertions     (get-skolem-assertions km-assertion-lst))
	 (non-skolem-assertions (order-km-assertions (extract-all-km-instances-in-list skolem-assertions)
						     (get-non-skolem-assertions km-assertion-lst))))
    (if skolem-assertions (ps-assert-skolem-assertions skolem-assertions t))
    (dolist (km-assertion non-skolem-assertions)
      (let ((start-time (get-internal-real-time)))
	(if t (format t "BPS: Asserting ~a " km-assertion))
	(ps-km-query km-assertion) 
	(if t (format t " ... done, (~a ms)~%" (- (get-internal-real-time) 
					    start-time))))))
    (setf rule-engine-triples 
	  (rlrunallrules (remove-duplicates all-scenario-instances)))
;
    (let* ((rule-engine-mesg  (generate-rule-engine-message rule-engine-triples)))
      (if *regression-include-rule-engine-output*
	  (setq *REGRESSION-REMARKS* (format nil "~a~%~a" *REGRESSION-REMARKS* rule-engine-mesg)))
      (format t "~a~%~%" rule-engine-mesg))
    rule-engine-triples))

(defun generate-rule-engine-message(rule-engine-triples)
  (let ((stream            (make-string-output-stream)))
    (if rule-engine-triples
	(progn 
	  (format stream "---------------------------------------------------------~%")
	  (format stream "The following rules were applied:~%")
	  (format stream "~a~%" (reverse *rlrulesfired*))
	  (format stream "---------------------------------------------------------~%")
	  (format stream "---------------------------------------------------------~%")
	  (format stream "The following triples were contributed by the rule-engine~%")
	  (format stream "---------------------------------------------------------~%")
	  (dolist (triple rule-engine-triples)
	    (format stream "~s~%" triple))
	  (if *rlremovetriples*
		(progn
		  (format stream "---------------------------------------------------------~%")
		  (format stream "The following triples were removed~%")
		  (format stream "---------------------------------------------------------~%")
		  (dolist (triple *rlremovetriples*)
			(format stream "~s~%" triple)))))
        (progn
	  (format stream "---------------------------------------------------------~%")
	  (format stream "No triples were contributed by the rule-engine~%")
	  (format stream "---------------------------------------------------------~%")))
    (get-output-stream-string stream)))

;;Clone initial scenario, and question, etc. 
;;So we do not need to *reboot* the context.
(defun clone-scenario-and-question (initial-scenario initial-question)
  (multiple-value-bind
      (cloned-triple-list orig->clone clone->orig)
      (ps-clone-triple-list initial-scenario)
    (let ((cloned-question (mapcar #'(lambda(x)
				       (find-clone x clone->orig))
				   initial-question)))
      (values cloned-triple-list
	      cloned-question
	      orig->clone
	      clone->orig))))

;; (defun testcase()
;;   (GET-INITIAL-VIEWPOINT-LST
;;    '((|_Child2359_c0| |instance-of| |Person|)
;;      (|_Initial Speed2361_c0| |instance-of| |Speed-Value|)
;;      (|_Acceleration Magnitude2364_c0| |instance-of|
;;       |Acceleration-Magnitude-Value|)
;;      (|_Duration2367_c0| |instance-of| |Duration-Value|)
;;      (|_Move10530_c0| |instance-of| |Locomotion|)
;;      (|_Move10530_c0| |agent| |_Child2359_c0|)
;;      (|_Initial Speed2361_c0| |initial-speed-of| |_Move10530_c0|)
;;      (|_Initial Speed2361_c0| |value|
;;       (:|pair| 0 |*meter-per-second|))
;;      (|_Acceleration Magnitude2364_c0| |acceleration-magnitude-of|
;;       |_Move10530_c0|)
;;      (|_Acceleration Magnitude2364_c0| |value|
;;       (:|pair| 2.5 |*meter-per-second-squared|))
;;      (|_Duration2367_c0| |duration-of| |_Move10530_c0|)
;;      (|_Duration2367_c0| |value| (:|pair| 5 |*second|)))
;;    '((|_Move10530_c0| |final-speed| *))
;;    '((|_Final Speed2375_c0| |det| |det-the|)
;;      (|_Final Speed2375_c0| |input-word|
;;       (:|pair| "final speed" |n|))
;;      (|_Final Speed2375_c0| |det| |det-what|)
;;      (|_Final Speed2375_c0| |input-word| (:|pair| "what" |n|))
;;      (|*second| |input-word| (:|pair| "s" |n|))
;;      (|_Duration2367_c0| |det| |det-the|)
;;      (|_Duration2367_c0| |input-word| (:|pair| "duration" |n|))
;;      (|_Move10530_c0| |det| |det-the|)
;;      (|_Move10530_c0| |input-word| (:|pair| "move" |n|))
;;      (|*meter-per-second-squared| |input-word|
;;       (:|pair| "m/s^2" |n|))
;;      (|_Acceleration Magnitude2364_c0| |det| |det-the|)
;;      (|_Acceleration Magnitude2364_c0| |input-word|
;;       (:|pair| "acceleration magnitude" |n|))
;;      (|*meter-per-second| |input-word| (:|pair| "m/s" |n|))
;;      (|_Initial Speed2361_c0| |det| |det-the|)
;;      (|_Initial Speed2361_c0| |input-word|
;;       (:|pair| "initial speed" |n|))
;;      (|_Child2359_c0| |det| |det-the|)
;;      (|_Move10530_c0| |input-word| (:|pair| "move" |v|))
;;      (|_Child2359_c0| |det| |det-a|)
;;      (|_Child2359_c0| |input-word| (:|pair| "child" |n|)))))

;;deprecated (defun get-initial-viewpoint(input-scenario input-question-triple-list &optional(cpl-nl-triples nil))
;;   (progn 
;;     (set-BPSCurStage    1)
;;     (set-BPSMaxStage    3)
;;     (set-BPSCurPosition 1)
;;     (set-BPSMaxPosition 2) 
;;     (multiple-value-bind
;; 	(cloned-scenario cloned-question-triple-list clone-mappings)
;; 	(clone-scenario-and-question-triple-list input-scenario input-question-triple-list)
;;       (let* ((rl-engine-triples     (apply-rule-engine-on-initial-scenario cloned-scenario))
;; 	     (scenario              (set-difference (remove-irrelevant-km-triples 
;; 						     (append cloned-scenario rl-engine-triples))
;; 						    *rlremovetriples* :test #'ps-triple-equal))
;; 	     (final-viewpoint-query (make-viewpoint-query-slot-for-question-triple-list cloned-question-triple-list)))
;; 	(set-BPSCurPosition 2)
;;       (mutate-cpl-scenario         scenario)
;;       (mutate-cpl-compute-question cloned-question-triple-list)
;;       (setf *CONTROLLER-INSPECTION-VIEWPOINT* (or (equal *MAX-CLOCK* 1)
;; 						  (not (search-elaboration-p scenario cloned-question-triple-list))))
;;       (let* ((vp-inst (create-1st-viewpoint-definition scenario	final-viewpoint-query)))
;; 	(assert-viewpoint-triples vp-inst)
;; 	(if (not *CONTROLLER-INSPECTION-VIEWPOINT*)
;; 	    (determine-pruning-condition vp-inst))
;; 	vp-inst)))))

