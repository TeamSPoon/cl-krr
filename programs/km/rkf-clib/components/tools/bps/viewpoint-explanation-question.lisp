;;
;; $Id: viewpoint-explanation-question.lisp,v 1.40 2008/10/30 18:57:37 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;30Oct08
;; (defun viewpoint-explain (cpl-scenario cpl-question)
;;   (let* ((instance (get-instance-to-explain cpl-scenario cpl-question))
;; 	 (vp-defn (cond ((specialized-p cpl-scenario instance)
;; 			 (explain-specialized-instance cpl-scenario instance))
;; 			(t
;; 			 (explain-skolem-instance cpl-scenario instance))))
;; 	 (vp-inst (assert-viewpoint-definition vp-defn)))
;;     (assert-viewpoint-triples vp-inst)
;;     (mutate-closedlist (cons vp-inst *closedlist*))
;;     vp-inst))

;;30Oct08
;; (defun explain-skolem-instance(cpl-scenario instance &optional(verbose t))
;;   (if *controller-open-domain-qa*
;;       (explain-skolem-instance-enable-open-domain-qa cpl-scenario instance verbose)
;;       (explain-skolem-instance-basic cpl-scenario instance verbose)))

;;30Oct08
;; ;;Skolem explanation.
;; (defun explain-skolem-instance-basic(cpl-scenario 
;; 				     instance 
;; 				     &optional(verbose nil))
;;   (let ((debug nil)
;; 	(concept  (get-concept-for-kb-instance instance))
;; 	(mentioned-slots (get-slots-for-instance-in-triple-list instance 
;; 								cpl-scenario))
;; 	)
;;     (if verbose (format t "BPS: Gathering description for ~a~a~%" 
;; 			instance
;; 			concept))
;;     (multiple-value-bind
;; 	(root concept-graph fixpoint?)
;; 	(ps-get-new-graph (car concept))
;;       (ps-assert-triples concept-graph debug)
;;       (let ((graph (ps-remove-constraint-triples concept-graph)))
;; 	(ps-km-query `(,root == ,instance) verbose)
;; 	`(|a| |Class-Definition-Viewpoint| |with|
;; 	  (|viewpoint-scenario| 	 (,(cons ':|set| 
;; 						 (affix-triple-prefix
;; 						  cpl-scenario))))
;; 	  (|viewpoint-target|	 ((:|seq| ,instance)))
;; 	  (|viewpoint-query|	 ((:|seq| (:|pair| |*class-description| nil))))
;; 	  (|viewpoint-model-graph| (,(construct-viewpoint-model-graph-filler 
;; 	  			      (deaggregate-instance-types graph))))
;; 	  )
;; 	))))

;;30Oct08
;; (defun construct-viewpoint-model-graph-filler(triple-list)
;;   (cond ((not (null triple-list))
;; 	 (cons ':|set| (affix-triple-prefix triple-list)))))

;; ;;Skolem explanation.
;; (defun explain-skolem-instance-enable-open-domain-qa(cpl-scenario instance &optional(verbose t))
;;   (let ((concept  (get-concept-for-kb-instance instance))
;; 	(input-words (get-input-words-for-instance instance cpl-scenario)))
;;     (if verbose (format t "BPS: Gathering description for ~a~a~%" 
;; 			instance
;; 			concept))
;;     (multiple-value-bind
;;       (root graph fixpoint?)
;;       (ps-get-new-graph (car concept))
;;       (ps-km-query `(,root == ,instance) verbose)
;;       `(|a| |Class-Definition-Viewpoint| |with|
;; 	(|viewpoint-textrunner-triples| (,(cons ':|set|
;; 						(affix-triple-prefix
;; 						 (use-textrunner-to-describe input-words)))))
;; 	(|viewpoint-scenario| 	 (,(cons ':|set| 
;; 					 (affix-triple-prefix
;; 					  cpl-scenario))))
;; 	(|viewpoint-target|	 ((:|seq| ,instance)))
;; 	(|viewpoint-query|	 ((:|seq| (:|pair| |*class-description| nil))))
;; 	;(|viewpoint-model-graph| (,(construct-viewpoint-model-graph-filler
;; 	;			    (deaggregate-instance-types graph))))
;; 	)
;; )))

#|
IS-EXPLANATION-QUESTION-P routine
Two indicators
  a) CPL question indicate "WHAT-IS-A" question type.
  b) CPL question instance is an isolated skolem, i.e. nil query-frame.
|#
(defun is-explanation-question-p (cpl-scenario cpl-question)
  (let* ((cpl-scenario (make-triple-proper cpl-scenario))
	 (val (car cpl-question))
	 (query-path   (first (remove-duplicates 
			       (ps-extract-question cpl-scenario 
						    (list val))
			       :test #'equal)))
	 (vp-slot-value-pair       (list  (second query-path)
					  (fourth query-path)))
	 (query-slot-name          (first  vp-slot-value-pair))
	 (query-frame-instance     (second vp-slot-value-pair)))
    (cond ((and (consp val)       ;;Case (a)
		(eql (first val) 'WHAT-IS-A))
	   (progn (format t "BPS: Descriptive question, case (a)~%")
		  (second val)))
	  ((and (atom val)        ;; Case (b)
		(null query-path) 
		(not (null cpl-question))
		(not (specialized-p cpl-scenario val)))
	   (progn (format t "BPS: Descriptive question, case (b)~%")
		  val))
	  (t nil))))

(defun get-instance-to-explain (cpl-scenario cpl-question)
  (let* ((cpl-scenario (make-triple-proper cpl-scenario))
	 (val          (car cpl-question))
	 (query-path   (first (remove-duplicates 
			       (ps-extract-question cpl-scenario 
						    (list val))
			       :test #'equal)))
	 (vp-slot-value-pair       (list  (second query-path)
					  (fourth query-path)))
	 (query-slot-name          (first  vp-slot-value-pair))
	 (query-frame-instance     (second vp-slot-value-pair)))
    (cond ((and (consp val)       ;;Case (a)
		(eql (first val) 'WHAT-IS-A))
	   (second val))
	  ((and (atom val)        ;; Case (b)
		(null query-path) 
		(not (specialized-p cpl-scenario val)))
	   val)
	  (t nil))))

(defun get-all-related-concepts(instance)
  (let((all-concepts (get-concept-for-kb-instance instance)))
    (remove-duplicates 
     (mappend #'(lambda(concept)
		  (get-all-subclasses concept))
	      all-concepts))))

;;Returns all concepts up and down the hierarchy
(defun get-all-subclasses(concept)
  (cons concept (ps-km-query `(|the| |all-subclasses| |of| ,concept))))

;; ;;
;; ;;Test cases
;; ;;
	   
;; ;;Example to test specialized instance explanation.
;; ;;Requires Tecuci's KB, which includes the necessary Reaction concept.
;; (let ((scenario '((|_React7908| |raw-material| |_Hydrogen7909|) 
;; 		  (|_React7908| |raw-material| |_Oxygen7910|)
;; 		  (|_React7908| |result| |_Water|)
;; 		  (|_React7908| |instance-of| |Reaction|) 
;; 		  (|_Oxygen7910| |instance-of| |O2-Substance|)
;; 		  (|_Water| |instance-of| |H2O-Substance|)
;; 		  (|_Hydrogen7909| |instance-of| |H2-Substance|)))
;;       (question '((WHAT-IS-A |_React7908|)))
;;       (kb (bps-get-kb)))
;;   (ps-assert-triples scenario)
;;   (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
;;   (bps-put-kb kb))

;; ;;Example to test skolem explanation
;; (let ((scenario '((|_React7908| |instance-of| |Reaction|)))
;;       (question '((WHAT-IS-A |_React7908|)))
;;       (kb (get-kb)))
;;   (ps-assert-triples scenario)
;;   (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
;;   (put-kb kb))

;; ;;Example to test non-existent specialized instance explanation
;; (let ((scenario '((|_H| |instance-of| |H|)
;; 		  (|_Chemical1| |instance-of| |Chemical|)
;; 		  (|_Chemical1| |has-basic-structural-unit| |_H|)))
;;       (question '((WHAT-IS-A |_Chemical1|)))
;;       (*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
;;       (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
;;       (kb (get-kb))) 
;;   (ps-assert-triples scenario)
;;   (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
;;   (put-kb kb))

;; ;;Additional example #1 - Tecuci's Metal-Nonmetal-Combination-Reaction concept 
;; (let ((scenario '((|_Metal| |instance-of| |Metal|)
;; 		  (|_Non-Metal| |instance-of| |Non-Metal|)
;; 		  (|_Reaction| |instance-of| |Reaction|)
;; 		  (|_Reaction| |raw-material| |_Metal|)
;; 		  (|_Reaction| |raw-material| |_Non-Metal|)))
;;       (question '((WHAT-IS-A |_Reaction|)))
;;       (*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
;;       (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
;;       (kb (get-kb)))
;;   (ps-assert-triples scenario)
;;   (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
;;   (put-kb kb))

;; ;;Additional example #2 - Tecuci's Combustion-In-Air concept 
;; (let ((scenario '((|_O2-Substance| |instance-of| |O2-Substance|)
;; 		  (|_H2O-Substance| |instance-of| |H2O-Substance|)
;; 		  (|_Reaction| |instance-of| |Reaction|)
;; 		  (|_Reaction| |raw-material| |_O2-Substance|)
;; 		  (|_Reaction| |result| |_H2O-Substance|)))
;;       (question '((WHAT-IS-A |_Reaction|)))
;;       (*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
;;       (*classification-enabled*           nil)  ;disable KM classification for non-root viewpoints.
;;       (kb (get-kb)))
;;   (ps-assert-triples scenario)
;;   (pprint (km0 `(|the| |viewpoint-answer-page| |of| ,(basic-problem-solver scenario question nil))))
;;   (put-kb kb))

