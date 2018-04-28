;;
;; $Id: controller-frontend.lisp,v 1.63 2009/08/21 19:57:06 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-dryrun()
  (progn 
    (setq *CONTROLLER-DRYRUN* (not *CONTROLLER-DRYRUN*))
    (if *CONTROLLER-DRYRUN*
	(format t "BPS: Entering dryrun mode~%")
      (format t "BPS: Exiting dryrun mode~%"))))

;;Tweak to make problem description palatable to CPL.
;;FIXME: Still need to capitalize 1st word of sentence.
(defun ps-make-cpl-string-proper(input)
  (let ((s (make-string-output-stream)))
    (dolist (sentence (break-into-sentences input))
      (format s "~a~%" (clean-sentence sentence)))
    (get-output-stream-string s)))

(defun ps-get-explanation(vp-inst)
  (with-standard-bps-parameters()
  (multiple-value-bind
      (ans expl timing error-str)
      (ps-get-explanation-aux vp-inst t)
      (if (or (null error-str) (string= error-str ""))
	  (values ans expl timing)
	(multiple-value-bind
	    (ans expl timing)
	    (ps-get-explanation-aux vp-inst nil)
    	  (values ans expl timing error-str))))))

(defun ps-get-explanation-aux (vp-inst strict?)
  (let ((start (get-internal-run-time))
	(*prototype-classification-enabled* nil)  ;temporarily turn on KM prototype classification
	(*classification-enabled*           nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|))
	(*on-error* (if strict? 'abort 'continue)))
    (multiple-value-bind
	(ans ans-error-str ans-error-struc ans-warn-str)
	(km `(|the| |viewpoint-answer| |of| ,vp-inst))
      (multiple-value-bind
	  (expl expl-error-str expl-error-struc expl-warn-str)
	  (km `(|the| |viewpoint-answer-page| |of| ,vp-inst))
	(values (car ans)
		(car expl)
		(- (get-internal-run-time) start)
		(concat ans-error-str (format nil "~%") expl-error-str))))))

;;Ideally, the problem-solver will examine the problem-solving tree to determine if its worthwhile trying somemore.
(defun continue-answer-question()
 (with-standard-bps-parameters()
 (multiple-value-bind
     (answer different? bps-error)
     (continue-problem-solver)
   (if (not (null answer))
       (multiple-value-bind
	   (short-answer html-answer expl-time-taken expl-error-str)
	   (ps-get-explanation (car answer))
	 (values html-answer short-answer t different?))
     (values "No additional answers found." nil nil nil)))))

;;Attempts question formulation and problem-solving.
(defun ps-attempt-question (question)
  (with-standard-bps-parameters()
  (let ((question (ps-make-cpl-string-proper question))
	*cpl-trace-parse* *cpl-trace-matcher*
	*cpl-syntactic-parse* *matcher-trace*
	*ls-remark* *ls-remark-nn* *ls-remark-coref*)
    (handler-case 
     (let (timer interpret-time bps-time explanation-time)
       (reset-regression-tester)
       (dialog-reset)
       (format t "Attempting : ~a~%" question)
       (setf timer (get-internal-run-time))
       (aura-dialog-interpret  question)
       (setf interpret-time   (- (get-internal-run-time) timer))
       (multiple-value-bind
	 (scenario compute-questions yes-no-questions)
	 (ps-triples-for-solver)
	 (setf timer (get-internal-run-time))
	 (let* ((answer (basic-problem-solver
			 scenario compute-questions yes-no-questions)))
	   (setf bps-time (- (get-internal-run-time) timer))
	   (multiple-value-bind
	       (explanation-text explanation-time)
	       (ps-get-explanation (car (get-answer-viewpoint)))
	     (list question 
		   *cpl-trace-parse*
		   *cpl-trace-matcher*
		   (list *ls-remark* *ls-remark-nn* *ls-remark-coref*)
		   (list scenario compute-questions yes-no-questions)
		   explanation-text 
		   (list interpret-time bps-time explanation-time)
		   (let ((s (make-string-output-stream)))
		     (format s "BPS: (satisfyp-tree-printer) turned off.")
		     (get-output-stream-string s))
		   *REGRESSION-REMARKS*
		   )))))
     (error (x) (let ((result (list question 
				    nil
				    nil
				    nil
				    nil
				    nil 
				    nil
				    nil
				    (format nil "BPS: Error encountered! ~a" x))))
		  (format t "BPS: Error encountered! ~a~%" x)
		  (format t "BPS: Returning ~a~%" result)
		  result))))))

(defun ps-triples-for-solver()
  (multiple-value-bind
      (scenario compute-questions yes-no-questions)
      (triples-for-solver)
    (values (make-triple-proper scenario)
	    compute-questions 
	    yes-no-questions)))

;;Generates the *scenario* and *compute-questions* parameters given a question string.
(defun ps-parse-question (question)
  (with-standard-bps-parameters()
  (let ((question (ps-make-cpl-string-proper question)))
    (format t "Attempting : ~a~%" question)
    (dialog-reset)
    (aura-dialog-interpret question)
    (multiple-value-bind
	(scenario compute-questions yes-no-questions)
	(ps-triples-for-solver)
      (let* ((scenario-without-cpl (remove-irrelevant-km-triples scenario))
	     (cpl-specific-triples (set-difference scenario scenario-without-cpl)))
	(format t "Scenario: ~s~%" scenario-without-cpl)
	(format t "CPL NLP : ~s~%" cpl-specific-triples)
	(format t "Query   : ~s~%" compute-questions)
	(format t "YN-query: ~s~%" yes-no-questions)
	(values scenario-without-cpl
		compute-questions 
		yes-no-questions
		cpl-specific-triples))))))

(defun ps-elaborate-question? (question)
  (multiple-value-bind
      (scenario compute-questions yes-no-questions)
      (ps-parse-question question)
    (let* ((query-instances  (remove-if-not 'km-instancep (flatten (append compute-questions yes-no-questions))))
	   (question-triple-list (ps-get-query-triples scenario query-instances)))
      (search-elaboration-p scenario
			    (REMOVE-INVALID-QUERY scenario 
						  question-triple-list)))))

(defun search-elaboration-p(input-scenario query-triple-list)
  (let ((scenario (get-scenario-without-query-triples input-scenario query-triple-list)))
    (not (eval 
	  (cons 'and 
		(mapcar #'(lambda(query-triple)
			    (let ((query-frame (triple-head query-triple)))
			      (inspection-questionp scenario (list query-frame)))) ;; check if query-frame is similar to skolem graph.
			query-triple-list))))))

;;Attempts description question type
;;Used inside expl-index.lisp
(defun ps-describe(concept-name)
  (let* ((concept  (intern concept-name :km))
	 (instance (ps-instantiate-concept concept))
	 (scenario `((,instance |instance-of| ,concept)))
	 (question `((WHAT-IS-A ,instance)))
	 (yes-no-question nil))
    (ps-get-explanation
     (basic-problem-solver scenario question yes-no-question))))

(defun ps-classify (inst &optional(enable-classification? t))
  (let ((*prototype-classification-enabled* enable-classification?)
	(*classification-enabled*           enable-classification?)
	(*built-in-remove-subsumers-slots*  '(|instance-of|)))
    (km inst)
    ;(classify inst)
))

#|
;;deprecated code

(defun ps-tryme(scenario question yn-questions &optional(draw-graph nil))
  (progn 
    (format t "[ps-tryme] Saving KB~%")
    (reset-problem-solver)
    (let ((backup-kb (bps-get-kb)))
      (ps-assert-triples scenario t)
      (let ((bps-output (car (basic-problem-solver scenario question yn-questions))))
	(let* ((*prototype-classification-enabled* nil)  ;temporarily turn on KM prototype classification
	       (*classification-enabled*           nil)
	       (result (km `(|the| |viewpoint-answer-page| |of| ,bps-output))))
	  (format t "[ps-tryme] Restoring KB~%")
	  (if draw-graph (format t "[ps-tryme] graphs for BPS attempt are placed in ~a~%" (draw-graphs-for-bps-attempt)))
	  (bps-put-kb backup-kb)
	  result)))))

;;Attempts question formulation and problem-solving.
(defun qa(question)
  (ps-attempt-question question))

;;Attempts to describe instance
(defun ps-describe-instance(instance-name)
  (let ((instance (intern instance-name :km))
	(s (make-string-output-stream)))
    (ps-non-global-km-situation)
    (let* ((graph   (affix-triple-prefix (gather-graph instance nil 3)))
	   (vp-defn `(|a| |Class-Definition-Viewpoint| |with|
		      (|viewpoint-scenario| 	 ())
		      (|viewpoint-target|	 ((:|seq| ,instance)))
		      (|viewpoint-query|	 ((:|seq| (:|pair| |*class-description| nil))))
		      (|viewpoint-model-graph| (,(cons ':|set| 
						       (deaggregate-instance-types graph))))))
	   (vp-inst (ps-km-query vp-defn)))
      (let ((explanation (ps-get-explanation vp-inst)))
	(undo)
	(format s "<html><body><pre>")
	(dolist (km-frame (triples-to-km-assertions (strip-triple-prefix graph)))
	  (format s "(~a ~a~%" (car km-frame) (cadr km-frame))
          (dolist (km-slotval (cddr km-frame))
            (format s "   ~a~%" km-slotval))
          (format s ")~%")
	)
	(format s "</pre></body></html>")
      (format nil "~a~a" (get-output-stream-string s) explanation)))))

;;Attempts similarity question-type
(defun ps-find-similarity(concept-name1 concept-name2)
  (let* ((concept1  (intern concept-name1 :km))
	 (concept2  (intern concept-name2 :km))
	 (instance1 (ps-instantiate-concept concept1))
	 (instance2 (ps-instantiate-concept concept2))
	 (_Be-Similar (ps-instantiate-concept '|Be-Similar|))
	 (scenario `((,instance1 |instance-of| ,concept1)
		     (,instance2 |instance-of| ,concept2)
		     (,_Be-Similar |instance-of| |Be-Similar|)
		     (,_Be-Similar |object| ,instance1)
		     (,_Be-Similar |object| ,instance2)))
	 (question `((,_Be-Similar)))
	 (yes-no-question nil))
    (ps-assert-triples scenario)
    (ps-get-explanation
     (basic-problem-solver scenario question yes-no-question))))

;;Attempts difference question-type, generates explanation
(defun ps-find-difference(concept-name1 concept-name2)
  (ps-get-explanation
   (ps-generate-find-difference-viewpoint concept-name1 concept-name2)))

;;Attempts difference question-type, generates explanation
(defun ps-generate-find-difference-viewpoint(concept-name1 concept-name2)
  (let* ((concept1  (intern concept-name1 :km))
	 (concept2  (intern concept-name2 :km))
	 (instance1 (ps-instantiate-concept concept1))
	 (instance2 (ps-instantiate-concept concept2))
	 (_Be-Different (ps-instantiate-concept '|Be-Different|))
	 (scenario `((,instance1 |instance-of| ,concept1)
		     (,instance2 |instance-of| ,concept2)
		     (,_Be-Different |instance-of| |Be-Different|)
		     (,_Be-Different |object| ,instance1)
		     (,_Be-Different |object| ,instance2)))
	 (question `((,_Be-Different)))
	 (yes-no-question nil))
    (ps-assert-triples scenario)
    (basic-problem-solver scenario question yes-no-question)))

(defun ps-ruleengine()
  (progn 
    (setq *CONTROLLER-ENABLE-RULE-ENGINE* (not *CONTROLLER-ENABLE-RULE-ENGINE*))
    (if *CONTROLLER-ENABLE-RULE-ENGINE*
	(format t "BPS: Rule-engine enabled~%")
      (format t "BPS: Rule-engine disabled~%"))))

(defun ps-elaborate?(scenario instance)
  (not (inspection-questionp scenario (list instance))))

(defun ps-get-instances-for-elaboration(scenario)
  (remove-if-not #'(lambda(x)
		     (ps-elaborate? scenario x))
		 (extract-all-instances-from-triple-list scenario)))

|#


; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; PDQA implementation

; no longer needed, all handled in controller.lisp
#|
(defun ps-get-explanation-aux-PDQA (vp-inst strict?)
  (let ((start (get-internal-run-time))
        (*prototype-classification-enabled* nil)  ;temporarily turn on KM prototype classification
        (*classification-enabled*           nil)
        (*built-in-remove-subsumers-slots*  '(|instance-of|))
        (*on-error* (if strict? 'abort 'continue)))
    (multiple-value-bind
        (ans ans-error-str ans-error-struc ans-warn-str)
        (km0 `(|the| |viewpoint-answer| |of| ,vp-inst))
      (multiple-value-bind
          (expl expl-error-str expl-error-struc expl-warn-str)
          (km0 `(|the| |viewpoint-answer-page| |of| ,vp-inst))
        (values (car ans)
                (append-debug-output (car expl) vp-inst)
                (- (get-internal-run-time) start)
                (concat ans-error-str (format nil "~%") expl-error-str))))))


(defun append-debug-output (expl-str vp-inst)
  (let ((debug-str (car (ps-km-query `(|the| |vp-debug| |of| ,vp-inst)))))
    (if debug-str
        (concat (strip-final-html-tags expl-str)
                debug-str
                "</body></html>"
        )
        expl-str
    )
  )
)

(defun strip-final-html-tags (html-str)
  (subseq html-str 0 (search "</body></html>" html-str))
)
|#

