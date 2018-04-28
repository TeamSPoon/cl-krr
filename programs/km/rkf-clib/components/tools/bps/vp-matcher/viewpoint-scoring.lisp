;;
;; $Id: viewpoint-scoring.lisp,v 1.13 2008/12/21 20:57:40 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;The viewpoint-candidate sorting code is highly inefficient as it computes
;;the viewpoint-scores everytime there is a comparison.
;;Ideally, the viewpoint-scores should be cached and reused.
;;we do this now.
(defun ps-compute-viewpoint-score(x)  
  (if (null (assoc x *viewpoint-score-compute-results*))
      (push (list x  (ps-compute-viewpoint-score-slow x) )
	    *viewpoint-score-compute-results*))
  (cadr (assoc x *viewpoint-score-compute-results*))
)

(defun ps-compute-viewpoint-score-slow(x)
  (let ((debug nil)
	(start-time (get-internal-run-time))
	(result ())
	(raw-viewpoint-score (cdr (car (get-viewpoint-score x)))))
    (push (list "concept applied" (stringify (get-viewpoint-concept x))) result)
    (push (list "concept-root-matched" (model-root-matched-in-viewpoint? x)) result)
    (push (list "#concepts applied"    (get-viewpoint-depth x)) result)
    (push (list "QA progress"         (get-viewpoint-answer-progress x)) result)
    (push (list "% of nodes matched"  (nth 0 raw-viewpoint-score)) result)
    (push (list "How well nodes matched up"  (nth 1 raw-viewpoint-score)) result)
    (push (list "global coverage score"  (nth 2 raw-viewpoint-score)) result)
    (push (list "taxonomic distance for concept matched"  (nth 3 raw-viewpoint-score)) result)
    (push (list "assumption triples"     (nth 4 raw-viewpoint-score)) result)
    (push (list "%query-triples-matched" (%query-triple-matched x)) result)
    (if debug (format t "(ps-compute-viewpoint-score-slow ~a) [~a ms]~%" x (- (get-internal-run-time) start-time)))
    result))

(defun model-root-matched-in-viewpoint?(vp-inst)
  (if (mentioned-in-viewpoint-correspondence? 
       (get-viewpoint-model-root vp-inst)
       vp-inst)
      1
   0))

(defun %query-triple-matched(vp-inst)
  (let* ((debug nil)
	 (matched-count 0)
	 (vp-correspondence-instance-lst (get-instances-inside-viewpoint-correspondence vp-inst)))
  (multiple-value-bind
      (simple-context full-context)
      (get-simplified-context-for-viewpoint vp-inst)
    (multiple-value-bind
	(lookup-query-lst yn-query-lst)
	(collate-viewpoint-query (get-viewpoint-query vp-inst))
      (dolist (entry lookup-query-lst)
	(multiple-value-bind
	      (type frame relation)
	    (parse-slot-value-answerable-query-entry entry)
	  (let ((filler (quasi-ps-slot-lookup-in-context frame relation vp-inst)))
	    (if (and
		 (not (null (intersection filler vp-correspondence-instance-lst)))
		 (member frame vp-correspondence-instance-lst))
		(setq matched-count (1+ matched-count)))
	  )
	))
      (if (not (null lookup-query-lst))
	  (* (/ matched-count (length lookup-query-lst) 1.00))
	0)
))))

(defun stringify-operator(x)
  (cond ((member x '(safe-> string>)) ">")
	((member x '(safe-< string<)) "<")
	(t (stringify x))))

(defun get-viewpoint-ranking-criteria()
  (cond ((equal *bps-mode* 'regular)
	 *viewpoint-ranking-criteria-regular*)
	((equal *bps-mode* 'forward-chaining)
	 *viewpoint-ranking-criteria-forward-chaining*)
  ) 
)

(defun get-viewpoint-criteria-desc()
  (mapcar #'(lambda(x)
	      (format nil "~a<BR>~a"
		      (nth 0 x)
		      (stringify-operator (nth 1 x))))
	  (get-viewpoint-ranking-criteria)))

(defun get-viewpoint-criteria-entries(vp-inst)
  (mapcar #'(lambda(criteria)
	      (let ((result (get-viewpoint-score-entry vp-inst (nth 0 criteria))))
		(if (typep result 'ratio)
		    (round-fraction result)
		  result)))
	  (get-viewpoint-ranking-criteria)))

(defun get-viewpoint-score-entry (vp-inst entry-desc)
  (cadr (assoc entry-desc (ps-compute-viewpoint-score vp-inst) :test 'string=)))

(defun sort-viewpoint-candidate-list(vp-candidate-list)
  (let* ((debug nil)
	 (start (get-internal-run-time)))
    (if debug (format t "Sorting viewpoint candidate list. "))
    (let ((result (sort (copy-list vp-candidate-list) 'better-viewpoint?)))
      (if debug (format t "Done. [~a ms]~%" (- (get-internal-run-time) start)))
      result)))

;;Different BPS modes affect ranking criteria.
(defun better-viewpoint?(x-viewpoint y-viewpoint)
  (better-viewpoint?-aux 
   x-viewpoint y-viewpoint
   (get-viewpoint-ranking-criteria))
)

;;If any elements in x are superior to elements in y. Then x is superior to y.
;;superior based on operator used.
;;if (car x) is better than (car y) or vice-versa then answer t and nil respectively.
;;otherwise, recurse on (cdr x) and (cdr y)
(defun better-viewpoint?-aux(x-viewpoint y-viewpoint criteria-lst)
  (cond ((consp criteria-lst)
	 (let* ((criteria         (car criteria-lst))
		(operator-desc    (nth 0 criteria))
		(operator         (nth 1 criteria)))
	   (cond ((apply operator
			 (list (get-viewpoint-score-entry x-viewpoint operator-desc)
			       (get-viewpoint-score-entry y-viewpoint operator-desc)))
		  t)
		 ((apply operator
			 (list (get-viewpoint-score-entry y-viewpoint operator-desc)
			       (get-viewpoint-score-entry x-viewpoint operator-desc)))
		  nil)
		 (t (better-viewpoint?-aux x-viewpoint y-viewpoint
					   (cdr criteria-lst))))))))

;;number of answerable-queries inherited from parent viewpoint.
(defun get-viewpoint-answer-progress(vp-inst)
  (if (null (get-viewpoint-query vp-inst)) 
      0
    (/ (length (get-viewpoint-answerable-query vp-inst))
       (length (get-viewpoint-query vp-inst)))))

(defun ps-score-viewpoint-instance(vp-inst simple-score taxonomic-score coverage-score concept-taxdist assumption-count
				   &optional (verbose nil))
  (car (ps-km-query `(,vp-inst |now-has| (|viewpoint-score| 
					  ((:|seq| ,simple-score ,taxonomic-score ,coverage-score ,concept-taxdist ,assumption-count))))
		    verbose)))
    
(defun ps-compute-coverage-score(root-viewpoint-triples elaborated-triples)
  (let* ((stemmed-root-viewpoint-triples  (unclone-triple-list-naively root-viewpoint-triples))
	 (stemmed-elaborated-triples      (unclone-triple-list-naively elaborated-triples))
	 (original-instance-list          (extract-all-instances-from-triple-list stemmed-root-viewpoint-triples)))
    (ps-compute-number-of-instances-with-different-concept original-instance-list
							   stemmed-root-viewpoint-triples
							   stemmed-elaborated-triples)))

(defun ps-compute-number-of-instances-with-different-concept(instance-list 
							     original-scenario 
							     new-scenario
							     &optional (verbose nil))
  (let ((target-set
	 (remove nil
		 (mapcar #'(lambda(x)
			     (let ((concept-in-original    (quasi-get-concept-for-kb-instance x original-scenario))
				   (concept-in-elaboration (quasi-get-concept-for-kb-instance x new-scenario)))
			       (cond ((not (property-value-p concept-in-original))
				      (if (not (equal concept-in-original concept-in-elaboration))
					  (progn 
					    (if verbose (format t "~a = ~a~%" concept-in-original concept-in-elaboration))
					    1)
					0)))))
			 instance-list))))
    (cond ((null target-set) 0)
	  (t (/ (reduce '+ target-set)
		(length target-set))))))

(defun ps-compute-tax-dist-for-matched-concept(candidate-concept candidate-root scenario-graph instance-map)
  (let* ((mapping (reverse-assoc candidate-root instance-map))
	 (scenario-root (cadr mapping)))
    (let ((potential-results 
	   (remove nil
		   (mapcar #'(lambda(x)
			       (abs-tax-dist candidate-concept x))
			   (quasi-get-concept-for-kb-instance scenario-root scenario-graph)))))
      (if (not (null potential-results))
	  (eval (cons 'max potential-results))))))

;; (defun testme()
;; (progn (ps-attempt-question "How many chromosomes are part of a cell?")(continue-answer-question)
;;        (tester-rename-file (draw-graphs-for-bps-attempt) "hlo-1761-graphs")))
