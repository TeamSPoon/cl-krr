;;
;; $Id: subgoal-identification.lisp,v 1.10 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *subgoal-explanation* "")

(defun identify-subgoals-debug-dump(scenario
				    additional-compute-questions 
				    subgoal-lst-triples
				    additional-eq-compute-questions 
				    eq-subgoal-lst-triples)
(progn
    (format t "compute questions identified from KM reasoning(~a):~%" (length additional-compute-questions))
    (dolist (x additional-compute-questions)
      (dolist (y (ps-get-query-triples (union scenario subgoal-lst-triples) x))
	(let ((slot (nth 1 y))
	      (frame (nth 0 y)))
	  (format t "   (the ~a of ~a) => ~a ~a~%" 
		  slot frame x 
		  (quasi-get-concept-for-kb-instance x (union scenario subgoal-lst-triples))))))
    (format t "compute questions identified from equations(~a):~%"    (length (set-difference additional-eq-compute-questions 
										     additional-compute-questions)))
    (dolist (x (set-difference additional-eq-compute-questions 
			       additional-compute-questions))
      (dolist (y (ps-get-query-triples (union scenario eq-subgoal-lst-triples) x))
	(let ((slot (nth 1 y))
	      (frame (nth 0 y)))
	  (format t "   (the ~a of ~a) => ~a ~a~%" 
		  slot frame x 
		  (quasi-get-concept-for-kb-instance x (union scenario eq-subgoal-lst-triples))))))
    ))

(defun identify-subgoals-in-cpl(input-scenario input-compute-questions input-yn-questions)
  (if (km-subgoal-patch?)
      (let ((debug nil)
	    (*logging* t)
	    (*prototype-classification-enabled* nil) ;;turn off classification
	    (*classification-enabled*           nil)    
	    (*eq-flag* nil))
	(multiple-value-bind 
	    (scenario compute-questions yn-questions orig->clone clone->orig)
	    (clone-and-assert-cpl-values input-scenario 
					 input-compute-questions 
					 input-yn-questions)    
	  (let* ((original-query-nodes   (extract-all-km-instances 
					  (append compute-questions yn-questions)))
		 (original-query-triples (ps-get-query-triples scenario original-query-nodes)))
	    (identify-subgoals scenario original-query-triples)
	    )))))

(defun identify-subgoals-for-instance (scenario instance)
  (let* ((original-query-triples (ps-get-query-triples scenario instance)))
    (identify-subgoals scenario original-query-triples)))

(defun identify-subgoals(scenario query-triple-lst)
  (if (and (km-subgoal-patch?) 
	   (consp query-triple-lst))
      (let ((debug nil)
	    (*logging* t)
	    (*prototype-classification-enabled* nil) ;;turn off classification
	    (*classification-enabled*           nil)    
	    (*eq-flag* nil))
	(multiple-value-bind
	    (additional-compute-question additional-triples)
	    (get-subgoals-for-query-triple scenario (car query-triple-lst))
	  (multiple-value-bind
	      (rest-additional-compute-question rest-additional-triples)
	      (identify-subgoals scenario (cdr query-triple-lst))
	    (values (union additional-compute-question
			   rest-additional-compute-question)
		    (union additional-triples
			   rest-additional-triples 
			   :test 'ps-triple-equal)))))))

(defun get-subgoals-for-query-triple(scenario query-triple)
  (let* ((debug nil)
	 (frame (nth 0 query-triple))
	 (slot  (nth 1 query-triple))
	 (filler (car (ps-km-query `(|the| ,slot |of| ,frame) debug))))
    (multiple-value-bind
	(subgoal-lst subgoal-lst-triples additional-compute-questions dep-lst)
	(get-subgoals-for-km-expr `(|the| |text-description| |of| (|the| ,slot |of| ,frame)))
      ;; most dependent compute questions... A->B->C, in this case, the most dep. ones are A->B.
      (let ((immediate-additional-compute-questions (flatten (car dep-lst))))
	(multiple-value-bind
	    (eq-dep-lst eq-subgoal-lst-triples)
	    (identify-missing-subgoals-in-eq-system immediate-additional-compute-questions)
	  (let ((full-triple-lst (remove-duplicates (append scenario subgoal-lst-triples eq-subgoal-lst-triples) :test 'ps-triple-equal)))
	    (explain-subgoal-lst "According to KM, ~%" 
				 dep-lst 
				 full-triple-lst)
	    (explain-subgoal-lst "According to the equation-solver,~%" 
				 eq-dep-lst
				 full-triple-lst)
	    (values
	     (remove-duplicates (union additional-compute-questions (flatten eq-dep-lst)))
	     (set-difference full-triple-lst scenario :test 'ps-triple-equal))))))))

(defun explain-subgoal-lst(entry-text input-subgoal-lst triple-lst)
  (let ((s (make-string-output-stream))
	(subgoal-lst (remove-if #'(lambda(entry) (null (cadr entry))) (copy-list input-subgoal-lst))))
  (if (not (null subgoal-lst)) 
      (progn 
	(format s entry-text)
	(dolist (subgoal-lst-entry subgoal-lst)
	  (explain-subgoal-lst-entry subgoal-lst-entry triple-lst s))
	(format s "~%~%")))
  (setf *subgoal-explanation* (format nil "~a~a" *subgoal-explanation* (get-output-stream-string s)))))

(defun explain-subgoal-lst-entry(subgoal-lst-entry triple-lst s)
  (let ((node (nth 0 subgoal-lst-entry))
	(dep  (nth 1 subgoal-lst-entry)))
    (if (not (null dep))
	(progn
	  (format s "Solving for ~A depends on~%" (pprint-instance-provenance (car node)))
	  (format s "~A" (pprint-dep-lst (car dep)))
	  (dolist (alternate-dep-list (cdr dep))
	    (progn
	      (format s "   OR~%")
	      (format s "~A" (pprint-dep-lst alternate-dep-list))))
	  (format s "~%")))))

(defun pprint-dep-lst(dep-lst)
  (let ((s (make-string-output-stream)))
    (dolist (dep-list-entry dep-lst)
      (format s "   ~A~%" (pprint-instance-provenance dep-list-entry)))
        (get-output-stream-string s)))

(defun pprint-instance-provenance(instance)
  (let ((s (make-string-output-stream))
	(query-paths-for-target (get-immediate-query-paths-for-instance instance)))
    (format s "~a (~a, ~a)" (car query-paths-for-target) instance (length query-paths-for-target))
    (get-output-stream-string s)))

(defun get-all-subgoal-expr(subgoal-lst)
  (mapcar 'car subgoal-lst))

(defun get-triples-from-subgoal-lst(subgoal-lst)
  (properly-terminate-triple-list
   (mappend #'(lambda(subgoal)
		(let* ((query (car subgoal))
		       (slot  (nth 1 query))
		       (frame (nth 3 query))
		       (filler (cadr subgoal))
		       (filler-concept (caddr subgoal)))
		  (if (not (null filler))
		      (cons
		       (list frame slot filler)
		       (mapcar #'(lambda(filler-concept-entry)
				   (list filler '|instance-of| filler-concept-entry))
			       filler-concept)))))
	    subgoal-lst)))

(defun get-additional-query-triples-for-cpl(scenario compute-question yn-question)
  (let ((original-query-triples (get-original-query-triples scenario compute-question yn-question)))
    (get-additional-query-triples scenario
				      (extract-all-km-instances original-query-triples)
				      original-query-triples)))

(defun get-additional-query-triples(scenario compute-questions &optional(query-triples))
  (cond ((set-equal query-triples (ps-get-query-triples scenario compute-questions))
	 query-triples)
	(t (let ((new-query-triples (ps-get-query-triples scenario compute-questions)))
	     (get-additional-query-triples scenario 
					   (union compute-questions
						  (extract-all-km-instances 
						   new-query-triples))
					   new-query-triples)))))

(defun identify-missing-subgoals-in-eq-system(compute-question-entry)
  (multiple-value-bind
      (eq-expr-list eq-symbol-table symbol-value-mapping)
      (collate-equation-system)
    (let* ((known-eq-sym-lst (mapcar 'car symbol-value-mapping))
	   (additional-triples ()))
      (values 
      (mapcar #'(lambda(eq-sym)
		  (list (list (eq-symbol-to-km eq-sym eq-symbol-table))
			(mapcar #'(lambda(dep-sym-lst)
				    (let ((dep-km-instance-lst 
					   (mapcar #'(lambda(sym)
						       (eq-symbol-to-km sym eq-symbol-table))
						   dep-sym-lst)))
				      (setf additional-triples (union (get-additional-triples-for-property-value-subgoals 
								       dep-km-instance-lst)
								      additional-triples :test 'ps-triple-equal))
				      dep-km-instance-lst))
				(get-dependent-eq-sym eq-expr-list 
						      eq-sym
						      known-eq-sym-lst))))
	       (km-to-eq-symbol compute-question-entry eq-symbol-table))
      additional-triples))))

(defun get-dependent-eq-expr(eq-expr-list eq-sym)
  (remove nil
	  (mapcar #'(lambda(eq-expr-entry)
		      (gleqns-solvefor eq-expr-entry eq-sym))
		  eq-expr-list)))

(defun get-dependent-instance-lst-for-eq-expr-lst(eq-expr-lst)
  (mapcar #'(lambda(expr)
	      (let ((lhs (nth 1 expr))
		    (rhs (nth 2 expr)))
		(list (list lhs) (varsin rhs))))
	  eq-expr-lst))

(defun get-dependent-eq-sym(eq-expr-lst eq-sym &optional(earlier-dependencies nil))
  (let* ((debug nil))
    (get-dependent-sym (get-dependent-instance-lst-for-eq-expr-lst 
			(get-dependent-eq-expr eq-expr-lst eq-sym))
		       (list eq-sym) earlier-dependencies)))

(defun get-dependent-sym(dependency-lst sym &optional(earlier-dependencies nil))
  (let* ((debug nil)
	 (result (remove-duplicates
		  (mapcar #'(lambda(dep-lst-entry)
			      (set-difference 
			       (set-difference (flatten dep-lst-entry) (flatten (list sym)))
			       earlier-dependencies))
			  (remove-if #'(lambda(dep-lst-entry) (not (equal (car dep-lst-entry) sym))) dependency-lst))
		  :test 'set-equal)))
    (if debug (format t "~a depends on ~a~%" sym result))
    (values (remove-if #'(lambda(branch)
			   (null (set-difference branch earlier-dependencies)))
		       result)
	    earlier-dependencies)))

(defun get-all-dependent-eq-sym(eq-expr-lst 
				eq-sym 
				max-depth
				&optional
				(earlier-dependencies nil))
  (sort 
   (remove-duplicates
    (get-all-dependent-eq-sym-aux eq-expr-lst
				  eq-sym
				  max-depth
				  earlier-dependencies
				  0 0 0
				  (list (list eq-sym)))
    :test 'equal)
    #'(lambda(x y)(< (length x) (length y)))))

(defun get-all-dependent-eq-sym-aux(eq-expr-lst 
				    eq-sym 
				    max-depth
				    &optional
				    (earlier-dependencies nil)
				    (current-depth 0) (current-branch 0) (current-variable 0)
				    (earlier-branch-lst   nil))
  (let ((debug  t))
    (cond ((or (member eq-sym earlier-dependencies)
	       (null eq-sym))
	   nil)
	  ((<= max-depth current-depth)
	   (list earlier-branch-lst))
	  (t
	   (progn
	     (if debug (format t "Investigating (~a.~a.~a), ~a. ~a ~%" current-depth current-branch current-variable eq-sym earlier-branch-lst))
	     (multiple-value-bind
		 (dep-syms-branch-lst new-dependencies)
		 (get-dependent-eq-sym eq-expr-lst eq-sym earlier-dependencies)
	       (if debug
		   (progn
		     (if (not (zerop (length dep-syms-branch-lst)))
			 (format t "~a branches to solve ~a, given ~a~%" (length dep-syms-branch-lst) eq-sym earlier-dependencies))
		     (dolist (branch dep-syms-branch-lst)
		       (format t "   ~a~%" branch))))
	       (cond ((null dep-syms-branch-lst)
		      (list (insert-at-end (list eq-sym) (insert-at-end eq-sym earlier-branch-lst))))
		     (t (mappend #'(lambda(branch)
				     (mappend #'(lambda(sym)
						  (get-all-dependent-eq-sym-aux eq-expr-lst sym max-depth
										(remove sym
											(cons eq-sym
											      (union branch
												     (union earlier-dependencies new-dependencies))))
										(1+ current-depth)
										(get-index-for branch dep-syms-branch-lst)
										(get-index-for sym branch)
										(insert-at-end branch (insert-at-end eq-sym earlier-branch-lst))))
					      branch))
				 dep-syms-branch-lst)))))))))

(defun get-all-dependent-sym(dep-lst
				sym 
				max-depth
				&optional
				(earlier-dependencies nil))
  (sort 
   (remove-duplicates
    (get-all-dependent-sym-aux (mapcar #'(lambda(x) 
					   (list (caar x) (cdr x)))
				       dep-lst)
			       sym
			       max-depth
			       earlier-dependencies
			       0 0 0
			       (list (list sym)))
    :test 'equal)
    #'(lambda(x y)(< (length x) (length y)))))

(defun get-all-dependent-sym-aux(eq-expr-lst 
				 eq-sym 
				 max-depth
				 &optional
				 (earlier-dependencies nil)
				 (current-depth 0) (current-branch 0) (current-variable 0)
				 (earlier-branch-lst   nil))
  (let ((debug  t))
    (cond ((or (member eq-sym earlier-dependencies)
	       (null eq-sym))
	   nil)
	  ((<= max-depth current-depth)
	   (list earlier-branch-lst))
	  (t
	   (progn
	     (if debug (format t "Investigating (~a.~a.~a), ~a. ~a ~%" current-depth current-branch current-variable eq-sym earlier-branch-lst)  )
	     (multiple-value-bind
		 (dep-syms-branch-lst new-dependencies)
		 (get-dependent-sym eq-expr-lst eq-sym earlier-dependencies)
	       (if debug
		   (progn
		     (if (not (zerop (length dep-syms-branch-lst)))
			 (format t "~a branches to solve ~a, given ~a~%" (length dep-syms-branch-lst) eq-sym earlier-dependencies))
		     (dolist (branch dep-syms-branch-lst)
		       (format t "   ~a~%" branch))))
	       (cond ((null dep-syms-branch-lst)
		      (list (insert-at-end (list eq-sym) (insert-at-end eq-sym earlier-branch-lst))))
		     (t (mappend #'(lambda(branch)
				     (mappend #'(lambda(sym)
						  (get-all-dependent-sym-aux eq-expr-lst sym max-depth
									     (remove sym
										     (cons eq-sym
											   (union branch
												  (union earlier-dependencies new-dependencies))))
									     (1+ current-depth)
									     (get-index-for branch dep-syms-branch-lst)
									     (get-index-for sym branch)
									     (insert-at-end branch (insert-at-end eq-sym earlier-branch-lst))))
					      branch))
				 dep-syms-branch-lst)))))))))

(defun get-index-for(x lst)
  (if (member x lst)
      (- (length lst) 
	 (length (member x lst)))))

(defun get-additional-triples-for-property-value-subgoals(compute-questions)
  (properly-terminate-triple-list
  (mappend #'(lambda(x)
	       (mappend #'(lambda(slot)
			    (mapcar #'(lambda(source-frame)
					(list source-frame (invert-slot slot) x))
				    (ps-km-query `(|the| ,slot |of| ,x))))
			(ps-km-query `(|the| |property-of-slot| |of| ,x))))
	   compute-questions)))

(defun eq-symbol-to-km(sym map)
    (let ((debug nil)
	  (result (cadr (assoc sym map))))
      (if debug (if result (format t "eq-to-km: ~a(~a)~%" sym result) (format t "eq-to-km: ~a (no applicable eq-symbol)~%" sym)))
      result))

(defun km-to-eq-symbol(sym map)
  (cond ((consp sym)
	 (remove nil
		 (cons (km-to-eq-symbol (car sym) map) 
		       (km-to-eq-symbol (cdr sym) map))))
	((and (atom sym)
	      (not (null sym)))
	 (let ((debug nil)
	       (result (cadr (assoc sym (invert-map map)))))
	   (if debug (if result (format t "km-to-eq: ~a(~a)~%" sym result) (format t "km-to-eq: ~a (no applicable eq-symbol)~%" sym)))
	   result))))

(defun get-fillers-for-km-query-lst(input)
  (mappend #'(lambda(subgoal)
	       (let* ((frame (nth 3 subgoal))
		      (slot  (nth 1 subgoal))
		      (subgoal-filler (ps-km-query subgoal)))
		 (if (null subgoal-filler)
		     (multiple-value-bind
			 (subgoal-filler subgoal-filler-concept)
			 (get-filler-for-slot-frame slot frame)
		       (list (list subgoal subgoal-filler subgoal-filler-concept)))
		   (mapcar #'(lambda(subgoal-filler-entry)
			       (list subgoal 
				     subgoal-filler-entry
				     (ps-km-query `(|the| |instance-of| |of| ,subgoal-filler-entry))))
			   subgoal-filler))))
	   (remove-if #'(lambda(x)
			  (or (listp (nth 3 x))
			      (not (km-instancep (nth 3 x)))))
		      input)))

;;returns multiple-value <subgoal-lst> <subgoal-lst-triples> <km-instances>
;; 1st value is a list of three-tuple, (<subgoal-expr> <subgoal-filler> (<subgoal-filler-type>)+)+
;;   a) If <subgoal-expr> returns nill, then <subgoal-filler> and <subgoal-filler-type> are nil
;;   b) If <subgoal-expr> returns multiple values, then there will be multiple three-tuple entries.
;; 2nd value is the list of triples translated from  <subgoal-lst>
(defun get-subgoals-for-km-expr-aux(expr)
  (let ((debug nil))
    (if (km-subgoal-patch?)
	(let ((*logging* t)
	      (*silent-spypoints* '((|the| ?x |of| ?y)))
	      (*eq-flag* nil))
	  (clear-silent-spy-log)	
	  (ps-km-query expr)
	  (let* ((subgoal-lst (clean-up-silent-spy-log 
			       (inspect-silent-spy-log)))
		 (result (get-fillers-for-km-query-lst subgoal-lst)))
	    (if debug 
		(progn
		  (format t "~a subgoals for ~a~%" (length subgoal-lst) expr)
		  (dolist (x result)
		    (format t "    (the ~a of ~a) => ~a ~a~%" (nth 1 (car x)) 
			    (nth 3 (car x))
			    (cadr x)
			    (caddr x)))))
	    (values result
		    (get-triples-from-subgoal-lst result)
		    (extract-all-km-instances result)))))))

(defun get-subgoals-for-km-expr(expr)
  (let ((debug nil))
  (collate-equation-system)
  (multiple-value-bind 
      (subgoal-lst subgoal-lst-triples additional-compute-questions)
      (get-subgoals-for-km-expr-aux expr)
    (let ((dep-lst (find-km-expr-subgoal-ordering subgoal-lst)))
      (if debug (format t "dep-lst: ~A~%" dep-lst))
    (values
     subgoal-lst subgoal-lst-triples additional-compute-questions dep-lst)))))

(defun remove-dependent-instances-with-same-query-path(dependent-instances-lst subgoal-lst)
  (cond ((null dependent-instances-lst)())
	(t (let* ((entry         (car dependent-instances-lst))
		  (entry-head    (caar entry))
		  (entry-dep-lst (cadr entry))
		  (entry-head-query-path-lst (get-query-paths-for-filler subgoal-lst entry-head)))
	     (cons (list (list entry-head)
			 (remove-if #'(lambda(x) (not (null (intersection entry-head-query-path-lst 
									  (get-query-paths-for-filler subgoal-lst x)
									  :test 'equal))))
				    entry-dep-lst))
		   (remove-dependent-instances-with-same-query-path (cdr dependent-instances-lst)
								    subgoal-lst))))))

(defun get-query-paths-for-filler(subgoal-lst filler)
  (mapcar 'car (remove-if-not #'(lambda(subgoal) (equal filler (nth 1 subgoal))) subgoal-lst)))

(defun install-skolem-fillers-for-null-subgoal-filler-entries(input-subgoal-lst)
  (remove-if #'(lambda(x) (null (nth 1 x)))  ;;handle null fillers, e.g., (_Property-Value value nil)
	     (mapcar #'(lambda(subgoal-lst-entry) 
			 (let ((query-path (nth 0 subgoal-lst-entry)))
			   (if (null (nth 1 subgoal-lst-entry))
			       (let ((slot (nth 1 query-path))
				     (frame (nth 3 query-path)))
				 (multiple-value-bind
				     (filler filler-concept-lst)
				     (get-filler-for-slot-frame slot frame)
				   (list query-path filler filler-concept-lst)))
	       subgoal-lst-entry)))
		     input-subgoal-lst)))

(defun find-km-expr-subgoal-ordering(input)
  (let* ((debug nil)
	 (subgoal-lst (install-skolem-fillers-for-null-subgoal-filler-entries input)))
    (if debug (format t "subgoal-lst :~A~%" subgoal-lst))
    (let ((dependent-instances-lst 
	   (remove-duplicates
	    (remove nil
		    (mapcar #'(lambda(x)
				(let ((expr (nth 0 x))
				      (filler (nth 1 x))
				      (filler-type (nth 2 x)))
				  (multiple-value-bind 
				      (sub-subgoal-lst sub-subgoal-lst-triples sub-additional-compute-questions)
				      (get-subgoals-for-km-expr-aux `(|the| |text-description| |of| ,expr))
				    (list (list filler)
					  (remove-if #'(lambda(x)
							 (progn
							   (or (equal x filler)
							       (null x))))
						     (remove-duplicates (mapcar #'(lambda(x)
										    (cadr x))
										sub-subgoal-lst)
									:test 'equal))))))
			    subgoal-lst))
	    :test #'(lambda(x y)
		      (and (equal (car x)(car y))
			   (set-equal (cadr x)(cadr y)))))))
      (if debug (format t "dependent-instances-lst :~%~A~%" dependent-instances-lst))
      (let ((cleaned-up-dependent-instances-lst (remove-dependent-instances-with-same-query-path dependent-instances-lst subgoal-lst)))
	(if debug (format t "dependent-instances-lst (after dep. instances with same-query-path removed) :~%~A~%" cleaned-up-dependent-instances-lst))
	(let ((reduced-dependent-instances-lst (reduce-subgoal-lst cleaned-up-dependent-instances-lst)))
	  (if debug (format t "reduced dependent-instances-lst :~A~%" reduced-dependent-instances-lst))
	  (mapcar #'(lambda(entry)
		      (list (car entry)
			    (remove nil (list (cadr entry)))))
		  reduced-dependent-instances-lst)
	)))))

(defun clean-up-silent-spy-log(input)
  (reverse 
   (remove-duplicates
    (remove-if #'(lambda(x) 
		   (let ((slot          (nth 1 x))
			 (frame-or-expr (nth 3 x)))
		     (and (not (member slot '(|input| |output|)))
			 (or (null frame-or-expr)
			     (null slot)
			     (listp slot)
			     (listp frame-or-expr)
			     (member slot '(|instance-of| |classes| |value|
					    |range-of| |/==| |inverse| |first| |equation-symbol|
					    |equation-uses| |primary-slot| |secondary-slot| |called|))
			     (member slot (ps-slot-lookup '|KM-Slot-Group| '|instances|))
			     (member slot (ps-slot-lookup '|NL-Slot-Group| '|instances|))
			     (member slot (ps-slot-lookup '|CLIB-Slot-Group| '|instances|))
			     (not (forward-relationp slot))
			     ;(not (clone-kb-instancep frame-or-expr))
			     ))))
	       input)
    :test 'equal)))

(defun get-viewpoint-query-path(vp-inst)
  (mapcar #'(lambda(vp-query-entry)
	      (let* ((triple (cdr (caddr vp-query-entry)))
		     (frame  (triple-head triple))
		     (slot   (triple-relation triple)))
		`(|the| ,slot |of| ,frame)))
	  (get-viewpoint-query vp-inst)
))

(defun get-viewpoint-query-as-triple-lst(vp-inst)
  (mapcar #'(lambda(vp-query-entry)
	      (let* ((triple (cdr (caddr vp-query-entry))))
		     triple))
	  (get-viewpoint-query vp-inst)))

(defun ls-sg-vp(vp-inst)
  (let ((scenario (append (get-simplified-context-for-viewpoint vp-inst)
			  (get-triple-list-for-vp-query-fillers vp-inst))))
    (setf *subgoal-explanation* "")
    (delete-frames-in-triple-lst scenario)
    (ps-assert-triples scenario t)
    (identify-subgoals scenario
		       (get-viewpoint-query-as-triple-lst vp-inst))
    (format t "~A" *SUBGOAL-EXPLANATION*)))
		       
(defun ls-sg-question(q-number)
    (setf *subgoal-explanation* "")
  (identify-subgoals-in-cpl (eval-tester-sym q-number 'cpl-scenario)
			    (eval-tester-sym q-number 'cpl-questions)
			    (eval-tester-sym q-number 'cpl-yn-questions))
    (format t "~A" *SUBGOAL-EXPLANATION*))

(defun reduce-subgoal-lst(lst)
  (cond ((null lst)())
	(t (cons (reduce-subgoal-lst-aux (car lst) (cdr lst))
		 (reduce-subgoal-lst (cdr lst))))))

(defun reduce-subgoal-lst-aux(lst reductions)
  (cond ((null reductions) lst)
	(t (let* ((subgoal (caar lst))
		  (other-subgoals (cadr lst))
		  (reduce-target         (car  reductions))
		  (reduce-subgoal        (caar reduce-target))
		  (other-reduce-subgoals (cadr reduce-target)))
	     (if (member reduce-subgoal other-subgoals)  
		 (reduce-subgoal-lst-aux
		  (list (list subgoal)
			(set-difference other-subgoals
					other-reduce-subgoals))
		  (cdr reductions))
	       (reduce-subgoal-lst-aux
		lst
		(cdr reductions)))))))

#|
(((A) (B1 B2 C D))
 ((B2) (C D))
 ((C) (D)))
=> (((A) (B1 B2) (B1))
    ((A) (B1 B2) (B2) (C) (D)))

(defun idiot()
(reduce-subgoal-lst
 '(((A) (B1 B2 C D))
   ((B2) (C D))
   ((C) (D)))))

|#

#|

|#


#|

(pprint (EXPAND-ALL-IN-LIST '(|_What1110_c34|
                            (|_Distance1108_c34| |_Distance1106_c34|))
                           '((|_Distance1108_c34|
                             ((|_def|))))))

(pprint (EXPAND-ALL-SUBGOALS-IN-LIST
           '(|_What1110_c39|
            ((|_Distance1108_c39| ((|_Distance1106_c39|)))
             |_Distance1106_c39|))
           '((|_Distance1106_c39| ((|_Distance1108_c39|))))))

|#


#|
;;examples
(get-query-paths-for-filler 
'(((|the| |output| |of| |_Sum1111_c1|) |_What1110_c1| (|Thing|))
    ((|the| |input| |of| |_Sum1111_c1|) |_Distance1108_c1| (|Length-Value|))
    ((|the| |input| |of| |_Sum1111_c1|) |_Distance1106_c1| (|Length-Value|))) 
'|_Distance1108_c1|)
=>
((|the| |input| |of| |_Sum1111_c1|))



(REMOVE-DEPENDENT-INSTANCES-WITH-SAME-QUERY-PATH
 '(((|_What1110_c8|) (|_Distance1108_c8| |_Distance1106_c8|))
  ((|_Distance1108_c8|) (|_Distance1106_c8|))
  ((|_Distance1106_c8|) (|_Distance1108_c8|)))
 '(((|the| |output| |of| |_Sum1111_c8|) |_What1110_c8| (|Thing|))
  ((|the| |input| |of| |_Sum1111_c8|) |_Distance1108_c8| (|Length-Value|))
  ((|the| |input| |of| |_Sum1111_c8|) |_Distance1106_c8| (|Length-Value|))))
=>
(((|_What1110_c8|) (|_Distance1108_c8| |_Distance1106_c8|))
 ((|_Distance1108_c8|) NIL) 
 ((|_Distance1106_c8|) NIL))



(get-dependent-instance-lst-for-eq-expr-lst 
'((= |d_x| (+ |d_1x| |d_2x|)) (= |d_y| (+ |d_1y| |d_2y|))
          (= |v_x| (/ |d_x| |t|)) (= |v_y| (/ |d_y| |t|))
          (= |t|
             (+ (|sqrt| (|expt| |t_1| 2)) (|sqrt| (|expt| |t_2| 2))))))
=>
(((|d_x|) (|d_2x| |d_1x|)) 
 ((|d_y|) (|d_2y| |d_1y|))
 ((|v_x|) (|t| |d_x|)) 
 ((|v_y|) (|t| |d_y|)) 
 ((|t|) (|t_2| |t_1|)))

(get-all-dependent-eq-sym '(
			  (= |_Average Velocity1126_c4|
			     (+ |_Speed-Value1193| |_Angle-Value1194|))
			  (= |_Speed-Value1193|
			     (+ |_Average Velocity1126_c4| 0)))
			  '|_Average Velocity1126_c4| 100)

(defun idiot(&optional(depth 3))
(dolist (x 
	 (GET-all-DEPENDENT-EQ-SYM 
	  '((= A (+ B1 (+ B2)))
	    (= B2 (+ (C D)))
	    (= C D))
	  'A 3))
  (format t "~a~%" x)))
=>
((A) (B1 B2) (B1))
((A) (B1 B2) (D) (C))



(defun idiot(&optional(depth 200))
  (dolist (x 
	   (GET-all-DEPENDENT-EQ-SYM 
	    '((= |d_x| (+ |d_1x| |d_2x|))
	      (= |d_y| (+ |d_1y| |d_2y|))
	      (= |v_x| (/ |d_x| |t|))
	      (= |v_y| (/ |d_y| |t|))
	      (= |t|
		 (+
		  (|sqrt| (|expt| |t_1| 2))
		  (|sqrt| (|expt| |t_2| 2)))))
	    '|d_x| depth))
    (format t "~a~%" x)))
=>
((d_x) d_x (d_1x d_2x) d_1x (d_1x))
((d_x) d_x (d_1x d_2x) d_2x (d_2x))
((d_x) d_x (v_x t) v_x (v_x))
((d_x) d_x (v_x t) t (d_y v_y) v_y (v_y))
((d_x) d_x (v_x t) t (t_1 t_2) t_1 (t_1))
((d_x) d_x (v_x t) t (t_1 t_2) t_2 (t_2))
((d_x) d_x (v_x t) t (d_y v_y) d_y (d_1y d_2y) d_1y (d_1y))
((d_x) d_x (v_x t) t (d_y v_y) d_y (d_1y d_2y) d_2y (d_2y))
|#

#|
;;Deprecated code
(defun remove-cycles-in-tree(tree &optional(previous-nodes))
  (cond ((null tree) ())
	((and (atom  (car  tree))
	      (listp (cadr tree)))
	 (if (not (member (car tree) previous-nodes))
	 (list 
	  (car tree)
	  (remove nil
		  (mapcar #'(lambda(branch)
			      (remove-cycles-in-tree branch
						     (cons (car tree) 
							   previous-nodes)))
			  (cadr tree))))))
	(t (set-difference tree previous-nodes))))

(defun expand-all-subgoals-in-list(list pattern-lst)
  (cond ((null pattern-lst) list)
	(t (expand-all-subgoals-in-list 
	    (replace-elements-in-list list (list (list (caar pattern-lst)
						       (car pattern-lst))))
	    (cdr pattern-lst)))))

|#

#|

(ps-parse-question
 "An object is falling.
The duration of the fall is 2 seconds.
What is the distance of the fall?")

;;identify all eq sym dependencies for compute-question
;;does not account for syms with bound values
(defun testcase1()
  (let ((*logging* t))
    (ps-assert-triples 
     '((|_Object1076| |instance-of| |Physical-Object|)
       (|_Fall7054| |instance-of| |Fall|)
       (|_Fall7054| |instance-of| |Free-Fall|)
       (|_Fall7054| |object| |_Object1076|)
       (|_Duration1078| |instance-of| |Duration-Value|)
       (|_Duration1078| |duration-of| |_Fall7054|)
       (|_Duration1078| |value| (:|pair| 2 |*second|))
       (|_Distance1082| |instance-of| |Length-Value|)
       (|_Distance1082| |distance-of| |_Fall7054|)) t)
    (identify-subgoals-in-eq-system '(|_Distance1082|))))

;;identify all eq sym dependencies for compute-question. 
;;accounts syms with bound values.
(defun testcase2()
  (let ((*logging* t))
    (ps-assert-triples 
     '((|_Object1076| |instance-of| |Physical-Object|)
       (|_Fall7054| |instance-of| |Fall|)
       (|_Fall7054| |instance-of| |Free-Fall|)
       (|_Fall7054| |object| |_Object1076|)
       (|_Duration1078| |instance-of| |Duration-Value|)
       (|_Duration1078| |duration-of| |_Fall7054|)
       (|_Duration1078| |value| (:|pair| 2 |*second|))
       (|_Distance1082| |instance-of| |Length-Value|)
       (|_Distance1082| |distance-of| |_Fall7054|)) t)
    (identify-missing-subgoals-in-eq-system '(|_Distance1082|))))

;;new version
(defun identify-subgoals-in-eq-system (compute-question-entry &optional(depth 200))
  (multiple-value-bind
      (eq-expr-list eq-symbol-table symbol-value-mapping)
	(collate-equation-system)
      (let* ((known-eq-sym-lst (mapcar 'car symbol-value-mapping)))
	(GET-all-DEPENDENT-EQ-SYM 
	 eq-expr-list
	 (km-to-eq-symbol compute-question-entry eq-symbol-table)
	 depth
	 known-eq-sym-lst))))

(defun testcase3()
  (let ((*logging* t))
    (ps-assert-triples 
     '((|_Object1076| |instance-of| |Physical-Object|)
       (|_Fall7054| |instance-of| |Fall|)
       (|_Fall7054| |instance-of| |Free-Fall|)
       (|_Fall7054| |object| |_Object1076|)
       (|_Duration1078| |instance-of| |Duration-Value|)
       (|_Duration1078| |duration-of| |_Fall7054|)
       (|_Duration1078| |value| (:|pair| 2 |*second|))
       (|_Distance1082| |instance-of| |Length-Value|)
       (|_Distance1082| |distance-of| |_Fall7054|)) t)
    (identify-missing-subgoals-in-eq-system '(|_Distance1082|))))

(defun testcase4()
  (let ((*logging* t))
    (ps-assert-triples 
     '((|_Object1076| |instance-of| |Physical-Object|)
       (|_Fall7054| |instance-of| |Fall|)
       (|_Fall7054| |instance-of| |Free-Fall|)
       (|_Fall7054| |object| |_Object1076|)
       (|_Duration1078| |instance-of| |Duration-Value|)
       (|_Duration1078| |duration-of| |_Fall7054|)
       (|_Duration1078| |value| (:|pair| 2 |*second|))
       (|_Distance1082| |instance-of| |Length-Value|)
       (|_Distance1082| |distance-of| |_Fall7054|)) t)
    (identify-missing-subgoals-in-eq-system '(|_Distance1082|))))

|#
