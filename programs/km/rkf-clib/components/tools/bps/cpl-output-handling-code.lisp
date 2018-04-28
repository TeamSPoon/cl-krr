;;
;; $Id: cpl-output-handling-code.lisp,v 1.12 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun install-cpl-hacks()
  (with-restoring-situations-reasoning ()
    (global-situation)
    (if (not (isa '|equal| '|Property-Relation|))
	(ps-km-query '(|equal| |now-has| (|instance-of| (|Property-Relation|)))))))

;;Sieves yn-triple-question entries
;;Only handles entries containing 'is-it-true-that-questionp marker.
;;Returns triples that need yn testing.
(defun yn-get-query-triples (input)
  (sieve-query-triple-for input '|is-it-true-that-questionp|))

;;Sieves yn-triple-question entries
;;Only handles entries containing <tag> marker.
(defun sieve-query-triple-for (input tag)
  (let (result)
    (dolist (yn-query-entry input)
      (if (and (listp yn-query-entry)
	       (member tag
		       (flatten yn-query-entry)))
	  (dolist (triple yn-query-entry)
	    (if (not (equal (triple-relation triple) '|query-varp|))
		(push triple result)))))
    result))

(defun generate-is-it-true-p-wrapper(triple-lst)
  (cond ((null triple-lst) ())
	(t (cons (list (car triple-lst)
		       '(|is-it-true-that-questionp| |query-varp| |t|))
		 (generate-is-it-true-p-wrapper (cdr triple-lst))))))

(defun generate-what-is-the-wrapper(instance-lst)
  (cond ((null instance-lst)())
	(t (cons (list 'WHAT-IS-THE 
		       (car instance-lst))
		 (generate-what-is-the-wrapper (cdr instance-lst))))))

(defun fix-cpl-output-for-yn-is-a(scenario compute-question yn-query)
  (let ((triples-to-verify (yn-get-query-triples yn-query)))
      (values scenario 
	      (remove-duplicates
	       (append compute-question 
		       (generate-what-is-the-wrapper 
			(mapcar 'car 
				(sieve-triple-list-having-relation '|is-a| triples-to-verify))))
	       :test 'equal)
	      yn-query)))

;;Changed |equal| relation in YN-QUESTION to be |same-as| relation
(defun fix-cpl-yn-question-containing-equal-relations(scenario compute-question yn-query)
  (values scenario 
	  compute-question
	  (replace-if-equal '|equal| '|same-as| yn-query)))

(defun ps-predicate-relationp(relation)
  (not (null (member relation 
		     '(|greater-than-or-equal-to|
		       |less-than-or-equal-to| |greater-than| 
		       |less-than| |same-as|
		       |same-type| |is-a|)))))

;;returns multi-valued;
;; 1st va;ue filler instance
;; 2nd value additional triples to hook up filler instance with scenario
(defun install-skolem-query-triple (scenario query-frame relation
				    &optional(filler-concept-lst nil))
  
  (multiple-value-bind
      (filler filler-triples)
      (ps-instantiate-concept-as-triple-list 
       (if (null filler-concept-lst) 
	   (ranges-of relation)
	   filler-concept-lst))
    (values filler 
	    (append 
	     filler-triples 
	     (list (list query-frame relation filler))))))

(defun fix-cpl-for-non-predicate-yn-question-relations(scenario compute-question yn-query)
  (let* ((debug nil)
	 (triples-to-verify (make-triple-proper (yn-get-query-triples yn-query)))
	 (problematic-triples-to-verify (remove-if #'(lambda(triple) (ps-predicate-relationp (triple-relation triple)))
						   triples-to-verify))
	 (additional-scenario-triples  nil)
	 (additional-triples-to-verify nil))
    (if debug (format t "problematic yn-query-triples(~a): ~a~%" (length problematic-triples-to-verify)
		      problematic-triples-to-verify))
    (dolist (problematic-yn-triple problematic-triples-to-verify)
      (let ((head     (triple-head     problematic-yn-triple))
	    (relation (triple-relation problematic-yn-triple))
	    (tail     (triple-tail     problematic-yn-triple)))
	(multiple-value-bind
	    (filler filler-triples)
	    ;(install-skolem-query-triple scenario head relation (quasi-get-concept-for-kb-instance tail scenario))
	    (install-skolem-query-triple scenario head relation)
	  (push (list (list tail '|is-a| filler)
		      '(|is-it-true-that-questionp| |query-varp| |t|))
		additional-triples-to-verify)
	  (push (list (list tail '|same-as| filler) 
		      '(|is-it-true-that-questionp| |query-varp| |t|))
		additional-triples-to-verify)
	  (dolist (triple filler-triples) (push triple additional-scenario-triples))
    )))
    (values (append scenario additional-scenario-triples)
	    compute-question 	    
	    (append additional-triples-to-verify yn-query))
))

(defun fix-cpl-output(scenario compute-question yn-query
			       &optional(func-lst 					 
					 '(;fix-cpl-yn-question-containing-equal-relations
					   ;fix-cpl-output-for-yn-is-a
					   ;fix-cpl-for-non-predicate-yn-question-relations
					   ;install-ae-triples-for-skolem-instances-in-yn-query
					   )))
  (cond ((null func-lst) 
	 (values scenario compute-question yn-query))
	(t (multiple-value-bind
	       (new-scenario new-compute-question new-yn-query)
	       (eval (list (car func-lst) 
			   (kwote scenario)
			   (kwote compute-question)
			   (kwote yn-query)))
	       (fix-cpl-output new-scenario new-compute-question new-yn-query
			       (cdr func-lst))))))

(defun get-is-a-yn-question-instance-lst(yn-question)
  (remove '|is-a|
	  (remove-duplicates
	   (flatten
	    (sieve-triple-list-having-relation 
	     '|is-a| 
	     (car yn-question))))))

;;return instances with null query map.
(defun get-isolated-instance-list(scenario compute-question yn-question)
  (let* ((debug nil)
	 (full-instance-query-map  (get-query-triples-for-all-instances scenario))
	 (all-query-triples        (mappend 'cadr full-instance-query-map))
	 (null-query-map-instances (mapcar 'car (remove-if 'cadr full-instance-query-map))))
    (let ((result 
	   (set-difference
	    (remove-if
	     #'(lambda(x)
		 (member x (flatten all-query-triples)))
	     null-query-map-instances)
	    (get-is-a-yn-question-instance-lst yn-question))))
      (if debug
	  (progn
	    (format t "all-query-triples(~A): ~A~%" 
		    (length all-query-triples)
		    all-query-triples)
	    (format t "instances-with-null-query-map(~A): ~A~%" 
		    (length null-query-map-instances)
		    null-query-map-instances)
	    (format t "isolated-instances-with-null-query-map(~A): ~A~%" 
		    (length result)
		    result)))
      result)))

;;Does 
;; a) remove unneccessary triples from scenario for isolated instances.
;; b) install necessary compute questions
;; c) rewrite problematic property-comparison questions
;; d) install necessary WHAT-IS-THE questions for is it true if X is a Y? question.
(defun rewrite-cpl-output-as-elaboration-question(scenario compute-question yn-question)
  (let* ((debug nil)
	 (all-compute-questions    
	  (union compute-question 
		 (extract-all-instances-from-triple-list
		  yn-question)))
	 (isolated-instances
	  (get-isolated-instance-list scenario
				      compute-question
				      yn-question))
	 (unneccessary-scenario-triples 
	  (mappend #'(lambda(x) (ps-get-subgraph x scenario)) 
		   isolated-instances)))
    (if debug
	(format t "unnecessary-scenario-triples(~A): ~A~%" 
		(length unneccessary-scenario-triples)
		unneccessary-scenario-triples))
    (values (set-difference scenario 
			    unneccessary-scenario-triples 
			    :test 'ps-triple-equal)
	    (set-difference all-compute-questions
			    isolated-instances)
	    yn-question)))

#|
;;deprecated (defun fix-cpl-output-for-yn-property-relations(scenario compute-question yn-query)
  (let ((triples-to-verify (yn-get-query-triples yn-query)))
    (let ((new-scenario    (add-skolem-fillers-for-question-triple-lst 
			    scenario 
			    (get-question-triples-for-yn-question scenario
								  triples-to-verify))))
      (values new-scenario 
	      compute-question
	      (generate-is-it-true-p-wrapper
	       (rewrite-triple-to-verify-lst 
		new-scenario
		triples-to-verify))))))

(defun testcase(question)
  (multiple-value-bind 
      (scenario question yn-question nl-triples)
      (ps-parse-question question)
    (REWRITE-CPL-OUTPUT-AS-ELABORATION-QUESTION
     scenario question yn-question))
)

(defun testcase1()
(REWRITE-CPL-OUTPUT-AS-ELABORATION-QUESTION
 '((|_Weak Base13397_c0| |instance-of| |Weak-base|)
   (|_Weak Electrolyte13398_c0| |instance-of| |Weak-Electrolyte|))
 NIL
 '(((|_Weak Base13397_c0| |is-a| |_Weak Electrolyte13398_c0|)
    (|is-it-true-that-questionp| |query-varp| |t|)))))

(defun testcase2()
  (multiple-value-bind 
    (scenario question yn-question nl-triples)
    (ps-parse-question "A person jumps.
The initial speed of the jump is 10 m/s.
The direction of the jump is 20 degrees.
The initial x position of the jump is 0 meters.
The initial y position of the jump is 0 meters.
The final y speed of the jump is 0 m/s.
Is it true that the x distance of the jump is greater than 5 m?")
  (REWRITE-CPL-OUTPUT-AS-ELABORATION-QUESTION
   scenario question yn-question)))

(defun testcase(question)
  (multiple-value-bind 
      (scenario compute-question yn-query)
      (ps-parse-question question)
    (fix-cpl-output scenario compute-question yn-query)
    (fix-cpl-output scenario compute-question yn-query)
))

(testcase "There is a sum of 10 m and 100 centimeters.
             Is it true that the sum is 1100 cm? 
             Is it true that the sum is 200 cm?")

(testcase "There is a sum of 10 m and 100 centimeters.
             Is it true that the sum is 1100 cm? 
             Is it true that the sum is 200 cm?
Is it true that a robot is a person?")

|#

