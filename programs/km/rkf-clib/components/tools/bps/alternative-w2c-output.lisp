;;
;; $Id: alternative-w2c-output.lisp,v 1.14 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Cannot do a (remove-subsumer) because CPL may return too specific a class, 
;;e.g., Move -> Locomotion
;;different problem when Throw and Two-Dimensional-Move are cousin concepts.
(defun get-alternative-word2concept-mappings(cpl-triples 
					     &optional(yes-no-triples nil))
  (let ((debug nil)
	(alternative-candidate-lst ()))
    (dolist (entry (target-input-words-for-alt-w2c-mappings (append (car yes-no-triples)
								     cpl-triples)))
      (let* ((instance      (triple-head entry))
	     (word          (cadr  (triple-tail entry)))
	     (pos           (caddr (triple-tail entry)))
	     (candidate-lst (remove-if-not 'concept-p 
					   (WORD2CONCEPTS word 
							  :POS pos 
							  :CLIMB-TAXONOMY NIL))))
	(if (> (length candidate-lst) 1)
	    (progn 
	      (push (list instance 
			  (union candidate-lst (cadr (assoc instance alternative-candidate-lst))))
		    alternative-candidate-lst)
	      (if debug (format t "~a, ~s(~A) => ~A~%" 
				instance word pos 
				(cadr (assoc instance alternative-candidate-lst))))
	      ))))
    (mappend #'(lambda(x)
		(get-candidate-w2c-substitutions x alternative-candidate-lst))
	    (powerset (remove-duplicates (mapcar 'car alternative-candidate-lst))))))

(defun target-input-words-for-alt-w2c-mappings(cpl-triples)
  (let ((debug  t)
	(result nil))
    (dolist (entry (sieve-triple-list-having-relation '|input-word| cpl-triples))
      (let* ((instance      (triple-head entry))
	     (word          (cadr  (triple-tail entry)))
	     (pos           (caddr (triple-tail entry)))
	     (subgraph      (extract-non-instance-triples
			     (ps-get-subgraph 
			      instance 
			      (remove-irrelevant-km-triples
			       cpl-triples)))))
	(if debug 
	    (format t "~a has ~a triples for subgraph~%" instance
		    (length subgraph)))
	(cond ((member instance (all-instances '|Unit-of-Measurement|))) ;;do nothing
	      ((isa instance '|Property-Value|))                         ;;do nothing
	      ;;We don't consider alt-w2c-mappings for isolated instances, e.g., Rock
	      ((zerop (length subgraph)))                                ;;do nothing
	      (t (push entry result)))))
    result))

(defun get-candidate-w2c-substitutions(target-lst alternative-candidate-lst)
  (mapcar 'flatten-to-triple-lst
	  (cross-product
	   (mapcar #'(lambda(y)
		       (mapcar #'(lambda(alternative)
				   (list y '|instance-of| alternative))
			       (cadr (assoc y alternative-candidate-lst))))
		   target-lst))))

(defun replace-instance-of-triples(target-lst triple-lst)
  (cond ((null target-lst) triple-lst)
	(t (let* ((target (car target-lst))
		  (frame  (triple-head target)))
	     (replace-instance-of-triples 
	      (cdr target-lst)
	      (cons target
		    (extract-triples-not-having-root-slot frame '|instance-of| triple-lst)))))))

(defun get-alternative-w2c-mappings-for-cpl-values(scenario compute-questions yes-no-questions)
  (let ((debug nil)
	(alt-scenario-lst (get-alternative-word2concept-mappings scenario 
								 yes-no-questions)))
    (if debug (format t "Considering ~a alternative scenarios~%" (length alt-scenario-lst)))
    (mapcar #'(lambda(candidate-scenario)
		(multiple-value-bind 
		    (alt-scenario alt-compute-questions 
				  alt-yes-no-questions 
				  orig->clone 
				  clone->orig)
		    (clone-cpl-values candidate-scenario compute-questions yes-no-questions)
		  (list alt-scenario alt-compute-questions alt-yes-no-questions)))
	    (let ((result
		   (remove-if 'triple-lst-violate-domain/range?
			      (mapcar #'(lambda(repl)
					  (replace-instance-of-triples repl
								       scenario))
				      alt-scenario-lst))))
	      (format t "BPS will use ~a alternative scenarios.~%" 
		      (length result))
	      result))))

(defun get-alternative-w2c-mappings-for-initial-viewpoint (scenario 
							   question-triple-list 
							   cpl-nl-triples)
  (get-scenario-with-alternate-w2c scenario cpl-nl-triples))

(defun get-scenario-with-alternate-w2c(scenario cpl-nl-triples)
  (if (and (boundp '*controller-allow-alternate-w2c*)
	   *controller-allow-alternate-w2c*)
      (let ((*logging* t))
	(bps-set-checkpoint 'get-scenario-with-alternate-w2c)
	(ps-assert-triples scenario t)
	(let ((result
	       (remove-if 'triple-lst-violate-domain/range?
			  (mapcar #'(lambda(repl)
				      (replace-instance-of-triples repl
								   scenario))
				  (get-alternative-word2concept-mappings 
				   (append scenario
					   cpl-nl-triples))))))
	  (bps-undo 'get-scenario-with-alternate-w2c)
	  result))))

;;a) need code to generate variety of triple-lst give alternatives [done] [done]
;;b) need code to check for domain/range violate given triple-lst or typed-triple-lst. [done]

;;Example #1
(defun testcase1()
  (let ((formulation "There is a throw.
  There is a second throw."))
  (multiple-value-bind
      (scenario compute-questions yes-no-questions cpl-nl-triples)  
      (ps-parse-question formulation)
    (get-alternative-w2c-mappings-for-cpl-values
     (append scenario cpl-nl-triples)
	     compute-questions yes-no-questions))))

;;Example #2
(defun testcase2()
   (let ((formulation
"A ball is thrown.
The mass of the ball is 100 g.
The initial x speed of the throw is 0 m/s.
The initial y speed of the throw is 20 m/s.
The initial y position of the throw is 1 m.
What is the initial speed of the throw?"))
  (multiple-value-bind
      (scenario compute-questions yes-no-questions cpl-nl-triples)  
      (ps-parse-question formulation)
    (get-alternative-w2c-mappings-for-cpl-values
     (append scenario cpl-nl-triples)
	     compute-questions yes-no-questions))))

;;Example #3... Involved metonymy and is broken on 03Jun08...
(defun testcase3()
  (let ((formulation
   "A child moves.
The initial speed of the child is 0 m/s.
The acceleration magnitude of the child is 2.5 m/s^2.
The duration of the move is 5 s.
What is the final speed of the child?"))
  (multiple-value-bind
      (scenario compute-questions yes-no-questions cpl-nl-triples)  
      (ps-parse-question formulation)
    (get-alternative-w2c-mappings-for-cpl-values
     (append scenario cpl-nl-triples)
	     compute-questions yes-no-questions))))

;;Example #3b... Alternative
(defun testcase3b()
  (let ((formulation
   "A child moves.
The initial speed of the move is 0 m/s.
The acceleration magnitude of the move is 2.5 m/s^2.
The duration of the move is 5 s.
What is the final speed of the move?"))
  (multiple-value-bind
      (scenario compute-questions yes-no-questions cpl-nl-triples)  
      (ps-parse-question formulation)
    (get-alternative-w2c-mappings-for-cpl-values
     (append scenario cpl-nl-triples)
	     compute-questions yes-no-questions))))

#|
(progn
  (global-situation)
  (km '(|Throw| |now-has| (|superclasses| (|Unobstruct|))))
  (km '(|initial-x-speed| |now-has| (|domain| (|Two-Dimensional-Move|))))
  (km '(|initial-x-speed-of| |now-has| (|range| (|Two-Dimensional-Move|))))
  (km '(|initial-y-speed| |now-has| (|domain| (|Two-Dimensional-Move|))))
  (km '(|initial-y-speed-of| |now-has| (|range| (|Two-Dimensional-Move|))))
  (km '(|initial-y-position| |now-has| (|domain| (|Two-Dimensional-Move|))))
  (km '(|initial-y-position-of| |now-has| (|range| (|Two-Dimensional-Move|))))
  (new-situation)
  )
|#

;;Include alternatives in problem-solving tree. mutate openlist and not include matching for root-viewpoint. Basically don't do matching if a viewpoint has a children

