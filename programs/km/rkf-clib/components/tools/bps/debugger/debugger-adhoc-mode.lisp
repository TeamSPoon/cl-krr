;;
;; $Id: debugger-adhoc-mode.lisp,v 1.21 2007/11/11 05:08:05 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun adhoc-command (operands)
  (progn
    (setq *ADHOC-MODE* (not *ADHOC-MODE*))
    (if *ADHOC-MODE* "Entering adhoc mode"
                     "Exiting adhoc mode")))

(defun parse-adhoc-command (input)
  (if (not *ADHOC-MODE*)
      "Error: Command not available outside adhoc mode."
      (let* ((command (car input)))
	(cond ((equal '|try|     command)   (try-command       (cdr input)))
	      ((equal '|pick|    command)   (pick-command      (cdr input)))
	      ((equal '|unpick|  command)   (unpick-command    (cdr input)))))))

(defun pick-command (operands)
  (let* ((input     (intern (first operands)))
	 (selection (if (is-viewpoint-member-of-openlist input)
			input
		        (first (find-viewpoint-having-target  
				(get-viewpoint-target-pairs 
				 (extract-configs-from-openlist *OPENLIST*))
				(intern (first operands) :km)))))
	 (output-str ""))
    (if (is-viewpoint-member-of-openlist selection)
	(progn 
	  (mutate-current-vp-inst selection)
	  (mutate-closedlist (insert-at-end *CURRENT-VP-INST* *CLOSEDLIST*))
	  (mutate-current-node 
	   (first (get-nodes-containing-vp-inst-from-openlist selection)))
	  (mutate-openlist nil)  
	  (format t "Picked ~a~%" selection)
	  (reset-eq-solver)
	  (assert-viewpoint-triples *CURRENT-VP-INST*)
	  (if (funcall *PRUNING-CONDITION* *CURRENT-VP-INST*)
	      (format t "~a satisfies the pruning condition.~%" *CURRENT-VP-INST*)
  	      (format t "~a does not satisfy the pruning condition.~%" *CURRENT-VP-INST*))
	  (format t "~a has viewpoint-answer => ~a. ~%" 				
		  selection
		  (get-viewpoint-answer *CURRENT-VP-INST*))
	  (ps-km-query `(,*CURRENT-VP-INST* |now-has| (|viewpoint-child| ())))
	  (try-all-concepts-command)
	  (print-candidate-list))
      (setq output-str
	    (concatenate 'STRING 
			 output-str
			 (format nil "Error : Picking an invalid concept ~a~%" (first operands)))))
    (setq output-str
	  (concatenate 'STRING
		       output-str
		       (format nil "The current Viewpoint is ~a~%" *CURRENT-VP-INST*)))))

(defun unpick-command (operands)
  (let* ((output-str ""))
    (if (> (length *CLOSEDLIST*) 1)
	(let* ((new_CLOSEDLIST     (reverse (cdr (reverse *CLOSEDLIST*))))
	       (vp-inst            (first   (last new_CLOSEDLIST)))
	       (ancestry           (get-ancestors vp-inst))
	       (neutral-cost       (length ancestry)))
	  (progn 
	    (reset-eq-solver)
	    (mutate-closedlist      new_CLOSEDLIST)
	    (mutate-current-vp-inst vp-inst)
	    (mutate-current-node    (make-node vp-inst
					       ancestry
					       neutral-cost))
	    (ps-km-query `(,vp-inst |now-has| (|viewpoint-child| ())))
	    (try-all-concepts-command)
	    (print-candidate-list)
	    )))
    (setq output-str
	  (concatenate 'STRING
		       output-str
		       (format nil "The current Viewpoint is ~a.~%" *CURRENT-VP-INST*)))
    output-str
    )
)

(defun try-all-concepts-command()
  (let ((candidate-vps (expand-viewpoint *CURRENT-VP-INST* *ALL-USER-DEFINED-CONCEPTS*)))
    (mutate-openlist 
     (make-nodes-from-candidate-vps candidate-vps
				    *HEURISTIC*))))

(defun try-concept-list-command (operands)
  (let ((candidate-vps (expand-viewpoint *CURRENT-VP-INST* 
					 operands)))
    (mutate-openlist 
     (make-nodes-from-candidate-vps *CURRENT-NODE*
				    *HEURISTIC*
				    candidate-vps))))

(defun try-command (operands)
  (progn 
    (ps-km-query `(,*CURRENT-VP-INST* |now-has| (|viewpoint-child| ())))
    (cond ((or (null operands)
	       (equal '|all| (first operands)))
	   (try-all-concepts-command))
	  (t (try-concept-list-command operands)))
    (print-candidate-list)))

(defun print-candidate-list()
  (progn
    (format t "~%")
    (print-viewpoint-target-pairs 
     (get-viewpoint-target-pairs 
      (extract-configs-from-openlist *OPENLIST*)))))
  
;;For a list of viewpoint target pairings. Dumping it to screen.
;;P.S. the triples worked with are not asserted in KM
(defun print-viewpoint-target-pairs(vp-target-pairings)
  (progn 
    (if (not (null vp-target-pairings))
	(output-candidate-list-table 
	 (mapcar #'(lambda (x)
		     (let* ((concept (nth 1 x))
			   (vp-inst (nth 0 x))
			   (score-vector   (cdr (car (nth 2 x))))
			   (coverage (car score-vector))
			   (quality  (cadr score-vector)))
		      (format nil "~a(~a) ~a ~a" 
			      concept
			      vp-inst
			      coverage
			      quality)))
		 vp-target-pairings)))
    (format t "~%")))

;;Converts all elements in a flat-list to be of String type.
(defun convert-all-elements-to-string (element-list)
  (mapcar #'(lambda (element)
	      (cond ((null element)
		     "")
		    ((stringp element) 
		     element)
		    (t (string element))))
	  element-list))
	      
;;Given a flat list of String elements, returns the longest string length.
(defun find-longest-length(string-set)
  (length (first (sort string-set #'(lambda (x y) (> (length x) (length y)))))))

(defun break-list-into-rows (flat-list row-count)
  (cond ((endp flat-list) ())
	((< (length flat-list) row-count) (list (convert-all-elements-to-string 
						 (append flat-list (make-list (- row-count 
										(length flat-list)))))))
	(t (cons (reverse (last (reverse flat-list) row-count))
		 (break-list-into-rows (nthcdr row-count flat-list) 
				       row-count)))))
	 
(defun output-candidate-list-table(try-candidate-list)
  (dolist (entry try-candidate-list)
    (format t "~a~%" entry)))

#|deprecated
(defun output-candidate-list-table(try-candidate-list)
  (let* ((name-list       (convert-all-elements-to-string try-candidate-list))
	 (longest-name    (find-longest-length name-list))
	 (name-table      (break-list-into-rows try-candidate-list 3))
	 (control-string  (format nil "~~:{~~&~~~aA  ~~~aA  ~~~aA~~}" longest-name longest-name longest-name)))
    (format t control-string name-table)))
|#

(defun find-viewpoint-having-target (vp-target-pairings target)
  (remove nil (mapcar #'(lambda (x)
			  (let ((vp-inst        (nth 0 x))
				(mapped-concept (nth 1 x))
				(score          (nth 2 x)))
			    (if (equal mapped-concept target)
				vp-inst)))
			vp-target-pairings)))

;;Given a Viewpoint instance, determine the KM concept type for the
;;Viewpoint target. 
;;P.S. the triples worked with are not asserted in KM
(defun get-concept-type-for-viewpoint-target (vp-inst)
  (let* ((cpl-model-graph 
	  (strip-triple-prefix 
	   (get-viewpoint-model vp-inst)))
	 (vp-target (first (get-viewpoint-target vp-inst))))
    (list vp-inst
	  vp-target
	  (get-viewpoint-score vp-inst))))
    

;;For a list of viewpoint instances, determine its viewpoint-targets
;;P.S. the triples worked with are not asserted in KM
(defun get-viewpoint-target-pairs(vp-insts)
    (mapcar #'(lambda (x)
		(get-concept-type-for-viewpoint-target x))
	    vp-insts))

;;Given a set of CPL triples, determine the concept-type for some instance name
;;P.S. the triples worked with are not asserted in KM
(defun find-concept-type-for-instance (cpl-triples instance)
  (let* ((triple (car cpl-triples)))
    (if (null cpl-triples) 
	nil
        (if (and (eq (triple-head triple) instance)
		 (eq (triple-relation triple) '|instance-of|))
	    (triple-tail triple)
	    (find-concept-type-for-instance (cdr cpl-triples) instance)))))
