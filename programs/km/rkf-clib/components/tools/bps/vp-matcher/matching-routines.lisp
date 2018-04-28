;;
;; $Id: matching-routines.lisp,v 1.6 2008/10/31 15:48:38 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Given a concept determine if it is a candidate in match-result-list
(defun is-match-for-viewpointp (concept match-result-list)
  (member concept match-result-list :test #'(lambda (concept match-result) 
					      (equal concept (first match-result)))))

;;Finds candidates that match up with viewpoint instance  
(defun find-matches-for-viewpoint (vp-inst)
  (match-triples-to-candidates (get-simplified-context-for-viewpoint vp-inst) 
			       (union *all-user-defined-concepts* 
				      (get-ut-pump-priming-concepts)
				      :test #'eql)))

(defun match-triples-to-candidates (concept-triples candidate-concept-list &optional (extraction-depth 3))
  (let ((result          nil))
    (reset-semantic-matcher)
    (dolist (candidate-concept candidate-concept-list)
      (let* ((root+graph       (get-km-prototype-def candidate-concept))
	     (candidate-root   (first  root+graph))
	     (candidate-graph  (second root+graph)))
	(if (and candidate-root 
		 (not (degenerate-encoding-p candidate-graph)))
	    (progn
	      (clrhash *km-to-node-mappings*)
	      (clrhash *node-to-km-mappings*)
	      (let* ((peter-question-graph  (convert-to-peters-form 
					     (ps-clone-triple-list concept-triples)))
		     (peter-candidate-graph (convert-to-peters-form 
					     (remove-irrelevant-km-triples candidate-graph)))
		     (match-result (kb-match peter-question-graph
					     peter-candidate-graph
					     2 'weak nil nil nil))
		     temp-matches)
		(dolist (mappings match-result)
		  (hash-match-mappings mappings *htable-mappings-for-g1* *htable-mappings-for-g2*)
		  (if (not (check-for-inconsistent-mappings mappings *htable-mappings-for-g1* *htable-mappings-for-g2*))
		      (setf temp-matches (cons mappings temp-matches))))
		(setf match-result temp-matches)
		(if (> (length match-result) 1)
		    (setf match-result (find-most (combine-disjoint-mappings match-result) :test #'> :form #'number-of-matched-triples))
		  (setf match-result (first match-result)))
		(if match-result 
		    (setf result (cons (list candidate-concept 
					     (compute-match-score match-result (length peter-question-graph)))
				       result)))
		)))))
    (sort result #'(lambda(x y) (> (cadr x) (cadr y))))))
