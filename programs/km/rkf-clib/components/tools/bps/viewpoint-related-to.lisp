;;
;; $Id: viewpoint-related-to.lisp,v 1.14 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Predicate to determine if question is a related-to question-type
(defun related-to-question-p (cpl-scenario) 
  (let (result)
    (dolist (cpl-triple cpl-scenario)
      (if (and (eql (triple-relation cpl-triple) '|instance-of|) 
	       (member (triple-tail cpl-triple) '(|Be-Related|) :test #'eql))
	  (progn (setf result t) (return))))
    result))

;;extractor to get necessary participants in question triple.
(defun extract-related-to-pair(question)
  (let* ((question-instance (first question))
	 (object (ps-slot-lookup question-instance '|object|))
	 (source (first  object))
	 (target (second object)))
    ;;How is source related to target?
  (list source target)))

(defun viewpoint-related-to-question(scenario question)
  (progn
    (ps-assert-triples scenario)
    (let* ((question-participant (extract-related-to-pair question))
	   (source (first  question-participant))
	   (target (second question-participant))
	   (result-triples (do-spreading-activation source target)))
      (ps-assert-triples result-triples)
      (output-slot-query-viewpoint scenario
				   question
				   result-triples
				   source 
				   target)
)))

(defun do-spreading-activation(source target)
  (let ((debug nil)
	(*use-loosespeak*       t)
	(LS-input (list (list source '|related-to| target))))
    (ls-clear-cache)
    (format t "*use-loosespeak*(~a)~%" *use-loosespeak*)
    (setf *ls-cpl-mode*       t)
    (setf *ls-cpl-choices*    nil)
    (setf *ls-auto-selection* nil)
    (setf *ls-exhaustive-search* t)
    (setf *ls-stop-criteria* 'is_subclass)
    (setf *ls-search-depth-bound* 2)
    (let ((LS-output (remove-duplicates
		      (cdr 
		       (car 
			(ls-parse-triples
			 (second 
			  (multiple-value-list (ls-separate-triples LS-input))))))
		      :test 'equal)))
      (format t "spreading activation(~A):~%~A~%" (length LS-OUTPUT)
	      LS-OUTPUT)
      (mappend #'(lambda (km-assertion)
		   (km2triples km-assertion))
	       LS-output))))

(defun get-mapping-to-related-to-yn-query(query-provenance)
  (let* ((triple-mapping (nth 0 query-provenance))
	 (node-mapping   (nth 1 query-provenance))
	 (related-to-triple-lst nil)
	 (result nil))
    (dolist (triple-mapping-entry triple-mapping)
      (let* ((mapping (car triple-mapping-entry))
	     (score   (cdr triple-mapping-entry))
	     (lhs (car (listify-triple (car mapping)))) ;;assume exactly 1 triple
	     (rhs (listify-triple (cadr mapping))))
	(cond ((equal (triple-relation lhs) '|related-to|)
	       (progn 
		 (push lhs related-to-triple-lst)
		 (setf result (append result rhs)))))))
    (values related-to-triple-lst result)
))

(defun output-slot-query-viewpoint (scenario result-triples related-to-query-triple-lst input-vp-inst)
  (let ((debug nil)
	(viewpoint-query nil))
    (dolist (entry related-to-query-triple-lst)
      (push `((:|seq| (:|pair| |*slot-query| (:|triple| |Be-Related| 
					       ,(triple-head entry)
					       ,(triple-tail entry)))))
	    viewpoint-query)
      )
    (let ((result (car (ps-km-query 
			`(|a| |Slot-Query-Viewpoint| |with|
			  (|viewpoint-scenario| (,(cons ':|seq| (affix-triple-prefix scenario))))
			  (|viewpoint-target|   (,(cons ':|seq| (affix-triple-prefix result-triples))))
			  (|viewpoint-query|     ,viewpoint-query)
			  )
			debug))))
      (reinstall-viewpoint-answer-provenance 
       result input-vp-inst)
      result
      )
    )
  )

#|

(let ((scenario '((|_Heart| |instance-of| |Heart|)
		    (|_Heart-Pumping| |instance-of| |Heart-Pumping|)
		    (|_Heart| |instrument-of| |_Heart-Pumping|)
		    (|_Lung| |instance-of| |Lung|)))
      (kb (get-kb)))
  (ps-assert-triples scenario)
  (pprint (do-spreading-activation '|_Heart| '|_Lung|))
  (put-kb kb))

|#
