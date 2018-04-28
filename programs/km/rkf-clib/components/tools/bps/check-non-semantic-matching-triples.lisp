;;
;; $Id: check-non-semantic-matching-triples.lisp,v 1.4 2009/06/22 02:14:14 jchaw Exp $
;;

;;Checks if two triple-subgraphs are unifiable
;;The triple-subgraphs contain triples that were not considered
;;for matching by the semantic matcher.
;;For example, small-v values.
(defun ps-ignored-triples-unify?(mapping 
				 graph1-ignored-triples
				 graph2-ignored-triples)
  (let* ((debug nil)
	 (*on-error* 'abort)
	 (root1 (car  mapping))
	 (root2 (cadr mapping))
    	 (ignored-subgraph1 
	  (ps-get-subgraph root1
			   graph1-ignored-triples))
    	 (ignored-subgraph2
	  (ps-get-subgraph root2
			   graph2-ignored-triples)))
    (or (and (null ignored-subgraph1) 
	     (null ignored-subgraph2)) 
	(handler-case
	 (let* ((new-root
		 (ps-instantiate-concept
		  (append
		   (quasi-get-concept-for-kb-instance root1
						      ignored-subgraph1)
		   (quasi-get-concept-for-kb-instance root2
						      ignored-subgraph2))))
		(graph (ps-clone-triple-list
			(replace-elements-in-list 
			 (append ignored-subgraph1 ignored-subgraph2)
			 (list (cons root1 new-root)
			       (cons root2 new-root))))))
		(ps-assert-triples graph debug)
	   (delete-frames-in-triple-lst graph)
	   t)
	 (error (condition)
		(progn
		  (if debug (format t "Rejecting mapping ~A with ~A~%" root1 root2))
		  nil))))))

#|
;;Need update for new API for (ps-perform-semantic-match ...)
;;Valid mapping
;; (defun testcase1()
;;   (let ((graph1 '((|_Move| |instance-of| |Move|)
;; 		  (|_Length-Value| |instance-of| |Length-Value|)
;; 		  (|_Length-Value| |distance-of| |_Move|)
;; 		  (|_Length-Value| |value| (:|pair| 20 |*meter|))))
;; 	(graph2 '((|_Move| |instance-of| |Move|)
;; 		  (|_Length-Value| |instance-of| |Length-Value|)
;; 		  (|_Length-Value| |distance-of| |_Move|)
;; 		  (|_Length-Value| |value| (:|pair| 20 |*meter|)))))
;;     (multiple-value-bind
;; 	(graph1-for-matching graph1-not-for-matching)
;; 	(ps-get-subgraph-for-matching (tag-instance "-A" graph1))
;;       (multiple-value-bind
;; 	  (graph2-for-matching graph2-not-for-matching)
;; 	  (ps-get-subgraph-for-matching (tag-instance "-B" graph2))
;; 	(multiple-value-bind
;; 	    (triple-map-listing instance-map score)
;; 	    (ps-perform-semantic-match 
;; 	     graph1-for-matching graph2-for-matching)
;; 	  (dolist (map instance-map)
;; 	    (ps-ignored-triples-unify? map
;; 				       graph1-not-for-matching
;; 				       graph2-not-for-matching)))))))

;; ;;Need update for new API for (ps-perform-semantic-match ...)
;; ;;Invalid mapping
;; (defun testcase2()
;;   (let ((graph1 '((|_Move| |instance-of| |Move|)
;; 		  (|_Length-Value| |instance-of| |Length-Value|)
;; 		  (|_Length-Value| |distance-of| |_Move|)
;; 		  (|_Length-Value| |value| (:|pair| 20 |*meter|))))
;; 	(graph2 '((|_Move| |instance-of| |Move|)
;; 		  (|_Length-Value| |instance-of| |Length-Value|)
;; 		  (|_Length-Value| |distance-of| |_Move|)
;; 		  (|_Length-Value| |value| (:|pair| 40 |*meter|)))))
;;     (multiple-value-bind
;; 	(graph1-for-matching graph1-not-for-matching)
;; 	(ps-get-subgraph-for-matching (tag-instance "-A" graph1))
;;       (multiple-value-bind
;; 	  (graph2-for-matching graph2-not-for-matching)
;; 	  (ps-get-subgraph-for-matching (tag-instance "-B" graph2))
;; 	(multiple-value-bind
;; 	    (triple-map-listing instance-map score)
;; 	    (ps-perform-semantic-match 
;; 	     graph1-for-matching graph2-for-matching)
;; 	  (dolist (map instance-map)
;; 	    (ps-ignored-triples-unify? map
;; 				       graph1-not-for-matching
;; 				       graph2-not-for-matching)))))))
|#

