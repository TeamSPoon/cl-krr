;;
;; $Id: controller-expand.lisp,v 1.40 2009/08/27 23:07:08 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun expand(node 
	      vp-inst
	      openlist
	      closedlist 
	      queue_fn 
	      heuristic 
	      candidate-concepts)
  (if (< (get-viewpoint-depth vp-inst) *CONTROLLER-MAX-DEPTH*)
      (expand0 node 
	       vp-inst
	       openlist
	       closedlist
	       queue_fn
	       heuristic
	       candidate-concepts)))

;;stack-insert == DFS, sorted-queue-insert == BFS
;;FIXME: should support best-first as well.
;;Toggle should be a global BPS parameter.
(defun expand0(node 
	       vp-inst
	       openlist
	       closedlist 
	       queue_fn 
	       heuristic 
	       candidate-concepts)
  (let* ((candidate-vps   (expand-viewpoint vp-inst candidate-concepts))
	 (new-nodes       (make-nodes-from-candidate-vps candidate-vps 
							 heuristic 
							 node)))
    ;(sort-viewpoint-candidate-list (append new-nodes openlist)))) ;; for subgoaling. But dies due to node structure (see controller-search.lisp) instead of simply a list of viewpoints.
(if (equal queue_fn 'sorted-queue-insert)
    (sorted-queue-insert new-nodes openlist)
  (stack-insert new-nodes openlist))))

(defun make-nodes-from-candidate-vps(candidate-vps &optional
						   (heuristic   nil)
						   (parent-node nil))
  (mapcar #'(lambda (vp-inst) 
	      (if (and (not (null heuristic))
		       (not (null parent-node)))
		  (make-node vp-inst
			     (insert-at-end vp-inst (get-history parent-node))
			     (calculate-cost heuristic parent-node vp-inst))
		  (new-node vp-inst)))
	  candidate-vps))

;;new version.
;;No longer clones triple-lst. So mapping between instances for parent and child viewpoint is preserved.
(defun expand-viewpoint(vp-inst 
			input-candidate-concepts
			&optional (verbose nil))
  (let ((candidate-vp-defns   (get-candidate-viewpoint-defns 
			       (get-simplified-context-for-viewpoint vp-inst)
			       (get-viewpoint-query vp-inst) 
			       vp-inst
			       input-candidate-concepts
			       (get-viewpoint-target vp-inst))))
    (if (not (null candidate-vp-defns))
	(progn
	  (if verbose (format t "BPS: ~a generated ~a candidate viewpoints.~%" 
			      vp-inst 
			      (length candidate-vp-defns)))
	  (mapcar #'(lambda (input-vp-defn) 
		      (first (ps-km-query input-vp-defn)))
		  candidate-vp-defns)))))

(defun update-query-slot-value(vp-query clone-mappings)
  (mapcar #'(lambda(vp-query-entry)
	      (update-individual-query-slot-entry vp-query-entry clone-mappings))
	  vp-query))

(defun update-individual-query-slot-entry (vp-query-entry clone-mappings)
  (multiple-value-bind
      (slotname query-frame expected-filler)
      (extract-query-components-from-vp-query-entry vp-query-entry)
    (let ((cloned-query-frame (find-clone query-frame clone-mappings)))
      (make-viewpoint-query-slot-entry slotname cloned-query-frame))))

;;If the total number of Viewpoints, e.g. sum of examined and 
;;to-be-examined viewpoints are less than viewpoint exploration bound, 
;;then it will call viewpoint-match to find candidate matches. 
;;FIXME - Save state and put it onto OPENLIST for Some form of continuations?
(defun get-candidate-viewpoint-defns (augmented-scenario 
				      query-slot-value 
				      vp-inst
				      candidate-concepts
				      vp-target-clone-list)
  (if (< (length *OPENLIST*) *CLOCK*)
      (viewpoint-match augmented-scenario
		       candidate-concepts
		       query-slot-value
		       vp-inst
		       vp-target-clone-list)))

(defun get-related-concepts(query-frame-concept input-candidate-list)
  (let* ((candidate-list (if (null input-candidate-list)
			     *all-user-defined-concepts*
			     input-candidate-list))
	 (ranked-list (rank-candidate-match-list query-frame-concept candidate-list)))
    (remove nil 
	    (mapcar #'(lambda (entry)
			(if (not (null (cdr entry)))
			    (car entry)))
		    ranked-list))))

(defun generate-ranked-candidate-match-list (query-frame candidate-list)
  (sort (mapcar #'(lambda (candidate)
		    (cons candidate (abs-tax-dist query-frame candidate)))
		candidate-list) #'(lambda(x y) 
				    (let ((lhs (cdr x))
					  (rhs (cdr y)))
				      (cond ((null lhs) nil)
					    ((null rhs) t)
					    (t (< (cdr x) (cdr y))))))))

(defun rank-candidate-match-list (query-frame-concept candidate-list)
  (sort (mapcar #'(lambda (candidate)
		    (let ((dist (abs-tax-dist query-frame-concept candidate)))
		      (cons candidate dist)))
		candidate-list)
	#'(lambda(x y) 
	    (let ((lhs (cdr x))
		  (rhs (cdr y)))
	      (cond ((null lhs) nil)
		    ((null rhs) t)
		    (t (< (cdr x) (cdr y))))))))

;;older version. Somehow we clone the triple-lst. I am unsure of the reason why we do it anymore.
;; (defun expand-viewpoint(vp-inst 
;; 			input-candidate-concepts
;; 			&optional (verbose nil))
;;   (let ((augmented-scenario (get-simplified-context-for-viewpoint vp-inst)))
;;     (multiple-value-bind
;; 	(cloned-triple-list orig->clone clone->orig)
;; 	(ps-clone-triple-list augmented-scenario)
;;       (let* ((vp-target-clone-list (find-clone-for-list (get-viewpoint-target vp-inst) 
;; 						       clone->orig))
;; 	    (query-slot-value     (update-query-slot-value (get-viewpoint-query vp-inst) 
;; 							   clone->orig))
;; 	    (candidate-vp-defns   (get-candidate-viewpoint-defns cloned-triple-list
;; 								 query-slot-value
;; 								 vp-inst
;; 								 input-candidate-concepts
;; 								 vp-target-clone-list)))
;; 	(if (not (null candidate-vp-defns))
;; 	    (progn 
;; 	      (if verbose (format t "BPS: ~a generated ~a candidate viewpoints.~%" 
;; 				  vp-inst 
;; 				  (length candidate-vp-defns)))
;; 	      (mapcar #'(lambda (input-vp-defn) 
;; 			  (first (ps-km-query input-vp-defn)))
;; 		      candidate-vp-defns)))))))

;; deprecated (defun determine-candidate-concepts(vp-inst input-candidate-concepts)
;;   (let ((candidate-threshold 100))
;;     (if (> (length input-candidate-concepts) candidate-threshold)
;; 	(let* ((1st-vp-query-entry (first (get-viewpoint-query vp-inst)))
;; 	       (query-frame-concept       (get-concept-for-kb-instance 
;; 					   (get-query-frame-in-vp-query-entry 
;; 					    1st-vp-query-entry))))
;; 	  (format t "BPS: More than ~s concepts in KB. Applying heuristic to prune candidates.~%"
;; 		  candidate-threshold)
;; 	  (get-related-concepts query-frame-concept 
;; 				input-candidate-concepts))
;;         input-candidate-concepts)))
    



