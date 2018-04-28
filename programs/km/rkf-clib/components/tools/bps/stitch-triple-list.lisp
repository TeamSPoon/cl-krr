;;
;; $Id: stitch-triple-list.lisp,v 1.2 2008/11/05 20:19:44 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;For simplicity...
;;Need to take into account other triples 
;;in source-graph. 
;;Two brittleness details.
;;They should be joined with relevant 
;;triples in concept-graph.
;;It does not do that currently.
;;It also assumes the kb-instance is an
;;instance of exactly 1 concept.
;;This can be tricky esp. when we need the match 
;;to be anchored at instance that is to be stitched.
;;Must be careful to return the same graph if no new 
;;info is added by stitching, e.g., Spatial-Entity 
;;only has an instance-of triple.
;;This is to prevent (my-ppsm ...) to infinitely loop
(defun ps-stitch(graph instance-lst)
  (cond ((null instance-lst) graph)
	((consp instance-lst)
	 (ps-stitch (ps-stitch graph 
			       (car instance-lst))
		    (cdr instance-lst)))
	(t 
	 (let* ((instance instance-lst)
		(concept
		 (car
		  (quasi-get-concept-for-kb-instance 
		   instance graph))))
	   (multiple-value-bind 
	       (concept-root concept-graph)
	       (ps-get-new-graph concept)
		 (remove-duplicates
		  (replace-elements-in-list
		   (append
		    graph
		    (ps-get-subgraph-for-matching
		     concept-graph))
		   (list `(,concept-root ,instance)))
		  :test 'ps-triple-equal)
)))))

;; (defun testme()
;;   (multiple-value-bind
;;       (root graph)
;;       (ps-get-graph '|Mitosis|)
;;     (ps-stitch graph '|_Anaphase32|)))

(defun find-stitchable-instances(triple-lst)
  (mappend #'(lambda(instance)
	      (let ((concept-lst (quasi-get-concept-for-kb-instance instance triple-lst)))
		(pprint concept-lst)
		(if (not (null (set-difference concept-lst *all-user-defined-concepts*)))
		    (list instance))))
	   (extract-all-km-instances triple-lst)))

;; (defun testme()
;;   (multiple-value-bind (root graph)
;;       (ps-get-graph '|Mitosis|)
;;     (find-stitchable-instances graph)))
	     
	  
