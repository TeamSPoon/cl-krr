;;
;; $Id: whynot-util.lisp,v 1.10 2008/08/20 18:54:27 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------
;;;; File: whynot-util.lisp
;;;;
;;;; This file provides the why not facility for the problem solver.
;;;; This module needs CPL to be loaded!
;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------

;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Top level function to explain why certain triples could not be
;;; matched.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: This function will take a viewpoint and give a reason for 
;;	 why a triple in the model graph was not matched. We have
;;	 only three reasons at the moment, and they are:
;;	   1) Different relations.
;;	   2) Different types.
;;	   3) No correspondence.
;;	 We may add more reasons (to enrich the explanation) if 
;;	 necessary.
;; INPUT: viewpoint = A viewpoint frame.
;; OUTPUT: 
;;----------------------------------------------------------------------
(defun compute-whynot-match-reasons (initial-graph concept)
  (let ((scenario-graph initial-graph)
	(irrelevant-relns 
	 (union '(|component| |equation-uses| |equation-expression| |equation-symbol|) 
		*triple-relations-to-ignore*))
	relations-to-extract 
	root+graph model-root model-graph
	mappings unmatched-scen-triples unmatched-model-triples 
	all-node-bindings 	  
	temp-result result)
    (setf relations-to-extract (append *vp-match-default-lookups* (include-subslots (extract-relations-in-graph-inv scenario-graph))))
    (setf model-root  (first (km0 `(|a| ,concept))))
    (setf model-graph (gather-graph model-root nil 3))
    ;; Match the scenario with the model.
    (setf scenario-graph (remove-irrelevant-triples-for-matching scenario-graph irrelevant-relns))
    (setf model-graph    (remove-irrelevant-triples-for-matching model-graph    irrelevant-relns))
    (setf mappings (ps-perform-semantic-match0 scenario-graph model-graph 'weak 2 nil nil))
    (setf unmatched-scen-triples  (get-complement mappings scenario-graph :pos 'source))
    (setf unmatched-model-triples (get-complement mappings model-graph 	  :pos 'target))
    (setf all-node-bindings 	  (get-all-node-bindings-for-mappings mappings))
    ;; Go through each unmatched triple and figure out the reason why it did not match.
    (dolist (unmatched-model-triple unmatched-model-triples)
      (cond
	((equal (triple-relation unmatched-model-triple) '|instance-of|))
	;; The node types are different.
	((setf temp-result (different-node-type-p unmatched-model-triple unmatched-scen-triples all-node-bindings))
	 (dolist (expln temp-result) 
	   (setf result (cons (list unmatched-model-triple expln 1) result))))
 	;; The node types are the same but the relations are different.
	((setf temp-result (different-reln-type-p unmatched-model-triple unmatched-model-triples unmatched-scen-triples all-node-bindings))
	 (dolist (expln temp-result) 
	   (setf result (cons (list unmatched-model-triple expln 2) result))))
	;; By default, there is no correspondence.
	(t
	 (setf result (cons (list unmatched-model-triple nil 0) result)))))
    (values result 
	    (extract-instance-triple-for-instance (extract-all-instances-from-triple-list unmatched-model-triples)
						  model-graph))))

;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Functions to determine the reason why a triple did not match.
;;; These are very preliminary implementations and will be built 
;;; upon in the future.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: This function checks whether a triple in the model graph could
;;	 not be matched b/c the type of one of its nodes is inconsistent.
;; INPUT: model-triple = An unmatched triple from the model graph.
;;	  unmatched-scenario-triples = A list of unmatched triples from
;;		the scenario.
;;	  all-node-bindings = A list of node bindings between the model
;;		and the scenario.
;; OUTPUT: A list of triples from the scenario could not be matched
;;	   because of a difference in the node types.
;;----------------------------------------------------------------------
(defun different-node-type-p (model-triple unmatched-scenario-triples all-node-bindings)
  (let (reln node-pair haystack result)
    ;; See if the triple in question is connected to a matched portion of the graph.
    (setf node-pair 
	  (or (first (member (triple-head model-triple) all-node-bindings :test #'(lambda (x y) (equal x (second y)))))
	      (first (member (triple-tail model-triple) all-node-bindings :test #'(lambda (x y) (equal x (second y)))))))
    ;; If so, then look for any unmatched triples in the scenario with the same relation!
    (cond
     (node-pair
      (setf reln (triple-relation model-triple))
      (setf haystack (find-all-triples-containing (first node-pair) unmatched-scenario-triples))
      (dolist (scenario-triple haystack)
	(if (or (eql reln (triple-relation scenario-triple)) (eql (invert-slot reln) (triple-relation scenario-triple)))
	    (setf result (cons scenario-triple result))))))
    result))

;;----------------------------------------------------------------------
;; DESC: 
;; INPUT:
;; OUTPUT:
;;----------------------------------------------------------------------
(defun different-reln-type-p (model-triple unmatched-model-triples unmatched-scenario-triples all-node-bindings)
  (let* ((head-node (triple-head model-triple))
	 (tail-node (triple-tail model-triple))
	 node1-type node2-type node-pair haystack result)
    ;; See if the triple in question is connected to a matched portion of the graph.
    (setf node-pair (first (member (triple-head model-triple) all-node-bindings :test #'(lambda (x y) (equal x (second y))))))
    (cond
     (node-pair
      (setf node1-type (get-cpl-instance-type tail-node unmatched-model-triples)))
     (t
      (setf node-pair (first (member (triple-tail model-triple) all-node-bindings :test #'(lambda (x y) (equal x (second y))))))
      (if node-pair (setf node1-type (get-cpl-instance-type head-node unmatched-model-triples)))))
    ;; If so, then look for triples that were unmatched because of different relations.
    (cond
     (node1-type
      (setf haystack (append (find-all-triples-with-head (first node-pair) unmatched-scenario-triples)
			     (mapcar #'(lambda (x) (invert-triple x))
				     (find-all-triples-with-tail (first node-pair) unmatched-scenario-triples))))
      (dolist (scenario-triple (remove-if #'(lambda (x) (equal (triple-relation x) '|instance-of|)) haystack))
	(setf node2-type (get-cpl-instance-type (triple-tail scenario-triple) unmatched-scenario-triples))
	(cond
	  ((or (tax-dist node1-type node2-type) (tax-dist node2-type node1-type))
	   (setf result (cons scenario-triple result)))))))
    result))


;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------
;;; Auxiliary functions to extract various pieces of information
;;; from a viewpoint and compute the complement.
;;;---------------------------------------------------------------------
;;;---------------------------------------------------------------------

;;----------------------------------------------------------------------
;; DESC: Given the mappings and a graph compute the complement.
;; INPUT: mappings = The matches between two graphs.
;;	  graph = The graph we want to compute the complement of.
;; OUTPUT: A list of triples.
;;----------------------------------------------------------------------
(defun get-complement (mappings graph &key pos)
  (let (matched-triple all-matched-triples)
    ;; Get all the matched triples.
    (dolist (mapping mappings)
      ;; Extract the appropriate matched triples.
      (cond
	((eql pos 'source) (setf matched-triple (get-mapping-source mapping)))
	((eql pos 'target) (setf matched-triple (get-mapping-target mapping)))
	(t		   (setf matched-triple nil)))
      ;; Save them.
      (if (path-p matched-triple)	
	  (setf all-matched-triples (append matched-triple all-matched-triples))
          (setf all-matched-triples (cons   matched-triple all-matched-triples))))
    ;; Get all the unmatched triples -- these are the complements.
    (set-difference graph all-matched-triples :test #'(lambda (x y) (or (equal x y) (equal x (invert-triple y)))))))


;;----------------------------------------------------------------------
;; DESC: Get the model graph from a viewpoint.
;; INPUT: viewpoint = A viewpoint frame.
;;	  relations-to-ignore (optional) = A list of relations to
;;		ignore -- i.e. we should not return these triples.
;; OUTPUT: A list of triples.
;;----------------------------------------------------------------------
(defun get-model-graph-from-vp (viewpoint &optional (relations-to-ignore *triples-to-ignore-for-matching*))
  (let ((model (km0 `(|the| |viewpoint-model-graph| |of| ,viewpoint))))
    ;; Remove the :triple keyword from each triple and remove any triples having a relation we said to ignore.
    (remove-if #'(lambda (triple) 
		  (member (triple-relation triple) relations-to-ignore
			  :test #'(lambda (x y) (or (equal x y) (equal x (invert-slot y))))))
    	       (mapcar #'(lambda (x) (rest x)) model))))

;;----------------------------------------------------------------------
;; DESC: Get the scenario graph from a viewpoint.
;; INPUT: viewpoint = A viewpoint frame.
;;	  relations-to-ignore (optional) = A list of relations to
;;		ignore -- i.e. we should not return these triples.
;; OUTPUT: A list of triples.
;;----------------------------------------------------------------------
(defun get-scenario-graph-from-vp (viewpoint &optional (relations-to-ignore *triples-to-ignore-for-matching*))
  (let ((scenario (km0 `(|the| |viewpoint-scenario-graph| |of| ,viewpoint))))
    ;; Remove the :triple keyword from each triple and remove any triples having a relation we said to ignore.
    (remove-if #'(lambda (triple) 
		  (member (triple-relation triple) relations-to-ignore
			  :test #'(lambda (x y) (or (equal x y) (equal x (invert-slot y))))))
    	       (mapcar #'(lambda (x) (rest x)) scenario))))

;;----------------------------------------------------------------------
;; DESC: Lookup the type of a given instance.
;; INPUT: 
;; OUTPUT: 
;;----------------------------------------------------------------------
(defun get-cpl-instance-type (cpl-instance triples)
  (let (instance-type)
    (dolist (triple triples)
      (if (and (eql '|instance-of| (triple-relation triple)) (eql cpl-instance (triple-head triple)))
	  (setf instance-type (cons (triple-tail triple) instance-type))))
    (if (eql (length instance-type) 1) (first instance-type) instance-type)))
