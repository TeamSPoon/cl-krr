;;
;; $Id: ps-triple-match.lisp,v 1.8 2009/06/06 19:07:51 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

#|
;;if strict match is null or < 1. Then attempt flexible-match. But if flexible-match is null, then simply
;;return results of strict match, which can be null or one with a taxonomic-score < 1.
(defun ps-triple-match(src-graph target-graph)
  (if (and (not (null src-graph))
	   (not (null target-graph)))
      (multiple-value-bind 
	  (strict-triple-map-list strict-instance-map-list strict-taxonomic-score)
	  (ps-strict-triple-match src-graph target-graph)
	(if (or (not (numberp strict-taxonomic-score))
		(< (abs strict-taxonomic-score) 1))
	    (multiple-value-bind 
		(flexible-triple-map-list flexible-instance-map-list flexible-taxonomic-score)
		(ps-flexible-triple-match src-graph target-graph)
	      (if (null flexible-taxonomic-score)
		  (values strict-triple-map-list strict-instance-map-list strict-taxonomic-score)
		(values flexible-triple-map-list flexible-instance-map-list flexible-taxonomic-score)))
	  (values strict-triple-map-list strict-instance-map-list strict-taxonomic-score)))))
|#

(defun ps-triple-match(src-graph target-graph)
  (let ((*allow-sibling-match* nil))
  (multiple-value-bind
      (triple-map-list instance-map-list taxonomic-score matching-type)
      (ps-triple-match-aux src-graph target-graph)
    #|
    (if (null triple-map-list)
	(let ((*allow-sibling-match* t))
	  (multiple-value-bind
	      (triple-map-list instance-map-list taxonomic-score matching-type)
	      (ps-triple-match-aux src-graph target-graph)
	    (values triple-map-list instance-map-list taxonomic-score matching-type 'sibling-match)))
    |#
    (values triple-map-list instance-map-list taxonomic-score matching-type 'non-sibling-match))))
    

;;if strict match is null or < 1. Then attempt flexible-match. But if flexible-match is null, then simply
;;return results of strict match, which can be null or one with a taxonomic-score < 1.
(defun ps-triple-match-aux(src-graph target-graph)
  (if (and (not (null src-graph))
	   (not (null target-graph)))
      (multiple-value-bind 
	  (strict-triple-map-list strict-instance-map-list strict-taxonomic-score)
	  (ps-strict-triple-match src-graph target-graph)
	(if (or (not (numberp strict-taxonomic-score))
		(< (abs strict-taxonomic-score) 1))
	    (multiple-value-bind 
		(flexible-triple-map-list flexible-instance-map-list flexible-taxonomic-score)
		(ps-flexible-triple-match src-graph target-graph)
	      (if (null flexible-taxonomic-score)
		  (values strict-triple-map-list strict-instance-map-list strict-taxonomic-score 'strict)
		(values flexible-triple-map-list flexible-instance-map-list flexible-taxonomic-score 'flexible)))
	  (values strict-triple-map-list strict-instance-map-list strict-taxonomic-score 'strict)))))

(defun ps-strict-triple-match (src-graph target-graph)
  (ps-perform-semantic-match-without-using-ablation
   src-graph
   target-graph 
   :match-type 'strong
   :depth 2 
   :discard-unit-matches? nil
   :only-consider-strongest? nil))

(defun ps-find-match-starting-point(pzyeh-src-graph pzyeh-target-graph)
  (let ((candidate (caar (remove nil (find-best-degenerate-match pzyeh-src-graph pzyeh-target-graph)))))
    (if (not (null candidate))
	(cadr (car (get-node-bindings-for-mapping candidate))))))

(defun ps-flexible-triple-match (src-graph target-graph)
  (progn 
    (reset-semantic-matcher)
    (let* ((pzyeh-src-graph      (ps-convert-to-semantic-matcher-form src-graph))
	   (pzyeh-target-graph   (ps-convert-to-semantic-matcher-form target-graph))
	   (possible-rewrite-lst (back-chain-on-consequent pzyeh-src-graph 3))
	   (starting-node        (ps-find-match-starting-point pzyeh-src-graph pzyeh-target-graph)))
      (if (not (null starting-node))
	  (progn 
	    (hash-graph-on-head pzyeh-src-graph    *htable-g1-indexed-by-head*)
	    (hash-graph-on-tail pzyeh-src-graph    *htable-g1-indexed-by-tail*)
	    (hash-graph-on-head pzyeh-target-graph *htable-g2-indexed-by-head*)
	    (hash-graph-on-tail pzyeh-target-graph *htable-g2-indexed-by-tail*)
	    (let ((match-result 
		   (mapcar #'(lambda(result)
			       (if (not (null result))
				   (cons (list pzyeh-src-graph
					       result) 1)))
			   (mappend #'(lambda(possible-rewrite)
					(find-subsumeable-pathes-for 
					 starting-node 
					 possible-rewrite 
					 *htable-g2-indexed-by-head*
					 *htable-g2-indexed-by-tail*
					 'strong))
				    possible-rewrite-lst))))
	      (let ((triple-map-listing (convert-match-result-to-triples match-result)))
		(values 
		 triple-map-listing
		 (generate-correspondence-pairs match-result)
		 (ps-compute-taxonomic-match-score triple-map-listing)))))))))