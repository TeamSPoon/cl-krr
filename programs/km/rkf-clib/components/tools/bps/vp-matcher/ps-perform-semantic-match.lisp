;;
;; $Id: ps-perform-semantic-match.lisp,v 1.11 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-perform-semantic-match-for-all-reaction-instances(triple-list)
  (let ((recognized-reaction (ps-perform-semantic-match-for-one-reaction-instance triple-list)))
    (cond ((null recognized-reaction) ())
	  (t (cons recognized-reaction
		   (ps-perform-semantic-match-for-all-reaction-instances 
		    (set-difference triple-list
				    (mapcar 'caar
					    (car recognized-reaction))
				    :test 'equal)))))))

(defun ps-perform-semantic-match-for-one-reaction-instance(triple-list)
  (let ((debug nil))
    (dolist (reaction-instance (ps-find-instances-of triple-list '|Reaction|))
      (progn 
	(if debug (format t "Identifying reaction ~A~%" reaction-instance))
	(multiple-value-bind
	 (successful? output)
	 (reaction-identify-me reaction-instance)
	 (if debug (format t "Identified reaction ~A as ~A~%" reaction-instance
			   (remove-subsumers (get-concept-for-kb-instance reaction-instance))))
	 (if successful?
	     (return-from ps-perform-semantic-match-for-one-reaction-instance
			  output)))))))

;;returns all match configurations involving lhs and rhs graphs
(defun ps-get-all-semantic-match-configurations (input-lhs-graph input-rhs-graph)
  (multiple-value-bind
      (triple-map-listing instance-map-listing avg-match-score)
      (ps-perform-semantic-match-without-using-ablation input-lhs-graph input-rhs-graph)
    (cond ((null avg-match-score) 
	   (progn
	     (format t "no matches found using ~A~%"
		     input-rhs-graph)
	     ()))
	  (t (cons (list triple-map-listing instance-map-listing avg-match-score)
		   (ps-get-all-semantic-match-configurations
		    input-lhs-graph
		    (set-difference input-rhs-graph 
				    (mappend #'(lambda(entry) 
						(let ((rhs-triple-lst (cadr (car entry))))
						  (if (atom (car rhs-triple-lst))
						      (list rhs-triple-lst)
						      rhs-triple-lst)))
					    triple-map-listing)
				    :test 'equal)))))))

;;Given earlier match-config, retry semantic matching on src-graph and concept-graph
;;returns differences.
;;Difference has to be an extension of earlier match-config.
(defun ps-find-additional-semantic-match-detail (src-graph
						 concept-graph
						 earlier-instance-mapping-lst
						 earlier-matched-src-triples
						 earlier-matched-concept-triples)
  (multiple-value-bind
   (triple-mapping instance-mapping avg-score)
   (PS-PERFORM-SEMANTIC-MATCH-without-using-ablation src-graph concept-graph)
   (let ((triple-mapping-without-scores (mapcar 'car triple-mapping)))
     (if (and (subsetp earlier-instance-mapping-lst instance-mapping :test 'equal))
	 (let ((additional-triple-map 
		(remove-if #'(lambda(entry)
			       (let ((src-triple (car entry))
				     (concept-triple (cadr entry)))
				 (not (null (member src-triple 
						    earlier-matched-src-triples
						    :test 'ps-triple-equal)))))
			   triple-mapping-without-scores))
	       (additional-instance-map
		(set-difference
		 instance-mapping 
		 earlier-instance-mapping-lst 
		 :test 'equal)))
	   (values
	    (list additional-triple-map additional-instance-map)
	    additional-triple-map
	    additional-instance-map))))))

;;get valid ablations
;;try semantic match on valid ablations to improve match.
;;Returns three values
;; a) match-config for regular match
;; b) additional-match-config for match using most general ablated src-graph
;; c) additional-match-config for match using src-graph ablated to taxonomy depth 0
;; d) additional-match-config for match using heuristic transformation rules.
;;By additional, we mean extra stuff not in regular match.
;;Thus (b) and (c) may have duplicate entries.
(defun ps-perform-semantic-match(src-graph concept-graph
				 &key
				 (matcher-type 'flexible-semantic-pzyeh)
				 (apply-transforms? t)
				)
    ;(hash-transformation-rules *domain-neutral-transformations*);;for testing
    (reset-semantic-matcher) ;;includes both neutral and heuristic rules.
    (multiple-value-bind
	(original-triple-mapping original-instance-mapping original-avg-score matched-src-triples matched-concept-triples)
	(PS-PERFORM-SEMANTIC-MATCH-without-using-ablation 
	 src-graph concept-graph
	 :matcher-type matcher-type
	 :apply-transforms? apply-transforms?)
     (values (if (not (null original-avg-score))
		 (list original-triple-mapping
		       original-instance-mapping
		       original-avg-score))
	       )))

;;predicate returns true is all km instances in src graph are mapped to something.
;;Thus all triples in src graph are matched.
(defun all-src-triples-matched?(input-src-graph match-configuration)
  (let ((debug nil)
	(config-triple-mapping-lst   (nth 0 match-configuration))
	(config-instance-mapping-lst (nth 1 match-configuration))
	(config-avg-mapping-score    (nth 2 match-configuration)))
    (null
     (set-difference 
      (extract-non-instance-triples input-src-graph)
     (mappend #'(lambda(config-triple-mapping)
		  (if debug (format t "config-triple-mapping~%~A~%" config-triple-mapping))
		  (let ((config-triple-lhs       (car  (car config-triple-mapping)))
			(config-triple-rhs       (cadr (car config-triple-mapping))))
		    (if (atom (car config-triple-lhs))
			(list config-triple-lhs) ;; matched exactly 1 rhs triple
		      config-triple-lhs)))       ;; matched 1+ rhs triples
	      config-triple-mapping-lst)
     :test 'ps-triple-equal))))

