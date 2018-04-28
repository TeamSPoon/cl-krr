;;
;; $Id: ps-kb-match.lisp,v 1.13 2009/06/06 19:07:51 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun invert-triple-map-listing(input)
  (mapcar #'(lambda(x)
	      (multiple-value-bind 
		  (lhs rhs score)
		  (parse-idiot-kb-match-result-entry x)
		(cons (list rhs lhs) score)))
	  input))

(defun parse-idiot-kb-match-result-entry(x)
  (let ((lhs   (caar x))
	(rhs   (cadr (car x)))
	(score (cdr x)))
    (values lhs rhs score)))

;;See if the 1st ply of a concept graph matches scenario.
(defun idiot-kb-match-1-ply(source-graph target-graph target-root)
  (multiple-value-bind
      (triple-map-list mapping-list taxonomic-score)
      (PS-PERFORM-SEMANTIC-MATCH-WITHOUT-USING-ABLATION
       source-graph
       (get-immediate-triples-for-instance target-root target-graph)
       :match-type? 'weak)
    (let ((*allow-sibling-match* nil))
      (idiot-kb-match-n-ply
       triple-map-list
       mapping-list
       source-graph
       target-graph
       target-root))))

;;See if deeper plys of concept graph
(defun idiot-kb-match-n-ply(triple-map-list ;; matching triples from previous ply
			    mapping-list    ;; mapping list from previous ply
			    source-graph    
			    target-graph 
			    target-root)
  (if (not (null triple-map-list))
      (multiple-value-bind 
	  (triple-map-list-head mapping-list-head)
	  (idiot-kb-match-n-ply-aux (car triple-map-list) 
				    mapping-list
				    source-graph 
				    target-graph 
				    target-root)
	(multiple-value-bind
	    (triple-map-list-tail mapping-list-tail)
	    (idiot-kb-match-n-ply (cdr triple-map-list) mapping-list source-graph target-graph target-root)
	  (values (append triple-map-list-head triple-map-list-tail)
		  (append mapping-list-head mapping-list-tail))))))

(defun extract-relevant-instance-mapping-for-triple-map (triple-mapping instance-mapping-list)
  (let ((source-triple (car  (car triple-mapping))))
    (values 
     (assoc (triple-head source-triple) instance-mapping-list)
     (assoc (triple-tail source-triple) instance-mapping-list))))

(defun idiot-kb-match-n-ply-aux(triple-mapping 
				instance-mapping-list 
				source-graph 
				target-graph 
				target-root 
				&optional 
				(verbose nil))
  (multiple-value-bind
      (triple-head-instance-mapping triple-tail-instance-mapping)
      (extract-relevant-instance-mapping-for-triple-map triple-mapping instance-mapping-list)    
    (if verbose (format verbose "Checking ~a with ~a and ~a~%" 
			triple-mapping 
			triple-head-instance-mapping 
			triple-tail-instance-mapping))
    (multiple-value-bind 
	(new-triple-map-list1 new-mapping-list1)
	(idiot-kb-match-check-subgraph triple-head-instance-mapping source-graph target-graph target-root)
      (multiple-value-bind 
	  (new-triple-map-list2 new-mapping-list2)
	  (idiot-kb-match-check-subgraph triple-tail-instance-mapping source-graph target-graph target-root)
	(cond ((and (not (null new-mapping-list1))
		    (or (equal triple-tail-instance-mapping '(nil nil)) ;; Special case for non-node bindings, e.g., (:seq (:pair 2 H) (:pair 1 O))
			(not (null new-mapping-list2))))
	       (progn
		 (if verbose (format verbose " Good!~%"))
		 (values
		  (cons triple-mapping (append new-triple-map-list1 new-triple-map-list2))
		  (append new-mapping-list1 new-mapping-list2))))
	      (t (progn (if verbose (format verbose " Bad!~%"))
			nil)))))))

(defun idiot-kb-match-check-subgraph(instance-mapping 
				     source-graph 
				     target-graph 
				     target-root 
				     &optional (verbose nil))
  (let ((source-graph-instance      (car  instance-mapping))
	(target-graph-instance      (cadr instance-mapping)))
    (cond ((equal target-root target-graph-instance) ;;why?
	   (values nil (list instance-mapping)))
	  (t
	   (let ((new-source-graph (extract-all-forward-pointing-triples-for-instance source-graph-instance source-graph))
		 (new-target-graph (extract-all-forward-pointing-triples-for-instance target-graph-instance target-graph)))
	     (cond ((all-instance-triple-p new-source-graph) ;; source is a skolem... applicable. 
		    (values nil	(list instance-mapping)))
		   ((all-instance-triple-p new-target-graph) ;; target is a skolem... applicable. why?
		    (values nil (list instance-mapping)))
		   #|
		   ((all-instance-triple-p new-target-graph)               ;; target is a skolem... We need to check if source contains anything fishy...
		    (let* ((target-instance-triple (car new-target-graph)) ;; should we introduce additional triples for matching skolem target subgraphs?
			   (concept-type           (triple-tail target-instance-triple)))
		      (cond ((property-value-p concept-type)               ;; Don't check for property-values. Assume good instance mapping.
			     (values nil (list instance-mapping)))
			    (t (let ((source-graph-instance-differ-from-skolem? (not (null (specialized-p new-source-graph source-graph-instance)))))
				 (if verbose (format t "Checking if ~a differs with contents of ~a skolem => ~a ~%" source-graph-instance concept-type source-graph-instance-differ-from-skolem?))
				 (if source-graph-instance-differ-from-skolem? nil
				   (values nil (list instance-mapping))))))))
		   |#
		   (t (idiot-kb-match-1-ply new-source-graph   ;; continue matching...
					    new-target-graph
					    target-graph-instance))))))))

(defun idiot-kb-match-result-entry=(x y)
  (multiple-value-bind 
      (lhs-x rhs-x score-x)
      (parse-idiot-kb-match-result-entry x)
    (multiple-value-bind 
	(lhs-y rhs-y score-y)
	(parse-idiot-kb-match-result-entry y)
      (and (equal lhs-x lhs-y)
	   (equal rhs-x rhs-y)
	   (equal score-x score-y)))))

;;;New semantic matching code specific for BPS.
(defun idiot-kb-match0(source-graph target-graph target-root)
  (multiple-value-bind 
      (triple-map instance-map)
      (idiot-kb-match-1-ply source-graph target-graph target-root)
    (let* ((instance-map-listing (remove-duplicates instance-map :test 'equal))
	   (triple-map-listing
	    (remove-invalid-triple-map-entries 
	     (remove-duplicates (remove nil triple-map) :test 'equal)
	     instance-map-listing)))
      (values 
       triple-map-listing
       instance-map-listing 
       (ps-compute-taxonomic-match-score triple-map-listing)))))


(defun order-instance-lst-by-immediate-subgraph-size(instance-lst triple-lst)
  (sort (copy-list instance-lst)
	#'(lambda(x y)
	    (> (length (get-immediate-triples-for-instance x triple-lst))
	       (length (get-immediate-triples-for-instance y triple-lst)))))
)

(defun idiot-kb-match(input-source-graph input-target-graph &optional(verbose nil))
  (let ((debug nil)
	(source-graph (remove-reflexive-triples-from-triple-list input-source-graph))
	(target-graph (remove-reflexive-triples-from-triple-list input-target-graph))
	(triple-map-listing   nil)
	(instance-map-listing nil))
    (if debug
	(progn 
	  (format t "source-graph roots(~a): ~a~%" 
		  (length (extract-all-non-property-value-root-instances-from-triple-list source-graph))
		  (extract-all-non-property-value-root-instances-from-triple-list source-graph))
	  (format t "target-graph roots(~a): ~a~%" 
		  (length (extract-all-non-property-value-root-instances-from-triple-list target-graph))
		  (extract-all-non-property-value-root-instances-from-triple-list target-graph))))
    (dolist (x 
	     (ORDER-INSTANCE-LST-BY-IMMEDIATE-SUBGRAPH-SIZE
	     (extract-all-non-property-value-root-instances-from-triple-list target-graph)
	     target-graph))
      (multiple-value-bind
	  (source-graph target-graph)
	  (ps-remove-mapped-triples source-graph target-graph triple-map-listing)
	(multiple-value-bind
	    (tmp-triple-map-listing tmp-instance-map-listing tmp-taxonomic-score)
	    (IDIOT-KB-MATCH0 source-graph target-graph x)
	  (if debug (format t "source-graph + target-graph(~a) => ~a~%"		  x		  triple-map-listing))
	  (if (valid-additional-match-detail?  triple-map-listing instance-map-listing
					   tmp-triple-map-listing tmp-instance-map-listing)
	      (progn
		(setq triple-map-listing   (union triple-map-listing   tmp-triple-map-listing   :test 'idiot-kb-match-result-entry=))
		(setq instance-map-listing (union instance-map-listing tmp-instance-map-listing :test 'equal)))))))
    (dolist (x 
	     (ORDER-INSTANCE-LST-BY-IMMEDIATE-SUBGRAPH-SIZE
	     (extract-all-non-property-value-root-instances-from-triple-list source-graph)
	     source-graph))
      (multiple-value-bind
	  (source-graph target-graph)
	  (ps-remove-mapped-triples source-graph target-graph triple-map-listing)
	(multiple-value-bind
	    (tmp-triple-map-listing tmp-instance-map-listing tmp-taxonomic-score)
	    (IDIOT-KB-MATCH0 target-graph source-graph x)
	  (if debug (format t "target-graph + source-graph(~a) => ~a~%"		  x		  tmp-triple-map-listing))
	  (if (valid-additional-match-detail?  triple-map-listing instance-map-listing
					       (invert-triple-map-listing tmp-triple-map-listing) (invert-map tmp-instance-map-listing))
	      (progn
		(setq triple-map-listing   (union triple-map-listing   (invert-triple-map-listing tmp-triple-map-listing) :test 'idiot-kb-match-result-entry=))
		(setq instance-map-listing (union instance-map-listing (invert-map tmp-instance-map-listing) :test 'equal)))))))
    (values triple-map-listing instance-map-listing (ps-compute-taxonomic-match-score triple-map-listing))))

;;Checks additional match-details, e.g., multirooted matches to make sure matched
;;instances have a 1-to-1 mapping. Returns true if instance has an existing mapping 
;;that is different.
(defun valid-additional-match-detail?(cur-triple-map-listing
				      cur-instance-map-listing
				      new-triple-map-listing
				      new-instance-map-listing)
  (let ((predicate ()))
    (dolist (instance-map-entry new-instance-map-listing)
      (push (bad-instance-map-entry? instance-map-entry cur-instance-map-listing)
	    predicate))
    (null (member t predicate))))

;;instances have a 1-to-1 mapping. Returns true if instance has an existing mapping that is different.
(defun bad-instance-map-entry?(instance-map-entry cur-instance-map-listing)
  (let ((src  (nth 0 instance-map-entry))
	(dest (nth 1 instance-map-entry)))
    (let ((src-mapping?    (assoc src  cur-instance-map-listing))
	  (dest-mapping?  (assoc dest (invert-map cur-instance-map-listing))))
      (or (and (not (null src-mapping?))
	       (not (equal instance-map-entry src-mapping?)))
	  (and (not (null dest-mapping?))
	       (not (equal instance-map-entry (reverse dest-mapping?))))))))

(defun ps-remove-mapped-triples(source-graph target-graph triple-map-listing)
  (cond ((null triple-map-listing) (values source-graph target-graph))
	(t (let* ((triple-map           (caar   triple-map-listing))
		  (mapped-source-triple (first  triple-map))
		  (mapped-target-triple (second triple-map)))
	     (ps-remove-mapped-triples (set-difference source-graph (list mapped-source-triple) :test 'ps-triple-equal)
				       (set-difference target-graph (list mapped-target-triple) :test 'ps-triple-equal)
				       (cdr triple-map-listing))))))

#|
(defun testcase-1()
  (idiot-kb-match
   '((|_Thing1| |instance-of| |Thing|)
     (|_Container1| |instance-of| |Entity|)
     (|_Thing1| |is-part-of| |_Container1|))
   '((|_Thing2| |instance-of| |Thing|)
     (|_Container2| |instance-of| |Entity|)
     (|_Thing2| |is-part-of| |_Container2|))))

(defun testcase-2()
  (ps-perform-semantic-match-without-using-ablation
   '((|_srcPerson| |instance-of| |Person|)
     (|_srcCompany| |instance-of| |Company|)
     (|_srcPerson| |is-part-of| |_srcCompany|))
   '((|_destPerson| |instance-of| |Person|)
     (|_destCompany| |instance-of| |Company|)
     (|_destTeam| |instance-of| |Team|)
     (|_destTeam| |is-part-of| |_destCompany|)
     (|_destPerson| |is-part-of| |_destTeam|))))

(defun testcase-3()
  (ps-perform-semantic-match-without-using-ablation
   '((|_destPerson| |instance-of| |Person|)
     (|_destCompany| |instance-of| |Company|)
     (|_destTeam| |instance-of| |Team|)
     (|_destTeam| |is-part-of| |_destCompany|)
     (|_destPerson| |is-part-of| |_destTeam|))
   '((|_srcPerson| |instance-of| |Person|)
     (|_srcCompany| |instance-of| |Company|)
     (|_srcPerson| |is-part-of| |_srcCompany|))))

;;demonstrates flexible matching
(defun testcase-4()
  (ps-perform-semantic-match-without-using-ablation
   '((|_Event| |instance-of| |Event|)
     (|_Entity1| |instance-of| |Entity|)
     (|_Entity2| |instance-of| |Entity|)
     (|_Event| |object| |_Entity1|)
     (|_Entity1| |has-part| |_Entity2|))
   '((|_xEvent| |instance-of| |Event|)
     (|_xEntity2| |instance-of| |Entity|)
     (|_xEvent| |object| |_xEntity2|))))

;;No flexible matching 
(defun testcase-5()
  (ps-perform-semantic-match-without-using-ablation
   '((|_Event| |instance-of| |Event|)
     (|_Entity2| |instance-of| |Entity|)
     (|_Event| |object| |_Entity2|))
   '((|_Event| |instance-of| |Event|)
     (|_Entity1| |instance-of| |Entity|)
     (|_Entity2| |instance-of| |Entity|)
     (|_Event| |object| |_Entity1|)
     (|_Entity1| |has-part| |_Entity2|))))

(defun idiot()
(testcase-2))

(defun bad()
(IDIOT-KB-MATCH 
 '((|_Move16_c511| |speed|                         |_Initial Speed29_c511|)
   (|_Move27_c511| |initial-speed|                         |_Initial Speed29_c511|)
   (|_Move27_c511| |distance| |_Distance47_c511|)
   (|_Move27_c511| |duration| |_Duration40_c511|)
   (|_Move27_c511| |final-speed|                         |_Final Speed32_c511|)
   (|_Sum52_c511| |input| |_Distance47_c511|)
   (|_Sum52_c511| |input| |_Distance49_c511|)
   (|_Duration40_c511| |value|                         (:|pair| 2 |*second|))
   (|_Final Speed32_c511| |value|                         (:|pair| 28 |*meter-per-second|))
   (|_Duration20_c511| |value|                         (:|pair| 10 |*second|))
   (|_Initial Speed29_c511| |value|                         (:|pair| 4 |*meter-per-second|))
   (|_Move16_c511| |duration| |_Duration20_c511|)
   (|_Move16_c511| |distance| |_Distance49_c511|)
   (|_Initial Speed29_c511| |instance-of|                         |Speed-Value|)
   (|_Move27_c511| |instance-of|                         |Motion-with-constant-velocity|)
   (|_Distance47_c511| |instance-of|                         |Length-Value|)
   (|_Duration40_c511| |instance-of|                         |Duration-Value|)
   (|_Final Speed32_c511| |instance-of|                         |Speed-Value|)
   (|_Sum52_c511| |instance-of| |Sum|)
   (|_Duration20_c511| |instance-of|                         |Duration-Value|)
   (|_Move16_c511| |instance-of|                         |Motion-with-constant-velocity|)
   (|_Distance49_c511| |instance-of|                         |Length-Value|))
 '((|_Speed-Value89_c232| |instance-of|                         |Speed-Value|)
   (|_Speed-Value123_c232| |instance-of|                         |Speed-Value|)
   (|_Length-Value94_c232| |instance-of|                         |Length-Value|)
   (|_Duration-Value85_c232| |instance-of|                         |Duration-Value|)
   (|_Motion-with-constant-velocity16_c232|                         |instance-of| |Motion-with-constant-velocity|)
   (|_Speed-Value127_c232| |instance-of|                         |Speed-Value|)
   (|_Motion-with-constant-velocity16_c232|                         |speed| |_Speed-Value89_c232|)
   (|_Motion-with-constant-velocity16_c232|                         |initial-speed| |_Speed-Value123_c232|)
   (|_Motion-with-constant-velocity16_c232|                         |distance| |_Length-Value94_c232|)
   (|_Motion-with-constant-velocity16_c232|                         |duration| |_Duration-Value85_c232|)
   (|_Motion-with-constant-velocity16_c232|                         |final-speed| |_Speed-Value127_c232|)))
)

(defun bad2()
 (VALID-ADDITIONAL-MATCH-DETAIL?
  '((((|_Move16_c511| |speed| |_Initial Speed29_c511|)
            (|_Motion-with-constant-velocity16_c232| |speed|
             |_Speed-Value89_c232|))
           . 1)
          (((|_Move16_c511| |distance| |_Distance49_c511|)
            (|_Motion-with-constant-velocity16_c232| |distance|
             |_Length-Value94_c232|))
           . 1)
          (((|_Move16_c511| |duration| |_Duration20_c511|)
            (|_Motion-with-constant-velocity16_c232| |duration|
             |_Duration-Value85_c232|))
           . 1))
  '((|_Initial Speed29_c511| |_Speed-Value89_c232|)
          (|_Distance49_c511| |_Length-Value94_c232|)
          (|_Move16_c511| |_Motion-with-constant-velocity16_c232|)
          (|_Duration20_c511| |_Duration-Value85_c232|))
  '((((|_Move27_c511| |final-speed| |_Final Speed32_c511|)
            (|_Motion-with-constant-velocity16_c232| |final-speed|
             |_Speed-Value127_c232|))
           . 1)
          (((|_Move27_c511| |initial-speed| |_Initial Speed29_c511|)
            (|_Motion-with-constant-velocity16_c232| |initial-speed|
             |_Speed-Value123_c232|))
           . 1))
  '((|_Final Speed32_c511| |_Speed-Value127_c232|)
          (|_Move27_c511| |_Motion-with-constant-velocity16_c232|)
          (|_Initial Speed29_c511| |_Speed-Value123_c232|))))

|#

;;stub!
(defun idiot-kb-match(input-source-graph input-target-graph &optional(verbose nil))
  (ps-perform-semantic-match-without-using-ablation input-source-graph input-target-graph))

