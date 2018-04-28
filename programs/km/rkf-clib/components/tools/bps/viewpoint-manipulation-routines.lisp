;;
;; $Id: viewpoint-manipulation-routines.lisp,v 1.53 2009/06/17 17:35:57 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-show-mapped-concepts(stream vp-inst)
  (let ((mapped-concepts (get-mapped-concepts vp-inst)))
    (if mapped-concepts
	(progn
	  (format stream "BPS: Mapped with ~a" (format-mapped-concepts mapped-concepts))
	  (format stream "~%")))))

(defun get-mapped-concepts(vp-inst 
			   &optional (verbose nil))
  (if (or (isa vp-inst '|Slot-Value-Viewpoint|)
	  (isa vp-inst '|Multislot-Value-Viewpoint|))
      (cdr (car (ps-km-query `(|the| |viewpoint-target| |of| ,vp-inst) verbose)))))

(defun format-mapped-concepts(mapped-concepts)
  (let ((stream (make-string-output-stream)))
    (if mapped-concepts
	(dolist (concept mapped-concepts)
	  (format stream "~a " concept))
      (format stream "Not mapped to any concepts"))
    (get-output-stream-string stream)))

;;Checks if symbol is a Viewpoint instance in the KB.
(defun kb-viewpointp(vp-inst)
  (not (null
	(and (known-frame vp-inst)
	     (ps-km-query `(,vp-inst |isa| |Viewpoint|))))))
	 
;;Determines if Viewpoint contains redundant mapping.
;;Consist of 2 tests.
;; a) Same target concept
;; b) Maps to same KB instances. Determined by visible naming property of all cloned KB-instances.
(defun redundant-viewpoint-mappingp(vp-inst)
  (if (not (root-viewpointp (car (get-viewpoint-parent vp-inst))))
  (let* ((vp-ancestry-mapping-list (get-ancestry-mapping-for-viewpoint vp-inst))
	 (own-mapping              (second (car (last vp-ancestry-mapping-list))))
	 (own-target-concept       (get-concept-for-kb-instance (get-viewpoint-target vp-inst))))
    (member t (mapcar #'(lambda (vp-ancestor-mapping)
			  (let* ((vp-ancestor-inst (first vp-ancestor-mapping)))
			    (if (not (null own-mapping))
				(and (equal (get-concept-for-kb-instance (get-viewpoint-target vp-ancestor-inst))
					    own-target-concept)
				     (subsetp own-mapping (second vp-ancestor-mapping) :test #'string-equal))
			    own-mapping)))
		      (remove-last vp-ancestry-mapping-list))))))

;;Returns the simplified mappings, i.e. without the _cNNN postfixes, for Viewpoints in lineage.
;;Each element in output list is a list of viewpoint instance and its simplified mappings.
(defun get-ancestry-mapping-for-viewpoint(vp-inst)
  (mapcar #'(lambda(target-vp)
	      (list target-vp 
		    (mapcar #'(lambda(mapping) 
				(let* ((target (intern (second mapping) :km)))
				  (ps-unclone-naively target)))
			    (get-viewpoint-correspondence target-vp))))
	      (flatten (get-ancestors vp-inst))))

;;Returns the set of triples making up context for Viewpoint
;;Aggregates similar relations.
;;Note: simplified triple-lst may not contain triples for answerable-queries.
;;does not include assumption triples.
(defun get-simplified-context-for-viewpoint(vp-inst)
    (get-context
     (get-viewpoint-scenario vp-inst)
     (get-viewpoint-model    vp-inst)
     (get-viewpoint-assumption-triples vp-inst)
     (convert-viewpoint-correspondence-to-assoc-list
      (get-viewpoint-correspondence vp-inst))))

;;Given scenario, model-graph, and their correspondence
;;returns triple-list resulting from the merging of scenario and model-graph.
;;merged model-graph instances are renamed with their scenario graph counterparts.
;;This is necessary as vp-query-entries continue to refer to scenario instances.
;;have to be careful to not remove dangling instances, e.g., single node that does not have additional
;;content triple. Such dangling instances are described in scenario and used for isa testing.
(defun get-context(input-scenario 
		   input-model 
		   input-assumption-triples 
		   correspondence-assoc-map)
  (let* ((scenario   (get-simplified-km-triples input-scenario))
	 (model      (get-simplified-km-triples input-model))
	 (assumption (get-simplified-km-triples input-assumption-triples))
	 (simple-aggregated-triples 
	  (remove-duplicates
	   (replace-elements-in-list 
	    (append scenario model)
	    (invert-map correspondence-assoc-map)) ;;necessary as vp-query-entries refer to scenario instances.
	   :test #'ps-triple-equal))
	 (full-aggregated-triples 
	  (remove-duplicates
	   (replace-elements-in-list 
	    (append scenario model assumption)
	    (invert-map correspondence-assoc-map)) ;;necessary as vp-query-entries refer to scenario instances.
	   :test #'ps-triple-equal)))
    (values
     (append (extract-non-instance-triples simple-aggregated-triples)
	     (deaggregate-instance-types
	      (aggregate-instance-types 
	       (extract-instance-triples simple-aggregated-triples))))
     (append (extract-non-instance-triples full-aggregated-triples)
	     (deaggregate-instance-types
	      (aggregate-instance-types 
	       (extract-instance-triples full-aggregated-triples)))))))

;;builds correspondence map between scenario and model graph
(defun convert-viewpoint-correspondence-to-assoc-list(viewpoint-correspondence)
  (mapcar #'(lambda(x)
	      (let ((lhs (nth 1 x))
		    (rhs (nth 2 x)))
		(cons (intern lhs :km) (intern rhs :km))))
	  viewpoint-correspondence))

;;Installs filler values for viewpoint-answerable-query slot.
;;The installed filler is always a :seq
(defun fill-viewpoint-answerable-query-slot(vp-inst input-filler)
  (let ((debug nil)
	(filler (if (not (km-seqp input-filler)) (cons ':|seq| input-filler) input-filler))) ;;installs :seq atom if input-filler not a :seq aggregate.
    (if (not (null (cdr filler)))   ;; We don't count :seq atom
	(progn
	  (if debug (format t "Asserting viewpoint-answerable-query slot for ~a, having ~a entries...~%" vp-inst (length filler)))
	  (ps-km-query `(,vp-inst |now-has| (|viewpoint-answerable-query| (,filler))) debug)
	  (if (> (length (cdr filler)) 1) ;; We don't count :seq atom
	      (ps-km-query `(,vp-inst |now-has| (|instance-of| (|Multislot-Value-Viewpoint|))) debug)
	      (ps-km-query `(,vp-inst |now-has| (|instance-of| (|Slot-Value-Viewpoint|))) debug))
	  (if debug (format t "answerable-query debug: ~a~%" (get-viewpoint-answerable-query vp-inst)))))
    t))
	      
;;Determine if a vp-inst is a root viewpoint in bps-tree.
(defun viewpoint-rootp(vp-inst)
  (null (get-viewpoint-parent vp-inst)))

(defun parse-vp-query-entry(vp-query-entry)
  (let* ((type           (nth 1 vp-query-entry))
	 (query-triple   (cdr (nth 2 vp-query-entry)))
	 (query-head     (triple-head     query-triple))
	 (query-relation (triple-relation query-triple))
	 (query-tail     (triple-tail     query-triple)))
    (values type query-triple query-head query-relation query-tail)))

;;returns only queries that asks for instance-of 
(defun extract-instance-of-vp-query (vp-query-list)
  (remove nil
	  (mapcar #'(lambda(vp-query-entry)
		      (multiple-value-bind 
			  (type query-triple query-head query-relation query-tail)
			  (parse-vp-query-entry vp-query-entry)
			(if (equal query-relation '|instance-of|) vp-query-entry)))
		  vp-query-list)))

;;Returns both simplified triple-lst and triples for answerable-queries.
;;Note: simplified triple-lst may not contain triples for answerable-queries.
(defun get-full-triple-list-for-viewpoint(vp-inst)
  (let ((context-triples (get-simplified-context-for-viewpoint vp-inst))
	(answer-triples  (get-triple-list-for-answerable-queries vp-inst)))
    (values (union context-triples answer-triples :test 'ps-triple-equal)
	    context-triples
	    answer-triples)))

(defun get-triple-list-for-answerable-queries(vp-inst)
  (mappend #'(lambda(entry)
	      (let* ((triple (car (strip-triple-prefix (last entry))))
		     (head   (triple-head triple))
		     (reln   (triple-relation triple))
		     (filler (ps-slot-lookup head reln)))
		(mapcar #'(lambda(filler-entry)
			    (list head reln filler-entry))
			filler)))
	   (get-viewpoint-answerable-query vp-inst)))

(defun get-triple-list-for-vp-query-fillers(vp-inst)
  (mappend #'(lambda(entry)
	      (let* ((triple (car (strip-triple-prefix (last entry))))
		     (head   (triple-head triple))
		     (reln   (triple-relation triple))
		     (applicable-concept (ranges-of reln)))
		(multiple-value-bind
		    (filler filler-triple-lst)
		    (ps-instantiate-concept-as-triple-list applicable-concept)
		    (cons (list head reln filler)
			  filler-triple-lst))))
	   (get-viewpoint-query vp-inst)))
