;;
;; $Id: controller-miscellaneous.lisp,v 1.46 2009/08/27 23:07:08 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Unkmify a set of kmified CPL triples by removing the :|triple| prefix
(defun strip-triple-prefix(kmified-cpl-triples)
  (mapcar #'(lambda (x) 
	      (remove ':|triple| x)) 
	  kmified-cpl-triples))

;;KMify the cpl-triples by attaching the :|triple| prefix
(defun affix-triple-prefix(cpl-triples)
  (mapcar #'(lambda (x) 
	      (cons ':|triple| x)) 
	  cpl-triples))

;;Unkmify list pairs by removing the :|pair| prefix
(defun strip-pair-prefix(input)
  (mapcar #'(lambda (x) 
	      (remove ':|pair| x)) 
	  input))

;;KMify list pairs by attaching the :|pair| prefix
(defun affix-pair-prefix(input)
  (mapcar #'(lambda (x) 
	      (cons ':|pair| x)) 
	  input))

;;Determines if some Viewpoint instance is a member of *OPENLIST*
;;Returns a boolean value
(defun is-viewpoint-member-of-openlist(vp-inst)
  (not (null (get-nodes-containing-vp-inst-from-openlist vp-inst))))

;;Return the node in *OPENLIST* whose configuration is the vp-inst
(defun get-nodes-containing-vp-inst-from-openlist(vp-inst)
  (get-nodes-containing-vp-inst-from-nodelist vp-inst *OPENLIST*))

;;FIXME. Should we generalize and move this to controller-search? 
(defun extract-configs-from-openlist(openlist)
  (mapcar #'(lambda (node) 
	      (get-config node))
	  openlist))

;;Given a nodelist, return the node whose configuration is the vp-inst
(defun get-nodes-containing-vp-inst-from-nodelist(vp-inst nodelist)
  (remove nil (mapcar #'(lambda (x)
			  (if (equal vp-inst (get-config x))
			      x))
		      nodelist)))

;;Return the node in *CLOSEDLIST* whose configuration is the vp-inst
(defun get-nodes-containing-vp-inst-from-closedlist(vp-inst)
  (get-nodes-containing-vp-inst-from-nodelist vp-inst *CLOSEDLIST*))

;;Returns the first viewpoint examined
(defun root-viewpoint()
  (first *closedlist*))

;;Alternate root viewpoint instances are generated from other possible word2concept mappings.
(defun alt-root-viewpoint()
  (remove-if #'(lambda(x) (not (null (get-viewpoint-model-root x))))
	     (get-viewpoint-children (root-viewpoint))))

;;Predicate to test if viewpoint instance is an alternate root viewpoint instance, 
;;Alternate root viewpoint instances are generated from other possible word2concept mappings.
(defun alt-root-viewpointp(vp-inst)
  (not (null (member vp-inst (alt-root-viewpoint)))))

;;Predicate to test if viewpoint instance is the root viewpoint instance, 
;;also known as the initial viewpoint.
(defun root-viewpointp(vp-inst)
  (or (equal (root-viewpoint) vp-inst)
      (null (get-viewpoint-parent vp-inst))))

;;Function to look into the CPL output triples and gather only those triples
;;containing a specific relation.
(defun sieve-cpl-scenario-by-relation (cpl-scenario relation)	
  (mapcar #'(lambda (x) 
	      (if (equal (triple-relation x) relation)
		  (cons (triple-head x) (triple-tail x))))
	  cpl-scenario))

;;Function to look into CPL output triples and return the assoc-list 
;;comprised of the head and tail of the triples having the instance-of relation.
(defun get-instance-of-for-instance-in-cpl-scenario (instance cpl-scenario)
  (remove nil (mapcar #'(lambda (x)
			  (if (equal (car x) instance)
			      (cdr x)))
		      (sieve-cpl-scenario-by-relation cpl-scenario 
						      '|instance-of|))))

(defun extract-query-components-from-vp-query-entry(vp-query-entry)
  (let* ((stmt (nth 2 vp-query-entry))
	 (frame-instance  (nth 1 stmt))
	 (slot-name       (nth 2 stmt))
	 (expected-filler (nth 3 stmt)))
    (values slot-name frame-instance expected-filler)))

;;Returns the query path for some Viewpoint instance
(defun extract-query-path (vp-query-entry)
  (multiple-value-bind
      (slotname query-frame expected-filler)
      (extract-query-components-from-vp-query-entry vp-query-entry)
    (list  (intern "the" :km) 
	   slotname
	   (intern "of" :km) 
	   query-frame)))

;;Returns set of instances in CPL output triples
(defun get-all-scenario-instances(scenario)
  (remove-duplicates (mapcar 'triple-head (extract-instance-triples scenario))))

;;(the <relation> of <query-frame>)
(defun get-query-paths (scenario question-instance &optional(verbose nil))
  (remove nil
	  ;;Only interested in query-paths whose query-frame is a KM instance.
	  (mapcar #'(lambda(query-path)
		      (let ((slot        (second query-path))
			    (query-frame (fourth query-path)))
			(if (km-instancep query-frame) query-path)))
		  (remove-duplicates (ps-extract-question scenario question-instance)
				     :test #'equal))))

;;Determines if input is a KM pair aggregate 
(defun km-pair-aggregatep (input)
  (and (listp input)
       (equal (car input) ':|pair|)))

(defun ps-replace-concept-in-km-pair-aggregate (old-concept
						new-concept
						km-pair-aggregate)
  (let ((result '(:|pair|)))
    (dolist (elem (cdr km-pair-aggregate))
      (if (equal elem old-concept)
	  (setf result (append result (list new-concept)))
	  (setf result (append result (list elem)))))
    result))

;;-----------------------------------------------------------------------
;; DESC: Replace all the old-concept in graph with new-concept.
;; INPUT: old-concept = a concept node -- e.g. (|Dog| . 1)
;;	  new-concept = a concept node
;;	  graph = a list of triples.
;; OUTPUT: A list of triples where old-concept is replaced with new 
;;	   concept.
;;-----------------------------------------------------------------------
(defun ps-replace-concept-in-graph (old-concept new-concept graph)
  (let (result)
    (dolist (triple graph)
      (cond
        ((equal (triple-head triple) old-concept)
         (push  (list new-concept 
		      (triple-relation triple) 
		      (triple-tail triple))
		result))
        ((equal (triple-tail triple) old-concept)
         (push  (list (triple-head triple) 
		      (triple-relation triple) 
		      new-concept)
		result))
	((km-pair-aggregatep (triple-tail triple))
	 (push (list (triple-head triple)
		     (triple-relation triple)
		     (ps-replace-concept-in-km-pair-aggregate old-concept
							      new-concept
							      (triple-tail triple)))
	       result))
        (t (push triple result))))
    result))

(defun aggregate-instance-types (cpl-triples)
  (append 
   (aggregate-similar-relations (extract-instance-triples cpl-triples))
   (extract-non-instance-triples cpl-triples)))

(defun aggregate-similar-relations (cpl-triples)
    ;; First, get unique set of head and relation pair for triple-list
    (let ((head-instances+relns 
	   (remove-duplicates 
	    (mapcar #'(lambda(cpl-triple)
			 (cons (triple-head     cpl-triple)
			       (triple-relation cpl-triple)))
		     cpl-triples)
	    :test #'equal)))
      ;;Copmpute aggregations
      (mapcar #'(lambda(head-relation)
		   (let* ((head           (car head-relation))
			 (relation        (cdr head-relation))
			 (tail-aggregate (aggregate-triple-tail 
					  (sieve-triple-list-having-head 
					   head
					   (sieve-triple-list-having-relation 
					    relation 
					    cpl-triples)))))
		     ;;Remove subsumable concepts
		     (if (eql relation '|instance-of|) 
			 (setf tail-aggregate (remove-subsumers tail-aggregate)))
                     ;;Make into an atom if list contain 1 element.
		     (if (null (cdr tail-aggregate))   
			 (setf tail-aggregate (car tail-aggregate)))
		     (list head relation tail-aggregate)))
	       head-instances+relns)))

;;Tests if atom is a Property-Value concept in KM
(defun property-value-p (input)
  (if (atom input)
      (not (null (member input (cons '|Property-Value| (all-subclasses '|Property-Value|)))))
      (null (set-difference input (cons '|Property-Value| (all-subclasses '|Property-Value|))))))

;;Tests if atom is a Property-Value instance in KM
(defun property-value-instance-p(input)
  (and (atom input)
       (isa input '|Property-Value|)))

;;Flattens a list and extracts all KM instance names
(defun extract-all-km-instance-names(input)
  (remove-duplicates
   (remove nil
	   (mapcar #'(lambda(x)
		       (if (km-instancep x) x))
		   (flatten input)))))
  
