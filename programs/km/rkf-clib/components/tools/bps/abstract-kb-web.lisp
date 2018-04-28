;;
;; $Id: abstract-kb-web.lisp,v 1.8 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(unless (find-package "GRAPH-ALGORITHMS")
  (defpackage "GRAPH-ALGORITHMS"
    (:nicknames "GRAL")))

(defparameter *sme-kb-web* nil)
(defparameter *abstract-kb-web-provenance* nil)
(defparameter *sme-kb-related-to-matrix*   nil)

(defun ps-stub(input)
  (if (atom input)
      (intern (format nil "_~a*bps-stub" input) :km)))

(defun ps-unstub(input)
  (let ((string (format nil "~A" input)))
    (if (and (numberp (search "_" string))
	     (zerop (search "_" string))
	     (numberp (search "*bps-stub" string)))
	(intern (subseq string 1 (search "*bps-stub" string)) :km)
      input)))

(defun quasi-ps-relate(concept1 concept2)
  (let ((stubbed-concept1 (ps-stub concept1))
	(stubbed-concept2 (ps-stub concept2)))
    (multiple-value-bind
	(triple-map instance-map avg-score)
	(ps-perform-semantic-match-without-using-ablation
	 `((,stubbed-concept1 |instance-of| ,concept1)
	   (,stubbed-concept2 |instance-of| ,concept2)
	   (,stubbed-concept1 |related-to| ,stubbed-concept2))
	 (get-dummy-sme-kb-web))
      (let ((target-triple-lst (mapcar #'(lambda(triple-map-entry)(cadr (car triple-map-entry))) triple-map)))
	(mapcar #'(lambda(related-to-abstract-triple)
		     (assoc related-to-abstract-triple
			    (cadr (assoc (taxonomy-depth) *abstract-kb-web-provenance*))
			    :test 'equal))
		 (mapcar #'(lambda(triple)
			     (list (ps-unstub (triple-head triple))
				   '|related-to|
				   (ps-unstub (triple-tail triple))))
			 target-triple-lst))
))))

(defun quasi-find-prototype-relating(concept1 concept2)
  (remove-duplicates (mappend 'cadr (quasi-ps-relate concept1 concept2))))

;;for (taxonomy-depth), returns kb web containing (<concept> related-to <concept>) triples.
(defun get-dummy-sme-kb-web()
  (append (mapcar #'(lambda (concept) 
		      `(,(ps-stub concept) |instance-of| ,concept)) *all-user-defined-concepts*)
	  (mapcar #'(lambda(triple)
		      (list (ps-stub (triple-head triple))
			    (triple-relation triple)
			    (ps-stub (triple-tail triple))))
		  (remove-duplicates
		   (mapcar 'car (cadr (assoc (taxonomy-depth) *abstract-kb-web-provenance*)))
		   :test 'equal))))

(defun build-full-provenance()
  (progn
    (format t "Building provenance for SME KB.~%")
    (setq *abstract-kb-web-provenance* nil)
    (dotimes (x (1+ (taxonomy-depth)))
      (format t "Building provenance for taxonomy depth ~A of ~A~%" x (taxonomy-depth))
      (push (list x (build-provenance x)) 
	    *abstract-kb-web-provenance*))))

(defun build-provenance(depth &optional(concepts-to-ignore nil))
  (let ((result nil)
	(start (get-internal-run-time)))
    (dolist (concept *all-user-defined-concepts*)
      (dolist (dummified-triple (get-abstract-triples-for-concept-prototype concept depth concepts-to-ignore))
	(let ((provenance-info (assoc dummified-triple result :test 'equal)))
	  (if (null provenance-info)
	      (push (list dummified-triple (list concept)) result)
	    (push (list dummified-triple (remove-duplicates (cons concept (cadr provenance-info))))
		  result)))))
    (format t "build-provenance took ~a ms~%" (- (get-internal-run-time) start))
    result))

(defun build-sme-kb-related-to-matrix()
  (let ((all-instances
	 (sort (copy-list *all-user-defined-concepts*)
	       #'(lambda(x y)(string< (stringify x) (stringify y))))))
    (setq 
     *sme-kb-related-to-matrix*
     (gral::floyd-warshall
      (build-matrix-for-abstract-kb-web
       all-instances
       (build-abstract-kb-web-with-dummy-relations (taxonomy-depth)))))))

(defun get-related-prototype-for-concept(concept)
  (let* ((all-instances
	 (sort (copy-list *all-user-defined-concepts*)
	       #'(lambda(x y)(string< (stringify x) (stringify y)))))
	 #|
	 (all-instances (get-nodes-in-abstract-kb-web 
	 (build-abstract-kb-web-with-dummy-relations (taxonomy-depth))))
	 |#
	 (pos (position concept all-instances))
	 (result nil))
    (dotimes (i (length *all-user-defined-concepts*))
      (let* ((prototype (nth i *all-user-defined-concepts*))
	     (prototype-pos (position prototype all-instances)))
	(if (null prototype-pos)
	    (format t "~a not in related-to matrix?~%" prototype)
	  (progn
	    ;(format t "aref[~a,~a]~%" pos prototype-pos)
	    (if (not (= (aref *sme-kb-related-to-matrix* 
			      pos
			      prototype-pos)
			gral::+fixnum-inf+))
	    (push prototype result))))))
    result))

(defun build-abstract-kb-web(depth &optional(concepts-to-ignore nil))
  (let ((start  (get-internal-run-time))
	(result (remove-duplicates
		 (generalize-type-triple-list
		  (build-sme-kb-web-for-matching)
		  depth
		  concepts-to-ignore)
		 :test 'equal)))
    (format t "build-abstract-kb-web took ~a ms~%" (- (get-internal-run-time) start))
    result))

(defun get-concept-prototypes-for-abstract-triple(triple rel-abstract-kb-web-provenance)
  (let* ((debug nil)
	 (provenance-info
	  (assoc triple 
		 rel-abstract-kb-web-provenance
		 :test 'equal))
	 (result (sort (copy-list
			(cadr provenance-info))
		       #'(lambda(x y) (string< (stringify x)(stringify y))))))
    (if debug (format t "~A => ~A~%" triple result))
    result))

(defun get-prototypes-subsuming-triple(triple)
  (remove-duplicates
   (mappend 'cadr
	    (remove-if-not #'(lambda(x) (subsumable-triple-p triple (car x)))
			   (cadr (assoc (taxonomy-depth) *abstract-kb-web-provenance*))))))

(defun get-subsuming-triples-in-prototype(triple prototype)
  (remove-if-not #'(lambda(entry) 
		     (let ((entry-triple (car entry))
			   (entry-triple-provenance (cadr entry)))
		       (and (not (null (member prototype entry-triple-provenance)))
			    (subsumable-triple-p triple entry-triple))))
		 (cadr (assoc (taxonomy-depth) *abstract-kb-web-provenance*))))

;;checks if input triple subsumes target
;;true if 
;;  a) (triple-head input)     subsumes (triple-head target)
;;  b) (triple-relation input) equals   (triple-relation target)
;;  c) (triple-tail input)     subsumes (triple-tail target)
(defun subsumable-triple-p(input target)
  (and (equal (triple-relation input) (triple-relation target))
       (not (null (member (triple-head target) (cons (triple-head input) (ps-all-subclasses (triple-head input))))))
       (not (null (member (triple-tail target) (cons (triple-tail input) (ps-all-subclasses (triple-tail input))))))))

(defparameter *ps-all-subclasses-cache* nil)
(defun ps-all-subclasses (concept)
  (if (null (assoc concept *ps-all-subclasses-cache*))
      (push (list concept (all-subclasses concept)) *ps-all-subclasses-cache*))
  (cadr (assoc concept *ps-all-subclasses-cache*)))

(defun get-concept-prototypes-for-abstract-triple-list(triple-lst depth &optional(concepts-to-ignore nil))
  (sort 
   (copy-list 
    (remove-duplicates 
     (mappend #'(lambda(triple)
		  (get-concept-prototypes-for-abstract-triple triple depth concepts-to-ignore))
	      triple-lst)))
   #'(lambda(x y) (string< (stringify x)(stringify y)))))

(defun build-isa-web()
  (mappend #'(lambda(concept)
	      (mapcar #'(lambda(superclass-entry)
			  (list concept '|isa| superclass-entry))
		      (remove-subsumers (all-superclasses concept))))
	   *all-user-defined-concepts*))

;;resulting graph is reflexive, e.g., both (X related-to Y) and (Y related-to X) are present.
(defun build-abstract-kb-web-with-dummy-relations(depth)
  (remove-duplicates
   (dummify-triple-list 
    (build-abstract-kb-web depth))
   :test 'equal))

(defun dummify-triple-list(triple-lst)
   (mappend #'(lambda(triple)
		(list (list (triple-head triple)
			    '|related-to|
			    (triple-tail triple))
		      (list (triple-tail triple)
			    '|related-to|
			    (triple-head triple))))
	    triple-lst))

(defparameter *concept-subgraph-for-matching* nil)

(defun get-concept-subgraph-for-matching(concept)
  (let ((result (assoc concept *concept-subgraph-for-matching*)))
    (if (not (null result)) (cadr result)
    (multiple-value-bind 
	(root graph)
	(procure-graph concept)
      (push (list concept 
		  (remove-duplicates
		   (type-triple-list
		    (ps-get-subgraph-for-matching graph))
		   :test 'equal))
		  *concept-subgraph-for-matching*)
      (cadr (assoc concept *concept-subgraph-for-matching*))))))
		  
(defun get-abstract-triples-for-concept-prototype(concept depth &optional(concepts-to-ignore nil))
  (generalize-type-triple-list 
   (dummify-triple-list 
    (get-concept-subgraph-for-matching concept))
   depth
   concepts-to-ignore))

(defun build-sme-kb-web-for-matching()
  (let ((debug nil))
    (if (null *sme-kb-web*)
	(progn 
	  (if debug (format t "Building abstract-kb-web~%"))
	  (setq *sme-kb-web*
		(type-triple-list
		 (mappend #'(lambda(concept)
			      (multiple-value-bind 
				  (root graph)
				  (procure-graph concept)
				(ps-get-subgraph-for-matching graph)))
			  *all-user-defined-concepts*))))
      *sme-kb-web*)))

#|
;;stubby code for debugging.
(defun procure-graph (concept)
  (setq *all-user-defined-concepts* '(|Mitosis| |Mitotic-phase| |Anaphase| |Sister-chromatids|))
  (cond ((equal concept '|Mitosis|)
	 (values '|_Mitosis| 
		 '((|_Mitosis| |instance-of| |Mitosis|)
		   (|_Anaphase| |instance-of| |Anaphase|)
		   (|_Mitosis| |subevent| |_Anaphase|))))
	((equal concept '|Mitotic-phase|)
	 (values '|_Mitotic-phase| 
		 '((|_Mitotic-phase| |instance-of| |Mitotic-phase|)
		   (|_Mitosis| |instance-of| |Mitosis|)
		   (|_Mitotic-phase| |subevent| |_Mitosis|))))
	((equal concept '|Anaphase|)
	 (values '|_Anaphase| 
		 '((|_Anaphase| |instance-of| |Anaphase|)
		   (|_Sister-chromatids| |instance-of| |Sister-chromatids|)
		   (|_Anaphase| |result| |_Sister-chromatids|))))))
|#

(defun get-shortest-path(start end predecessor-matrix)
  (let ((predecessor-node (aref predecessor-matrix start end)))
    (cond ((equal start predecessor-node) 
	   (if (equal start end)
	       nil
	     (list start end)))
	  (t (km::insert-at-end end 
				(get-shortest-path start
						   predecessor-node
						   predecessor-matrix))))))

(defun get-nodes-in-abstract-kb-web(dummy-web)
  (sort
   (copy-list (remove-duplicates
	       (mappend #'(lambda(triple)
			    (list (triple-head triple)
				  (triple-tail triple)))
			dummy-web)))
   #'(lambda (x y)(string< (stringify x) (stringify y)))))

(defun build-matrix-for-abstract-kb-web(all-instances dummy-web)
  (let* ((debug nil)
	 (start-time    (get-internal-run-time))
	 (matrix (make-array (list (length all-instances) 
				   (length all-instances))
			     :element-type 'fixnum
			     :initial-element gral::+FIXNUM-INF+)))
    (dolist (triple dummy-web)
      (let ((x-pos (position (triple-head triple)
			     all-instances))
	    (y-pos (position (triple-tail triple)
			     all-instances)))
	(cond ((and (not (null x-pos))
		    (not (null y-pos)))
	       (progn 
		 (setf (aref matrix x-pos y-pos)	1)
		 (setf (aref matrix y-pos x-pos)	1)))
	      (t (format t "build-matrix ignoring ~a ~a~% "(triple-head triple) (triple-tail triple))))))
    (if debug 
	(format t "new matrix[~a], abstract-kb-web[~a] ~a ms~%" (length all-instances)
		(length dummy-web) (- (get-internal-run-time) start-time)))
    matrix))

(defun convert-abstract-path-to-abstract-triple-lst(abstract-path)
  (convert-abstract-path-to-abstract-triple-lst0 abstract-path))

(defun convert-abstract-path-to-abstract-triple-lst0(abstract-path)
  (cond ((null abstract-path)        ())    
	((=(length abstract-path) 1) ())
	(t (cons (list (car abstract-path) '|related-to| (cadr abstract-path))
		 (convert-abstract-path-to-abstract-triple-lst0 (cdr abstract-path))))))

(defun get-reflexive-triples(triple-lst)
  (remove-if-not #'(lambda(triple) (equal (triple-head triple)(triple-tail triple)))
	     triple-lst))

(defun get-relevant-reflexive-triples (all-instances original-triple-lst)
  (remove-if-not #'(lambda(triple)(member (triple-head triple) all-instances))
		 (get-reflexive-triples original-triple-lst)))

(defun filter-abstract-kb-web (dummy-web rel-concepts depth)
  (let ((debug nil)
	(result (remove-if #'(lambda(triple)
			       (null (intersection rel-concepts 
						   (get-concept-prototypes-for-abstract-triple triple depth))))
			   dummy-web)))
    (if debug (format t "abstract-kb-web: orig[~a], filtered[~a], rel concepts[~a]~A~%"
		      (length dummy-web) (length result) (length rel-concepts) rel-concepts))
    result))

(defun get-path-in-abstract-kb-web(start-node end-node dummy-web)
  (let* ((debug nil)
	(all-instances (get-nodes-in-abstract-kb-web dummy-web))
	(start-idx (position start-node all-instances))
	(end-idx   (position end-node all-instances))
	(matrix (build-matrix-for-abstract-kb-web all-instances dummy-web))
	(start-time (get-internal-run-time)))
    (if debug (format t "Getting path from ~a to ~a~%" start-node end-node))
    (cond ((equal start-idx end-idx) (list start-node))
	  ((and (not (null start-idx))
		(not (null end-idx)))
	   (multiple-value-bind 
	       (distance predecessor)
	       (gral::floyd-warshall matrix)
	     (if debug
		 (format t "floyd-warshall(matrix[~a], abstract-kb-web[~a]): ~a ms~%" 
			 (length all-instances)
			 (length dummy-web) (- (get-internal-run-time) start-time)))
	     (let ((abstract-path (mapcar #'(lambda(idx) (nth idx all-instances))
					  (get-shortest-path start-idx end-idx predecessor))))
	       (if debug (format t "abstract-kb-web[~a], path for ~a to ~a: ~A~%"
				 (length dummy-web)
				 start-node
				 end-node
				 abstract-path))
	       abstract-path))))))

(defun get-all-paths-in-abstract-kb-web (start-node end-node dummy-web)
  (let* ((debug         nil)
         (path          (get-path-in-abstract-kb-web start-node end-node dummy-web))
         (rel-triples   (convert-abstract-path-to-abstract-triple-lst path))
         (new-dummy-web (set-difference dummy-web (append (invert-triple-list rel-triples)
                                                          rel-triples)
                                        :test 'equal)))
    (if debug (format t "abstract-kb-web[~A], new-abstract-kb-web[~A]~%" 
    		      (length dummy-web)
		      (length new-dummy-web)))
    (if (< (length new-dummy-web)
           (length dummy-web))
        (cons path
              (get-all-paths-in-abstract-kb-web start-node
						end-node
						new-dummy-web)))))

#|
(defun get-all-paths-in-abstract-kb-web (start-node end-node depth &optional(rel-concepts nil))
  (let* ((dummy-web (filter-abstract-kb-web 
		     (build-abstract-kb-web-with-dummy-relations depth)
		     rel-concepts
		     depth))
	 (all-instances (get-nodes-in-abstract-kb-web dummy-web)))
    (get-all-paths-in-abstract-kb-web0 start-node end-node all-instances dummy-web depth)))

(defun get-all-paths-in-abstract-kb-web0 (start-node end-node all-instances dummy-web depth)
  (let* ((debug         t)
	 (path          (get-path-in-abstract-kb-web start-node end-node
						     all-instances
						     dummy-web
						     depth))
	 (rel-triples   (convert-abstract-path-to-abstract-triple-lst path))
	 (provenance    (get-concept-prototypes-for-abstract-triple-list rel-triples depth all-instances))
	 (new-dummy-web (dummify-triple-list
			 (build-abstract-kb-web depth
						(remove start-node
							(remove end-node
								(append (get-nodes-in-abstract-kb-web dummy-web)
									provenance))))))
	 )
    (if (and (not (null path))
	     (not (set-equal dummy-web new-dummy-web)))
	(progn
	  (if debug (format t "path: ~A, provenance: ~A~%" path 
			    (sort (copy-list 
				   (intersection *all-user-defined-concepts* provenance))
				  #'(lambda(x y)(string< (stringify x)(stringify y))))))
	  (cons (list path provenance)
		(get-all-paths-in-abstract-kb-web0 start-node 
						   end-node 
						   (get-nodes-in-abstract-kb-web new-dummy-web)
						   new-dummy-web
						   depth))))))
|#

(defun tester(concept1 concept2)
  (let ((debug nil)
	(rel-concepts *all-user-defined-concepts*))
    (dotimes (i 2);(taxonomy-depth))
      (let* ((depth (1+ i))
	     (all-paths+provenance (get-all-paths-in-abstract-kb-web 
				    concept1 concept2 
				    depth rel-concepts)))
	(format t "Taxonomy depth: ~A, relating ~a(~A) to ~a(~a), "
		depth
		(car (generalize-concept-to-tax-dist concept1 depth))
		concept1 
		(car (generalize-concept-to-tax-dist concept2 depth))
		concept2)
	(format t "considering concepts[~a] ~a~%" (length rel-concepts) (sort (copy-list rel-concepts)
									      #'(lambda(x y)(string< (stringify x)(stringify y)))))
	(format t "Found ~a paths in ~a relevant concepts.~%" 
		(length all-paths+provenance)
		(length (mappend 'cadr all-paths+provenance)))	
	(if debug
	(dolist (entry all-paths+provenance)
	  (let ((path (car entry))
		(provenance (cadr entry)))
	    (format t "path(~A): ~A~%" (length path) path)
	    (format t "provenance(~A): ~A~%" (length provenance)  provenance)
	    )))
	(setq rel-concepts (intersection *all-user-defined-concepts* (mappend 'cadr all-paths+provenance)))))))

(defun testcase1()
  (tester '|Mitotic-phase| '|Sister-chromatids|))

(defun testcase2()
  (get-concept-prototypes-for-abstract-triple-list
  (get-all-paths-in-abstract-kb-web '|Mitotic-phase| '|Unknown-concept|)))

(defun testcase3()
  (get-concept-prototypes-for-abstract-triple-list
   (get-all-paths-in-abstract-kb-web '|Mitotic-phase| '|Mitotic-phase|)))

(defun tester(start end depth concept-lst)
  (let* ((abstract-web (build-abstract-kb-web depth concept-lst))
	(provenance   (build-provenance depth concept-lst))
	(paths (get-all-paths-in-abstract-kb-web start end (dummify-triple-list abstract-web)))
	(rel-triples  (mappend 'convert-abstract-path-to-abstract-triple-lst paths))
	(rel-real-triples (find-real-triples rel-triples provenance))
	(add-concepts (get-nodes-in-abstract-kb-web rel-real-triples)))
    (if (or (null paths)
	    (set-equal concept-lst add-concepts))
	(progn 
	  (format t "Found ~a paths. ~a relevant concepts.~%~a~%" (length paths)
		  concept-lst paths)
	  concept-lst)
      (tester start end depth (append concept-lst add-concepts)))))

(defun find-real-triples (rel-triples provenance)
  (let ((temp-result
	 (mappend #'(lambda(triple)
		      (format t "~%Doing ~A~%" triple)
		      (mappend #'(lambda(concept)
				   (format t "~a " concept)
				   (get-subsuming-triples-in-prototype triple concept))
			       (get-concept-prototypes-for-abstract-triple triple provenance)))
		  (remove-duplicates rel-triples :test 'equal))))
    (remove-duplicates (mapcar 'car temp-result) :test 'equal)))

#|
    (sort
     (copy-list
      (remove-duplicates result
			 :test
			 #'(lambda (x y)
			     (and (equal (car x) (car y))
				  (subsetp (cadr x)(cadr y)))))
      )
     #'(lambda(x y)(> (length (cadr x))(length (cadr y))))))
)


(defun analyse-provenance()
  (let ((most-common-cnt (length (cadr (car *abstract-kb-web-provenance*))))
	(result nil))
    (dotimes (x (1+ most-common-cnt))
      (push (list
	      (length (remove-if-not #'(lambda(y) (= (length (cadr y)) x)) 
				     *abstract-kb-web-provenance*))
	      X)
	    result)
      (format t "~a triples appear in ~a concept prototypes.~%"
	      (length (remove-if-not #'(lambda(y) (= (length (cadr y)) x)) 
				     *abstract-kb-web-provenance*))
	      x))
    (remove '(0 0) result :test 'equal)))

(defun build-line-chart(input)
  (let ((s (make-string-output-stream))
	(data (mapcar  'car input))
	(label (mapcar 'cadr input)))
    (format s "http://chart.apis.google.com/chart?cht=lc&chs=250x100")
    (format s "&chd=t:")
    (dolist (data-entry data)
      (format s "~a," data-entry))
    (if (not (null data))(format s "~a" #\Backspace))
    (format s "&chl=")
    (dolist (label-entry label)
      (format s "~a|" label-entry))
    (if (not (null label))(format s "~A" #\Backspace))
    (get-output-stream-string s)))

|#
