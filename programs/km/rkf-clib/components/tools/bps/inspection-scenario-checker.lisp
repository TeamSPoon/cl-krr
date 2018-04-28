;;
;; $Id: inspection-scenario-checker.lisp,v 1.6 2009/08/27 21:20:47 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *ps-verify-single-path-meiomized-data* nil)

(defun reset-ps-verify-single-path()
  (setq *ps-verify-single-path-meiomized-data* nil))
  
;;Makes sure semantic-match for path has
;;a) all nodes are mapped
;;b) all nodes in RHS are taxonomically south-of LHS, e.g.,
;;    i) allow for Move-Apart(LHS) and Anaphase(RHS)
;;   ii) disallow for Anaphase(LHS) and Move-Apart(LHS)
(defun ps-semantic-match-for-verified-path?(triple-lst
					    concept-graph
					    instance-map-lst)
  (let ((debug nil))
  (and 
    (ps-all-nodes-matched?
     triple-lst
     instance-map-lst)
   (eval
    (cons 
     'and 
     (mapcar
      #'(lambda(instance-map)
	  (let* 
	      ((lhs (car  instance-map))
	       (rhs (cadr instance-map))
	       (lhs-concept 
		(quasi-get-concept-for-kb-instance
		 lhs triple-lst))
	       (rhs-concept 
		(quasi-get-concept-for-kb-instance
		 rhs concept-graph)))
	    (if debug 
		(progn
		  (format t "lhs-concept: ~A~%"
			  lhs-concept)
		  (format t "rhs-concept: ~A~%"		 
			  rhs-concept)))
	    (not (null (intersection 
			rhs-concept
			(append lhs-concept 
				(mappend 'all-subclasses 
					 lhs-concept)))))))
      instance-map-lst))))))

(defun ps-verify-single-path(triple-lst-for-path root-node full-triple-lst)
  (let ((debug nil)
	(meiomized-entry (assoc (list triple-lst-for-path root-node full-triple-lst) 
				*ps-verify-single-path-meiomized-data* 
				:test #'(lambda(x y)
					  (and (null (set-difference 
						      (extract-non-instance-triples (nth 0 x))
						      (extract-non-instance-triples (nth 0 y))
						      :test 'ps-triple-equal))
					       (equal (nth 1 x)(nth 1 y))
					       (null (set-difference 
						      (extract-non-instance-triples (nth 2 x))
						      (extract-non-instance-triples (nth 2 y))
						      :test 'ps-triple-equal)))))))
    (if (not (null meiomized-entry))
      (progn
	(if debug (format t "returning meiomized result for (ps-verify-single-path...) path length[~A]~%"
			  (length
			   (extract-non-instance-triples triple-lst-for-path))))
	(cadr meiomized-entry))
      (let ((result (ps-verify-single-path-aux triple-lst-for-path root-node full-triple-lst)))
	(push (list (list triple-lst-for-path root-node full-triple-lst) result)
	      *ps-verify-single-path-meiomized-data*)
	result))))    

(defun ps-verify-single-path-aux(triple-lst-for-path root-node full-triple-lst)
  (let ((root-concept ;; multiple concept??
	 (car 
	  (quasi-get-concept-for-kb-instance 
	   root-node triple-lst-for-path))))
    (multiple-value-bind
	(graph-root graph)
	(ps-get-graph root-concept)
      (multiple-value-bind
	  (ppsm-result ppsm-concept-graph)
	  (my-ps-perform-semantic-match
	   triple-lst-for-path
	   graph)
	(let* ((instance-map-lst (nth 1 ppsm-result))
	       (verified-path?
		(ps-semantic-match-for-verified-path?
		 triple-lst-for-path
		 ppsm-concept-graph
		 instance-map-lst)))
	  #|
	  (format t "(ps-verify-single-path ...) => ~a for ~%~a~%~%"
	  verified-path? 
	  (pretty-print-verify-single-path-triple-lst
	  (extract-non-instance-triples triple-lst-for-path)
	  root-node))
	  |#
	  (if (not verified-path?)
	      (multiple-value-bind
		  (expanded-graph additional-triple-lst candidate-lst)
		  (EXPAND-TRIPLE-LIST-DIAMETER-BY-ONE-EDGE
		   triple-lst-for-path
		   full-triple-lst)
		(if (null additional-triple-lst)
		    (progn
		      ;(format t "No expanded path to retry.~%")
		      nil)
		  (let ((result
			 (eval (cons 'or
				     (mapcar #'(lambda(additional-triple candidate-entry)
						 (let ((new-root-node 
							(car (set-difference 
							      (extract-all-km-instances additional-triple)
							      (list root-node)))))
						   #|
						   (format t "Retrying (ps-verify-single-path ...) on expanded path [~a] ~%~a~%" 
							   (length 
							    (extract-non-instance-triples 
							     candidate-entry))
							   (pretty-print-verify-single-path-triple-lst 
							    (extract-non-instance-triples 
							     candidate-entry)
							    new-root-node))
						   |#
						   (ps-verify-single-path 
						    candidate-entry
						    new-root-node
						    full-triple-lst)))
					     additional-triple-lst candidate-lst)))))
		    result)))
	    (progn
	      verified-path?)))))))

(defun pretty-print-verify-single-path-triple-lst(triple-lst-for-path root-node)
  (let* ((AE-triple-lst (sieve-triple-list-having-instance root-node triple-lst-for-path))
	 (EE-triple-lst (set-difference triple-lst-for-path AE-triple-lst :test 'ps-triple-equal))
	 (s (make-string-output-stream)))
    (dolist (triple AE-triple-lst)
      (if (equal (triple-head triple) root-node)
	  (format s "[AE] ~s~%" triple)
	  (format s "[AE] ~s~%" (invert-triple triple))))
    (dolist (triple EE-triple-lst)
      (format s "[EE] ~s~%" triple))
    (get-output-stream-string s)))

;; (defun testme()
;;   (ps-verify-single-path
;;    *scenario*
;;    '|_Mitosis2163_c1|))

;;This seems a little odd... 
;;At the beginning... 
;;Which direction should we pick the root-node for AE? [fixed]
;;The heuristic we use now is for forward-pointingp triples [no longer necessary]
;;We now test it both ways. It has to satisfy at-least one direction.
;;Also, what about (not so important now)
;; a) test for concept subsumtion
;; b) test for slot cardinality. 
(defun ps-verify-all-triple-lst(triple-lst)
  (let ((debug nil))
  (cond ((null (extract-non-instance-triples (remove-irrelevant-km-triples triple-lst))) t)
	(t 
	 (let* ((content-triple (car (extract-non-instance-triples
				      (make-triple-proper
				       (remove-irrelevant-km-triples
					triple-lst)))))
		(content-triple-lst (quasi-properly-terminate-triple-list
				     (list content-triple) triple-lst)))
	   (if debug (format t "(ps-verify-all-triple-lst ...) checking ~a ~%" content-triple))
	   (and 
	    (or (progn
		  (ps-verify-single-path
		   content-triple-lst
		   (triple-head content-triple)
		   triple-lst)
		)
		(progn
		  (if debug (format t "Trying reverse direction~%"))
		  (ps-verify-single-path
		   content-triple-lst
		   (triple-tail content-triple)
		 triple-lst)
		)
	     )
	    (ps-verify-all-triple-lst 
	     (set-difference triple-lst
			     (list content-triple)
			     :test 'ps-triple-equal))))))))

(defun inspection-scenario?(triple-lst &optional(timeout 30))
  (sys:with-timeout 
   (timeout
    (progn 
      (format t "BPS: (inspection-scenario? ...) timeout'ed after ~a seconds. Assuming it's not an inspection-scenario." timeout)
      nil))
   (progn
     (reset-ps-verify-single-path)
     (ps-verify-all-triple-lst 
      (remove-irrelevant-km-triples triple-lst)))))

;; ;;In the context of biology-sme-kb dated sometime 10Oct08
;; (defparameter *specialized-scenario*
;;   '((|_Chromosome2162_c1| |instance-of| |Chromosome|)
;;     (|_Mitosis2163_c1| |instance-of| |Event|)
;;     (|_Separation2159_c1| |instance-of| |Move-Apart|)
;;     (|_Separation2159_c1| |object| |_Chromosome2162_c1|)
;;     (|_Mitosis2163_c1| |subevent| |_Separation2159_c1|)))

;; (defun testme-specialized()
;; (ps-verify-all-triple-lst *specialized-scenario*))

;; ;;In the context of biology-sme-kb dated sometime 10Oct08
;; (defparameter *unspecialized-scenario*
;;   '((|_Chromosome2162_c1| |instance-of| |Chromosome|)
;;     (|_Mitosis2163_c1| |instance-of| |Mitosis|)
;;     (|_Separation2159_c1| |instance-of| |Move-Apart|)
;;     (|_Separation2159_c1| |object| |_Chromosome2162_c1|)
;;     (|_Mitosis2163_c1| |subevent| |_Separation2159_c1|)))

;; (defun testme-unspecialized()
;; (ps-verify-all-triple-lst *unspecialized-scenario*))

;; (defun testme()
;;   (mapcar #'(lambda(q-number)
;; 	      (format t "Trying ~a ~%" q-number)
;; 	      (let ((result 
;; 		     (ps-verify-all-triple-lst
;; 		      (remove-irrelevant-km-triples
;; 		       (eval-tester-sym q-number 'cpl-scenario)))))
;; 		(format t "Tried ~a [~a]~%" q-number result)))
;; 	  *target-question-lst*))

(defun testme(&optional(q-number-lst *q-cpl-set*))
  (remove nil
	  (mapcar #'(lambda(q-number)
	      (if (not (null (eval-tester-sym q-number 'cpl-scenario)))
		  (multiple-value-bind
		      (scenario compute-question yn-query)
		      (fix-cpl-output (eval-tester-sym q-number 'cpl-scenario)
				      (eval-tester-sym q-number 'cpl-questions)
				      (eval-tester-sym q-number 'cpl-yn-questions)
				      '(install-ae-triples-for-skolem-instances-in-yn-query))
		    (format t "Trying ~a ~%" q-number)
		    (let ((result 
			   (inspection-scenario?
			    (remove-irrelevant-km-triples
			     (ps-clone-triple-list scenario)))))
		      (format t "Tried ~a [~a]~%" q-number result)
		      (if result q-number)))))
		  (sort-q-number-list q-number-lst))))


;; (defun testme()
;;   (setq *ps-verify-single-path-meiomize* nil)
;;   (PS-VERIFY-SINGLE-PATH
;;    '((|_Cell2002| |instance-of| |Cell|)
;;      (|_Volume2004| |instance-of| |Volume-Value|)
;;      (|_Cell2002| |volume| |_Volume2004|))
;;    '|_Cell2002|
;;    '((|_Volume2004| |instance-of| |Volume-Value|)
;;      (|_Cell2002| |instance-of| |Cell|)
;;      (|_Volume2005| |instance-of| |Volume-Value|)
;;      (|_Cell2003| |instance-of| |Cell|)
;;      (|_Cell2002| |volume| |_Volume2004|)
;;      (|_Cell2003| |volume| |_Volume2005|)
;;      (|_Volume2005| |greater-than| |_Volume2004|)
;;      (|_Amount2010| |instance-of| |Quantity-Value|)
;;      (|_Cytoplasm2006| |instance-of| |Cytoplasm|)
;;      (|_Amount2011| |instance-of| |Quantity-Value|)
;;      (|_Cytoplasm2008| |instance-of| |Cytoplasm|)
;;      (|_Cytoplasm2006| |quantity| |_Amount2010|)
;;      (|_Cell2002| |encloses| |_Cytoplasm2006|)
;;      (|_Cytoplasm2008| |quantity| |_Amount2011|)
;;      (|_Cell2003| |encloses| |_Cytoplasm2008|)
;;      (|_Amount2011| |greater-than| |_Amount2010|)
;;      (|_Number2018| |instance-of| |Unitless-Value|)
;;      (|_Wrinkle2012| |instance-of| |Conceptual-Entity|)
;;      (|_Surface2013| |instance-of| |Artifact|)
;;      (|_Number2019| |instance-of| |Unitless-Value|)
;;      (|_Wrinkle2015| |instance-of| |Conceptual-Entity|)
;;      (|_Surface2016| |instance-of| |Artifact|)
;;      (|_Wrinkle2012| |number-of-elements| |_Number2018|)
;;      (|_Surface2013| |has-on-it| |_Wrinkle2012|)
;;      (|_Cell2003| |has-part| |_Surface2013|)
;;      (|_Wrinkle2015| |number-of-elements| |_Number2019|)
;;      (|_Surface2016| |has-on-it| |_Wrinkle2015|)
;;      (|_Cell2002| |has-part| |_Surface2016|))))


;;odd. But this call fails. semantic-matcher appears to get stuck.
;;The matcher got stuck because (graph-match) returned 1400+ candidates.
;;Considering them all was too expensive. Instead, (kb-match) should only
;;use the top-ranking ones to improve match.
;;This is triggered by setting the parameter consider-only-strongest? to t.
;; (defun testme()
;;   (reset-ps-verify-single-path)
;;   (PS-VERIFY-SINGLE-PATH
;;    '((|_Transcription3847_c44|    |instance-of|    |Synthesis|)
;;      (|_Transcription3847_c44|    |instance-of|    |Transcription|)
;;      (|_Tangible-Entity4439_c44|    |instance-of|    TRNA)
;;      (|_Tangible-Entity4439_c44|    |instance-of|    |Tangible-Entity|)
;;      (|_MRNA3970_c44|    |instance-of|    |Tangible-Entity|)
;;      (|_MRNA3970_c44|    |instance-of|    MRNA)
;;      (|_Synthesis3978_c44|    |instance-of|    |Synthesis|)
;;      (|_Synthesis4440_c44|    |instance-of|    |Synthesis|)
;;      (|_Translation3846_c44|    |instance-of|    |Event|)
;;      (|_Translation3846_c44|    |instance-of|    |Translation|)
;;      (|_Attach3979_c44|    |instance-of|    |Attach|)
;;      (|_Attach4441_c44|    |instance-of|    |Attach|)
;;      (|_Attach3983_c44|    |instance-of|    |Attach|)
;;      (|_Tangible-Entity3981_c44|    |instance-of|    |Ribosome|)
;;      (|_Tangible-Entity3981_c44|    |instance-of|    |Tangible-Entity|)
;;      (|_Transcription3847_c44|    |next-event|    |_Translation3846_c44|)
;;      (|_MRNA3970_c44|    |base-of|    |_Attach4441_c44|)
;;      (|_Translation3846_c44|    |subevent|    |_Attach4441_c44|)
;;      (|_Tangible-Entity4439_c44|    |object-of|    |_Attach4441_c44|)
;;      (|_Synthesis4440_c44|    |prev-event|    |_Attach4441_c44|)
;;      (|_Attach3983_c44|    |object|    |_MRNA3970_c44|)
;;      (|_Attach3983_c44|    |subevent-of|    |_Translation3846_c44|)
;;      (|_Attach3983_c44|    |first-subevent-of|    |_Translation3846_c44|)
;;      (|_Attach3983_c44|    |next-event|    |_Attach4441_c44|)
;;      (|_Tangible-Entity3981_c44|    |agent-of|    |_Synthesis3978_c44|)
;;      (|_Tangible-Entity3981_c44|    |agent-of|    |_Translation3846_c44|)
;;      (|_Tangible-Entity3981_c44|    |agent-of|    |_Synthesis4440_c44|)
;;      (|_Tangible-Entity3981_c44|    |site-of|    |_Translation3846_c44|)
;;      (|_Tangible-Entity3981_c44|    |site-of|    |_Attach3979_c44|)
;;      (|_Tangible-Entity3981_c44|    |site-of|    |_Attach4441_c44|)
;;      (|_Attach3983_c44|    |base|    |_Tangible-Entity3981_c44|))
;;    '|_Translation3846_c44|
;;    '((|_Attach3983_c44| |instance-of| |Attach|)
;;      (|_Protein3982_c44| |instance-of| |Entity|)
;;      (|_Protein3982_c44| |instance-of| |Protein|)
;;      (|_Tangible-Entity3981_c44| |instance-of| |Ribosome|)
;;      (|_Tangible-Entity3981_c44| |instance-of| |Tangible-Entity|)
;;      (|_Attach3979_c44| |instance-of| |Attach|)
;;      (|_Synthesis3978_c44| |instance-of| |Synthesis|)
;;      (|_Nucleus3972_c44| |instance-of| |Nucleus|)
;;      (|_Gene3971_c44| |instance-of| |Gene|)
;;      (|_MRNA3970_c44| |instance-of| |Tangible-Entity|)
;;      (|_MRNA3970_c44| |instance-of| MRNA)
;;      (|_Transcription3847_c44| |instance-of| |Synthesis|)
;;      (|_Transcription3847_c44| |instance-of| |Transcription|)
;;      (|_Translation3846_c44| |instance-of| |Event|)
;;      (|_Translation3846_c44| |instance-of| |Translation|)
;;      (|_Protein Synthesis2034_c20| |instance-of| |Protein-synthesis|)
;;      (|_Amino-Acid4436_c44| |instance-of| |Amino-Acid|)
;;      (|_Tangible-Entity4439_c44| |instance-of| TRNA)
;;      (|_Tangible-Entity4439_c44| |instance-of| |Tangible-Entity|)
;;      (|_Synthesis4440_c44| |instance-of| |Synthesis|)
;;      (|_Attach4441_c44| |instance-of| |Attach|)
;;      (|_Leave2585_c44| |instance-of| |Leave|)
;;      (|_Entity2034_c44| |instance-of| |Nucleic-acid|)
;;      (|_Attach3983_c44| |base| |_Tangible-Entity3981_c44|)
;;      (|_Attach3983_c44| |object| |_MRNA3970_c44|)
;;      (|_Attach3983_c44| |subevent-of| |_Translation3846_c44|)
;;      (|_Attach3983_c44| |first-subevent-of| |_Translation3846_c44|)
;;      (|_Attach3983_c44| |next-event| |_Attach4441_c44|)
;;      (|_Protein3982_c44| |result-of| |_Translation3846_c44|)
;;      (|_Protein3982_c44| |result-of| |_Protein Synthesis2034_c20|)
;;      (|_Tangible-Entity3981_c44| |agent-of| |_Synthesis3978_c44|)
;;      (|_Tangible-Entity3981_c44| |agent-of| |_Translation3846_c44|)
;;      (|_Tangible-Entity3981_c44| |agent-of| |_Synthesis4440_c44|)
;;      (|_Tangible-Entity3981_c44| |site-of| |_Attach3979_c44|)
;;      (|_Tangible-Entity3981_c44| |site-of| |_Translation3846_c44|)
;;      (|_Tangible-Entity3981_c44| |site-of| |_Attach4441_c44|)
;;      (|_Nucleus3972_c44| |site-of| |_Transcription3847_c44|)
;;      (|_Gene3971_c44| |object-of| |_Transcription3847_c44|)
;;      (|_MRNA3970_c44| |result-of| |_Transcription3847_c44|)
;;      (|_MRNA3970_c44| |base-of| |_Attach4441_c44|)
;;      (|_Transcription3847_c44| |next-event| |_Translation3846_c44|)
;;      (|_Transcription3847_c44| |next-event| |_Leave2585_c44|)
;;      (|_Transcription3847_c44| |subevent-of| |_Protein Synthesis2034_c20|)
;;      (|_Transcription3847_c44| |first-subevent-of|  |_Protein Synthesis2034_c20|)
;;      (|_Translation3846_c44| |subevent| |_Synthesis4440_c44|)
;;      (|_Translation3846_c44| |subevent| |_Attach4441_c44|)
;;      (|_Translation3846_c44| |subevent-of| |_Protein Synthesis2034_c20|)
;;      (|_Protein Synthesis2034_c20| |agent| |_Entity2034_c44|)
;;      (|_Amino-Acid4436_c44| |is-possessed-by| |_Tangible-Entity4439_c44|)
;;      (|_Amino-Acid4436_c44| |raw-material-of| |_Synthesis4440_c44|)
;;      (|_Tangible-Entity4439_c44| |object-of| |_Attach4441_c44|)
;;      (|_Synthesis4440_c44| |prev-event| |_Attach4441_c44|)
;;      (|_Entity4635_c44| |instance-of| |Polymer|)
;;      (|_Entity4635_c44| |instance-of| |Entity|)
;;      (|_Atom2938_c44| |instance-of| |Atom|)
;;      (|_Nucleic Acid2035_c20| |instance-of| |Nucleic-acid|)
;;      (|_Entity4635_c44| |is-part-of| |_Nucleic Acid2035_c20|)
;;      (|_Atom2938_c44| |is-basic-structural-unit-of|  |_Nucleic Acid2035_c20|))
;;    ))

