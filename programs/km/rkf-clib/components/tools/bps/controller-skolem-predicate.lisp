;;
;; $Id: controller-skolem-predicate.lisp,v 1.51 2008/10/05 15:33:38 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *get-specialized-triples-for-instance-ignore-list* ())

;;deprecated (defun init-get-specialized-triples-for-instance()
;;   (setf *get-specialized-triples-for-instance-ignore-list* ()))


;;deprecated (defun get-specialized-triples-for-concept(concept)
;;   (get-specialized-triples-for-instance (ps-instantiate-concept concept)))

;;deprecated (defun get-specialized-triples-for-instance(instance)
;;   (progn (init-get-specialized-triples-for-instance)
;; 	 (get-specialized-triples-for-instance0 instance)))

;;deprecated (defun get-specialized-triples-for-instance0(instance)
;;   (if (not (member instance
;; 		   *get-specialized-triples-for-instance-ignore-list*))
;;       (let* ((immediate-triples     (query-immediate-forward-slots instance))
;;    	     (specialized-triples   (get-specialized-triples 
;; 				     (properly-terminate-triple-list
;; 				      immediate-triples)))
;; 	     (interesting-instances (extract-all-instances-from-triple-list 
;; 				     immediate-triples)))
;; 	(push instance 
;; 	      *get-specialized-triples-for-instance-ignore-list*)
;; 	(let ((interesting-instance-triples
;; 	       (mappend 
;; 		'get-specialized-triples-for-instance0
;; 		interesting-instances)))
;;     (remove-duplicates
;;      (append 
;;       specialized-triples
;;       interesting-instance-triples
;;       (mappend #'(lambda(instance)
;; 		   (sieve-triple-list-having-instance instance
;; 						      immediate-triples))
;; 	       (extract-all-instances-from-triple-list interesting-instance-triples)))
;;      :test 'ps-triple-equal)))))

;;Returns list of all specialized instances
(defun get-all-specialized-instances(scenario)
  (let ((all-instances (extract-all-instances-from-triple-list scenario)))
    (remove nil
	    (mapcar #'(lambda(instance)
			(if (specialized-p scenario instance) instance))
		    all-instances))))

;;Returns true if the instance subgraph differs
;;from skolem graph
;; a) different cardinality 
;; b) different slot filler type
(defun specialized-p (scenario instance)
  (let* ((scenario      
	  (make-triple-proper 
	   (remove-irrelevant-km-triples scenario)))
	 (instance-subgraph 
	  (deaggregate-triple-list
	   (ps-get-subgraph instance scenario))))
    (and (not 
	  (null 
	   (extract-non-instance-triples 
	    instance-subgraph)))
	 (not 
	  (null 
	   (get-specialized-triples 
	    scenario 
	    instance-subgraph))))))

;;Returns specialized triples when
;; a) cardinality of (X r ?) is different from (every X r ?), skolem version.
;; b) (X r Y) when it is (every X r Z), i.e., having a different slot filler concept compared to skolem version.
(defun get-specialized-triples (scenario 
				&optional(target-triple-lst 
					  scenario))
  (let* ((cardinality-mismatch-triples
	  (get-triples-with-specialized-root-slot-cardinality 
	   target-triple-lst))
	 (filler-concept-mismatch-triples
	  (get-triples-with-different-root-slot-concept 
	   scenario 
	   target-triple-lst)))
    (remove-duplicate-triples
     (quasi-properly-terminate-triple-list
      (append cardinality-mismatch-triples
	      filler-concept-mismatch-triples)
      scenario))))

;;Returns the specialized-triple-with-filler-of-differeny-type 
;;subset of target-triple-lst
;;i.e., triples whose slot filler concept differs 
;;from skolem version
;;This thing does not account for context.
(defun get-triples-with-different-root-slot-concept 
  (scenario target-triple-lst)
  (remove
   nil
   (mapcar 
    #'(lambda(target-triple)
	(triple-with-different-root-slot-concept? 
	 scenario
	 target-triple))
    (extract-non-instance-triples 
     target-triple-lst))))

;;Extracts all triples whose root/slot cardinality differs from skolem version
;;We want to take into account cardinality as well.
;;e.g. Reaction having two unspecialized raw-materials. 
;;     In this case, the Reaction instance is specialized 
;;     since the defn of Reaction only has 1 raw-material. 
(defun get-triples-with-specialized-root-slot-cardinality(input-target-triple-lst)
  (let ((target-triple-lst (make-triple-proper input-target-triple-lst)))
    (quasi-properly-terminate-triple-list
     (remove-duplicate-triples 
      (mappend #'(lambda(root-slot-pair)
		   (let ((root (first  root-slot-pair))
			 (slot (second root-slot-pair)))
		     (if (not (root-slot-same-cardinality-p root slot target-triple-lst))
			 (extract-triples-for-root-slot root slot target-triple-lst))))
	       (get-all-root-slot-pairings target-triple-lst)))
     target-triple-lst)))

;;Checks if the cardinality for slot of root to be the same as skolem version
(defun root-slot-same-cardinality-p (root slot scenario)
  (let* ((debug        nil)
	 (concept-lst  (quasi-get-concept-for-kb-instance root scenario))
	 (count        (length (quasi-ps-slot-lookup root slot scenario)))
	 (larger-cardinality? 
	  (mapcar #'(lambda(concept)
		      (> count (quasi-get-cardinality-for-concept-slot concept slot)))
		  concept-lst)))
    (if (and debug (member t larger-cardinality?))
	(format t "BPS: Specialized triple, (~a ~a ?) has cardinality ~a, but (~a ~a ?) had smaller cardinality.~%"
		root
		slot
		count
		concept-lst
		slot))
    (not (member t larger-cardinality?))))

;;Returns triple if 
;;filler for slot is different from instance-of concepts, i.e., not subsumable
;;false otherwise
(defun triple-with-different-root-slot-concept?(scenario 
						triple)
  (let* ((head     (triple-head     triple))
	 (relation (triple-relation triple))
	 (tail     (triple-tail     triple))
	 (head-concept-lst 
	  (remove-subsumers 
	   (quasi-get-concept-for-kb-instance 
	    head scenario)))
	 (filler-concept-lst
	  (remove-subsumers 
	   (quasi-get-concept-for-kb-instance 
	    tail scenario)))
	 (expected-filler-concept-lst 
	  (remove-subsumers 
	   (mappend #'(lambda(head-concept) 
			(get-type-for-slot-lookup 
			 head-concept 
			 relation))
		    head-concept-lst))))
    (if (not (set-equal 
	      (remove-subsumers 
	       (append expected-filler-concept-lst
		       filler-concept-lst))
	      expected-filler-concept-lst))
	triple)))

;;Returns the type for slot of some concept.
;;This version is safe, i.e. result computed by instantiating concept and doing slot queries.
(defun get-type-for-slot-lookup-safe(concept slot)
  (let* ((head-inst   (ps-instantiate-concept concept)))
    (remove-subsumers
     (get-concept-for-kb-instance
      (ps-slot-lookup head-inst slot)))))

;;Returns the type for slot of some concept.
;;This version is unsafe, i.e. result computed using quasi-km routines.
(defun get-type-for-slot-lookup-unsafe(concept slot)
  (multiple-value-bind 
    (root graph fixpoint?)
    (ps-get-graph concept)
    (let ((target-instances (quasi-ps-slot-lookup root slot graph)))
      (remove-subsumers 
       (quasi-get-concept-for-kb-instance target-instances
					  graph)))))

;;Returns the type for slot of some concept.
(defun get-type-for-slot-lookup(concept slot)
  (cond ((not (cached-p concept))
	 (get-type-for-slot-lookup-safe concept slot))
	(t (get-type-for-slot-lookup-unsafe concept slot))))

;;Improved version. Checks if any concept referring to question instance is specialized.
;;Only checks against skolem graph. 
;;Ideally, it should also check using context, e.g., chemical-formula in HF contains X, Y , Z... 
;;Which is different from the skolem graph for chemical-formula.
;(defun inspection-questionp(input-scenario question &optional(verbose nil))
;  (let* ((scenario         (remove-irrelevant-km-triples input-scenario))
;	 (target-instances (flatten (get-dependent-instances scenario (car question))))
;	 (eval-predicate   (cons 'or
;				 (mapcar #'(lambda(instance)
;					     (not (null (specialized-p scenario instance))))
;					 target-instances))))
;    (if verbose (format t "inspection-questionp predicate ~a~%" eval-predicate))
;    (not (eval eval-predicate))))

(defun inspection-questionp(input-scenario question &optional(verbose nil))
t)

(defun get-dependent-instances(scenario instance &optional(ancestor-instances nil))
  (let* ((scenario   	   (make-triple-proper 
			    (remove-irrelevant-km-triples scenario)))
	 (target-instances (extract-all-km-instances
			    (flatten
			     (mapcar 'triple-tail
				     (sieve-triple-list-having-head 
				      instance
				      scenario))))))
    (cond ((null target-instances) (list instance nil))
	  (t (list instance
		   (mapcar #'(lambda(x)
			       (if (null (member x ancestor-instances))
				   (get-dependent-instances 
				    scenario 
				    x
				    (cons x ancestor-instances))))
			   target-instances))))))

(defun specialized-p-tester (triple-lst instance expected-result)
  (let ((result (specialized-p
		 triple-lst
		 instance)))
    (equal (not (null result)) expected-result)))

#|

;;Test-cases

(defun controller-skolem-predicate-tester()
  (let ((*classification-enabled* nil))
    (list 
     (specialized-p-tester  
      '((|_React7194| |raw-material| |_Chemical7195|) 
	(|_React7194| |instance-of| |Reaction|)
	(|_Chemical7195| |instance-of| |Chemical|))
      '|_React7194|
      nil)
 
   ;;Reaction having 2 raw-material	;
     (specialized-p-tester  
      '((|_React7194| |raw-material| |_Chemical7195|) 
	(|_React7194| |raw-material| |_Chemical7196|) 
	(|_React7194| |instance-of| |Reaction|)
	(|_Chemical7196| |instance-of| |Chemical|) 
	(|_Chemical7195| |instance-of| |Chemical|))
      '|_React7194|
      t)

   ;;O2-Substance as raw-material of Reaction. ;
     (specialized-p-tester  
      '((|_Chemical1203x1| |has-basic-structural-unit| _H27210) 
	(|_React7209| |raw-material| |_Chemical1203x1|)
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
	(|_React7209| |raw-material| |_Chemical1202x2|)
	(|_Chemical1202x2| |instance-of| |O2-Substance|) 
	(|_Chemical1203x1| |instance-of| |H2-Substance|) 
	(|_React7209| |instance-of| |Reaction|)
	(_O27211 |instance-of| O2) 
	(_H27210 |instance-of| H2))
      '|_React7209|
      t)

     (specialized-p-tester  
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |raw-material| |_Chemical1202x2|) 
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|)
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211)
	(|_Chemical1202x2| |instance-of| |Chemical|) 
	(_O27211 |instance-of| O2))
      '|_O27211|
      'nil)

     ;;Chemical instance specialized to be O2-Substance.
     (specialized-p-tester  
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |raw-material| |_Chemical1202x2|)
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|) 
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
	(|_Chemical1202x2| |instance-of| |Chemical|) 
	(_O27211 |instance-of| O2) )
      '|_Chemical1202x2|
      t)

     (specialized-p-tester  
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |raw-material| |_Chemical1202x2|)
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|) 
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
	(|_Chemical1202x2| |instance-of| |Chemical|) 
	(_O27211 |instance-of| O2) )
      '|_Chemical1203x1|
      nil)

     ;;Reaction having 2 raw-material and 1 of them is an O2-Substance.
     (specialized-p-tester
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |raw-material| |_Chemical1202x2|)
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|) 
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
	(|_Chemical1202x2| |instance-of| |Chemical|) 
	(_O27211 |instance-of| O2))
      '|_React7209|
      t)

     ;;Chemical1202x2 is O2 Substance.
     (specialized-p-tester
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |raw-material| |_Chemical1202x2|)
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|) 
	(|_Chemical1203x1| |has-basic-structural-unit| |_Chemical-Entity8888|) 
	(|_Chemical-Entity8888| |instance-of| |Chemical-Entity|)
	(|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
	(|_Chemical1202x2| |instance-of| |Chemical|) 
	(_O27211 |instance-of| O2) )
      '|_Chemical1202x2|
      t)

     (specialized-p-tester
      '((|_React7209| |raw-material| |_Chemical1203x1|)
	(|_React7209| |instance-of| |Reaction|)
	(|_Chemical1203x1| |instance-of| |Chemical|) 
	(|_Chemical1203x1| |has-basic-structural-unit| |_Chemical-Entity8888|) 
	(|_Chemical-Entity8888| |instance-of| |Chemical-Entity|)
	(_O27211 |instance-of| O2) )
      '|_Chemical1203x1|
      nil)

     (specialized-p-tester
      '((|_Chemical-Entity| |instance-of| |Chemical-Entity|)
	(|_Chemical| |instance-of| |Chemical|)
	(|_Chemical| |has-basic-structural-unit| |_Chemical-Entity|))
      '|_Chemical|
      nil)
)))

;;SPECIALIZED-P bug
(defun test-case2()
(SPECIALIZED-P '((|_Substance9370_c0| |string-name| "FeCl_3")
		 (|_Substance9370_c0| |has-basic-structural-unit| |_Ionic-Compound9320_c0|)
		 (|_Ionic-Compound9320_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9277_c0|)
		 (|_Chemical-Formula9277_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 3 |Cl|)))
		 (|_Substance9369_c0| |string-name| "K_3PO_4")
		 (|_Substance9369_c0| |has-basic-structural-unit| |_Ionic-Compound9368_c0|)
		 (|_Ionic-Compound9368_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9346_c0|)
		 (|_Chemical-Formula9346_c0| |term| (:|seq| (:|pair| 3 K) (:|pair| 1 P) (:|pair| 4 O)))
		 (|_React7686_c0| |raw-material| |_Substance9370_c0|) (|_React7686_c0| |raw-material| |_Substance9369_c0|)
		 (|_Substance9578_c0| |result-of| |_React7686_c0|) (|_Substance9578_c0| |string-name| "FePO_4")
		 (|_Substance9578_c0| |has-basic-structural-unit| |_Ionic-Compound9577_c0|)
		 (|_Ionic-Compound9577_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9556_c0|)
		 (|_Chemical-Formula9556_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 1 P) (:|pair| 4 O)))
		 (|_Substance9874_c0| |result-of| |_React7686_c0|) (|_Substance9874_c0| |string-name| "KCl")
		 (|_Substance9874_c0| |has-basic-structural-unit| |_Ionic-Compound9873_c0|)
		 (|_Ionic-Compound9873_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9830_c0|)
		 (|_Chemical-Formula9830_c0| |term| (:|seq| (:|pair| 1 K) (:|pair| 1 |Cl|)))
		 (|_Equation7700_c0| |chemical-equation-of| |_React7686_c0|)
		 (|_Equation7700_c0| |instance-of| |Chemical-Equation-Expression|)
		 (|_Chemical-Formula9830_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9873_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance9874_c0| |instance-of| |Ionic-Compound-Substance|)
		 (|_Chemical-Formula9556_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9577_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance9578_c0| |instance-of| |Ionic-Compound-Substance|) 
		 (|_React7686_c0| |instance-of| |Reaction|)
		 (|_Chemical-Formula9346_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9368_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance9369_c0| |instance-of| |Ionic-Compound-Substance|)
		 (|_Chemical-Formula9277_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9320_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance9370_c0| |instance-of| |Ionic-Compound-Substance|))
	       '|_React7686_c0|))

;;Bug
(get-specialized-triples
 '((|_Substance9370_c0| |string-name| "FeCl_3")
   (|_Substance9370_c0| |has-basic-structural-unit| |_Ionic-Compound9320_c0|)
   (|_Ionic-Compound9320_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9277_c0|)
   (|_Chemical-Formula9277_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 3 |Cl|)))
   (|_Substance9369_c0| |string-name| "K_3PO_4")
   (|_Substance9369_c0| |has-basic-structural-unit| |_Ionic-Compound9368_c0|)
   (|_Ionic-Compound9368_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9346_c0|)
   (|_Chemical-Formula9346_c0| |term| (:|seq| (:|pair| 3 K) (:|pair| 1 P) (:|pair| 4 O)))
   (|_React7686_c0| |raw-material| |_Substance9370_c0|) (|_React7686_c0| |raw-material| |_Substance9369_c0|)
   (|_Substance9578_c0| |result-of| |_React7686_c0|) (|_Substance9578_c0| |string-name| "FePO_4")
   (|_Substance9578_c0| |has-basic-structural-unit| |_Ionic-Compound9577_c0|)
   (|_Ionic-Compound9577_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9556_c0|)
   (|_Chemical-Formula9556_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 1 P) (:|pair| 4 O)))
   (|_Substance9874_c0| |result-of| |_React7686_c0|) (|_Substance9874_c0| |string-name| "KCl")
   (|_Substance9874_c0| |has-basic-structural-unit| |_Ionic-Compound9873_c0|)
   (|_Ionic-Compound9873_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9830_c0|)
   (|_Chemical-Formula9830_c0| |term| (:|seq| (:|pair| 1 K) (:|pair| 1 |Cl|)))
   (|_Equation7700_c0| |chemical-equation-of| |_React7686_c0|)
   (|_Equation7700_c0| |instance-of| |Chemical-Equation-Expression|)
   (|_Chemical-Formula9830_c0| |instance-of| |Chemical-Formula|)
   (|_Ionic-Compound9873_c0| |instance-of| |Ionic-Compound|)
   (|_Substance9874_c0| |instance-of| |Ionic-Compound-Substance|)
   (|_Chemical-Formula9556_c0| |instance-of| |Chemical-Formula|)
   (|_Ionic-Compound9577_c0| |instance-of| |Ionic-Compound|)
   (|_Substance9578_c0| |instance-of| |Ionic-Compound-Substance|) (|_React7686_c0| |instance-of| |Reaction|)
   (|_Chemical-Formula9346_c0| |instance-of| |Chemical-Formula|)
   (|_Ionic-Compound9368_c0| |instance-of| |Ionic-Compound|)
   (|_Substance9369_c0| |instance-of| |Ionic-Compound-Substance|)
   (|_Chemical-Formula9277_c0| |instance-of| |Chemical-Formula|)
   (|_Ionic-Compound9320_c0| |instance-of| |Ionic-Compound|)
   (|_Substance9370_c0| |instance-of| |Ionic-Compound-Substance|)))

;;Specialized because
;; a) raw-material has cardinality 1 in Reaction skolem
;; b) One of the chemicals hbsu O2
(GET-SPECIALIZED-TRIPLES
 '((|_React7209| |raw-material| |_Chemical1203x1|) 
   (|_React7209| |raw-material| |_Chemical1202x2|)
   (|_React7209| |instance-of| |Reaction|) 
   (|_Chemical1203x1| |instance-of| |Chemical|)
   (|_Chemical1203x1| |has-basic-structural-unit| |_Chemical-Entity8888|) 
   (|_Chemical-Entity8888| |instance-of| |Chemical-Entity|)
   (|_Chemical1202x2| |has-basic-structural-unit| _O27211) 
   (|_Chemical1202x2| |instance-of| |Chemical|) 
   (_O27211 |instance-of| O2)))

;;not specialized.
(get-specialized-triples '((|_Chemical-Entity| |instance-of| |Chemical-Entity|)
			   (|_Chemical| |instance-of| |Chemical|)
			   (|_Chemical| |has-basic-structural-unit| |_Chemical-Entity|)))

(defun test-case()
  (let* ((target
	 '((|_Time-Interval772_c3| |instance-of| |Time-Interval|) (|_Cell802_c3| |instance-of| |Cell|) (|_Cell803_c3| |instance-of| |Cell|)
	   (|_Prophase801_c3| |instance-of| |Prophase|) (|_Prometaphase800_c3| |instance-of| |Prometaphase|)
	   (|_Metaphase799_c3| |instance-of| |Metaphase|) (|_Anaphase798_c3| |instance-of| |Anaphase|)
	   (|_Telophase797_c3| |instance-of| |Telophase|) (|_Time-Interval806_c3| |instance-of| |Time-Interval|)
	   (|_Cell777_c3| |instance-of| |Cell|) (|_Create780_c3| |instance-of| |Create|) (|_Be-Broken779_c3| |instance-of| |Be-Broken|)
	   (|_Cleavage-furrow778_c3| |instance-of| |Cleavage-furrow|) (|_Time-Interval817_c3| |instance-of| |Time-Interval|)
	   (|_Spindle-pole784_c3| |instance-of| |Spindle-pole|) (|_Metaphase-plate781_c3| |instance-of| |Metaphase-plate|)
	   (|_Chromatid786_c3| |instance-of| |Chromatid|) (|_Chromatid774_c3| |instance-of| |Chromatid|)
	   (|_Chromatid773_c3| |instance-of| |Chromatid|) (|_Move775_c3| |instance-of| |Move|) (|_Move776_c3| |instance-of| |Move|)
	   (|_Go-To785_c3| |instance-of| |Go-To|) (|_Cytokinesis796_c3| |instance-of| |Cytokinesis|)
	   (|_Time-Interval841_c3| |instance-of| |Time-Interval|) (|_Move-Together782_c3| |instance-of| |Move-Together|)
	   (|_Time-Interval849_c3| |instance-of| |Time-Interval|) (|_Take-Apart788_c3| |instance-of| |Take-Apart|)
	   (|_Come-Together787_c3| |instance-of| |Come-Together|) (|_Come-Together854_c3| |instance-of| |Come-Together|)
	   (|_Time-Interval861_c3| |instance-of| |Time-Interval|) (|_Contract795_c3| |instance-of| |Contract|)
	   (|_Come-Together794_c3| |instance-of| |Come-Together|) (|_Come-Together793_c3| |instance-of| |Come-Together|)
	   (|_Chromosome874_c3| |instance-of| |Chromosome|) (|_Ribosome877_c3| |instance-of| |Ribosome|)
	   (|_Plasma-membrane878_c3| |instance-of| |Plasma-membrane|) (|_Cytosol876_c3| |instance-of| |Cytosol|)
	   (|_Organism807_c3| |instance-of| |Organism|) (|_Chromosome882_c3| |instance-of| |Chromosome|)
	   (|_Ribosome885_c3| |instance-of| |Ribosome|) (|_Plasma-membrane886_c3| |instance-of| |Plasma-membrane|)
	   (|_Cytosol884_c3| |instance-of| |Cytosol|) (|_Organism888_c3| |instance-of| |Organism|)
	   (|_Carbohydrate892_c3| |instance-of| |Carbohydrate|) (|_Cytoplasm883_c3| |instance-of| |Cytoplasm|)
	   (|_Container896_c3| |instance-of| |Container|) (|_Protein894_c3| |instance-of| |Protein|)
	   (|_Phospholipid895_c3| |instance-of| |Phospholipid|) (|_Shape-Value893_c3| |instance-of| |Shape-Value|)
	   (|_Protein913_c3| |instance-of| |Protein|) (|_RRNA914_c3| |instance-of| RRNA) (|_Synthesis915_c3| |instance-of| |Synthesis|)
	   (|_Gene923_c3| |instance-of| |Gene|) (|_DNA925_c3| |instance-of| DNA) (|_Protein924_c3| |instance-of| |Protein|)
	   (|_Carbohydrate935_c3| |instance-of| |Carbohydrate|) (|_Cytoplasm875_c3| |instance-of| |Cytoplasm|)
	   (|_Container939_c3| |instance-of| |Container|) (|_Protein937_c3| |instance-of| |Protein|)
	   (|_Phospholipid938_c3| |instance-of| |Phospholipid|) (|_Shape-Value936_c3| |instance-of| |Shape-Value|)
	   (|_Protein956_c3| |instance-of| |Protein|) (|_RRNA957_c3| |instance-of| RRNA) (|_Synthesis958_c3| |instance-of| |Synthesis|)
	   (|_Gene966_c3| |instance-of| |Gene|) (|_DNA968_c3| |instance-of| DNA) (|_Protein967_c3| |instance-of| |Protein|)
	   (|_Time-Interval976_c3| |instance-of| |Time-Interval|) (|_Spatial-Entity977_c3| |instance-of| |Spatial-Entity|)
	   (|_Microtubule790_c3| |instance-of| |Microtubule|) (|_Mitotic-spindle789_c3| |instance-of| |Mitotic-spindle|)
	   (|_Go-To791_c3| |instance-of| |Go-To|) (|_Go-To980_c3| |instance-of| |Go-To|)
	   (|_Time-Interval981_c3| |instance-of| |Time-Interval|) (|_Spatial-Entity982_c3| |instance-of| |Spatial-Entity|)
	   (|_Chromatid868_c3| |instance-of| |Chromatid|) (|_Chromatid867_c3| |instance-of| |Chromatid|)
	   (|_Go-To792_c3| |instance-of| |Go-To|) (|_Go-To985_c3| |instance-of| |Go-To|) (|_Go-To986_c3| |instance-of| |Go-To|)
	   (|_Time-Interval987_c3| |instance-of| |Time-Interval|) (|_Chromatin862_c3| |instance-of| |Chromatin|)
	   (|_Chromosome863_c3| |instance-of| |Chromosome|) (|_Time-Interval988_c3| |instance-of| |Time-Interval|)
	   (|_Spindle-pole853_c3| |instance-of| |Spindle-pole|) (|_Microtubule852_c3| |instance-of| |Microtubule|)
	   (|_Mitotic-spindle851_c3| |instance-of| |Mitotic-spindle|) (|_Go-To989_c3| |instance-of| |Go-To|)
	   (|_Go-To990_c3| |instance-of| |Go-To|) (|_Time-Interval991_c3| |instance-of| |Time-Interval|)
	   (|_Microtubule783_c3| |instance-of| |Microtubule|) (|_Go-To993_c3| |instance-of| |Go-To|) (|_Go-To994_c3| |instance-of| |Go-To|)
	   (|_Time-Interval995_c3| |instance-of| |Time-Interval|) (|_Nuclear-envelope850_c3| |instance-of| |Nuclear-envelope|)
	   (|_Be-Broken996_c3| |instance-of| |Be-Broken|) (|_Time-Interval997_c3| |instance-of| |Time-Interval|)
	   (|_Chromatid843_c3| |instance-of| |Chromatid|) (|_Tangible-Entity845_c3| |instance-of| |Tangible-Entity|)
	   (|_Move998_c3| |instance-of| |Move|) (|_Move999_c3| |instance-of| |Move|)
	   (|_Mitotic-spindle844_c3| |instance-of| |Mitotic-spindle|) (|_Time-Interval1000_c3| |instance-of| |Time-Interval|)
	   (|_Shorten821_c3| |instance-of| |Shorten|) (|_Exert-Force1002_c3| |instance-of| |Exert-Force|)
	   (|_Time-Interval1003_c3| |instance-of| |Time-Interval|) (|_Exert-Force1004_c3| |instance-of| |Exert-Force|)
	   (|_Time-Interval1005_c3| |instance-of| |Time-Interval|) (|_Exert-Force1006_c3| |instance-of| |Exert-Force|)
	   (|_Time-Interval1007_c3| |instance-of| |Time-Interval|) (|_Tangible-Entity1008_c3| |instance-of| |Tangible-Entity|)
	   (|_Be-Broken1009_c3| |instance-of| |Be-Broken|) (|_Spatial-Entity1010_c3| |instance-of| |Spatial-Entity|)
	   (|_Dividing-cell1012_c3| |instance-of| |Dividing-cell|) (|_Tangible-Entity1015_c3| |instance-of| |Tangible-Entity|)
	   (|_Spindle-pole1017_c3| |instance-of| |Spindle-pole|) (|_Spatial-Entity1016_c3| |instance-of| |Spatial-Entity|)
	   (|_Dividing-cell1018_c3| |instance-of| |Dividing-cell|) (|_Tangible-Entity1021_c3| |instance-of| |Tangible-Entity|)
	   (|_Time-Interval1022_c3| |instance-of| |Time-Interval|) (|_Physical-Object1023_c3| |instance-of| |Cell|)
	   (|_Time-Interval1024_c3| |instance-of| |Time-Interval|) (|_Nuclear-envelope809_c3| |instance-of| |Nuclear-envelope|)
	   (|_Chromosome1025_c3| |instance-of| |Chromosome|) (|_Ribosome1028_c3| |instance-of| |Ribosome|)
	   (|_Plasma-membrane1029_c3| |instance-of| |Plasma-membrane|) (|_Cytosol1027_c3| |instance-of| |Cytosol|)
	   (|_Organism1031_c3| |instance-of| |Organism|) (|_Mitosis771_c3| |instance-of| |Mitosis|)
	   (|_Cell777_c3| |has-structural-part| |_Plasma-membrane1029_c3|) (|_Cell777_c3| |has-structural-part| |_Cytosol1027_c3|)
	   (|_Cell777_c3| |is-part-of| |_Organism1031_c3|) (|_Cell777_c3| |has-part| |_Cytosol1027_c3|)
	   (|_Cell777_c3| |has-part| |_Plasma-membrane1029_c3|) (|_Cell777_c3| |has-part| |_Ribosome1028_c3|)
	   (|_Cell777_c3| |has-part| |_Chromosome1025_c3|) (|_Cell777_c3| |has-functional-part| |_Plasma-membrane1029_c3|)
	   (|_Cell777_c3| |has-functional-part| |_Ribosome1028_c3|) (|_Cell777_c3| |has-functional-part| |_Chromosome1025_c3|)
	   (|_Create780_c3| |result| |_Nuclear-envelope809_c3|) (|_Create780_c3| |time-during| |_Time-Interval1024_c3|)
	   (|_Be-Broken779_c3| |object| |_Physical-Object1023_c3|) (|_Be-Broken779_c3| |time-during| |_Time-Interval1022_c3|)
	   (|_Time-Interval817_c3| |time-before| |_Time-Interval1007_c3|) (|_Time-Interval817_c3| |time-before| |_Time-Interval806_c3|)
	   (|_Spindle-pole784_c3| |is-region-of| |_Tangible-Entity1021_c3|) (|_Spindle-pole784_c3| |is-region-of| |_Dividing-cell1018_c3|)
	   (|_Spindle-pole784_c3| |location-of| |_Spatial-Entity1016_c3|) (|_Spindle-pole784_c3| |location-of| |_Create780_c3|)
	   (|_Spindle-pole784_c3| |is-opposite| |_Spindle-pole1017_c3|) (|_Spindle-pole784_c3| |destination-of| |_Go-To994_c3|)
	   (|_Spindle-pole784_c3| |destination-of| |_Go-To993_c3|) (|_Metaphase-plate781_c3| |is-region-of| |_Tangible-Entity1015_c3|)
	   (|_Metaphase-plate781_c3| |is-region-of| |_Dividing-cell1012_c3|)
	   (|_Metaphase-plate781_c3| |location-of| |_Spatial-Entity1010_c3|) (|_Chromatid773_c3| |agent-of| |_Go-To785_c3|)
	   (|_Chromatid773_c3| |object-of| |_Come-Together787_c3|) (|_Chromatid773_c3| |object-of| |_Move775_c3|)
	   (|_Chromatid773_c3| |object-of| |_Go-To785_c3|) (|_Cytokinesis796_c3| |resulting-state| |_Be-Broken1009_c3|)
	   (|_Cytokinesis796_c3| |object| |_Tangible-Entity1008_c3|) (|_Cytokinesis796_c3| |time-during| |_Time-Interval1007_c3|)
	   (|_Move775_c3| |caused-by| |_Exert-Force1006_c3|) (|_Move775_c3| |object| |_Chromatid786_c3|)
	   (|_Move775_c3| |origin| |_Metaphase-plate781_c3|) (|_Move775_c3| |destination| |_Spindle-pole784_c3|)
	   (|_Move775_c3| |time-during| |_Time-Interval1005_c3|) (|_Move776_c3| |caused-by| |_Exert-Force1004_c3|)
	   (|_Move776_c3| |object| |_Chromatid774_c3|) (|_Move776_c3| |origin| |_Metaphase-plate781_c3|)
	   (|_Move776_c3| |destination| |_Spindle-pole784_c3|) (|_Move776_c3| |time-during| |_Time-Interval1003_c3|)
	   (|_Go-To785_c3| |agent| |_Mitotic-spindle789_c3|) (|_Go-To785_c3| |agent| |_Chromatid786_c3|)
	   (|_Go-To785_c3| |caused-by| |_Exert-Force1002_c3|) (|_Go-To785_c3| |next-event| |_Shorten821_c3|)
	   (|_Go-To785_c3| |object| |_Mitotic-spindle789_c3|) (|_Go-To785_c3| |object| |_Chromatid786_c3|)
	   (|_Go-To785_c3| |origin| |_Metaphase-plate781_c3|) (|_Go-To785_c3| |destination| |_Spindle-pole784_c3|)
	   (|_Go-To785_c3| |time-during| |_Time-Interval1000_c3|) (|_Time-Interval841_c3| |time-before| |_Time-Interval817_c3|)
	   (|_Move-Together782_c3| |agent| |_Mitotic-spindle844_c3|) (|_Move-Together782_c3| |subevent| |_Move999_c3|)
	   (|_Move-Together782_c3| |subevent| |_Move998_c3|) (|_Move-Together782_c3| |first-subevent| |_Move999_c3|)
	   (|_Move-Together782_c3| |first-subevent| |_Move998_c3|) (|_Move-Together782_c3| |object| |_Tangible-Entity845_c3|)
	   (|_Move-Together782_c3| |object| |_Chromatid843_c3|) (|_Move-Together782_c3| |destination| |_Metaphase-plate781_c3|)
	   (|_Move-Together782_c3| |time-during| |_Time-Interval997_c3|) (|_Time-Interval849_c3| |time-before| |_Time-Interval841_c3|)
	   (|_Take-Apart788_c3| |resulting-state| |_Be-Broken996_c3|) (|_Take-Apart788_c3| |object| |_Nuclear-envelope850_c3|)
	   (|_Take-Apart788_c3| |time-during| |_Time-Interval995_c3|) (|_Come-Together787_c3| |subevent| |_Go-To994_c3|)
	   (|_Come-Together787_c3| |subevent| |_Go-To993_c3|) (|_Come-Together787_c3| |subevent| |_Go-To785_c3|)
	   (|_Come-Together787_c3| |prev-event| |_Take-Apart788_c3|) (|_Come-Together787_c3| |first-subevent| |_Go-To994_c3|)
	   (|_Come-Together787_c3| |first-subevent| |_Go-To993_c3|) (|_Come-Together787_c3| |first-subevent| |_Go-To785_c3|)
	   (|_Come-Together787_c3| |object| |_Mitotic-spindle789_c3|) (|_Come-Together787_c3| |object| |_Microtubule783_c3|)
	   (|_Come-Together787_c3| |object| |_Chromatid786_c3|) (|_Come-Together787_c3| |destination| |_Spindle-pole784_c3|)
	   (|_Come-Together787_c3| |time-during| |_Time-Interval991_c3|) (|_Come-Together854_c3| |subevent| |_Go-To990_c3|)
	   (|_Come-Together854_c3| |subevent| |_Go-To989_c3|) (|_Come-Together854_c3| |prev-event| |_Come-Together787_c3|)
	   (|_Come-Together854_c3| |first-subevent| |_Go-To990_c3|) (|_Come-Together854_c3| |first-subevent| |_Go-To989_c3|)
	   (|_Come-Together854_c3| |object| |_Mitotic-spindle851_c3|) (|_Come-Together854_c3| |object| |_Microtubule852_c3|)
	   (|_Come-Together854_c3| |destination| |_Spindle-pole853_c3|) (|_Come-Together854_c3| |time-during| |_Time-Interval988_c3|)
	   (|_Time-Interval861_c3| |time-before| |_Time-Interval849_c3|) (|_Contract795_c3| |result| |_Chromosome863_c3|)
	   (|_Contract795_c3| |object| |_Chromatin862_c3|) (|_Contract795_c3| |time-during| |_Time-Interval987_c3|)
	   (|_Come-Together794_c3| |subevent| |_Go-To986_c3|) (|_Come-Together794_c3| |subevent| |_Go-To985_c3|)
	   (|_Come-Together794_c3| |subevent| |_Go-To792_c3|) (|_Come-Together794_c3| |first-subevent| |_Go-To986_c3|)
	   (|_Come-Together794_c3| |first-subevent| |_Go-To985_c3|) (|_Come-Together794_c3| |first-subevent| |_Go-To792_c3|)
	   (|_Come-Together794_c3| |object| |_Chromatid867_c3|) (|_Come-Together794_c3| |object| |_Chromatid868_c3|)
	   (|_Come-Together794_c3| |object| |_Mitotic-spindle789_c3|) (|_Come-Together794_c3| |destination| |_Spatial-Entity982_c3|)
	   (|_Come-Together794_c3| |time-during| |_Time-Interval981_c3|) (|_Come-Together793_c3| |subevent| |_Go-To980_c3|)
	   (|_Come-Together793_c3| |subevent| |_Go-To791_c3|) (|_Come-Together793_c3| |first-subevent| |_Go-To980_c3|)
	   (|_Come-Together793_c3| |first-subevent| |_Go-To791_c3|) (|_Come-Together793_c3| |object| |_Mitotic-spindle789_c3|)
	   (|_Come-Together793_c3| |object| |_Microtubule790_c3|) (|_Come-Together793_c3| |destination| |_Spatial-Entity977_c3|)
	   (|_Come-Together793_c3| |time-during| |_Time-Interval976_c3|) (|_Chromosome874_c3| |has-part| |_Protein967_c3|)
	   (|_Chromosome874_c3| |has-part| |_DNA968_c3|) (|_Chromosome874_c3| |has-part| |_Gene966_c3|)
	   (|_Chromosome874_c3| |has-functional-part| |_DNA968_c3|) (|_Chromosome874_c3| |has-functional-part| |_Gene966_c3|)
	   (|_Ribosome877_c3| |agent-of| |_Synthesis958_c3|) (|_Ribosome877_c3| |has-part| |_RRNA957_c3|)
	   (|_Ribosome877_c3| |has-part| |_Protein956_c3|) (|_Ribosome877_c3| |has-functional-part| |_RRNA957_c3|)
	   (|_Ribosome877_c3| |has-functional-part| |_Protein956_c3|) (|_Cytosol876_c3| |encloses| |_Chromosome874_c3|)
	   (|_Cytosol876_c3| |encloses| |_Ribosome877_c3|) (|_Cytosol876_c3| |is-inside| |_Cytoplasm875_c3|)
	   (|_Plasma-membrane878_c3| |shape| |_Shape-Value936_c3|) (|_Plasma-membrane878_c3| |has-structural-part| |_Phospholipid938_c3|)
	   (|_Plasma-membrane878_c3| |has-structural-part| |_Protein937_c3|) (|_Plasma-membrane878_c3| |has-part| |_Carbohydrate935_c3|)
	   (|_Plasma-membrane878_c3| |has-part| |_Phospholipid938_c3|) (|_Plasma-membrane878_c3| |has-part| |_Protein937_c3|)
	   (|_Plasma-membrane878_c3| |has-functional-part| |_Protein937_c3|) (|_Plasma-membrane878_c3| |plays| |_Container939_c3|)
	   (|_Plasma-membrane878_c3| |encloses| |_Ribosome877_c3|) (|_Plasma-membrane878_c3| |encloses| |_Cytoplasm875_c3|)
	   (|_Plasma-membrane878_c3| |encloses| |_Chromosome874_c3|) (|_Plasma-membrane878_c3| |is-between| |_Chromosome874_c3|)
	   (|_Plasma-membrane878_c3| |is-between| |_Cytoplasm875_c3|) (|_Plasma-membrane878_c3| |is-between| |_Carbohydrate935_c3|)
	   (|_Chromosome882_c3| |has-part| |_Protein924_c3|) (|_Chromosome882_c3| |has-part| |_DNA925_c3|)
	   (|_Chromosome882_c3| |has-part| |_Gene923_c3|) (|_Chromosome882_c3| |has-functional-part| |_DNA925_c3|)
	   (|_Chromosome882_c3| |has-functional-part| |_Gene923_c3|) (|_Ribosome885_c3| |agent-of| |_Synthesis915_c3|)
	   (|_Ribosome885_c3| |has-part| |_RRNA914_c3|) (|_Ribosome885_c3| |has-part| |_Protein913_c3|)
	   (|_Ribosome885_c3| |has-functional-part| |_RRNA914_c3|) (|_Ribosome885_c3| |has-functional-part| |_Protein913_c3|)
	   (|_Cytosol884_c3| |encloses| |_Chromosome882_c3|) (|_Cytosol884_c3| |encloses| |_Ribosome885_c3|)
	   (|_Cytosol884_c3| |is-inside| |_Cytoplasm883_c3|) (|_Plasma-membrane886_c3| |shape| |_Shape-Value893_c3|)
	   (|_Plasma-membrane886_c3| |has-structural-part| |_Phospholipid895_c3|)
	   (|_Plasma-membrane886_c3| |has-structural-part| |_Protein894_c3|) (|_Plasma-membrane886_c3| |has-part| |_Carbohydrate892_c3|)
	   (|_Plasma-membrane886_c3| |has-part| |_Phospholipid895_c3|) (|_Plasma-membrane886_c3| |has-part| |_Protein894_c3|)
	   (|_Plasma-membrane886_c3| |has-functional-part| |_Protein894_c3|) (|_Plasma-membrane886_c3| |plays| |_Container896_c3|)
	   (|_Plasma-membrane886_c3| |encloses| |_Ribosome885_c3|) (|_Plasma-membrane886_c3| |encloses| |_Cytoplasm883_c3|)
	   (|_Plasma-membrane886_c3| |encloses| |_Chromosome882_c3|) (|_Plasma-membrane886_c3| |is-between| |_Chromosome882_c3|)
	   (|_Plasma-membrane886_c3| |is-between| |_Cytoplasm883_c3|) (|_Plasma-membrane886_c3| |is-between| |_Carbohydrate892_c3|)
	   (|_Cell802_c3| |has-structural-part| |_Plasma-membrane886_c3|) (|_Cell802_c3| |has-structural-part| |_Cytosol884_c3|)
	   (|_Cell802_c3| |is-part-of| |_Organism888_c3|) (|_Cell802_c3| |has-part| |_Cytosol884_c3|)
	   (|_Cell802_c3| |has-part| |_Plasma-membrane886_c3|) (|_Cell802_c3| |has-part| |_Ribosome885_c3|)
	   (|_Cell802_c3| |has-part| |_Chromosome882_c3|) (|_Cell802_c3| |has-functional-part| |_Plasma-membrane886_c3|)
	   (|_Cell802_c3| |has-functional-part| |_Ribosome885_c3|) (|_Cell802_c3| |has-functional-part| |_Chromosome882_c3|)
	   (|_Cell803_c3| |has-structural-part| |_Plasma-membrane878_c3|) (|_Cell803_c3| |has-structural-part| |_Cytosol876_c3|)
	   (|_Cell803_c3| |is-part-of| |_Organism807_c3|) (|_Cell803_c3| |has-part| |_Cytosol876_c3|)
	   (|_Cell803_c3| |has-part| |_Plasma-membrane878_c3|) (|_Cell803_c3| |has-part| |_Ribosome877_c3|)
	   (|_Cell803_c3| |has-part| |_Chromosome874_c3|) (|_Cell803_c3| |has-functional-part| |_Plasma-membrane878_c3|)
	   (|_Cell803_c3| |has-functional-part| |_Ribosome877_c3|) (|_Cell803_c3| |has-functional-part| |_Chromosome874_c3|)
	   (|_Prophase801_c3| |subevent| |_Come-Together793_c3|) (|_Prophase801_c3| |subevent| |_Come-Together794_c3|)
	   (|_Prophase801_c3| |subevent| |_Contract795_c3|) (|_Prophase801_c3| |time-during| |_Time-Interval861_c3|)
	   (|_Prometaphase800_c3| |subevent| |_Come-Together854_c3|) (|_Prometaphase800_c3| |subevent| |_Come-Together787_c3|)
	   (|_Prometaphase800_c3| |subevent| |_Take-Apart788_c3|) (|_Prometaphase800_c3| |prev-event| |_Prophase801_c3|)
	   (|_Prometaphase800_c3| |first-subevent| |_Take-Apart788_c3|) (|_Prometaphase800_c3| |time-during| |_Time-Interval849_c3|)
	   (|_Metaphase799_c3| |subevent| |_Move-Together782_c3|) (|_Metaphase799_c3| |prev-event| |_Prometaphase800_c3|)
	   (|_Metaphase799_c3| |time-during| |_Time-Interval841_c3|) (|_Anaphase798_c3| |subevent| |_Go-To785_c3|)
	   (|_Anaphase798_c3| |subevent| |_Move776_c3|) (|_Anaphase798_c3| |subevent| |_Move775_c3|)
	   (|_Anaphase798_c3| |prev-event| |_Metaphase799_c3|) (|_Anaphase798_c3| |next-event| |_Cytokinesis796_c3|)
	   (|_Anaphase798_c3| |first-subevent| |_Go-To785_c3|) (|_Anaphase798_c3| |first-subevent| |_Move776_c3|)
	   (|_Anaphase798_c3| |first-subevent| |_Move775_c3|) (|_Anaphase798_c3| |object| |_Chromatid773_c3|)
	   (|_Anaphase798_c3| |object| |_Chromatid774_c3|) (|_Anaphase798_c3| |object| |_Chromatid786_c3|)
	   (|_Anaphase798_c3| |origin| |_Metaphase-plate781_c3|) (|_Anaphase798_c3| |destination| |_Spindle-pole784_c3|)
	   (|_Anaphase798_c3| |time-during| |_Time-Interval817_c3|) (|_Telophase797_c3| |instrument| |_Cleavage-furrow778_c3|)
	   (|_Telophase797_c3| |resulting-state| |_Be-Broken779_c3|) (|_Telophase797_c3| |subevent| |_Create780_c3|)
	   (|_Telophase797_c3| |prev-event| |_Anaphase798_c3|) (|_Telophase797_c3| |first-subevent| |_Create780_c3|)
	   (|_Telophase797_c3| |result| |_Cell777_c3|) (|_Telophase797_c3| |result| |_Cell803_c3|)
	   (|_Telophase797_c3| |object| |_Cell802_c3|) (|_Telophase797_c3| |time-during| |_Time-Interval806_c3|)
	   (|_Mitosis771_c3| |subevent| |_Telophase797_c3|) (|_Mitosis771_c3| |subevent| |_Anaphase798_c3|)
	   (|_Mitosis771_c3| |subevent| |_Metaphase799_c3|) (|_Mitosis771_c3| |subevent| |_Prometaphase800_c3|)
	   (|_Mitosis771_c3| |subevent| |_Prophase801_c3|) (|_Mitosis771_c3| |first-subevent| |_Prophase801_c3|)
	   (|_Mitosis771_c3| |result| |_Cell803_c3|) (|_Mitosis771_c3| |object| |_Cell802_c3|)
	   (|_Mitosis771_c3| |time-during| |_Time-Interval772_c3|)))
	(specialized-triples     (get-specialized-triples target))
	(non-specialized-triples (set-difference target specialized-triples :test 'ps-triple-equal)))
    (format t "Total triples       : ~a~%" (length target))
    (format t "Specialized triples : ~a~%" (length specialized-triples))
    (format t "Skolem triples      : ~a~%" (length non-specialized-triples))))
    
(determine-necessary-triples
 '((|_Time-Interval772_c3| |instance-of| |Time-Interval|) (|_Cell802_c3| |instance-of| |Cell|) (|_Cell803_c3| |instance-of| |Cell|)
   (|_Prophase801_c3| |instance-of| |Prophase|) (|_Prometaphase800_c3| |instance-of| |Prometaphase|)
   (|_Metaphase799_c3| |instance-of| |Metaphase|) (|_Anaphase798_c3| |instance-of| |Anaphase|)
   (|_Telophase797_c3| |instance-of| |Telophase|) (|_Time-Interval806_c3| |instance-of| |Time-Interval|)
   (|_Cell777_c3| |instance-of| |Cell|) (|_Create780_c3| |instance-of| |Create|) (|_Be-Broken779_c3| |instance-of| |Be-Broken|)
   (|_Cleavage-furrow778_c3| |instance-of| |Cleavage-furrow|) (|_Time-Interval817_c3| |instance-of| |Time-Interval|)
   (|_Spindle-pole784_c3| |instance-of| |Spindle-pole|) (|_Metaphase-plate781_c3| |instance-of| |Metaphase-plate|)
   (|_Chromatid786_c3| |instance-of| |Chromatid|) (|_Chromatid774_c3| |instance-of| |Chromatid|)
   (|_Chromatid773_c3| |instance-of| |Chromatid|) (|_Move775_c3| |instance-of| |Move|) (|_Move776_c3| |instance-of| |Move|)
   (|_Go-To785_c3| |instance-of| |Go-To|) (|_Cytokinesis796_c3| |instance-of| |Cytokinesis|)
   (|_Time-Interval841_c3| |instance-of| |Time-Interval|) (|_Move-Together782_c3| |instance-of| |Move-Together|)
   (|_Time-Interval849_c3| |instance-of| |Time-Interval|) (|_Take-Apart788_c3| |instance-of| |Take-Apart|)
   (|_Come-Together787_c3| |instance-of| |Come-Together|) (|_Come-Together854_c3| |instance-of| |Come-Together|)
   (|_Time-Interval861_c3| |instance-of| |Time-Interval|) (|_Contract795_c3| |instance-of| |Contract|)
   (|_Come-Together794_c3| |instance-of| |Come-Together|) (|_Come-Together793_c3| |instance-of| |Come-Together|)
   (|_Chromosome874_c3| |instance-of| |Chromosome|) (|_Ribosome877_c3| |instance-of| |Ribosome|)
   (|_Plasma-membrane878_c3| |instance-of| |Plasma-membrane|) (|_Cytosol876_c3| |instance-of| |Cytosol|)
   (|_Organism807_c3| |instance-of| |Organism|) (|_Chromosome882_c3| |instance-of| |Chromosome|)
   (|_Ribosome885_c3| |instance-of| |Ribosome|) (|_Plasma-membrane886_c3| |instance-of| |Plasma-membrane|)
   (|_Cytosol884_c3| |instance-of| |Cytosol|) (|_Organism888_c3| |instance-of| |Organism|)
   (|_Carbohydrate892_c3| |instance-of| |Carbohydrate|) (|_Cytoplasm883_c3| |instance-of| |Cytoplasm|)
   (|_Container896_c3| |instance-of| |Container|) (|_Protein894_c3| |instance-of| |Protein|)
   (|_Phospholipid895_c3| |instance-of| |Phospholipid|) (|_Shape-Value893_c3| |instance-of| |Shape-Value|)
   (|_Protein913_c3| |instance-of| |Protein|) (|_RRNA914_c3| |instance-of| RRNA) (|_Synthesis915_c3| |instance-of| |Synthesis|)
   (|_Gene923_c3| |instance-of| |Gene|) (|_DNA925_c3| |instance-of| DNA) (|_Protein924_c3| |instance-of| |Protein|)
   (|_Carbohydrate935_c3| |instance-of| |Carbohydrate|) (|_Cytoplasm875_c3| |instance-of| |Cytoplasm|)
   (|_Container939_c3| |instance-of| |Container|) (|_Protein937_c3| |instance-of| |Protein|)
   (|_Phospholipid938_c3| |instance-of| |Phospholipid|) (|_Shape-Value936_c3| |instance-of| |Shape-Value|)
   (|_Protein956_c3| |instance-of| |Protein|) (|_RRNA957_c3| |instance-of| RRNA) (|_Synthesis958_c3| |instance-of| |Synthesis|)
   (|_Gene966_c3| |instance-of| |Gene|) (|_DNA968_c3| |instance-of| DNA) (|_Protein967_c3| |instance-of| |Protein|)
   (|_Time-Interval976_c3| |instance-of| |Time-Interval|) (|_Spatial-Entity977_c3| |instance-of| |Spatial-Entity|)
   (|_Microtubule790_c3| |instance-of| |Microtubule|) (|_Mitotic-spindle789_c3| |instance-of| |Mitotic-spindle|)
   (|_Go-To791_c3| |instance-of| |Go-To|) (|_Go-To980_c3| |instance-of| |Go-To|)
   (|_Time-Interval981_c3| |instance-of| |Time-Interval|) (|_Spatial-Entity982_c3| |instance-of| |Spatial-Entity|)
   (|_Chromatid868_c3| |instance-of| |Chromatid|) (|_Chromatid867_c3| |instance-of| |Chromatid|)
   (|_Go-To792_c3| |instance-of| |Go-To|) (|_Go-To985_c3| |instance-of| |Go-To|) (|_Go-To986_c3| |instance-of| |Go-To|)
   (|_Time-Interval987_c3| |instance-of| |Time-Interval|) (|_Chromatin862_c3| |instance-of| |Chromatin|)
   (|_Chromosome863_c3| |instance-of| |Chromosome|) (|_Time-Interval988_c3| |instance-of| |Time-Interval|)
   (|_Spindle-pole853_c3| |instance-of| |Spindle-pole|) (|_Microtubule852_c3| |instance-of| |Microtubule|)
   (|_Mitotic-spindle851_c3| |instance-of| |Mitotic-spindle|) (|_Go-To989_c3| |instance-of| |Go-To|)
   (|_Go-To990_c3| |instance-of| |Go-To|) (|_Time-Interval991_c3| |instance-of| |Time-Interval|)
   (|_Microtubule783_c3| |instance-of| |Microtubule|) (|_Go-To993_c3| |instance-of| |Go-To|) (|_Go-To994_c3| |instance-of| |Go-To|)
   (|_Time-Interval995_c3| |instance-of| |Time-Interval|) (|_Nuclear-envelope850_c3| |instance-of| |Nuclear-envelope|)
   (|_Be-Broken996_c3| |instance-of| |Be-Broken|) (|_Time-Interval997_c3| |instance-of| |Time-Interval|)
   (|_Chromatid843_c3| |instance-of| |Chromatid|) (|_Tangible-Entity845_c3| |instance-of| |Tangible-Entity|)
   (|_Move998_c3| |instance-of| |Move|) (|_Move999_c3| |instance-of| |Move|)
   (|_Mitotic-spindle844_c3| |instance-of| |Mitotic-spindle|) (|_Time-Interval1000_c3| |instance-of| |Time-Interval|)
   (|_Shorten821_c3| |instance-of| |Shorten|) (|_Exert-Force1002_c3| |instance-of| |Exert-Force|)
   (|_Time-Interval1003_c3| |instance-of| |Time-Interval|) (|_Exert-Force1004_c3| |instance-of| |Exert-Force|)
   (|_Time-Interval1005_c3| |instance-of| |Time-Interval|) (|_Exert-Force1006_c3| |instance-of| |Exert-Force|)
   (|_Time-Interval1007_c3| |instance-of| |Time-Interval|) (|_Tangible-Entity1008_c3| |instance-of| |Tangible-Entity|)
   (|_Be-Broken1009_c3| |instance-of| |Be-Broken|) (|_Spatial-Entity1010_c3| |instance-of| |Spatial-Entity|)
   (|_Dividing-cell1012_c3| |instance-of| |Dividing-cell|) (|_Tangible-Entity1015_c3| |instance-of| |Tangible-Entity|)
   (|_Spindle-pole1017_c3| |instance-of| |Spindle-pole|) (|_Spatial-Entity1016_c3| |instance-of| |Spatial-Entity|)
   (|_Dividing-cell1018_c3| |instance-of| |Dividing-cell|) (|_Tangible-Entity1021_c3| |instance-of| |Tangible-Entity|)
   (|_Time-Interval1022_c3| |instance-of| |Time-Interval|) (|_Physical-Object1023_c3| |instance-of| |Cell|)
   (|_Time-Interval1024_c3| |instance-of| |Time-Interval|) (|_Nuclear-envelope809_c3| |instance-of| |Nuclear-envelope|)
   (|_Chromosome1025_c3| |instance-of| |Chromosome|) (|_Ribosome1028_c3| |instance-of| |Ribosome|)
   (|_Plasma-membrane1029_c3| |instance-of| |Plasma-membrane|) (|_Cytosol1027_c3| |instance-of| |Cytosol|)
   (|_Organism1031_c3| |instance-of| |Organism|) (|_Mitosis771_c3| |instance-of| |Mitosis|)
   (|_Cell777_c3| |has-structural-part| |_Plasma-membrane1029_c3|) (|_Cell777_c3| |has-structural-part| |_Cytosol1027_c3|)
   (|_Cell777_c3| |is-part-of| |_Organism1031_c3|) (|_Cell777_c3| |has-part| |_Cytosol1027_c3|)
   (|_Cell777_c3| |has-part| |_Plasma-membrane1029_c3|) (|_Cell777_c3| |has-part| |_Ribosome1028_c3|)
   (|_Cell777_c3| |has-part| |_Chromosome1025_c3|) (|_Cell777_c3| |has-functional-part| |_Plasma-membrane1029_c3|)
   (|_Cell777_c3| |has-functional-part| |_Ribosome1028_c3|) (|_Cell777_c3| |has-functional-part| |_Chromosome1025_c3|)
   (|_Create780_c3| |result| |_Nuclear-envelope809_c3|) (|_Create780_c3| |time-during| |_Time-Interval1024_c3|)
   (|_Be-Broken779_c3| |object| |_Physical-Object1023_c3|) (|_Be-Broken779_c3| |time-during| |_Time-Interval1022_c3|)
   (|_Time-Interval817_c3| |time-before| |_Time-Interval1007_c3|) (|_Time-Interval817_c3| |time-before| |_Time-Interval806_c3|)
   (|_Spindle-pole784_c3| |is-region-of| |_Tangible-Entity1021_c3|) (|_Spindle-pole784_c3| |is-region-of| |_Dividing-cell1018_c3|)
   (|_Spindle-pole784_c3| |location-of| |_Spatial-Entity1016_c3|) (|_Spindle-pole784_c3| |location-of| |_Create780_c3|)
   (|_Spindle-pole784_c3| |is-opposite| |_Spindle-pole1017_c3|) (|_Spindle-pole784_c3| |destination-of| |_Go-To994_c3|)
   (|_Spindle-pole784_c3| |destination-of| |_Go-To993_c3|) (|_Metaphase-plate781_c3| |is-region-of| |_Tangible-Entity1015_c3|)
   (|_Metaphase-plate781_c3| |is-region-of| |_Dividing-cell1012_c3|)
   (|_Metaphase-plate781_c3| |location-of| |_Spatial-Entity1010_c3|) (|_Chromatid773_c3| |agent-of| |_Go-To785_c3|)
   (|_Chromatid773_c3| |object-of| |_Come-Together787_c3|) (|_Chromatid773_c3| |object-of| |_Move775_c3|)
   (|_Chromatid773_c3| |object-of| |_Go-To785_c3|) (|_Cytokinesis796_c3| |resulting-state| |_Be-Broken1009_c3|)
   (|_Cytokinesis796_c3| |object| |_Tangible-Entity1008_c3|) (|_Cytokinesis796_c3| |time-during| |_Time-Interval1007_c3|)
   (|_Move775_c3| |caused-by| |_Exert-Force1006_c3|) (|_Move775_c3| |object| |_Chromatid786_c3|)
   (|_Move775_c3| |origin| |_Metaphase-plate781_c3|) (|_Move775_c3| |destination| |_Spindle-pole784_c3|)
   (|_Move775_c3| |time-during| |_Time-Interval1005_c3|) (|_Move776_c3| |caused-by| |_Exert-Force1004_c3|)
   (|_Move776_c3| |object| |_Chromatid774_c3|) (|_Move776_c3| |origin| |_Metaphase-plate781_c3|)
   (|_Move776_c3| |destination| |_Spindle-pole784_c3|) (|_Move776_c3| |time-during| |_Time-Interval1003_c3|)
   (|_Go-To785_c3| |agent| |_Mitotic-spindle789_c3|) (|_Go-To785_c3| |agent| |_Chromatid786_c3|)
   (|_Go-To785_c3| |caused-by| |_Exert-Force1002_c3|) (|_Go-To785_c3| |next-event| |_Shorten821_c3|)
   (|_Go-To785_c3| |object| |_Mitotic-spindle789_c3|) (|_Go-To785_c3| |object| |_Chromatid786_c3|)
   (|_Go-To785_c3| |origin| |_Metaphase-plate781_c3|) (|_Go-To785_c3| |destination| |_Spindle-pole784_c3|)
   (|_Go-To785_c3| |time-during| |_Time-Interval1000_c3|) (|_Time-Interval841_c3| |time-before| |_Time-Interval817_c3|)
   (|_Move-Together782_c3| |agent| |_Mitotic-spindle844_c3|) (|_Move-Together782_c3| |subevent| |_Move999_c3|)
   (|_Move-Together782_c3| |subevent| |_Move998_c3|) (|_Move-Together782_c3| |first-subevent| |_Move999_c3|)
   (|_Move-Together782_c3| |first-subevent| |_Move998_c3|) (|_Move-Together782_c3| |object| |_Tangible-Entity845_c3|)
   (|_Move-Together782_c3| |object| |_Chromatid843_c3|) (|_Move-Together782_c3| |destination| |_Metaphase-plate781_c3|)
   (|_Move-Together782_c3| |time-during| |_Time-Interval997_c3|) (|_Time-Interval849_c3| |time-before| |_Time-Interval841_c3|)
   (|_Take-Apart788_c3| |resulting-state| |_Be-Broken996_c3|) (|_Take-Apart788_c3| |object| |_Nuclear-envelope850_c3|)
   (|_Take-Apart788_c3| |time-during| |_Time-Interval995_c3|) (|_Come-Together787_c3| |subevent| |_Go-To994_c3|)
   (|_Come-Together787_c3| |subevent| |_Go-To993_c3|) (|_Come-Together787_c3| |subevent| |_Go-To785_c3|)
   (|_Come-Together787_c3| |prev-event| |_Take-Apart788_c3|) (|_Come-Together787_c3| |first-subevent| |_Go-To994_c3|)
   (|_Come-Together787_c3| |first-subevent| |_Go-To993_c3|) (|_Come-Together787_c3| |first-subevent| |_Go-To785_c3|)
   (|_Come-Together787_c3| |object| |_Mitotic-spindle789_c3|) (|_Come-Together787_c3| |object| |_Microtubule783_c3|)
   (|_Come-Together787_c3| |object| |_Chromatid786_c3|) (|_Come-Together787_c3| |destination| |_Spindle-pole784_c3|)
   (|_Come-Together787_c3| |time-during| |_Time-Interval991_c3|) (|_Come-Together854_c3| |subevent| |_Go-To990_c3|)
   (|_Come-Together854_c3| |subevent| |_Go-To989_c3|) (|_Come-Together854_c3| |prev-event| |_Come-Together787_c3|)
   (|_Come-Together854_c3| |first-subevent| |_Go-To990_c3|) (|_Come-Together854_c3| |first-subevent| |_Go-To989_c3|)
   (|_Come-Together854_c3| |object| |_Mitotic-spindle851_c3|) (|_Come-Together854_c3| |object| |_Microtubule852_c3|)
   (|_Come-Together854_c3| |destination| |_Spindle-pole853_c3|) (|_Come-Together854_c3| |time-during| |_Time-Interval988_c3|)
   (|_Time-Interval861_c3| |time-before| |_Time-Interval849_c3|) (|_Contract795_c3| |result| |_Chromosome863_c3|)
   (|_Contract795_c3| |object| |_Chromatin862_c3|) (|_Contract795_c3| |time-during| |_Time-Interval987_c3|)
   (|_Come-Together794_c3| |subevent| |_Go-To986_c3|) (|_Come-Together794_c3| |subevent| |_Go-To985_c3|)
   (|_Come-Together794_c3| |subevent| |_Go-To792_c3|) (|_Come-Together794_c3| |first-subevent| |_Go-To986_c3|)
   (|_Come-Together794_c3| |first-subevent| |_Go-To985_c3|) (|_Come-Together794_c3| |first-subevent| |_Go-To792_c3|)
   (|_Come-Together794_c3| |object| |_Chromatid867_c3|) (|_Come-Together794_c3| |object| |_Chromatid868_c3|)
   (|_Come-Together794_c3| |object| |_Mitotic-spindle789_c3|) (|_Come-Together794_c3| |destination| |_Spatial-Entity982_c3|)
   (|_Come-Together794_c3| |time-during| |_Time-Interval981_c3|) (|_Come-Together793_c3| |subevent| |_Go-To980_c3|)
   (|_Come-Together793_c3| |subevent| |_Go-To791_c3|) (|_Come-Together793_c3| |first-subevent| |_Go-To980_c3|)
   (|_Come-Together793_c3| |first-subevent| |_Go-To791_c3|) (|_Come-Together793_c3| |object| |_Mitotic-spindle789_c3|)
   (|_Come-Together793_c3| |object| |_Microtubule790_c3|) (|_Come-Together793_c3| |destination| |_Spatial-Entity977_c3|)
   (|_Come-Together793_c3| |time-during| |_Time-Interval976_c3|) (|_Chromosome874_c3| |has-part| |_Protein967_c3|)
   (|_Chromosome874_c3| |has-part| |_DNA968_c3|) (|_Chromosome874_c3| |has-part| |_Gene966_c3|)
   (|_Chromosome874_c3| |has-functional-part| |_DNA968_c3|) (|_Chromosome874_c3| |has-functional-part| |_Gene966_c3|)
   (|_Ribosome877_c3| |agent-of| |_Synthesis958_c3|) (|_Ribosome877_c3| |has-part| |_RRNA957_c3|)
   (|_Ribosome877_c3| |has-part| |_Protein956_c3|) (|_Ribosome877_c3| |has-functional-part| |_RRNA957_c3|)
   (|_Ribosome877_c3| |has-functional-part| |_Protein956_c3|) (|_Cytosol876_c3| |encloses| |_Chromosome874_c3|)
   (|_Cytosol876_c3| |encloses| |_Ribosome877_c3|) (|_Cytosol876_c3| |is-inside| |_Cytoplasm875_c3|)
   (|_Plasma-membrane878_c3| |shape| |_Shape-Value936_c3|) (|_Plasma-membrane878_c3| |has-structural-part| |_Phospholipid938_c3|)
   (|_Plasma-membrane878_c3| |has-structural-part| |_Protein937_c3|) (|_Plasma-membrane878_c3| |has-part| |_Carbohydrate935_c3|)
   (|_Plasma-membrane878_c3| |has-part| |_Phospholipid938_c3|) (|_Plasma-membrane878_c3| |has-part| |_Protein937_c3|)
   (|_Plasma-membrane878_c3| |has-functional-part| |_Protein937_c3|) (|_Plasma-membrane878_c3| |plays| |_Container939_c3|)
   (|_Plasma-membrane878_c3| |encloses| |_Ribosome877_c3|) (|_Plasma-membrane878_c3| |encloses| |_Cytoplasm875_c3|)
   (|_Plasma-membrane878_c3| |encloses| |_Chromosome874_c3|) (|_Plasma-membrane878_c3| |is-between| |_Chromosome874_c3|)
   (|_Plasma-membrane878_c3| |is-between| |_Cytoplasm875_c3|) (|_Plasma-membrane878_c3| |is-between| |_Carbohydrate935_c3|)
   (|_Chromosome882_c3| |has-part| |_Protein924_c3|) (|_Chromosome882_c3| |has-part| |_DNA925_c3|)
   (|_Chromosome882_c3| |has-part| |_Gene923_c3|) (|_Chromosome882_c3| |has-functional-part| |_DNA925_c3|)
   (|_Chromosome882_c3| |has-functional-part| |_Gene923_c3|) (|_Ribosome885_c3| |agent-of| |_Synthesis915_c3|)
   (|_Ribosome885_c3| |has-part| |_RRNA914_c3|) (|_Ribosome885_c3| |has-part| |_Protein913_c3|)
   (|_Ribosome885_c3| |has-functional-part| |_RRNA914_c3|) (|_Ribosome885_c3| |has-functional-part| |_Protein913_c3|)
   (|_Cytosol884_c3| |encloses| |_Chromosome882_c3|) (|_Cytosol884_c3| |encloses| |_Ribosome885_c3|)
   (|_Cytosol884_c3| |is-inside| |_Cytoplasm883_c3|) (|_Plasma-membrane886_c3| |shape| |_Shape-Value893_c3|)
   (|_Plasma-membrane886_c3| |has-structural-part| |_Phospholipid895_c3|)
   (|_Plasma-membrane886_c3| |has-structural-part| |_Protein894_c3|) (|_Plasma-membrane886_c3| |has-part| |_Carbohydrate892_c3|)
   (|_Plasma-membrane886_c3| |has-part| |_Phospholipid895_c3|) (|_Plasma-membrane886_c3| |has-part| |_Protein894_c3|)
   (|_Plasma-membrane886_c3| |has-functional-part| |_Protein894_c3|) (|_Plasma-membrane886_c3| |plays| |_Container896_c3|)
   (|_Plasma-membrane886_c3| |encloses| |_Ribosome885_c3|) (|_Plasma-membrane886_c3| |encloses| |_Cytoplasm883_c3|)
   (|_Plasma-membrane886_c3| |encloses| |_Chromosome882_c3|) (|_Plasma-membrane886_c3| |is-between| |_Chromosome882_c3|)
   (|_Plasma-membrane886_c3| |is-between| |_Cytoplasm883_c3|) (|_Plasma-membrane886_c3| |is-between| |_Carbohydrate892_c3|)
   (|_Cell802_c3| |has-structural-part| |_Plasma-membrane886_c3|) (|_Cell802_c3| |has-structural-part| |_Cytosol884_c3|)
   (|_Cell802_c3| |is-part-of| |_Organism888_c3|) (|_Cell802_c3| |has-part| |_Cytosol884_c3|)
   (|_Cell802_c3| |has-part| |_Plasma-membrane886_c3|) (|_Cell802_c3| |has-part| |_Ribosome885_c3|)
   (|_Cell802_c3| |has-part| |_Chromosome882_c3|) (|_Cell802_c3| |has-functional-part| |_Plasma-membrane886_c3|)
   (|_Cell802_c3| |has-functional-part| |_Ribosome885_c3|) (|_Cell802_c3| |has-functional-part| |_Chromosome882_c3|)
   (|_Cell803_c3| |has-structural-part| |_Plasma-membrane878_c3|) (|_Cell803_c3| |has-structural-part| |_Cytosol876_c3|)
   (|_Cell803_c3| |is-part-of| |_Organism807_c3|) (|_Cell803_c3| |has-part| |_Cytosol876_c3|)
   (|_Cell803_c3| |has-part| |_Plasma-membrane878_c3|) (|_Cell803_c3| |has-part| |_Ribosome877_c3|)
   (|_Cell803_c3| |has-part| |_Chromosome874_c3|) (|_Cell803_c3| |has-functional-part| |_Plasma-membrane878_c3|)
   (|_Cell803_c3| |has-functional-part| |_Ribosome877_c3|) (|_Cell803_c3| |has-functional-part| |_Chromosome874_c3|)
   (|_Prophase801_c3| |subevent| |_Come-Together793_c3|) (|_Prophase801_c3| |subevent| |_Come-Together794_c3|)
   (|_Prophase801_c3| |subevent| |_Contract795_c3|) (|_Prophase801_c3| |time-during| |_Time-Interval861_c3|)
   (|_Prometaphase800_c3| |subevent| |_Come-Together854_c3|) (|_Prometaphase800_c3| |subevent| |_Come-Together787_c3|)
   (|_Prometaphase800_c3| |subevent| |_Take-Apart788_c3|) (|_Prometaphase800_c3| |prev-event| |_Prophase801_c3|)
   (|_Prometaphase800_c3| |first-subevent| |_Take-Apart788_c3|) (|_Prometaphase800_c3| |time-during| |_Time-Interval849_c3|)
   (|_Metaphase799_c3| |subevent| |_Move-Together782_c3|) (|_Metaphase799_c3| |prev-event| |_Prometaphase800_c3|)
   (|_Metaphase799_c3| |time-during| |_Time-Interval841_c3|) (|_Anaphase798_c3| |subevent| |_Go-To785_c3|)
   (|_Anaphase798_c3| |subevent| |_Move776_c3|) (|_Anaphase798_c3| |subevent| |_Move775_c3|)
   (|_Anaphase798_c3| |prev-event| |_Metaphase799_c3|) (|_Anaphase798_c3| |next-event| |_Cytokinesis796_c3|)
   (|_Anaphase798_c3| |first-subevent| |_Go-To785_c3|) (|_Anaphase798_c3| |first-subevent| |_Move776_c3|)
   (|_Anaphase798_c3| |first-subevent| |_Move775_c3|) (|_Anaphase798_c3| |object| |_Chromatid773_c3|)
   (|_Anaphase798_c3| |object| |_Chromatid774_c3|) (|_Anaphase798_c3| |object| |_Chromatid786_c3|)
   (|_Anaphase798_c3| |origin| |_Metaphase-plate781_c3|) (|_Anaphase798_c3| |destination| |_Spindle-pole784_c3|)
   (|_Anaphase798_c3| |time-during| |_Time-Interval817_c3|) (|_Telophase797_c3| |instrument| |_Cleavage-furrow778_c3|)
   (|_Telophase797_c3| |resulting-state| |_Be-Broken779_c3|) (|_Telophase797_c3| |subevent| |_Create780_c3|)
   (|_Telophase797_c3| |prev-event| |_Anaphase798_c3|) (|_Telophase797_c3| |first-subevent| |_Create780_c3|)
   (|_Telophase797_c3| |result| |_Cell777_c3|) (|_Telophase797_c3| |result| |_Cell803_c3|)
   (|_Telophase797_c3| |object| |_Cell802_c3|) (|_Telophase797_c3| |time-during| |_Time-Interval806_c3|)
   (|_Mitosis771_c3| |subevent| |_Telophase797_c3|) (|_Mitosis771_c3| |subevent| |_Anaphase798_c3|)
   (|_Mitosis771_c3| |subevent| |_Metaphase799_c3|) (|_Mitosis771_c3| |subevent| |_Prometaphase800_c3|)
   (|_Mitosis771_c3| |subevent| |_Prophase801_c3|) (|_Mitosis771_c3| |first-subevent| |_Prophase801_c3|)
   (|_Mitosis771_c3| |result| |_Cell803_c3|) (|_Mitosis771_c3| |object| |_Cell802_c3|)
   (|_Mitosis771_c3| |time-during| |_Time-Interval772_c3|)))

|#

#|deprecated!

;; ;;FIXME: Should output be strictly boolean?
;; ;;Returns "true" if either
;; ;;a) filler for slot is different from instance-of concepts, i.e., not subsumable
;; ;;b) filler is subsumable, but it is specialized.
;; deprecated (defun specialized-triple-p (scenario triple)
;;   (if (not (instance-triple-p triple))
;;       (quasi-properly-terminate-triple-list
;;        (or
;; 	 (specialized-triple-with-filler-of-different-type scenario triple)
;; 	 (specialized-triple-with-non-skolem-filler scenario triple))
;;        scenario)))


;; ;;Returns the specialized-triple-with-filler-of-differeny-type subset of target-triple-lst
;; ;;i.e. triples whose slot filler concept differs from skolem version
;; deprecated (defun get-triples-with-specialized-root-slot-filler (scenario 
;; 						      target-triple-lst)
;;   (mappend #'(lambda(target-triple)
;; 	       (triple-with-non-skolem-filler? scenario target-triple))
;; 	   (extract-non-instance-triples target-triple-lst)))

;; ;;Returns "true" if either
;; ;;filler is subsumable, but specialized.
;; deprecated (defun triple-with-non-skolem-filler?(scenario triple)
;;   (let* ((tail     (triple-tail     triple))
;; 	 (special-tail-triples
;; 	  (specialized-p scenario tail))) ;;is slot filler a skolem?
;;     (if (not (null special-tail-triples))
;; 	(cons triple special-tail-triples))))

;; ;;Finds the set of "necessary triples", i.e. containing extra information not in concept defn.
;; ;;FIXME: Performance tweak, make sure all necessary concepts are in cache before using this routine. 
;; ;;       The cache-miss is very expensive, as evidenced in "What is X?" questions which refers to 
;; ;;       many concepts in KB.
;; deprecated (defun determine-necessary-triples (km-triples)
;;   (let ((all-concepts (extract-all-concepts-in-triple-list km-triples)))
;;     (cond ((not (cached-p all-concepts)) km-triples)
;; 	  (t (let ((specialized-triples (get-specialized-triples km-triples)))
;; 	       (cond ((null specialized-triples)
;; 		      (extract-instance-triples km-triples))
;; 		     (t (union 
;; 			 (extract-instance-triple-for-instance (extract-all-instances-from-triple-list 
;; 								specialized-triples) 
;; 							       km-triples)
;; 			 specialized-triples))))))))

|#

;;SPECIALIZED-P bug
(defun test-case2()
(SPECIALIZED-P '((|_Substance-A| |string-name| "FeCl_3")
		 (|_Substance-A| |has-basic-structural-unit| |_Ionic-Compound-A|)
		 (|_Ionic-Compound-A| |nested-atomic-chemical-formula| |_Chemical-Formula9277_c0|)
		 (|_Chemical-Formula9277_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 3 |Cl|)))
		 (|_Substance-B| |string-name| "K_3PO_4")
		 (|_Substance-B| |has-basic-structural-unit| |_Ionic-Compound9368_c0|)
		 (|_Ionic-Compound9368_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9346_c0|)
		 (|_Chemical-Formula9346_c0| |term| (:|seq| (:|pair| 3 K) (:|pair| 1 P) (:|pair| 4 O)))
		 (|_React| |raw-material| |_Substance-A|) 
		 (|_React| |raw-material| |_Substance-B|)
		 (|_Substance-A| |result-of| |_React|) 
		 (|_Substance-A| |string-name| "FePO_4")
		 (|_Substance-A| |has-basic-structural-unit| |_Ionic-Compound9577_c0|)
		 (|_Ionic-Compound9577_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9556_c0|)
		 (|_Chemical-Formula9556_c0| |term| (:|seq| (:|pair| 1 |Fe|) (:|pair| 1 P) (:|pair| 4 O)))
		 (|_Substance-B| |result-of| |_React|) 
		 (|_Substance-B| |string-name| "KCl")
		 (|_Substance-B| |has-basic-structural-unit| |_Ionic-Compound9873_c0|)
		 (|_Ionic-Compound9873_c0| |nested-atomic-chemical-formula| |_Chemical-Formula9830_c0|)
		 (|_Chemical-Formula9830_c0| |term| (:|seq| (:|pair| 1 K) (:|pair| 1 |Cl|)))
		 (|_Equation| |chemical-equation-of| |_React|)
		 (|_Equation| |instance-of| |Chemical-Equation-Expression|)
		 (|_Chemical-Formula9830_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9873_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance-B| |instance-of| |Ionic-Compound-Substance|)
		 (|_Chemical-Formula9556_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9577_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance-A| |instance-of| |Ionic-Compound-Substance|) 
		 (|_React| |instance-of| |Reaction|)
		 (|_Chemical-Formula9346_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound9368_c0| |instance-of| |Ionic-Compound|)
		 (|_Substance-B| |instance-of| |Ionic-Compound-Substance|)
		 (|_Chemical-Formula9277_c0| |instance-of| |Chemical-Formula|)
		 (|_Ionic-Compound-A| |instance-of| |Ionic-Compound|)
		 (|_Substance-A| |instance-of| |Ionic-Compound-Substance|))
	       '|_React|))

(defun get-basic-graph(triple-lst)
  (mappend 
   #'(lambda(triple)
       (let ((head     (triple-head triple))
	     (tail     (triple-tail triple))
	     (relation (triple-relation triple)))
	 (multiple-value-bind
	     (head-instance head-graph)
	     (ps-instantiate-concept-as-triple-list
	      (domains-of relation))
	      (multiple-value-bind
		  (tail-instance tail-graph)
		  (ps-instantiate-concept-as-triple-list
		   (ranges-of relation))
	      (append 
	       head-graph
	       tail-graph
	       (list 
		(list head-instance 
		      relation 
		      tail-instance)))))))
   (extract-non-instance-triples triple-lst)
))

