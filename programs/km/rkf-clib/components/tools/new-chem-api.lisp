;;
;; $Id: new-chem-api.lisp,v 1.22 2010/05/19 20:44:51 kbarker Exp $
;;


(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(setf *chem-recog-debug* nil)
(require :regexp2)

;; ===========================================================================
;; INTERFACE FUNCTIONS

;; FUNCTIONALITY:
;; - retrieves the Chemical-Entity class given its name/has-chf/atomic-chf/nested-atomic-chf/charge
;; (returns an instance ot class name)
;; - if a Chemical-Entity with the given charact is not in the KB, 
;;   creates a new instance of Ch-Ent with the specified charact and returns it 
;;
;; do we need to create Ions as well?
;;
;; REPLACES KM methods:
;; - Compute-Compound-from-Chemical-Formula
;; - Compute-Compound-from-Chemical-Name

(defun compute-chemical-entity-from-defs (&key (name nil) (has-chemical-formula nil) (atomic-chemical-formula nil) 
					       (nested-atomic-chemical-formula nil) (charge nil) (instance t))
  ;;(with-bps-standard-parameters ()
     (let ((ch-ent (compute-chemical-from-defs name has-chemical-formula nested-atomic-chemical-formula 
					       atomic-chemical-formula charge :instance instance)))
       (if ch-ent 
	   ;; already in the KB
	   ch-ent 
	 ;; need to create a new one
	 (cond (name (create-chemical-entity-from-name name)) 
	       (has-chemical-formula (create-chemical-entity-from-formula has-chemical-formula 'has-chemical-formula))
	       (atomic-chemical-formula (create-chemical-entity-from-formula atomic-chemical-formula 'atomic-chemical-formula))
	       (nested-atomic-chemical-formula (create-chemical-entity-from-formula nested-atomic-chemical-formula 'nested-atomic-chemical-formula))
	       (t (format t "~% Not enough info to create a Chemical-Entity"))))))
;;
;;============================================================================


;; ================================================================================
;; FUNCTIONALITY:
;; - creates a new Chemical-Entity from name
;; REPLACES 
;; - Compute-Compound-from-Chemical-Name
;; - Compute-Compound-Computing-Method-from-Name
;; - Compute-Ionic-Compound-from-Chemical-Name
(defun create-chemical-entity-from-name (name)
  (let ((name-comps (rest (first (split-compound-name-components name)))))
    (if (string= (second name-comps) "acid")
	;;then Compute-Acid-Compound-from-Chemical-Name
	(create-acid-compound-from-name name-comps)
      (let* ((cations (compute-cation-from-defs (first name-comps) nil nil nil nil :whole-entry t))
	     (anions (compute-anion-from-defs (second name-comps) nil nil nil nil :whole-entry t)))
	(if (and cations anions)
	    ;; then Compute-Ionic-Compound-from-Chemical-Name
	    (create-ionic-compound-from-ions :cation-entry (first cations)
					     :anion-entry (first anions))
	  (let ((molecule (create-molecule-from-name name)))
	    (if molecule 
		molecule
	      ;; default
	      (create-generic-chemical-entity-from-name name))))))))
;;
;; ===========================================================================


;; ===========================================================================
;; FUNCTIONALITY:
;; - creates a new Chemical-Entity based on its chemical-formula
;; - assumes there is no such chemical entity in KB
;; - returns an instance of Chemical-Entity with as much info as it can get from the formula
;; 
;; REPLACES:
;; - Compute-Compound-from-Chemical-Name
;; - Compute-Compound-Computing-Method-from-Name
(defun create-chemical-entity-from-formula (formula type)
  (if (is-acid-chf formula)
      (create-acid-compound-from-formula formula type)
    ;; else if there are cation-anion pairs with matching charges
    (let* ((matching-cation-anion-pair (compute-ions-from-formula formula type)))
      (if matching-cation-anion-pair 
	  ;;      than compute-ionic-compound
	  (create-ionic-compound-from-ions :cation-term  (first matching-cation-anion-pair) 
					   :cation-entry (second matching-cation-anion-pair) 
					   :anion-term (third matching-cation-anion-pair) 
					   :anion-entry (fourth matching-cation-anion-pair))
	;;      else create a generic Chemical-Entity
	(create-generic-chemical-entity-from-formula formula type)))))
;; ================================================================================


;; ===========================================================================
;; FUNCTIONALITY
;; dispatches to appropriate compute-ions-from* function based on formula type

(defun compute-ions-from-formula (formula type)
  (case type 
    ((nested-atomic-chemical-formula atomic-chemical-formula |nested-atomic-chemical-formula| |atomic-chemical-formula|)
     (compute-ions-from-nested-atomic-chf formula))
    ((has-chemical-formula |has-chemical-formula|)
     (compute-ions-from-has-chf formula))))
;;
;;==============================================================================


;; ===================================================================
;; NOT USED ANYWHERE ??
;; FUNCTIONALITY:
;; - selects a pair anion-cation so that charge1*coeff1 + charge2*coeff2 = 0
;; NOTE only works for ionic compounds with one kind of anion and one kind of cation
;; (i.e. will not work on Fe2O3 which has two kinds of Fe ions)
;; TESTCASES:
;; [1] KM(8): (compute-ions-from-atomic-chf '(:|seq| (:|pair| 1 |Fe|) (:|pair| 3 |Br|)))
;; (((:|pair| 1 |Fe|) (|Fe-Plus-3| ("iron(iii)") (:|seq| (:|pair| 1 |Fe|)) (:|seq| (:|pair| 1 |Fe|)) (:|seq| (:|pair| 1 |Fe|)) (:|pair| 3 |*unity|)))
;; ((:|pair| 3 |Br|) (|Br-Minus| ("bromide") (:|seq| (:|pair| 1 |Br|)) (:|seq| (:|pair| 1 |Br|)) (:|seq| (:|pair| 1 |Br|)) (:|pair| -1 |*unity|))))
(defun compute-ions-from-atomic-chf (atomic-chf)
  (if (is-water atomic-chf)
      (list '(:|pair| 1 |H-Plus|) (assoc '|H-Plus| *chemical-defs-table*)
	    '(:|pair| 1 |OH-Minus|) (assoc '|OH-Minus| *chemical-defs-table*))
    (if (= (length (all-terms atomic-chf)) 2)
	(let* ((terms (all-terms atomic-chf))
	       (atom-type1 (term-type (first terms)))
	       (atom-coeff1 (term-coeff (first terms)))
	       (atom-type2 (term-type (second terms)))
	       (atom-coeff2 (term-coeff (second terms)))
	       (type1-ions (get-atomic-ions atom-type1)))
	  (dolist (ion1 type1-ions)
	    (let* ((ion1-charge (chem-def-entry-charge-value ion1))
		   (type2-charge-hyp (- (/ (* ion1-charge atom-coeff1) atom-coeff2)))
		   (type2-ions (if (integerp type2-charge-hyp)
				   (get-atomic-ions atom-type2 :charge `(:|pair| ,TYPE2-CHARGE-HYP |*unity|)))))
	      (if type2-ions 
		  (return-from compute-ions-from-atomic-chf
		    (if (is-metal-atom-p atom-type1)
			(list (list atom1-coeff (chem-def-entry-acf ion1)) ion1 
			      (list atom2-coeff (chem-def-entry-acf (first type2-ions))) (first type2-ions))
		      (list (list atom2-coeff (chem-def-entry-acf (first type2-ions))) (first type2-ions)
			    (list atom1-coeff (chem-def-entry-acf ion1)) ion1))))))))))
;;
;; ==========================================================================



;; ===========================================================================
;; FUNCTIONALITY:
;; - selects a pair cation-anion so that charge1*coeff1 + charge2*coeff2 = 0
;;
;; TESTCASES 
;; - if O-Minus not in KB
;; KM(119): (compute-ions-from-nested-atomic-chf '(:|seq| (:|pair| 1 |Cu|) (:|pair| 2 |O|)))
;; NIL
;; [2] KM(120): (compute-ions-from-nested-atomic-chf '(:|seq| (:|pair| 1 |Cu|) (:|pair| 1 |O|)))
;; (((1 (:|seq| (:|pair| 1 |Cu|))) (|Cu-Plus-2| ("copper(ii)") (:|seq| (:|pair| 1 |Cu|)) (:|seq| (:|pair| 1 |Cu|)) (:|seq| (:|pair| 1 |Cu|)) (:|pair| 2 |*unity|)))
;; ((1 (:|seq| (:|pair| 1 O))) (|O-Minus-2| ("oxide") (:|seq| (:|pair| 1 O)) (:|seq| (:|pair| 1 O)) (:|seq| (:|pair| 1 O)) (:|pair| -2 |*unity|))))

(defun compute-ions-from-nested-atomic-chf (nested-atomic-chf)
  (if (is-water nested-atomic-chf)
      (list '(1 (:|seq| (:|pair| 1 |H|))) (assoc '|H-Plus| *chemical-defs-table*)
	    '(1 (:|seq| (:|pair| 1 |O|) (:|pair| 1 |H|))) (assoc '|OH-Minus| *chemical-defs-table*))
    (let* ((cation-term (extract-cation-term-from-nested nested-atomic-chf))
	   (cation-coeff (first cation-term))
	   (cation-formula (second cation-term))
	   (anion-term (extract-anion-term-from-nested nested-atomic-chf :cation-formula cation-formula))
	   (anion-coeff (if anion-term (first anion-term)))
	   (anion-formula (if anion-term (second anion-term)))
	   (cations (if anion-formula (get-cations cation-formula))))
      (if anion-term
	  (dolist (cation cations)
	    (let* ((cation-charge (chem-def-entry-charge-value cation))
		   (anion-charge-hyp (- (/ (* cation-charge cation-coeff) anion-coeff)))
		   (anions (if (integerp anion-charge-hyp)
			       (get-anions anion-formula :charge `(:|pair| ,ANION-CHARGE-HYP |*unity|)))))
	      (if anions 
		  (return-from compute-ions-from-nested-atomic-chf 
		    (list cation-term cation anion-term (first anions))))))))))
  ;;
;; ============================================================================


;; ===========================================================================
;; FUNCTIONALITY:
;; - computes ions from has-chemical-formula
;;
;; TESTCASES 
;; KM(148): (compute-ions-from-has-chf '(:|seq| (:|pair| 2 |H-Plus|) (:|pair| 1 |SO4-Minus-2|)))
;; (((:|pair| 2 |H-Plus|) (|H-Plus| ("hydrogen") (:|seq| (:|pair| 1 H)) (:|seq| (:|pair| 1 H)) (:|seq| (:|pair| 1 H)) (:|pair| 1 |*unity|)))
;; ((:|pair| 1 |SO4-Minus-2|)
;;  (|SO4-Minus-2| ("sulfate") (:|seq| (:|pair| 1 S) (:|pair| 4 O)) (:|seq| (:|pair| 1 S) (:|pair| 4 O)) (:|seq| (:|pair| 1 S) (:|pair| 4 O)) (:|pair| -2 |*unity|))))

;; TODO: do we need special case for H2O ?
(defun compute-ions-from-has-chf (has-chf)
  (if (= (length (all-terms has-chf)) 2)
      (let* ((terms (all-terms has-chf))
	     (cation-type (term-type (first terms)))
	     (cation-coeff (term-coeff (first terms)))
	     (rest-type (term-type (second terms)))
	     (rest-coeff (term-coeff (second terms)))
	     (cation-entry (assoc cation-type *chemical-defs-table*))
	     (rest-entry (assoc rest-type *chemical-defs-table*)))
	(if (and cation-entry rest-entry)
	    (let* ((cation-charge (chem-def-entry-charge-value cation-entry))
		   (rest-charge (chem-def-entry-charge-value rest-entry)))
	      (if (and (numberp cation-charge) (numberp cation-coeff)
		       (numberp rest-charge) (numberp rest-coeff)
		       (zerop (+ (* cation-charge cation-coeff) 
				 (* rest-charge rest-coeff))))
		  (list (first terms) cation-entry (second terms) rest-entry)
		  #|(list (list cation-coeff (chem-def-entry-acf cation-entry)) cation-entry
			(list rest-coeff (chem-def-entry-acf rest-entry)) rest-entry)|#))))))
;;
;; ============================================================================



;; ============================================================
;; FUNCTIONALITY: 
;; creates a new instance of Ionic-Compound given an cation-anion pair
;;
;; TESTCASES:
;;[1] KM(81): (create-ionic-compound-from-ions :cation-entry (assoc '|NH4-Plus| *chemical-defs-table*) :anion-entry (assoc '|O-Minus-2| *chemical-defs-table*))
;;(|_Ionic-Compound36960|)
;;[1] KM(82): (showme '|_Ionic-Compound36960|)
;;(_Ionic-Compound36960 has 
;;  (instance-of (Ionic-Compound)))

;;(in-situation _Situation22
;;  (_Ionic-Compound36960 has 
;;    (has-part ((a NH4-Plus)
;;               (a O-Minus-2)))
;;    (has-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 2 NH4-Plus) (:pair 1 O-Minus-2)))))))
;;    (atomic-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 2 N) (:pair 8 H) (:pair 1 O)))))))
;;    (nested-atomic-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 2 (:seq (:pair 1 N) (:pair 4 H))) (:pair 1 O)))))))))

(defun create-ionic-compound-from-ions (&key (class '|Ionic-Compound|) (cation-term nil) 
					     (cation-entry nil) (anion-term nil) (anion-entry nil))
  (if (and cation-entry anion-entry)
      (let* ((cation-coeff (if cation-term (car cation-term)
			     (abs (chem-def-entry-charge-value anion-entry))))
	     (cation-type (chem-def-entry-class-name cation-entry))
	     (cation-formula (chem-def-entry-acf cation-entry))
	     (anion-coeff (if anion-term (car anion-term)
			    (abs (chem-def-entry-charge-value cation-entry))))
	     (anion-type (chem-def-entry-class-name anion-entry))
	     (anion-formula (chem-def-entry-acf anion-entry))
	     (has-chemical-formula `(:|seq| (:|pair| ,CATION-COEFF ,CATION-TYPE) 
					    (:|pair| ,ANION-COEFF ,ANION-TYPE)))
	     (nested-atomic-chemical-formula (car (compute-nested-atomic-chf-from-ions cation-coeff cation-formula
										  anion-coeff anion-formula)))
	     (atomic-chemical-formula (car (compose-component-structures cation-coeff cation-formula 
									 anion-coeff anion-formula))))

	(km `(|a| ,CLASS |with|
		  ;; do we want to 
		  (|has-part| ((|a| ,CATION-TYPE) (|a| ,ANION-TYPE)))
		  (|has-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,HAS-CHEMICAL-FORMULA)))))
		  (|atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,ATOMIC-CHEMICAL-FORMULA)))))
		  (|nested-atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,NESTED-ATOMIC-CHEMICAL-FORMULA))))))))))
;;
;; ==========================================================================


;; ===================================================================================
;;

(defun compute-nested-atomic-chemical-formula-from-has-chemical-formula (chf)
  (if (is-water (car chf))
      `((:|seq| (:|pair| 2 |H|) (:|pair| 1 |O|)))
      ;; compute ions from has-chemical-formula
      ;; compute-nested-atomic-chf-from-ions
      (let* ((parts (compute-ions-from-has-chf (car chf)))
	     (cation-term (first parts))
	     (cation-entry (second parts))
	     (anion-term (third parts))
	     (anion-entry (fourth parts))
	     (cation-coeff (term-coeff cation-term))
	     (cation-type (term-type cation-term))
	     (cation-atomic-chf (chem-def-entry-acf cation-entry))
	     (anion-coeff (term-coeff anion-term))
	     (anion-type (term-type anion-term))
	     (anion-atomic-chf (chem-def-entry-acf anion-entry)))
	(format *chem-recog-debug* "~% cation-term: ~a, cation-coeff: ~a, cation-type: ~a~% cation-entry: ~a " cation-term cation-coeff cation-type cation-entry)
	(format *chem-recog-debug* "~% anion-term: ~a, anion-coeff: ~a, anion-type: ~a~% anion-entry: ~a " anion-term anion-coeff anion-type anion-entry)	
	(if (and (numberp cation-coeff)
		 (valid-chem-formula cation-atomic-chf)
		 (numberp anion-coeff)
		 (valid-chem-formula anion-atomic-chf))
	    (compute-nested-atomic-chf-from-ions cation-coeff cation-atomic-chf
						 anion-coeff anion-atomic-chf)))))
;; 
;; ==================================================================================  


;;; this fails when a nested atomic chef cannot be computed. e.g
;;; (COMPUTE-ATOMIC-CHEMICAL-FORMULA-FROM-HAS-CHEMICAL-FORMULA '((:|seq| (:|pair| 1 C) (:|pair| 1 O) (:|pair| 4 H))))
;; 
(defun compute-atomic-chemical-formula-from-has-chemical-formula (chf)
  (let ((nested-chf (compute-nested-atomic-chemical-formula-from-has-chemical-formula chf)))
    (if nested-chf 
	(denest-atomic-formula (car nested-chf))
      ;; when a nested atomic chem cannot be computed, return the has-chemical formula provided
      chf)))

;; ==================================================================================
;;

(defun compute-has-chemical-formula-from-nested (nested)
  (let* ((ions (compute-ions-from-nested-atomic-chf nested))
	 (cation (first ions))
	 (anion (second ions))
	 (cation-coeff (term-coeff (first cation)))
	 (cation-type (chem-def-entry-class-name (second cation)))
	 (cation-atomic-chf (chem-def-entry-acf (second cation)))
	 (anion-coeff (term-coeff (first anion)))
	 (anion-type (chem-def-entry-class-name (second anion)))
	 (anion-atomic-chf (chem-def-entry-acf (second anion))))
    (if (and cation-coeff cation-type
	     anion-coeff anion-type)
	`(:|seq| (:|pair| ,CATION-COEFF ,CATION-TYPE) 
		 (:|pair| ,ANION-COEFF ,ANION-TYPE)))))

;; =================================================================================================
;;

(defun compute-nested-atomic-chf-from-ions (cation-coeff cation-formula anion-coeff anion-formula)
  (if (and (numberp cation-coeff) (numberp anion-coeff))
      (let* ((cation-comp  (if (= 1 cation-coeff)
			       (all-terms cation-formula)
			     (if (> (length (all-terms cation-formula)) 1)
				 (list (list ':|pair| cation-coeff cation-formula))
			       (list (list ':|pair| (* cation-coeff (term-coeff (first (all-terms cation-formula))))
					   (term-type (first (all-terms cation-formula))))))))
	     (anion-comp (if (> (length (all-terms anion-formula)) 1)
			     (if (> anion-coeff 1)
				 (list (list ':|pair| anion-coeff anion-formula))
			       (all-terms anion-formula))
			   (list (list ':|pair| (* anion-coeff (term-coeff (first (all-terms anion-formula))))
				       (term-type (first (all-terms anion-formula))))))))
	(list (cons ':|seq| (append cation-comp anion-comp))))))
  ;;
;;=================================================================================================


;; ============================================================
;; FUNCTIONALITY: 
;; creates a new acid compound given its formula
;; assumes there is no acid with the same formula in KB, otherwise this function should not be called
;; replaces  Compute-Acid-Compound-from-Formula.km
;; 
;; TESTCASES
;; KM(182): (create-acid-compound-from-formula '(:|seq| (:|pair| 1 |H-Plus|) (:|pair| 1 |Cl-Minus|)) 'has-chf)
;; (|_Acid-Compound15255|)
;; KM(183): (showme '|_Acid-Compound15255|)
;; (_Acid-Compound15255 has 
;;   (instance-of (Acid-Compound)))
;;
;; (in-situation _Situation22
;;  (_Acid-Compound15255 has 
;;    (has-part ((a H-Plus)
;;               (a Cl-Minus)))
;;    (has-chemical-formula ((:seq (:pair 1 H-Plus) (:pair 1 Cl-Minus))))
;;    (atomic-chemical-formula ((:seq (:pair 1 H) (:pair 1 Cl))))))

;; KM(189): (create-acid-compound-from-formula '(:|seq| (:|pair| 1 |H-Plus|) (:|pair| 1 |A|)) 'has-chf)
;;(|_Acid-Compound15256|)
;;[2] KM(190): (showme '|_Acid-Compound15256|)
;;(_Acid-Compound15256 has 
;;  (instance-of (Acid-Compound)))
;;
;;(in-situation _Situation22
;;  (_Acid-Compound15256 has 
;;    (has-part ((a H-Plus)
;;               (a Anion)))
;;    (has-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 1 H-Plus) (:pair 1 Anion)))))))
;;    (nested-atomic-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 1 H) (:pair 1 A)))))))
;;    (atomic-chemical-formula ((a Chemical-Formula with (term ((:seq (:pair 1 H) (:pair 1 A)))))))))

(defun create-acid-compound-from-formula (chf type)
  (if (or (is-anonymous-acid (km-unique0 chf))
	  (is-ha chf))
      (km `(|a| |Acid-Compound| |with| 
		(|has-part| ((|a| |H-Plus|) (|a| |Anion|)))
		(|has-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ((:|seq| (:|pair| 1 |H-Plus|) (:|pair| 1 |Anion|)))))))
		(|nested-atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ((:|seq| (:|pair| 1 |H|) (:|pair| 1 |A|)))))))
		(|atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ((:|seq| (:|pair| 1 |H|) (:|pair| 1 |A|)))))))))
    (let* ((parts (compute-ions-from-formula chf type)) ;; here
	   (cation-term (first parts))
	   (cation-entry (second parts))
	   (anion-term (third parts))
	   (anion-entry (fourth parts))
	   (cation-coeff (first cation-term))
	   (cation-type (chem-def-entry-class-name cation-entry))
	   (cation-atomic-chf (chem-def-entry-acf cation-entry))
	   (anion-coeff (first anion-term))
	   (anion-type (chem-def-entry-class-name anion-entry))
	   (anion-atomic-chf (chem-def-entry-acf anion-entry)))
      (if (and cation-coeff cation-type 
	       anion-coeff anion-type) 
	  (km `(|a| |Acid-Compound| |with|
		    ;; do we want to 
		    (|has-part| ((|a| ,CATION-TYPE) (|a| ,ANION-TYPE)))
		    (|has-chemical-formula| ((|a| |Chemical-Formula| |with|
					      (|term| ((:|seq| (:|pair| ,CATION-COEFF ,CATION-TYPE) 
							   (:|pair| ,ANION-COEFF ,ANION-TYPE)))))))
		    (|atomic-chemical-formula| ((|a| |Chemical-Formula| |with|
						 (|term| (,(COMPOSE-COMPONENT-STRUCTURES CATION-COEFF CATION-ATOMIC-CHF
										       ANION-COEFF ANION-ATOMIC-CHF))))))))))))
;;
;; ========================================================================================================


;; ===========================================================================
;; FUNCTIONALITY:
;; - reates an Acid-Compound from its name
;; REPLACES:
;; - Compute-Acid-Compound-from-Chemical-Name
(defun create-acid-compound-from-name (name-comps)
  (let ((anion-entry (compute-anion-from-defs nil nil nil nil nil :whole-entry t :acid-name (first name-comps)))
	(h-plus-entry (assoc '|H-Plus| *chemical-defs-table*)))
    (if anion-entry
	(create-ionic-compound-from-ions :class '|Acid-Compound| 
					 :cation-entry h-plus-entry 
					 :anion-entry (first anion-entry)))))
;;
;;==========================================================================================================



;; =========================================================================
;;
;; FUNCTIONALITY
;; - creates a new molecule from chemical name
;;
;; REPLACES:
;; - Compute-Molecule-from-Chemical-Name

(defun create-molecule-from-name (name)
  (let* ((components (rest (first (split-molecular-compound-name-components name))))
	 (has-chf ())
	 (parts (all-term-types (car has-chf)))
	 (nested-acf (compute-nested-atomic-formula-from-formula (car has-chf)))
	 (atomic-chf (denest-atomic-formula (car nested-acf))))

    (km `(|a| |Molecule| |with|
	      (|has-part| ((|a| ,(FIRST PARTS)) (|a| ,(SECOND PARTS))))
	      (|has-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ,HAS-CHF))))
	      (|nested-atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ,NESTED-ACF))))
	      (|atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| ,ATOMIC-CHF))))))))


;; ================================================================
;; FUNCTIONALITY
;; creates a generic chemical-entity with a specified name
	 
(defun create-generic-chemical-entity-from-name (name)
  (km `(|a| |Chemical-Entity| |with| (|has-chemical-name| (,NAME)))))
;;
;;================================================================


;; ================================================================
;; FUNCTIONALITY
;; creates a generic chemical-entity with a specified name
	 
(defun create-generic-chemical-entity-from-formula (formula type)
  (case type
    (has-chemical-formula
     (km `(|a| |Chemical-Entity| |with| (|has-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,FORMULA))))))))
    (atomic-chemical-formula
     (km `(|a| |Chemical-Entity| |with| (|atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,FORMULA))))))))
    (nested-atomic-chemical-formula
     (km `(|a| |Chemical-Entity| |with| 
	       (|atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,(DENEST-ATOMIC-FORMULA FORMULA))))))
	       (|nested-atomic-chemical-formula| ((|a| |Chemical-Formula| |with| (|term| (,FORMULA))))))))))
    
;;
;;================================================================



;; ===================================================================
;; 
;; FUNCTIONALITY:
;; - computes nested-atomic-chf from formula 
;; REPLACES 
;; - Compute-Nested-Atomic-Formula-from-Formula
(defun compute-nested-atomic-formula-from-formula (formula)
  (let* ((terms (all-terms formula))
	 (c1 (term-coeff (first terms)))
	 (f1 (chem-def-entry-nacf (assoc (term-type (first terms)) *chemical-defs-table*)))
	 (c2 (term-coeff (second terms)))
	 (f2 (chem-def-entry-nacf (assoc (term-type (second terms)) *chemical-defs-table*))))
    (if (and c1 f1 c2 f2)
	(compose-component-structures c1 f1 c2 f2))))


;; =================================================================================
;; FUNCTIONALITY
;; - computes the has-chemical-name given has-chemical-formula
;; REPLACES:
;; - Compute-Chemical-Name-from-Formula-of-Ionic-Compound

(defun compute-chemical-name-from-formula-of-ionic-compound (formula)
  (if formula
      (let* ((terms (all-terms (car formula)))
	     (cation (first terms))
	     (cation-entry (assoc (term-type cation) *chemical-defs-table*))
	     (anion (second terms))
	     (anion-entry (assoc (term-type anion) *chemical-defs-table*))
	     (cation-name (first (chem-def-entry-name cation-entry)))
	     (anion-name (first (chem-def-entry-name anion-entry))))
	(if (and cation-name anion-name)
	    (concatenate 'string cation-name " " anion-name)))))
;;
;; =========================================================================


;; ==============================================================================
;; FUNCTIONALITY:
;; - computes ionic parts from nested-atomic-chemical-formula
;; replaces same name function in get-ch-ent
;; TODO:
;; - special cases water and anonymous acid

(defun compute-ic-parts-from-nested-formula (nested-chf)
  (let ((ions (compute-ions-from-nested-atomic-chf nested-chf)))
    (if ions
	(list (KM-UNIQUE0 `(|an| |instance| |of| ,(CHEM-DEF-ENTRY-CLASS-NAME (SECOND IONS))))
	      (KM-UNIQUE0 `(|an| |instance| |of| ,(CHEM-DEF-ENTRY-CLASS-NAME (FOURTH IONS))))))))
;;
;; ================================================================================



;; =============================================================================
;; 
;; creates a list of names of compounds that can be recognized by the system
;; (i.e. their cations and anions are in CLib)
;; - acids ("acid" + "all those with acid names")
;; - ionic-comps ("cation name" * "anion name")
  
(defun create-list-of-recognizable-ionic-compounds ()
  (let* ((all-cations (all-cation-names))
	 (all-anions (all-anion-names))
	 (all-anion-acid-names (all-anion-acid-names))
	 (all-acid-names (mappend #'make-acid-name all-anion-acid-names))
	 (all-ionic-compound-names (mappend #'(lambda (cation-name)
						(mappend #'(lambda (anion-name)
							     (make-ionic-compound-name cation-name anion-name))
							 all-anions))
					    all-cations))
	 (all-compounds (mapcar #'remove-junk-from-names 
				(append all-acid-names all-ionic-compound-names))))
    (remove-if #'null 
	       (mapcar #'(lambda (name)
			   (let* ((comp (compute-chemical-entity-from-defs :name name :instance nil))
				  (chf (if (km-instancep (car comp))
					   (sri-km-query `(|the| |has-chemical-formula| |of| ,(car comp)))))
				  (chf-term (if chf (sri-km-query `(|the| |term| |of| ,chf))))
				  (string-name (if chf (sri-km-query `(|the| |string-name| |of| ,chf)))))
			     (if (and comp (valid-chem-formula (car chf-term)))
				 (progn 
				  (format t "~% Name: ~a Comp: ~a ChF: ~a String-name ~a" name comp chf-term string-name) 
				  (list name string-name)))))
		       all-compounds))))

	 

;; removes junk from names
(defun remove-junk-from-names (name)
  (join " " (remove-if #'(lambda (x) (member x '("ion") :test #'string=))
		       (excl::split-re " " name))))

(defun all-cation-names ()
  (remove-if #'null 
	     (mapcar #'(lambda (entry) (if (and (numberp (chem-def-entry-charge-value entry))
						(> (chem-def-entry-charge-value entry) 0))
					   (chem-def-entry-name entry)))
		     *chemical-defs-table*)))

(defun all-anion-names ()
  (remove-if #'null 
	     (mapcar #'(lambda (entry) (if (and (numberp (chem-def-entry-charge-value entry))
						(< (chem-def-entry-charge-value entry) 0))
					   (chem-def-entry-name entry)))
		     *chemical-defs-table*)))

(defun all-anion-acid-names ()
  (remove-if #'null 
	     (mapcar #'(lambda (entry) (if (and (numberp (chem-def-entry-charge-value entry))
						(< (chem-def-entry-charge-value entry) 0))
					   (chem-def-entry-acid-name entry)))
		     *chemical-defs-table*)))

(defun make-ionic-compound-name (cation-names anion-names)
  (mappend #'(lambda (cation-name)
	       (mapcar #'(lambda (anion-name)
			   (concatenate 'string cation-name " " anion-name))
		       anion-names))
	   cation-names))

(defun make-acid-name (anion-acid-names)
  (mapcar #'(lambda (anion-name)
	      (concatenate 'string anion-name " " "acid"))
	  anion-acid-names))

