;;
;;    $Id: naming.lisp,v 1.29 2010/05/19 20:44:51 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;; Handling count prefixes

(defconstant *number-prefixes*
    #("mono" "di" "tri" "tetra" "penta" "hexa" "septa" "octa" "nano" "deca"))

(defun translate-to-count-prefix (number)
  (aref *number-prefixes* (1- number)))

(defun translate-prefix-to-count (prefix)
  (let ((prefix-length (length prefix)))
    (1+ (position prefix *number-prefixes*
		  :test #'(lambda (str1 str2)
			    (search str1 str2 :end2 prefix-length))))))

;;; Common molecular names

;;obsolete
(defvar *common-compounds* nil)

;; obsolete
(defstruct common-compound
  name
  formula
  expression)

;; obsolete
(defun define-common-compound (name formula km-expr)
  (let ((old-compound
	 (member name *common-compounds* :test #'equal :key #'common-compound-name))
	(new-compound (make-common-compound :name name :formula formula :expression km-expr)))
    (if old-compound
	(setf (car old-compound) new-compound)
      (push new-compound *common-compounds*))))

;; Some common compounds

;; obsolete
(define-common-compound "water" '(:|seq| (:|pair| 2 |H|) (:|pair| 1 |O|)) '(|a| |H2O|))
(define-common-compound "hydrogen peroxide" '(:|seq| (:|pair| 2 |H|) (:|pair| 2 |O|)) '(|a| |H2O2|))
(define-common-compound "ammonia" '(:|seq| (:|pair| 1 |N|) (:|pair| 3 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |NH3-Substance|)))

(define-common-compound "hydrogen" '(:|seq| (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |H2-Substance|)))
(define-common-compound "hydrogen gas" '(:|seq| (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |H2-Substance|)))

(define-common-compound "oxygen" '(:|seq| (:|pair| 2 |O|))
			'(|the| |has-basic-structural-unit| |of| (|a| |O2-Substance|)))
(define-common-compound "oxygen gas" '(:|seq| (:|pair| 2 |O|))
			'(|the| |has-basic-structural-unit| |of| (|a| |O2-Substance|)))

(define-common-compound "nitrogen" '(:|seq| (:|pair| 2 |N|))
			'(|the| |has-basic-structural-unit| |of| (|a| |N2-Substance|)))
(define-common-compound "nitrogen gas" '(:|seq| (:|pair| 2 |N|))
			'(|the| |has-basic-structural-unit| |of| (|a| |N2-Substance|)))

(define-common-compound "methanol" 
                        '(:|seq| (:|pair| 1 |C|) (:|pair| 3 |H|) (:|pair| 1 |O|) (:|pair| 1 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |CH3OH-Substance|)))

(define-common-compound "methane" 
                        '(:|seq| (:|pair| 1 |C|) (:|pair| 4 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |CH4-Substance|)))

(define-common-compound "aniline" 
                        '(:|seq| (:|pair| 6 |C|) (:|pair| 5 |H|) (:|pair| 1 |N|) (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |C6H5NH2-Substance|)))

(define-common-compound "dimethylamine" 
                        '(:|seq| (:|pair| 2 ((:|pair| 1 |C|) (:|pair| 3 |H|))) 
				 (:|pair| 1 |N|) (:|pair| 1 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |CH3_2_NH-Substance|)))

(define-common-compound "ethylamine" 
                        '(:|seq| (:|pair| 2 |C|) (:|pair| 5 |H|) (:|pair| 1 |N|) (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |C2H5NH2-Substance|)))

(define-common-compound "hydrazine" 
                        '(:|seq| (:|pair| 2 |H|) (:|pair| 1 |N|) (:|pair| 1 |N|) (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |H2NNH2-Substance|)))

(define-common-compound "hydroxylamine" 
                        '(:|seq| (:|pair| 1 |H|) (:|pair| 1 |O|) (:|pair| 1 |N|) (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |HONH2-Substance|)))

(define-common-compound "methylamine" 
                        '(:|seq| (:|pair| 1 |C|) (:|pair| 3 |H|) (:|pair| 1 |N|) (:|pair| 2 |H|))
			'(|the| |has-basic-structural-unit| |of| (|a| |CH3NH2-Substance|)))

(define-common-compound "pyridine" 
                        '(:|seq| (:|pair| 5 |C|) (:|pair| 5 |H|) (:|pair| 1 |N|))
			'(|the| |has-basic-structural-unit| |of| (|a| |C5H5N-Substance|)))

(define-common-compound "trimethylamine" 
                        '(:|seq| (:|pair| 1 |C|) (:|pair| 3 |H|) (:|pair| 1 |C|) (:|pair| 1 |N|))
			'(|the| |has-basic-structural-unit| |of| (|a| |CH3_3_N-Substance|)))

(defun translate-from-common-compound-name (name)
  (compute-chemical-from-defs name NIL NIL nil nil))

;; new version, takes advantage of chemical-hash table
(defun translate-from-common-compound-formula (formula)
  (compute-chemical-from-defs NIL NIL formula nil nil))


;;; Adjoining molecule name components

(defun starts-with-vowel-p (str)
  (and (stringp str)
       (case (char str 0)
	 ((#\a #\e #\i #\o #\u) t)
	 (t nil))))

(defun ends-with-a-or-o-p (str)
  (and (stringp str)
       (case (char str (1- (length str)))
	 ((#\a #\o) t)
	 (t nil))))

(defun strip-final-a-or-o (str)
  (if (ends-with-a-or-o-p str)
      (subseq str 0 (1- (length str)))
    str))

(defun compute-two-component-molecule-name (count1 name1 count2 name2)
  (let ((count1-str (unless (= count1 1)
		      (translate-to-count-prefix count1)))
	(count2-str (translate-to-count-prefix count2))
	(name1 (string-downcase name1)))
    (concatenate 'string
      (if (starts-with-vowel-p name1)
	  (strip-final-a-or-o count1-str)
	count1-str)
      name1
      " "
      (if (starts-with-vowel-p name2)
	  (strip-final-a-or-o count2-str)
	count2-str)
      name2)))

;;; Splitting string name into components

;;; This function caches, since the same input and output shall
;;; be applicable for many calls in a row

(let ((last-input nil)
      (last-output nil))
  (defun split-compound-name-components (string)
    (if (eql string last-input)
	last-output
      (setq last-input string
	    last-output (let ((split-position (position #\Space string)))
			  ;; Assumption: all common names shall be single word names
			  (when split-position
			    `((:|seq| ,(subseq string 0 split-position)
				      ,(subseq string (1+ split-position))))))))))


(defun find-molecular-component-element (component first-p)
  (loop with component-length = (length component)
      for element in *chemical-defs-table*
      for name = (if first-p (car (chem-def-entry-name element))
		   (car (chem-def-entry-molecular-name element)))
      for name-length = (if (stringp name) (length name) 100000)
      when (and name
		(>= component-length name-length)
		(string-equal component name :start1 (- component-length name-length)))
      return (values (chem-def-entry-class-name element) (- component-length name-length))))


;; NOTE: uses KM reasoning, creates instances
;;; Splitting molecular names into components
#|(defun find-molecular-component-element (component first-p)
  (loop with component-length = (length component)
      for element in (all-subclasses '|Atom|)
      for name = (km-unique0 `(|the| ,(if first-p '|has-chemical-name| '|has-molecular-name|)
				    |of| (|a| ,element)))
      for name-length = (if (stringp name) (length name) 100000)
      when (and name
		(>= component-length name-length)
		(string-equal component name :start1 (- component-length name-length)))
      return (values element (- component-length name-length))))|#


(defun split-molecule-component-count-and-name (component first-p)
  ;; FIRST-P indicates if this is the first component. The first
  ;; component has the mono- prefix implicit, while the second
  ;; does not.
  (multiple-value-bind (element name-start)
      (find-molecular-component-element component first-p)
    (if (and element name-start)
	`(:|pair| ,(if (and first-p (zerop name-start))
		       1
		     (if (numberp (subseq component 0 name-start))
			 (translate-prefix-to-count (subseq component 0 name-start))
		       1))
		  ,element))))

(defun split-molecular-compound-name-components (string)
  (let ((split-position (position #\Space string)))
    ;; Assumption: all common names shall be single word names
    (when split-position
      (let ((sub1 (subseq string 0 split-position))
	    (sub2 (subseq string (1+ split-position))))
	(let ((sub1-spec (split-molecule-component-count-and-name sub1 t))
	      (sub2-spec (split-molecule-component-count-and-name sub2 nil)))
	  `((:|seq| ,sub1-spec ,sub2-spec)))))))

;;; Composing structural formulas

(defun reduce-to-subcomponents (count struct)
  (cond ((= count 1) (cdr struct))
	((null (cddr struct))
	 (destructuring-bind (subcount substruct) (cdr (cadr struct))
	   `((:|pair| ,(* count subcount) ,substruct))))
	(t `((:|pair| ,count ,struct)))))

(defun compose-component-structures (count1 struct1 count2 struct2)
  ;; Both struct1 and struct2 have a pattern (:seq (:pair ...) ...)
  (let ((sub1 (reduce-to-subcomponents count1 struct1))
	(sub2 (reduce-to-subcomponents count2 struct2)))
    (denest-atomic-formula `(:|seq| ,@sub1 ,@sub2))))

;;; Computations on structural formula

(defun simplified-first-element-in-formula (formula)
  (let ((first-structure (second formula)))
    (if (consp (third first-structure))
	(third first-structure)
      `(:|seq| (:|pair| 1 ,(third first-structure))))))

(defun %denest-atomic-formula (formula)
  ;; Counting is done in alists
  ;; This is probably the most inefficient implementation
  ;; But it is also the easiest to do
  (cond ((atom formula) (list (cons formula 1)))
	((eql (car formula) :|seq|)
	 (reduce #'combine-atom-count-lists
		 (mapcar #'count-atoms-in-subformula (cdr formula))
		 :initial-value nil))
	(t (error "Unexpected input ~S." formula))))

;; this is only for UT's internal use.
;; DO NOT call in *Global (i.e. through sri-km-query)!!
(defun denest-atomic-formula (formula)
  (let ((atom-counts (%denest-atomic-formula formula)))
    `((:|seq|
       ,@(loop for (atom . count) in atom-counts
	   collect `(:|pair| ,count ,atom))))))

;; this is the same as denest-atomic-formula, but with proper wrapping for SRI.
;; can be called in *Global
(defun sri-denest-atomic-formula (formula) 
  (with-standard-bps-parameters ()
     (let ((atom-counts (%denest-atomic-formula formula)))
       `((:|seq|
	  ,@(loop for (atom . count) in atom-counts
	      collect `(:|pair| ,count ,atom)))))))

(defun count-atoms-in-subformula (subformula)
  (cond ((and (consp subformula) (eql (car subformula) ':|pair|))
	 (destructuring-bind (subcount formula) (cdr subformula)
	   (multiply-atoms-in-subformula subcount (%denest-atomic-formula formula))))
	((and (consp subformula) (eql (car subformula) ':|seq|)) ;; cases like (:seq 2 (;seq (:pair 1 O) (:pair 1 H)))
	 (mapcar #'(lambda (entry)
		     (cons (car entry)
			   (* (second subformula) (cdr entry))))
		 (%denest-atomic-formula (third subformula))))
	(t (error "Unexpected input ~S." subformula))))

(defun multiply-atoms-in-subformula (count formula)
  (if (= count 1)
      formula
    (mapcar #'(lambda (atom-count)
		(cons (car atom-count) (* count (cdr atom-count))))
	    formula)))

(defun combine-atom-count-lists (list1 list2)
  (if (or (null list1) (null list2))
      (or list1 list2)
    (let ((list2 (copy-list list2)))
      (loop for atom-count in list1
	  for (atom . count) = atom-count
	  for atom-count2 = (assoc atom list2)
	  when atom-count2
	  do (setf (cdr atom-count) (+ count (cdr atom-count2)))
	     (setq list2 (delete atom-count2 list2)))
      (nconc list1 list2))))

;;; Parsing components of nested atomic formulas

(defvar *formula-to-name* nil)
(defvar *formula-to-name-cation* nil)
(defvar *formula-to-name-anion* nil)

(defun decompose-formula-with-nested-first-term (formula)
  ;; We are looking for (:seq (:pair <count> (:seq ...)) <rest>)
  (when (consp (third (second formula)))
    (values (caddr (cadr formula))
	    (cons :|seq| (cddr formula)))))

(defun nested-formula-equal-p (formula1 formula2)
  (equal formula1 formula2))

;; obsolete ??
(defun find-anion-with-formula (formula)
  (multiple-value-bind (sub-formula empty-formula)
      (decompose-formula-with-nested-first-term formula)
    (cond ((equal empty-formula '(:|seq|))
	   (km-unique0
	    `(|the| |instance-of| |of|
	      ,(first
		(compute-anion-from-defs
		 nil nil sub-formula nil nil))))
	   )
	  ((and (null sub-formula) (null (cddr formula)))
	   (loop for anion-class in (km0 '(|the| |all-subclasses| |of| |Anion|))
	       for anion-instance = (km-unique0 `(|a| ,anion-class))
	       for anion-formula = (km-unique0 
				    `(|the| |term| |of| (|the| |nested-atomic-chemical-formula| |of|
							 ,ANION-INSTANCE)))
	       when (and (null (cddr anion-formula))
			 (formula-fragments-match-p (cadr formula) (cadr anion-formula)))
	       return anion-class))
	  ((null sub-formula)
	   (km-unique0
	    `(|the| |instance-of| |of|
	      ,(first
		(compute-anion-from-defs
		 nil nil formula nil nil)))
	    ))
	  (t nil))))

(defun nested-formula-p (formula)
  (and (consp formula)
       (eql (car formula) ':|seq|)
       (consp (cadr formula))
       (eql (car (cadr formula)) ':|pair|)
       (consp (caddr (cadr formula)))))

(defun return-unmatched-formula-fragment (formula fragment)
  "Matches an initial subsequence of formula to the given fragment, and
returns the unmatched subsequence of the formula as a value. If the
initial subsequence does not match, then NIL is returned."
  (let ((fragment (cdr fragment))
	(formula (cdr formula)))
    (loop for frag-component in fragment
	for rest-formula on formula
	for formula-component = (car rest-formula)
	always (equal frag-component formula-component)
	finally (return (cons :|seq| (cdr rest-formula))))))

(defun formula-fragments-match-p (formula-frag component-frag)
  ;; formula-frag is from the compound formula
  ;; component-frag is from the ion
  (and (equal (third formula-frag) (third component-frag))
       (zerop (rem (second formula-frag) (second component-frag)))))

;; obsolete ??
(defun %remaining-formula-is-anion-p (formula cation)
  (let* ((cation-formula
	  (km-unique0 `(|the| |term| |of| (|the| |nested-atomic-chemical-formula| |of| ,CATION))))
	 (simple-cation-p (null (cddr cation-formula))))
    (when cation-formula
      (or
       ;; If the first term is nested, then the first term should be a cation fragment
       ;;   and the rest an anion fragment.
       (multiple-value-bind (formula1 formula-rest)
	   (decompose-formula-with-nested-first-term formula)
	 (and formula1
	      (nested-formula-equal-p formula1 cation-formula)
	      (find-anion-with-formula formula-rest)))
       ;; For a nested cation, try to match the cation formula to an initial subsequence
       ;;   of the formula, and match the remainder to an anion.
       (when simple-cation-p
	 (let ((formula-rest (return-unmatched-formula-fragment formula cation-formula)))
	   (and formula-rest
		(find-anion-with-formula formula-rest))))
       ;; Otherwise, match the cation formula to the first element of the given formula,
       ;;   and the remainder to an anion.
       (when (> (length formula) (length cation-formula))
	 (loop for rest-formula on (cdr formula)
	     for formula-fragment = (car rest-formula)
	     for cation-fragment in (cdr cation-formula)
	     always (formula-fragments-match-p formula-fragment cation-fragment)
	     finally (return (find-anion-with-formula `(:|seq| ,@rest-formula)))))))))

;; obsolete ??
(defun remaining-formula-is-anion-p (formula cation)
  (if (eql formula *formula-to-name*)
      (when *formula-to-name-anion*
	t)
    (let ((anion (%remaining-formula-is-anion-p formula cation)))
      (when anion
	(setq *formula-to-name-anion* anion)
	(setq *formula-to-name-cation* nil)
	(setq *formula-to-name* formula)
	t))))

;; obsolete ??
(defun %get-formula-anion-component (formula)
  (dolist (cation (km0 '(|the| |all-subclasses| |of| |Cation|)))
    (let ((anion (%remaining-formula-is-anion-p formula (km0 `(|a| ,cation)))))
      (when anion
	(return-from %get-formula-anion-component anion)))))

(defun get-formula-anion-component0 (formula &optional (cation nil cation-set-p))
  (if (eql formula *formula-to-name*)
      *formula-to-name-anion*
    (prog1 (setq *formula-to-name-anion*
	     (if cation-set-p
		 (%remaining-formula-is-anion-p formula cation)
	       (%get-formula-anion-component formula)))
      (setq *formula-to-name* formula)
      (if cation-set-p
	  (setq *formula-to-name-cation* cation)))))

(defun get-formula-anion-component (formula &optional (cation nil cation-set-p))
  (multiple-value-bind
    (anion-cardinality anion-formula)
    (extract-anion-formula-from-nested formula)
    (let ((computed-anions (compute-anion-from-defs nil anion-formula anion-formula anion-formula nil)))
      ;;(format t "~% Computed anions: ~a" computed-anions)
      (if (first computed-anions)
	  (km0 `(|the| |instance-of| |of| ,(first computed-anions)))
	'|Anion|)
      )))

(defun decompose-formula-using-anion (formula anion-instance)
  (let ((anion-formula 
	 (km-unique0 `(|the| |term| |of| (|the| |nested-atomic-chemical-formula| |of| ,ANION-INSTANCE))))
	(last-fragment (car (last formula))))
    ;; Final component may represent anion
    (cond ((and (null (cddr anion-formula))
		(formula-fragments-match-p last-fragment (cadr anion-formula)))
	   (values `(:|seq| ,last-fragment)
		   (/ (cadr last-fragment) (cadr (cadr anion-formula)))
		   (butlast formula)))
	  ((equal (caddr last-fragment) anion-formula)
	   (values `(:|seq| ,last-fragment) (cadr last-fragment) (butlast formula)))
	  ;; Final few components may represent anion
	  (t (let* ((anion-length (length anion-formula))
		    (formula-rest-length (1+ (- (length formula) anion-length)))
		    (formula-anion (subseq formula formula-rest-length)))
	       (when (equal (cdr anion-formula) formula-anion)
		 (values `(:|seq| ,@formula-anion) 1 (subseq formula 0 formula-rest-length))))))))


(defun get-cation-count (cation-component cation-formula)
  (cond ((and (null (cddr cation-formula))
	      (null (cddr cation-component))
	      (formula-fragments-match-p (cadr cation-component) (cadr cation-formula)))
	 (/ (cadr (cadr cation-component)) (cadr (cadr cation-formula))))
	((equal (caddr (cadr cation-component)) cation-formula)
	 (cadr (cadr cation-component)))
	((equal cation-component cation-formula) 1)
	(t nil)))
	 
(defun cation-component-matches-p (cation-component cation-instance anion-charge)
  ;; get the formula for the cation
  (let* ((cation-formula
	  (km-unique0 `(|the| |term| |of| (|the| |nested-atomic-chemical-formula| |of| ,CATION-INSTANCE))))
	 ;; match it against the formula in cation-component
	 (cation-count (get-cation-count cation-component cation-formula)))
    (when (and cation-count
	       ;; check if total cation charge is negative anion charge
	       (= (* cation-count (get-ion-charge cation-instance)) (- anion-charge)))
      t)))

(defun %get-formula-cation-component (formula anion)
  (let ((anion-instance (km-unique0 `(|a| ,anion))))
    (multiple-value-bind (anion-component anion-count cation-component)
	(decompose-formula-using-anion formula anion-instance)
      (assert anion-component () "Could not decompose formula ~S using anion ~S." formula anion)
      (let ((anion-charge (* anion-count (get-ion-charge anion-instance))))
	(km-unique0 `(|oneof| (|the| |all-subclasses| |of| |Cation|)
			     |where| (#'(LAMBDA ()
					(CATION-COMPONENT-MATCHES-P
					 ',CATION-COMPONENT (KM-UNIQUE0 '(|an| |instance| |of| |It|))
					 ',ANION-CHARGE)))))))))

;; input: a nested formula
;; output: a cation class
(defun get-formula-cation-component (formula)
   (let* ((ret-cation (get-formula-cation-component-instance formula)))
    (km0 `(|the| |instance-of| |of| ,ret-cation))
    ;;'(|Cation|)
    ))

;;mrglass: added this function to return an instance since that's what the callers want anyway
(defun get-formula-cation-component-instance (formula)
  (let* ((c-form (extract-cation-formula-from-nested formula))
	 (cations (compute-cation-from-defs nil c-form c-form c-form nil))
	 (ret-cation nil))
    ;;(format t "~% == c-form: ~a ~% == cations: ~a" c-form cations)
    (if (> (length cations) 1)
       (do* ((cation (pop cations) (pop cations))
             (cation-charge (get-ion-charge cation) (get-ion-charge cation)))
	    ((or (not cation)
             ret-cation))
       ;;(format t "~% == cation: ~a ~% == ret-cation: ~a" cation ret-cation)
       (multiple-value-bind (a-card a-form)
          (extract-anion-formula-from-nested formula cation)
          (let* ((anion (get-anion-for-formula a-form))
                (anion-charge (* (get-ion-charge anion) a-card)))
          ;;(format t "~% == Anion: ~a ~% a-form ~a ~% == anion charge: ~a" anion a-form anion-charge)
          (if (and anion 
                a-form
                (zerop (+ cation-charge anion-charge)))
          (setf ret-cation cation)))))
       (setf ret-cation (first cations)))
    ret-cation
    ;;'(|Cation|)
    ))

(defun get-anion-for-formula(a-form)
  (let ((anion (compute-anion-from-defs nil a-form a-form a-form nil)))
    (if (> (length anion) 1)
	(error "get-anion-for-formula returning more than 1 candidate... BUG!")
        (first anion))))

(defun get-ion-charge (ion)
  (let ((result (nth 1 
		     (car 
		      (km0 `(|the| |value| |of| 
			     (|the| |charge| |of| ,ion)))))))
    (if (numberp result) result 0)))
	

;; ==================================================================================
;; FUNCTIONALITY
;; extracts the anion term (coeff & formula) from nested-atomic-chf
;;
;; TESTCASES:
;; KM(7): (extract-anion-term-from-nested '(:|seq| (:|pair| 2 (:|seq| (:|pair| 1 N) (:|pair| 4 H))) (:|pair| 1 O)))
;; (1 (:|seq| (:|pair| 1 O))
;; KM(8): (EXTRACT-ANION-TERM-FROM-NESTED '(:|seq| (:|pair| 2 |Fe|) (:|pair| 3 O)) :CATION-FORMULA '(:|seq| (:|pair| 1 |Fe|)))
;; (3 (:|seq| (:|pair| 1 O)))
;; FIXME: there is s bug here as the function assumes there is only one type of cation
;; this is not true for Fe3O4 where there are 2 Fe-Plus-3 and 1 Fe-Plus-2

(defun extract-anion-term-from-nested (formula &key (cation-formula nil)) 
  (let ((match nil))
    ;; nested formula of four types:
    (cond
     ;; NH5_3_PO4_2 -- hypothetical
     ((setf match (minimatch formula '(:|seq| (:|pair| ?x (:|seq| &rest)) (:|pair| ?w (:seq &rest)))))
      (list (third match) (cons ':|seq| (fourth match))))
     ;; NH4_2_O
     ((setf match (minimatch formula '(:|seq| (:|pair| ?x (:|seq| &rest)) &rest)))
      (list 1 (cons ':|seq| (third match))))
     ;; Ba_OH_2
     ((setf match (minimatch formula '(:|seq| (:|pair| ?x ?y) (:|pair| ?z (:|seq| &rest)))))
      (list (third match) (cons ':|seq| (fourth match))))
     ;; Fe2O3, CuO2, NaN3
     ((setf match (minimatch formula '(:|seq| (:|pair| ?x ?y) (:|pair| ?w ?z))))
      ;; cation is either given or will be computed
      (dolist (cation (get-cations (if cation-formula 
				       cation-formula
				     (extract-cation-term-from-nested formula))))
	(let* ((cation-class (chem-def-entry-class-name cation))
	       (cation-charge (chem-def-entry-charge-value cation))
	       (anion-charge) (anion-formula) (anions))
	  (if (and (numberp cation-charge) 
		   (numberp (first match)) 
		   (numberp (third match))
		   (integerp (- (/ (* cation-charge (first match)) (third match)))))
	      (progn
		(setf anion-charge (- (/ (* cation-charge (first match)) (third match))))
		(setf anion-formula `(:|seq| (:|pair| 1 ,(fourth match))))
		(setf anions (if (integerp anion-charge) (get-anions anion-formula :charge `(:|pair| ,ANION-CHARGE |*unity|))))
		(if anions 
		    (return-from extract-anion-term-from-nested
		      (list (third match) anion-formula))))
	    (progn
	      (setf anion-charge (- (* cation-charge (first match))))
	      (setf anion-formula `(:|seq| (:|pair| ,(third match) ,(fourth match))))
	      (setf anions (if (integerp anion-charge) (get-anions anion-formula :charge `(:|pair| ,ANION-CHARGE |*unity|))))
	      (if anions 
		  (return-from extract-anion-term-from-nested
		    (list 1 anion-formula))))))))
     ;; NaNO3
     ((setf match (minimatch formula '(:|seq| (:|pair| ?x ?y) &rest)))
      (list 1 (cons ':|seq| (third match)))))))
;; =============================================================================


;; ==========================================================================
;; FUNCTIONALITY
;; - extracts the cation term (coeff & formula) from a nested-atomic-chf
;;
;; TESTCASES:
;; km> (extract-cation-term-from-nested '(:|seq| (:|pair| 2 (:|seq| (:|pair| 1 N) (:|pair| 4 H))) (:|pair| 1 O)))
;; (2 (:|seq| (:|pair| 1 N) (:|pair| 4 H)))
;;

(defun extract-cation-term-from-nested (formula)    
  (let ((match nil))
  ;; nested formula of four types:
  (cond
   ;; NH5_3_PO4_2 -- hypothetical
   ((setf match (minimatch formula '(:|seq| (:|pair| ?x (:|seq| &rest)) (:|pair| ?w (:seq &rest)))))
    (list (first match) (cons ':|seq| (second match))))
   ;; NH4_2_O
   ((setf match (minimatch formula '(:|seq| (:|pair| ?x (:|seq| &rest)) &rest)))
    (list (first match) (cons ':|seq| (second match))))
   ;; Ba_OH_2
   ((setf match (minimatch formula '(:|seq| (:|pair| ?x ?y) (:|pair| ?z (:|seq| &rest)))))
    (list (first match) (list ':|seq| `(:|pair| 1 ,(second match)))))
   ;; Na2O
   ((setf match (minimatch formula '(:|seq| (:|pair| ?x ?y) &rest)))
    (list (first match) (list ':|seq| `(:|pair| 1 ,(second match))))))))

;; ===================================================================================



;; obsolete
;; Sunil's original implementation, now obsolete
(defun get-formula-cation-component0 (formula)
  (if (eql formula *formula-to-name*)
      (or *formula-to-name-cation*
	  (progn
	    (unless *formula-to-name-anion*
	      (setq *formula-to-name-anion*
		(%get-formula-anion-component formula)))
	    (setq *formula-to-name-cation*
	      (%get-formula-cation-component formula *formula-to-name-anion*))))
    (prog1
	(progn
	  (setq *formula-to-name-anion*
	    (%get-formula-anion-component formula))
	  (setq *formula-to-name-cation*
	    (%get-formula-cation-component formula *formula-to-name-anion*)))
      (setq *formula-to-name* formula))))

;;; EOF
