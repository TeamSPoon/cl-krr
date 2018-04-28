;;
;; $Id: eq-solver-interface.lisp,v 1.96 2009/02/05 05:09:22 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defvar *aura-trace-equation-solver*        nil)
(defvar *EQ-FLAG*                           nil)
(defvar *EQ-SOLVER-IN-USE*                  nil)
(defvar *EQ-SOLVER-REWRITE-LIST*            nil)
(defvar *APPLICABLE-EQ-SYSTEMS-CACHE*       nil)
(defvar *EQUATION-SOLVE-CACHE*              nil)
(defvar *EQ-SOLVER-TOUCHED-INSTANCES*       nil)
(defvar *EQ-SOLVER-SYSTEM-INSTANCES-CACHE*  nil)
(defvar *EQ-SOLVER-VIOLATION-ABORT*         nil)
(defvar *EQ-SOLVER-SYMBOL-TABLE*            nil)
(defvar *TURN-OFF-ATAN-FIX*                 nil)
(defvar *CONTROLLER-BPS-VARIABLE-BINDINGS*  nil)
(defvar *CONTROLLER-BPS-EXPRESSIONS*        nil)
(defvar *CONTROLLER-EQ-HISTORY*             nil)
(defvar *EQ-SOLVER-SYSTEMS*                 nil)

(defun reset-eq-solver()
  (progn
    (setq *CONTROLLER-BPS-VARIABLE-BINDINGS*  nil)
    (setq *CONTROLLER-BPS-EXPRESSIONS*        nil)
    (setq *CONTROLLER-EQ-HISTORY*             nil)
    (setq *EQ-SOLVER-SYSTEM-INSTANCES-CACHE*  nil)
    (setq *EQ-SOLVER-TOUCHED-INSTANCES*       nil)
    (setq *EQ-SOLVER-SYMBOL-TABLE*            nil)
    (setq *EQUATION-SOLVE-CACHE*              nil)
    (setq *APPLICABLE-EQ-SYSTEMS-CACHE*       nil)
    (setq *EQ-SOLVER-VIOLATION-ABORT*         nil)
    (setq *EQ-SOLVER-REWRITE-LIST*            nil)
    ;(clear-evaluation-cache)
))

(defun test-equations(expr-list bindings)
  (dolist (sym (get-symbols-from-eq-expr-list expr-list))
    (let ((result (BPS-SOLVOBJVAR 
		   expr-list
		   sym
		   bindings)))
      (if (null (car result))
	  (format t "~a => nil~%" sym)
	(format t "~a => ~a~%" sym result)))))

(defun get-symbols-from-eq-expr-list(input)
  (remove-if 'numberp
	     (set-difference (remove-duplicates (flatten input))
			     '(= - + / * |expt| |sin| |cos| |atan| |tan| |sqrt| sqrt))))

;;Interface with novak's equation-solver and UoM system.
(defun bps-solveeq (eqns 
		    goalvar 
		    defined 
		    &optional (concept nil) 
		    (verbose nil)
		    (debug nil))
  (if debug (test-equations eqns defined))
    (cond ((null goalvar)
	   (if verbose (format t "BPS: Warning! Attempting eq-solver with null goal variable.~%")))
	  (*EQ-SOLVER-VIOLATION-ABORT*
	   (format t "   Skipping this concept due to constraint violation~%")
	   (setq *EQ-SOLVER-VIOLATION-ABORT* nil))
	  (t (handler-case
	      (let* ((*PACKAGE* (find-package 'km))
		     (novak-result (bps-solvobjvar
				   (sort-eq-expr-list
				    (make-eq-expr-list-proper eqns))
				   goalvar 
				   defined)))
		(setq *CONTROLLER-BPS-VARIABLE-BINDINGS* defined)
		(setq *CONTROLLER-BPS-EXPRESSIONS*       eqns)
		(setq *CONTROLLER-EQ-HISTORY* (copy-list *equations-history*))
		(bps-novak-output-into-clib-small-v-value novak-result concept))
	      (error (error)
		     (cond ((format t "BPS: Equation solver error, ~A~%Inputs were: eqns: ~a~%goalvar: ~a~%defined: ~a~%concept: ~a~%" error eqns goalvar defined concept)
			    nil)))))))

(defun find-clib-property-value-for-uom(uom)
  (car (ps-km-query `(|the| |range| |of| (|the| |cardinal-unit-class-of| |of| (|the| |instance-of| |of| ,uom))))))

(defun bps-clib-small-v-value-into-clib-property-value(small-v-value)
  (let ((debug nil))
    (if (values-consistent small-v-value)
	(let* ((uom (nth 2 (car small-v-value)))
	       (clib-property-value (find-clib-property-value-for-uom uom)))
	  (if (not (null clib-property-value))
	      (let ((instance (ps-instantiate-concept clib-property-value)))
		(ps-km-query `(,instance |now-has| (|value| ,small-v-value)) debug)))))))

;;For coercing UoMs not covered by Novak's code into CLib UoMs.
(defun find-applicable-UOM (kmified-uom km-class)  
  (cond ((or (null kmified-uom) (equal kmified-uom '|*unity|))
	 (cond ((equal km-class '|Angle-Value|)         '|*radian|)
	       ((equal km-class '|Concentration-Value|) '|*molar|)
	       (t '|*unity|)))
	(t kmified-uom)))

(defun bps-novak-output-into-clib-small-v-value(novak-result concept)
  (if (not (null (car novak-result)))
      (let* ((quantity  (first  novak-result))
	     (novak-uom (second novak-result))
	     (num_uom   (find-applicable-uom (kmifyunit (my-glunkmify novak-uom))
					     concept)))
	(if quantity
	    (if (member concept (get-applicable-property-value num_uom))
		`((:|pair| ,quantity
		    ,num_uom))
	      (progn (format t "BPS: Cannot coerce into CLib representation. Rejecting eq-solver output ~A.~%"
			     novak-result)
		     nil))))))

(defun get-applicable-Property-Value(uom)
  (remove-duplicates
   (flatten 
    (mapcar #'(lambda(x)
		(ps-km-query `(|the| |range| |of| ,x)))
	    (remove nil
		    (mapcar #'(lambda(x) 
				(if (member uom
					    (ps-km-query `(|the| |instances| |of| (|the| |cardinal-unit-class| |of| ,x))))
				    x))
			    (all-instances '|Property|)))))))

;;See HLO-2406. Need to replace *molar with *unity.
;;eq-solver does not handle *molar well.
(defun fix-problematic-var-bindings(var-bindings)
  (replace-elements-in-list 
   var-bindings
   '((|*molar| . |*unity|))))

(defun bps-solvobjvar(exprs goal var-bindings &optional (verbose nil))
  (handler-case (bps-solvobjvar0 exprs goal var-bindings)
		(error (condition)
		       (progn
			 (if verbose (format t "bps-solvobjvar crashed with ~S. Returning nil. ~%" condition))
			 nil))))

(defun bps-solvobjvar0(exprs goal var-bindings)
  (let ((result (solvobjvar (reorder-eq-expressions exprs) 
			    goal 
			    (fix-problematic-var-bindings var-bindings))))
    (if (complexp (first result))
	(cond ((zerop (floor (imagpart (first result))))
	       (progn
		 (format t "BPS: eq-solver returned complex number ~a, imaginary part < 0.5. Returning only real portion.~%" (first result))
		 (list (realpart (first result)) (second result))))
	      (t (progn 
		   (format t "BPS: eq-solver returned complex number ~a, imaginary part > 0.5. Returning nil.~%" (first result))
		   nil)))
        result)))

(defun get-eq-symbol-table (eq-frame-list)
  (let ((*logging* t)
	(debug     nil))
    (if (not (null *SILENT-SPYPOINTS*))
	(format t "Potential error in (get-eq-symbol-table ...) SILENT-SPYPOINT is not re-entrant!~%"))
    (clear-evaluation-cache)
    (remove-duplicates
     (remove-if 
      #'(lambda(mapping) (null (second mapping)))
      (mapcar #'(lambda(mapping)
		  (if (quotep (first mapping))
		      (list (unquote (first mapping)) (second mapping))
		    mapping))
	      (mappend #'(lambda(eq-frame)
			   (let ((*silent-spypoints* '((|the| ?x |of| ?y)))
				 (*eq-flag* nil))
			     (bps-set-checkpoint 'get-eq-symbol-table)
			     (clear-silent-spy-log)			   
			     (ps-km-query `(|the| |equation-symbol| |of| ,eq-frame) debug)
			     (bps-undo 'get-eq-symbol-table)
			     (get-fillers-for-km-query-lst
			      (remove-if #'(lambda(query-path) (not (acceptable-range-p 
								     (car (ranges-of 
									   (nth 1 query-path)) )
								     '(|Property-Value|))))
					 (clean-up-silent-spy-log
					  (inspect-silent-spy-log))))
			     (strip-pair-prefix (ps-km-query `(|the| |equation-symbol| |of| ,eq-frame) 
							     debug))))
		       eq-frame-list)))
     :test 'equal)))

(defun acceptable-range-p(slot applicable-range-concept-lst)
  (null 
   (intersection 
    (ranges-of slot)
    (mappend 'all-instances applicable-range-concept-lst))))

(defun expand-vector-eq-symbols ()
)

(defun extract-all-eq-symbols(symbol-table)
  (remove-duplicates 
   (flatten (mapcar #'(lambda (entry)
			(second entry))
		    symbol-table))))

;;Does two things
;; a) Remove quotes
;; b) Massage it so that LHS contains only 1 var.
(defun make-eq-expr-list-proper(input)
  (mapcar #'(lambda(expr)
	      (cond ((quotep expr)
		     (glfixequation (unquote expr)))
		    (t (glfixequation expr))))
	  input))

(defun sort-eq-expr-list(input)
  (sort (copy-list input) #'(lambda(x y) (< (length (flatten x)) (length (flatten y))))))

;;Performs shallow lookup of variable bindings. 
;;It is important that  we do not trigger eq-solver in this exercise.
;;If not, we may end up in an infinite loop.
(defun extract-symbol-value-map (eq-symbol-table)
  (let ((*EQ-FLAG* nil))
    (let ((result (remove-duplicates
		   (remove nil	  
			   (mapcar 'extract-symbol-value-map-entry
				   eq-symbol-table))
		   :test #'(lambda(x y)
			     (equal (flatten x) (flatten y))))))
      result)))

(defun extract-symbol-value-map-entry(entry)
  (let* ((*EQ-FLAG* nil)
	 (symbol        (first  entry))
	 (Big-P-Value   (second entry))
	 (Small-P-Value (lookup-small-v-value Big-P-Value)))
    (if (not (null Small-P-Value))
	(list symbol
	      (car (strip-pair-prefix Small-P-Value))))))

(defun get-connected-to-list(x)
  (GET-SLOTSVALS x
		 :FACET 'OWN-PROPERTIES
		 :SITUATION (curr-situation)))

;;Sometimes KM changes the KM-instance after doing some query... So need to update it.
(defun update-km-instance-name(x &optional(verbose nil))
  (let ((*EQ-FLAG* nil))
    (let ((result (remove-duplicates
		   (remove nil
			   (mappend #'(lambda(target)
				       (let ((slot (invert-slot (car target)))
					     (frame-list (cadr target)))
					 (mappend #'(lambda(y)
						      (if (not (equal slot '|equation-uses|))
							  (km `(|the| ,slot |of| ,y))))
						  frame-list)))
				   (get-connected-to-list x))))))
      (if verbose (format t "checking if ~x is a member of ~a.~%" x result))
      (cond ((member x result) x)
	    ((= (length result) 1) (car result))))))

(defun activate-big-p-value(Big-P-Value &optional(verbose nil))
  (let ((*EQ-FLAG* nil)
	(property-of-list (GET-SLOTSVALS Big-P-Value
					 :FACET 'OWN-PROPERTIES
					 :SITUATION (curr-situation))))
    (dolist (property property-of-list)
      (let ((slotname (invert-slot (car property)))
	    (frame    (car (cadr property))))
	(if (kb-objectp frame)
	    (ps-km-query `(|the| ,slotname |of| ,frame) verbose))))))

(defun lookup-small-v-value(Big-P-Value &optional(verbose nil))
  (let ((*EQ-FLAG* nil))
    (bps-set-checkpoint 'LOOKUP-SMALL-V-VALUE)
    (activate-big-p-value Big-P-Value)
    (let ((small-v-lookup (ps-km-query `(|the| |value| |of| ,Big-P-Value) verbose)))
      (bps-undo 'LOOKUP-SMALL-V-VALUE)
      (cond ((null small-v-lookup) nil)
	    ((values-consistent small-v-lookup)
	     (list (car small-v-lookup)))
	    (t 
	     (progn
	       (format t "EQ-SOLVER failed: inconsistent small-v values for ~a.~%" Big-P-Value)
	       (throw 'BPS-REASONING-ERROR 
		      (list 'BPS-REASONING-ERROR
			    (format nil "EQ-SOLVER failed: inconsistent small-v values for ~a.~%" 
				        Big-P-Value)))))))))

;;Returns the concepts referring to instance. Found via applicable domain relations.
(defun get-domain-for-Property-value-instance(instance)
  (cond ((atom instance)
	 (let ((target-slots (get-applicable-inverse-slots-for-instance instance)))
	   (mappend #'(lambda(slot)
			(km-slotvals instance slot))
		    target-slots)))
	(t (union
	     (get-domain-for-Property-value-instance (car instance))
	     (get-domain-for-Property-value-instance (cdr instance))))))

;;Get-all-symbols in equation
(defun get-all-eq-symbols (eq-symbol-table)
  (mapcar #'(lambda(binding)
	      (car binding))
	  eq-symbol-table))

(defun get-all-eq-km-instances(eq-symbol-table)
  (mapcar #'(lambda(binding)
	      (cdr binding))
	  eq-symbol-table))

;;Prefix eq-symbols. Returns a map to be used for substitution.
(defun prefix-eq-symbols(system-name symbol-list)
  (let ((*EQ-FLAG* nil)
	(prefix (car (ps-km-query `(|the| |component| |of| ,system-name)))))
  (mapcar #'(lambda(symbol)
	      (cons symbol (intern (format nil "~a-~a" prefix symbol) :km)))
	      symbol-list)))

(defun extract-applicable-eq-systems-for-instance()
  (progn 
    ;(bps-set-checkpoint 'EXTRACT-APPLICABLE-EQ-SYSTEMS-FOR-INSTANCE)
    (multiple-value-bind
	(x y)
	(extract-equation-for-system-list (get-all-relevant-systems))
      ;(bps-undo 'EXTRACT-APPLICABLE-EQ-SYSTEMS-FOR-INSTANCE)
      (values x y))))

;;Extracts eq-expressions and symbol table from system(s).
(defun extract-equation-for-system-list(input)
  (cond ((null input)())
	((atom input) 
	 (extract-equation-for-system input))
	(t (multiple-value-bind
	       (rest-eq-expr-list rest-eq-symbol-table)
	       (extract-equation-for-system-list (cdr input))
	     (multiple-value-bind 
	       (eq-expr-list eq-symbol-table)
		 (extract-equation-for-system (car input))
	       (integrate-eq-expr-and-symbols rest-eq-expr-list rest-eq-symbol-table
					      eq-expr-list eq-symbol-table))))))

(defun extract-equation-for-system(system-frame &optional(debug nil))
  (if (not (null system-frame))
      (let* ((connected-instance (ps-km-query `(|the| |component| |of| ,system-frame)))
	     (eq-frame-list      (ps-km-query `(|the| |equation| |of| ,system-frame)))
	     (eq-symbol-table    (get-eq-symbol-table eq-frame-list))
	     (eq-expr-list       (make-eq-expr-list-proper 
				  (ps-km-query `(|the| |equation-expression| |of| ,(cons ':|set| eq-frame-list)))))
	     (symbol-list      (get-all-eq-symbols eq-symbol-table)))
	(if debug
	    (let ((prefix-map  (prefix-eq-symbols system-frame symbol-list)))
					;(format t "Extract-equations for ~a(~a).~%" system-frame connected-instance)
	      (values (replace-elements-in-list eq-expr-list
						prefix-map)
		      (replace-elements-in-list eq-symbol-table
						prefix-map)))
	  (values eq-expr-list eq-symbol-table)))))

(defun integrate-eq-expr-and-symbols(eq-expr-list 
				     eq-symbol-table 
				     new-eq-expr-list 
				     new-eq-symbol-table)
  (multiple-value-bind 
    (resolved-eq-expr-list resolved-eq-symbol-table resolved-new-eq-expr-list resolved-new-eq-symbol-table)
      (resolve-all-conflicting-symbols eq-expr-list 
				 eq-symbol-table 
				 new-eq-expr-list 
				 new-eq-symbol-table)
    (values (remove-duplicates (append resolved-eq-expr-list resolved-new-eq-expr-list) :test 'equal)
	    (remove-duplicates (append resolved-eq-symbol-table resolved-new-eq-symbol-table) :test 'equal))))

(defun generate-conflict-symbol-rewrite-map (conflicting-symbols other-symbols)
  (mapcar #'(lambda(conflicted-symbol)
	      (let ((new-symbol (rewrite-conflicting-symbol-as conflicted-symbol other-symbols)))
		(cons conflicted-symbol new-symbol)))
	  conflicting-symbols))

(defun rewrite-conflicting-symbol-as(conflicted-symbol other-symbols)
  (multiple-value-bind
      (_prefix _suffix)
      (eq-solver-decompose-symbol conflicted-symbol)
    (find-non-conflicting-rewrite-for-stem _prefix
					   conflicted-symbol
					   other-symbols)))
	     
(defun eq-solver-decompose-symbol(x)
  (let ((_prefix (get-prefix (stringify x) "_"))
	(_suffix (get-suffix (stringify x) "_")))
    (if (and (stringp _prefix)
	     (stringp _suffix)
	     (numberp (string-to-number _suffix)))
	(values _prefix _suffix)
        (values x nil))))

(defun get-next-rewrite-number-for-conflicted-symbol(_prefix)
  (let ((prev-idx  (cadr (assoc _prefix
				*EQ-SOLVER-REWRITE-LIST* :test 'string=))))
    (cond ((not (null prev-idx))
	   (1+ prev-idx))
	  (t 1))))

(defun find-non-conflicting-rewrite-for-stem(stem 
					     conflicted-symbol 
					     other-symbols)
  (let* ((idx (get-next-rewrite-number-for-conflicted-symbol stem))
	 (candidate (intern (format nil "~a_~a" stem idx) :km)))
    (while (or (equal candidate conflicted-symbol)
	       (member candidate other-symbols))
      (setf idx (1+ idx))
      (setf candidate (intern (format nil "~a_~a" stem idx) :km)))
    (push (list stem idx) *EQ-SOLVER-REWRITE-LIST*)
    candidate))

;;Rewrites only new equations.
(defun resolve-conflicting-symbols(current-eq-expr-list 
				   current-eq-symbol-table 
				   new-eq-expr-list 
				   new-eq-symbol-table)
  (multiple-value-bind
      (conflicting-symbols other-symbols)
      (analyse-symbol-table current-eq-symbol-table new-eq-symbol-table)
    (cond ((null conflicting-symbols) (values new-eq-expr-list new-eq-symbol-table))
	  (t (let ((rewrite-map (generate-conflict-symbol-rewrite-map conflicting-symbols other-symbols)))
	       (resolve-conflicting-symbols current-eq-expr-list
					    current-eq-symbol-table
					    (replace-elements-in-list new-eq-expr-list
								      rewrite-map)
					    (replace-elements-in-list new-eq-symbol-table
								      rewrite-map)))))))

;;rewrites both current and new equations.
(defun resolve-all-conflicting-symbols(current-eq-expr-list 
				       current-eq-symbol-table 
				       new-eq-expr-list 
				       new-eq-symbol-table)
  (multiple-value-bind
      (conflicting-symbols other-symbols)
      (analyse-symbol-table current-eq-symbol-table new-eq-symbol-table)
    (cond ((null conflicting-symbols) (values current-eq-expr-list current-eq-symbol-table new-eq-expr-list new-eq-symbol-table))
	  (t (let ((rewrite-map-for-current (generate-conflict-symbol-rewrite-map conflicting-symbols other-symbols))
		   (rewrite-map-for-new     (generate-conflict-symbol-rewrite-map conflicting-symbols other-symbols)))
	       (resolve-all-conflicting-symbols (replace-elements-in-list current-eq-expr-list
									  rewrite-map-for-current)
						(replace-elements-in-list current-eq-symbol-table
									  rewrite-map-for-current)
						(replace-elements-in-list new-eq-expr-list
									  rewrite-map-for-new)
						(replace-elements-in-list new-eq-symbol-table
									  rewrite-map-for-new)))))))

(defun analyse-symbol-table(current-symbol-table new-symbol-table)
  (let ((conflicting-symbols (determine-conflicting-symbols current-symbol-table new-symbol-table))
	(all-symbols         (union (get-symbols-in-symbol-table current-symbol-table)
				    (get-symbols-in-symbol-table new-symbol-table))))
    (values conflicting-symbols
	    (set-difference all-symbols	conflicting-symbols))))

(defun determine-conflicting-symbols(current-symbol-table new-symbol-table)
  (let ((new-symbols (get-symbols-in-symbol-table new-symbol-table)))
    (remove nil
	    (mapcar #'(lambda(symbol-table-entry)
			(if (does-symbol-conflict-p symbol-table-entry current-symbol-table)
			    (car symbol-table-entry)))
		    new-symbol-table))))

(defun does-symbol-conflict-p (symbol-table-entry symbol-table)
  (let ((symbol (car symbol-table-entry)))
    (and (assoc symbol symbol-table)
	 (not (equal symbol-table-entry
		     (assoc symbol symbol-table))))))

(defun get-symbols-in-symbol-table(symbol-table)
  (mapcar 'car symbol-table))

(defun eq-solver-query-immediate-slots(instance &optional(verbose nil))
  (let ((debug nil)
	(*eq-flag* nil)
	(slots-to-query (intersection 
			 (extract-all-slots *controller-triple-list*)
			 (applicable-slots-for instance))))
    (if debug
	(format t "eq-solver: eagerly evaluating ~a slots for ~A~%"
		(length slots-to-query)
		instance))
  (mappend #'(lambda(relation)
	       (if (valid-domain-for instance relation)
		   (get-triples-for-instance-query instance relation verbose)))
	   slots-to-query)))

(defun equation-solve(target-instance &optional(verbose nil))
  (let ((debug nil)
	(result           nil)
	(adhoc-km-logging nil)
	(*controller-triple-instances* 
	 (insert-at-end target-instance 
			*controller-triple-instances*)))
    ;(bps-activate-eq-solver-variable-bindings)
    (reset-eq-solver)
    (if (null *LOGGING*) (progn 
			   (if verbose (format t "BPS: Weird, KM logging should be turned on for eq-solver. Turning it on for now.~%"))
			   (setq adhoc-km-logging t)
			   (start-logging :with-comment verbose)))
    (cond ((null target-instance)())
	  ((cadr (assoc target-instance
			*EQUATION-SOLVE-CACHE*))
	   (setq result (cadr (assoc target-instance
				     *EQUATION-SOLVE-CACHE*))))
	  ((lookup-small-v-value target-instance)
	   (setq result (lookup-small-v-value target-instance)))
	  (t (setq result (try-solving-for target-instance))))
    (setq *EQUATION-SOLVE-CACHE* 
	  (cons (list target-instance result)
		*EQUATION-SOLVE-CACHE*))
    (if adhoc-km-logging (stop-logging :with-comment verbose))
    (if (and (not (null target-instance))
	     (not (null result))
	     (values-consistent result))
      (ps-km-query `(,target-instance |now-has| (|value| ,result)) debug))
    result))

(defun get-additional-instances-to-touch(target-instance)
  (remove-if-not 'km-instancep (flatten (get-connected-to-list target-instance))))

;;Need to make it take in a list of instances?
;; a) Aggregate all Equation expressions and its symbols
;; b) At the same time, resolve conflicting symbols and rewrite expressions.
(defun try-solving-for (target-instance &optional(verbose nil))
  (let ((equation-solve0-begin (get-universal-time)) 
	resolved-symbol val command ans unit)
    (if (not *EQ-SOLVER-IN-USE*)
	(if (and verbose)
	    (format t "EQ-SOLVER(~a) on ~a~%" *EQ-FLAG* target-instance)))
    (cond ((and (not (null target-instance))
		*EQ-FLAG*
		(not *EQ-SOLVER-IN-USE*)
		(isa target-instance '|Property-Value|))
	   (multiple-value-bind
	       (eq-expr-list target-symbol symbol-value-mapping desired-property-value)
	       (ps-get-connected-eq-systems target-instance)
	     (push (list *current-vp-inst*
			 (list eq-expr-list 
			       nil 
			       symbol-value-mapping
			 )
		   )
		   *EQ-SOLVER-SYSTEMS*)
	     (bps-solveeq eq-expr-list
			  target-symbol
			  symbol-value-mapping
			  desired-property-value))))))

(defun ps-get-connected-eq-systems(target-instance &optional(verbose nil))
  (let ((*EQ-SOLVER-IN-USE* t)
	(begin-time (get-universal-time)))
    #|;;deprecated. (ps-assert-triples ...) eagerly eval slot rules.
    (touch-all-properties-for-instances-in-scenario 
     (get-additional-instances-to-touch target-instance))
    ;(clear-evaluation-cache);;Redundant call. To be safe.
    |#
    (multiple-value-bind
	(eq-expr-list eq-symbol-table symbol-value-mapping)
	(collate-equation-system)
      (let ((target-symbol          (determine-eq-solver-target-symbol target-instance eq-symbol-table))
	    (desired-property-value (car (ps-km-query `(|the| |instance-of| |of| ,target-instance)))))
	(if verbose (format t "Gathering equation system took ~a secs.~%" (- (get-universal-time) begin-time)))
	(values eq-expr-list target-symbol symbol-value-mapping desired-property-value)))))

(defun collate-equation-system()
  (let ((debug nil))
    (setq *EQ-SOLVER-REWRITE-LIST*            nil)
    (identify-novel-instances  *CONTROLLER-TRIPLE-INSTANCES*)
    (if debug (format t "EQ-SOLVER: Returning systems for ~a~%" *CONTROLLER-TRIPLE-INSTANCES*))
    (if debug (format t "~a" (ps-show-provenance *CONTROLLER-TRIPLE-INSTANCES*)))
    (multiple-value-bind 
	(eq-expr-list eq-symbol-table)
	(extract-applicable-eq-systems-for-instance)
      (let* (
             ;; for each vector in eq-symbol-table
             ;;     find its x, y, mag and angle using known eqns in eq-expr-list
             ;; for each eqn in eq-expr-list
             ;;     add an equation with all vecs replaced by their x
             ;;     add an equation with all vecs replaced by their y
             (all-vector-parts (find-all-vector-parts eq-symbol-table eq-expr-list))
             (expanded-eq-expr-list (add-vector-part-eqns all-vector-parts eq-expr-list))
 
             (dup-bindings (find-duplicate-bindings eq-symbol-table))
             (expanded-eq-expr-list 
	      (append 
	       (generate-equivalence-equations-for-duplicate-binding-list dup-bindings)
	       expanded-eq-expr-list))
            )
        (setq *EQ-SOLVER-SYMBOL-TABLE* eq-symbol-table)
	(let ((symbol-value-mapping 
	       (extract-symbol-value-map eq-symbol-table)))
	  (values expanded-eq-expr-list eq-symbol-table symbol-value-mapping)
)))))

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; vector rewrite code begins here                                     ;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun find-all-vector-parts (symbol-table eqn-list)
  (let ((vector-list (get-vector-symbols symbol-table))
        (vector-parts-list nil)
       )
    (dolist (vector-symbol vector-list)
       (setq vector-parts-list
             (append vector-parts-list (list (get-vector-parts vector-symbol symbol-table eqn-list)))
       )
    )
    vector-parts-list
  )
)

(defun get-vector-symbols (sym-table)
  (let ((vector-symbols nil))
    (dolist (sym-pair sym-table)
      (if (and (not (null (second sym-pair))) ;; need this guard to prevent KM-abort errors due to (instance-of nil)
	       (instance-of (second sym-pair) '|Property-Vector-Value|))
	  (setq vector-symbols (append vector-symbols (list (first sym-pair))))
      )
    )
    vector-symbols
  )
)

(defun get-vector-parts (vector-sym sym-table eqns)
  (let* ((magnitude-component (get-vector-magnitude-component vector-sym sym-table))
         (direction-component (get-vector-direction-component vector-sym sym-table))
         (x-component (get-vector-x-component vector-sym sym-table magnitude-component eqns))
         (y-component (get-vector-y-component vector-sym sym-table magnitude-component eqns))
        )
    (list vector-sym 
          magnitude-component 
          direction-component
          x-component
          y-component
    )
  )
)

;; vector magnitude is (the (the secondary-slot of vectorinst) of vectorinst)
(defun get-vector-magnitude-component (v-sym sym-table)
  (let* ((vector-inst (lookup-instance-using-symbol v-sym sym-table))
         (magnitude-inst  (car (ps-km-query `(|the| (|the| |secondary-slot| |of| ,VECTOR-INST)
                                                    |of| ,VECTOR-INST))))
        )
    (lookup-symbol-using-instance magnitude-inst sym-table)
  )
)

;; vector direction is (the direction of vectorinst)
(defun get-vector-direction-component (v-sym sym-table)
  (let* ((vector-inst (lookup-instance-using-symbol v-sym sym-table))
         (angle-inst  (car (ps-km-query `(|the| |direction| |of| ,VECTOR-INST))))
        )
    (lookup-symbol-using-instance angle-inst sym-table)
  )
)

(defun get-vector-x-component (v-sym sym-table mag-sym eqn-list)
  (or (get-vector-x-component-from-inst v-sym sym-table)
      (get-vector-x-component-from-eqns v-sym sym-table mag-sym eqn-list)
  )
)

(defun get-vector-y-component (v-sym sym-table mag-sym eqn-list)
  (or (get-vector-y-component-from-inst v-sym sym-table)
      (get-vector-y-component-from-eqns v-sym sym-table mag-sym eqn-list)
  )
)

;; vector x-component is (the (the x-component-slot of vectorinst) of vectorinst)
(defun get-vector-x-component-from-inst (v-sym sym-table)
  (let* ((vector-inst (lookup-instance-using-symbol v-sym sym-table))
         (x-component-inst (car (ps-km-query `(|the| (|the| |x-component-slot| |of| ,VECTOR-INST)
                                                     |of| ,VECTOR-INST))))
        )
    (lookup-symbol-using-instance x-component-inst sym-table)
  )
)

;; or vector x-component is found in an eqn of the form
;; (= mag-symbol (sqrt (+ (expt y-symbol 2) (expt x-symbol 2))))
(defun get-vector-x-component-from-eqns (v-sym sym-table mag-sym eqn-list)
 (second
  (third
   (second
    (third
       (car (member-if #'(lambda (eqn) (and (eq (second eqn) mag-sym)
                                            (eq (car (third eqn)) '|sqrt|)
                                            (eq (car (second (third eqn))) '+)
                                            (eq (first (second (second (third eqn)))) '|expt|)
                                            (eq (third (second (second (third eqn)))) 2)
                                            (eq (first (third (second (third eqn)))) '|expt|)
                                            (eq (third (third (second (third eqn)))) 2)
                                       )
                       )
                       eqn-list
            )
       )
    )
   )
  )
 )
)

;; vector y-component is (the (the y-component-slot of vectorinst) of vectorinst)
(defun get-vector-y-component-from-inst (v-sym sym-table)
  (let* ((vector-inst (lookup-instance-using-symbol v-sym sym-table))
         (y-component-inst (car (ps-km-query `(|the| (|the| |y-component-slot| |of| ,VECTOR-INST)
                                                     |of| ,VECTOR-INST))))
        )
    (lookup-symbol-using-instance y-component-inst sym-table)
  )
)

;; vector y-component is found in an eqn of the form
;; (= mag-symbol (sqrt (+ (expt y-symbol 2) (expt x-symbol 2))))
(defun get-vector-y-component-from-eqns (v-sym sym-table mag-sym eqn-list)
 (second
  (second
   (second
    (third
       (car (member-if #'(lambda (eqn) (and (eq (second eqn) mag-sym)
                                            (eq (car (third eqn)) '|sqrt|)
                                            (eq (car (second (third eqn))) '+)
                                            (eq (first (second (second (third eqn)))) '|expt|)
                                            (eq (third (second (second (third eqn)))) 2)
                                            (eq (first (third (second (third eqn)))) '|expt|)
                                            (eq (third (third (second (third eqn)))) 2)
                                       )
                       )
                       eqn-list
            )
       )
    )
   )
  )
 )
)

(defun lookup-instance-using-symbol (symbol symbol-table)
  (if symbol-table
      (if (eq symbol (first (car symbol-table)))
          (second (car symbol-table))
          (lookup-instance-using-symbol symbol (cdr symbol-table))
      )
  )
)

(defun lookup-symbol-using-instance (instance symbol-table)
  (if symbol-table
      (if (eq instance (second (car symbol-table)))
          (first (car symbol-table))
          (lookup-symbol-using-instance instance (cdr symbol-table))
      )
  )
)

(defun add-vector-part-eqns (vector-parts-list eqn-list)
  (let ((expanded-eqn-list eqn-list))
    (dolist (eqn eqn-list)
      (let ((rewritten-eqn (rewrite-eqn-with-x-components eqn vector-parts-list)))
        (if rewritten-eqn
            (setq expanded-eqn-list (append expanded-eqn-list (list rewritten-eqn)))
        )
      )
      (let ((rewritten-eqn (rewrite-eqn-with-y-components eqn vector-parts-list)))
        (if rewritten-eqn
            (setq expanded-eqn-list (append expanded-eqn-list (list rewritten-eqn)))
        )
      )
    )
    expanded-eqn-list
  )
)

(defun rewrite-eqn-with-x-components (eqn vector-parts-list)
  (let* ((vector-list (mapcar #'car vector-parts-list))
         (eqn-vectors (intersection vector-list (flatten eqn)))
         (rewritten-eqn nil)
        )
    (if eqn-vectors
        (progn
          (setq rewritten-eqn eqn)
          (dolist (vector eqn-vectors)
            (setq rewritten-eqn (replace-element-in-eqn vector 
                                                        (get-vector-x-part vector vector-parts-list)
                                                        rewritten-eqn
                                )
            )
          )
        )
    )
    rewritten-eqn
  )
)

(defun rewrite-eqn-with-y-components (eqn vector-parts-list)
  (let* ((vector-list (mapcar #'car vector-parts-list))
         (eqn-vectors (intersection vector-list (flatten eqn)))
         (rewritten-eqn nil)
        )
    (if eqn-vectors
        (progn
          (setq rewritten-eqn eqn)
          (dolist (vector eqn-vectors)
            (setq rewritten-eqn (replace-element-in-eqn vector 
                                                        (get-vector-y-part vector vector-parts-list)
                                                        rewritten-eqn
                                )
            )
          )
        )
    )
    rewritten-eqn
  )
)

(defun get-vector-x-part (vector vector-parts-list)
  (fourth (car (member-if #'(lambda (vp) (eq vector (car vp))) vector-parts-list)))
)

(defun get-vector-y-part (vector vector-parts-list)
  (fifth (car (member-if #'(lambda (vp) (eq vector (car vp))) vector-parts-list)))
)

(defun replace-element-in-eqn (e1 e2 eqn)
  (if eqn
      (cond ((eq e1 eqn) e2)
            ((atom eqn) eqn)
            (t (cons (replace-element-in-eqn e1 e2 (car eqn))
                     (replace-element-in-eqn e1 e2 (cdr eqn))))
      )
  )
)

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; vector rewrite code ends here                                       ;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;Determines the equation symbol given target-instance and eq-symbol-table
(defun determine-eq-solver-target-symbol (target-instance eq-symbol-table)
  (progn
    (second (assoc 
	     (update-km-instance-name target-instance)
	     (invert-map eq-symbol-table)))))

(defun ps-show-provenance(instance)
  (let ((s (make-string-output-stream)))
    (cond ((null instance))
	  ((atom instance)
	   (progn
	     (format s "~a~%" instance)
	     (dolist (path (ps-get-provenance instance))
	       (format s "  ~a~%" path))))
	  (t (format s "~a~a"
		     (ps-show-provenance (car instance))
		     (ps-show-provenance (cdr instance)))))
    (get-output-stream-string s)))

(defun ps-get-provenance(instance)
  (let* ((debug nil)
	 (slot-lst (ps-km-query `(|the| |property-of-slot| |of| ,instance) debug)))
    (mappend #'(lambda(slot)
		 (let ((parent (ps-km-query `(|the| ,slot |of| ,instance) debug)))
		   (mapcar #'(lambda(p)
			       (list '|the| (invert-slot slot) '|of| p))
			   parent)))
	     slot-lst)))

(defun get-all-relevant-systems(&optional (instance-list nil)
					  (identified-systems nil))
  (remove-duplicates 
   (get-system-for-km-instance-list
    (get-triple-instances-for-eq-solver))))
   
(defun get-triple-instances-for-eq-solver()
  (append-controller-triple-instances 
   (extract-all-km-constants *controller-triple-list*)))

(defun get-all-instances-mentioned-in-system (input)
  (cond ((null input) ())
	((atom input) (get-all-instances-mentioned-in-single-system input))
	(t (union (get-all-instances-mentioned-in-single-system (car input))
		  (get-all-instances-mentioned-in-system (cdr input))))))

(defun get-all-instances-mentioned-in-single-system (input)
  (cond ((assoc input *EQ-SOLVER-SYSTEM-INSTANCES-CACHE*)
	 (cadr (assoc input *EQ-SOLVER-SYSTEM-INSTANCES-CACHE*)))
	(t (let ((result (get-all-instances-mentioned-in-single-system0 input)))
	     (setf *EQ-SOLVER-SYSTEM-INSTANCES-CACHE* 
		   (cons (list input result)
			 *EQ-SOLVER-SYSTEM-INSTANCES-CACHE*))
	     result))))

(defun get-all-instances-mentioned-in-single-system0 (input &optional(verbose nil))
  (let ((eq-set-list (ps-km-query `(|the| |equation| |of| ,input) verbose)))
    (mappend #'(lambda(eq-set)
		(progn
		  (ps-km-query `(|the| |equation-expression| |of| ,eq-set) verbose)
		  (ps-km-query `(|the| |equation-uses| |of| ,eq-set) verbose)))
	    eq-set-list)))

;;Returns the system bound to km instances in list
(defun get-system-for-km-instance-list(target-instance-list &optional(verbose t))
  (let* ((verbose nil)
	 (*EQ-FLAG* nil)
	 (instance-list (get-list-of-instances-connected-to-equation-system target-instance-list))
	 (result (remove-duplicates
		  (if (and (atom instance-list)
			   (not (null instance-list)))
		      (get-system-for-km-instance instance-list verbose)
		    (mappend 'get-system-for-km-instance
			     instance-list)))))
    (if verbose (format t "BPS: ~a connected to ~a equation systems~%" instance-list result))
    result))

(defun get-list-of-instances-connected-to-equation-system(instance-list)
  (remove-if-not 'connected-to-equation-system-p (copy-list instance-list)))

(defun connected-to-equation-system-p(instance &optional(verbose nil))
  (let ((*EQ-FLAG* nil)
	(concept-list   (ps-km-query `(|the| |instance-of| |of| ,instance))))
    (let ((connected-systems 
	   (mappend #'(lambda(concept)
			(ps-km-query `(|the| |component-of| |of| ,(ps-instantiate-concept concept))))
		    concept-list)))
      (if verbose (format t "BPS: ~a connected to equation-system => ~a~%" instance (not (null connected-systems))))
      (values (not (null connected-systems))
	      (length connected-systems)))))
    
;;Returns the system bound to km instance(s)
(defun get-system-for-km-instance(input &optional(verbose nil))
  (let ((*EQ-FLAG* nil))
    (if (not (null input))
	(union (ps-km-query `(|the| |equation-of| |of| 
			      (|the| |used-in-equation| |of| ,input)) verbose)
	       (ps-km-query `(|the| |component-of| |of| ,input) verbose)))))

#|Example usage
(REPLACE-DUPLICATE-BINDINGS 
 '((|_Move2-v| . |_Move2-v|) (|_Move1-v| . |_Move2-v|))
 '((= |_Move1-d| (* |_Move1-v| |_Move1-t|)) (= |_Move2-d| (* |_Move2-v| |_Move2-t|)))
 '((|_Move1-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
   (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|)))
=>;;multiple-valued-output
((= |_Move1-d| (* |_Move2-v| |_Move1-t|)) (= |_Move2-d| (* |_Move2-v| |_Move2-t|)))
((|_Move2-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
 (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|))
|#
(defun replace-duplicate-bindings(dup-binding-replace-map eq-expr-list eq-symbol-table)
  (values (replace-duplicate-bindings-for-eq-expr-list    dup-binding-replace-map
							  eq-expr-list)
	  (replace-duplicate-bindings-for-eq-symbol-table dup-binding-replace-map
							  eq-symbol-table)))

#|Example usage
(REPLACE-DUPLICATE-BINDINGS-FOR-EQ-EXPR-LIST 
 '((|_Move2-v| . |_Move2-v|) (|_Move1-v| . |_Move2-v|))
 '((= |_Move1-d| (* |_Move1-v| |_Move1-t|)) (= |_Move2-d| (* |_Move2-v| |_Move2-t|))))
=>
((= |_Move1-d| (* |_Move2-v| |_Move1-t|)) (= |_Move2-d| (* |_Move2-v| |_Move2-t|)))
|#
(defun replace-duplicate-bindings-for-eq-expr-list(dup-binding-replace-map eq-expr-list)
  (mapcar #'(lambda(eq-expr)
	      (replace-elements-in-list eq-expr dup-binding-replace-map))
	  eq-expr-list))

#|Example usage
(REPLACE-DUPLICATE-BINDINGS-FOR-EQ-SYMBOL-TABLE 
 '((|_Move2-v| |_Move2-v|) (|_Move1-v| |_Move2-v|))
 ((|_Move1-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
  (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|)))
=>
  ((|_Move2-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
   (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|))
|#

(defun replace-duplicate-bindings-for-eq-symbol-table(dup-binding-replace-map eq-symbol-table)
  (mapcar #'(lambda(entry)
	      (let* ((symbol      (first  entry))
		     (var         (second entry))
		     (replacement (cdr (assoc symbol dup-binding-replace-map))))
		(cond ((not (null replacement))
		       (list replacement var))
		      (t entry))))
	  eq-symbol-table))
  
#|Example usage
(generate-replace-map-for-duplicate-bindings 
 '((|_Velocity-Value773| (|_Move2-v| |_Move1-v|))))
=>
  ((|_Move2-v| . |_Move2-v|) (|_Move1-v| . |_Move2-v|))
|#
(defun generate-replace-map-for-duplicate-bindings(duplicate-binding-list)
  (mappend #'(lambda (dup-binding)
	       (let ((var          (first  dup-binding))
		     (dup-sym-list (second dup-binding)))
		 (let ((new-sym (first dup-sym-list)))
		   (mapcar #'(lambda (dup-sym)
			       (cons dup-sym new-sym))
			   dup-sym-list))))
	   duplicate-binding-list))
		      
#|Example usage
(find-duplicate-bindings 
 '((|_Move1-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
   (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|)))
=>
  ((|_Velocity-Value773| (|_Move2-v| |_Move1-v|)))
|#

(defun find-duplicate-bindings(eq-symbol-map)
  (remove nil  
	  (mapcar #'(lambda(entry)
		      (if (> (length (second entry)) 1) entry))
		  (aggregate-symbol-for-variable eq-symbol-map))))

#|Example usage
(aggregate-symbol-for-variable 
 '((|_Move1-v| |_Velocity-Value773|) (|_Move1-t| |_Duration-Value774|) (|_Move1-d| |_Length-Value772|)
   (|_Move2-v| |_Velocity-Value773|) (|_Move2-t| |_Duration-Value782|) (|_Move2-d| |_Length-Value780|)))
=>
  ((|_Length-Value780| (|_Move2-d|)) (|_Duration-Value782| (|_Move2-t|)) (|_Velocity-Value773| (|_Move2-v| |_Move1-v|))
   (|_Length-Value772| (|_Move1-d|)) (|_Duration-Value774| (|_Move1-t|)) (|_Velocity-Value773| (|_Move1-v|)))
|#

(defun aggregate-symbol-for-variable(eq-symbol-map)
  (let (var-symbol-map)
    (dolist (entry eq-symbol-map)
      (let* ((symbol   (first  entry))
	     (var (second entry))
	     (var-symbol-map-entry (assoc var var-symbol-map)))
	(cond ((null var-symbol-map-entry)
	       (let ((new-var-symbol-map-entry (list var (list symbol))))
		 (setf var-symbol-map 
		       (cons new-var-symbol-map-entry 
			     var-symbol-map))))
	      (t 
	       (let ((new-var-symbol-map-entry (list var (cons symbol
							       (second var-symbol-map-entry)))))
		 (setf var-symbol-map (cons new-var-symbol-map-entry
					    var-symbol-map)))))))
    var-symbol-map))

#|
;;Test-cases
(bps-solveeq '((= (- |r_f| |r_i|) (+ (* |v_i| |t|) (/ (* |a| (|expt| |t| 2)) 2))) 
		    (= |v_avg| (/ (+ |v_f| |v_i|) 2))
		    (= (- |v_f| |v_i|) (* |a| |t|)) 
		    (= (- (|expt| |v_f| 2) (|expt| |v_i| 2)) (* (* 2 |a|) |d|)))
		  '|v_f| 
		  '((|a| (9.8 METER-PER-SECOND-SQUARED))
		    (|v_i| (-9.8 |*meter-per-second|)) 
		    (|t| (1 |*second|)))
		  '|Velocity-Value|)

(glfixequation '(= (- |r_f| |r_i|) (+ (* |v_i| |t|) (/ (* |a| (|expt| |t| 2)) 2))) )
(glfixequation '(= |v_avg| (/ (+ |v_f| |v_i|) 2)))
(glfixequation '(= (- |v_f| |v_i|) (* |a| |t|)) )
(glfixequation '(= (- (|expt| |v_f| 2) (|expt| |v_i| 2)) (* (* 2 |a|) |d|)))

|#

#|Test-KB
;;Motivating example
(_Move1 has 
       (instance-of (My-Move))
       (distance    ((a Distance-Value with (value ((:pair 10 *meter))))))
       (duration    ((a Duration-Value with (value ((:pair 2 *second)))))))

(_Move2 has 
       (instance-of (My-Move))
       (velocity    ((the velocity of _Move1)))
       (duration    ((a Duration-Value with (value ((:pair 4 *second)))))))
	
(the value of (the velocity of _Move1))   ;;Simple direct eq-solving
(the value of (the velocity of _Move2))   ;;Simple direct eq-solving. But exhibits eq-renaming.
(the value of (the distance of _Move2))   ;;Demonstrates eq-chaining.
|#



#|;;Test case
(generate-equivalence-equations-for-duplicate-binding-list 
 '((|_Duration-ValueB| (|_A-t| |_B-t| |_C-t|))))
|#

(defun generate-equivalence-equations-for-duplicate-binding-list(duplicate-binding-list)
  (mappend 'generate-equivalence-equations-for-duplicate-binding
	   duplicate-binding-list))

(defun generate-equivalence-equations-for-duplicate-binding(duplicate-binding)
  (let ((var         (first duplicate-binding))
	(symbol-list (second duplicate-binding)))
    (cond ((< (length symbol-list) 2) nil)
	  ((= (length symbol-list) 2)
	   (list (list '= 
		       (first  symbol-list)
		       (second symbol-list))))
	  (t (cons
	      (list '= 
		    (first  symbol-list)
		    (second symbol-list))
	      (generate-equivalence-equations-for-duplicate-binding
	       (list var (cdr symbol-list))))))))

;;For identifying additional instances in different equations mentioned in scenario
;;Also perform km query to "bind" eq-symbols to KM instances.
(defun identify-novel-instances(lst)
  (let ((debug nil))
    (if (not (null lst))
	(let ((novel-instances (identify-novel-instances0 (car lst))))
	  (cond ((not (null novel-instances))
		 (progn
		   (if debug (format t "changing controller-triple-instances(~a + ~a)~%"
				     (length *CONTROLLER-TRIPLE-INSTANCES*)
				     (length novel-instances)))
		   (append-controller-triple-instances novel-instances)
		   (identify-novel-instances (append (cdr lst)
						     novel-instances))))
		(t (identify-novel-instances (cdr lst))))))
  (if debug (format t "*CONTROLLER-TRIPLE-INSTANCES*(~a):~a~%" 
		    (length *controller-triple-instances*)
		    *controller-triple-instances*))))

(defun append-controller-triple-instances(additional-instances)
  ;;additional instances must always be at end of list.
  ;;Ordering of list must be stable
  (dolist (x additional-instances)
    ;(if (not (member x *CONTROLLER-TRIPLE-INSTANCES*))
	(setq *CONTROLLER-TRIPLE-INSTANCES* 
	      (insert-at-end x *controller-triple-instances*)))
  ;)
  *controller-triple-instances*)

(defun identify-novel-instances0(instance)
  (let* ((novel-eq-use-instances (identify-novel-equation-use-instances instance))
	 (novel-property-of-instances (identify-novel-property-of-instances instance))
	 (novel-instances (union novel-eq-use-instances novel-property-of-instances)))
    novel-instances))

(defun identify-novel-property-of-instances(instance)
  (set-difference 
   (identify-property-of-frame instance)
    (get-triple-instances-for-eq-solver)))

(defun identify-novel-equation-use-instances(instance)
  (let ((debug nil))
  (set-difference 
   (ps-km-query `(|the| |equation-uses| |of| 
		  (|the| |equation| |of| 
		   (|the| |component-of| |of| ,instance))) debug)
    (get-triple-instances-for-eq-solver))))

;;routine for debugging equation-systems inside triple-lst
(defun test-equation-system(triple-lst)
  (let ((*logging* t)
	(*CONTROLLER-TRIPLE-LIST* triple-lst)
	(*CONTROLLER-TRIPLE-INSTANCES* 
	 (append
	  (extract-all-instances-from-triple-list-ordered-by-dependency
	   triple-lst)
	  (extract-all-km-constants triple-lst))))
    (delete-frames-in-triple-lst triple-lst)
    (ps-assert-triples triple-lst t)
    (collate-equation-system)
))

;;routine to make any last-minute changes in the ordering of eq expressions
;;(e.g., to put problematic sqrt expressions at the end so they're used only
;;if no other solution exists)
(defun reorder-eq-expressions (exprs)
  (put-atan-eq-expressions-at-end (put-sqrt-eq-expressions-at-end exprs))
)

(defun put-sqrt-eq-expressions-at-end (exprs)
  (let (
        (exprs-noroot '())
        (exprs-withroot '())
       )
    (dolist (expr exprs)
       (if (eq-expression-uses-sqrt expr)
           (setq exprs-withroot (append exprs-withroot (list expr)))
           (setq exprs-noroot   (append exprs-noroot   (list expr)))
       )
    )
    (append exprs-noroot exprs-withroot)
  )
)

(defun eq-expression-uses-sqrt (expr)
  (or (member 'SQRT (flatten expr))
      (member '|sqrt| (flatten expr))
  )
)

(defun put-atan-eq-expressions-at-end (exprs)
  (let (
        (exprs-noatan '())
        (exprs-withatan '())
       )
    (dolist (expr exprs)
       (if (eq-expression-uses-atan expr)
           (setq exprs-withatan (append exprs-withatan (list expr)))
           (setq exprs-noatan   (append exprs-noatan   (list expr)))
       )
    )
    (append exprs-noatan exprs-withatan)
  )
)

(defun eq-expression-uses-atan (expr)
  (or (member 'ATAN (flatten expr))
      (member '|atan| (flatten expr))
  )
)

(defun identify-property-of-frame(instance)
  (let* ((debug nil)
	 (slot-lst (ps-km-query `(|the| |property-of-slot| |of| ,instance) debug)))
    (mappend #'(lambda(slot)
		 (ps-km-query `(|the| ,slot |of| ,instance) debug))
	     slot-lst)))

#|

Additional debugging examples to highlight eq-symbol re-writing.

(ps-parse-question
"A person drives a car.
The speed of the driving is 60 km/h.
The duration of the driving is 30 minutes.
A person walks.
The speed of the walking is 4 km/h.
The duration of the walking is 30 minutes.
What is the sum of the distance of the driving and the distance of the walking?")


(defun testcase()
(basic-problem-solver
'((|_Person1120| |instance-of| |Person|)
  (|_Car1121| |instance-of| |Car|)
  (|_Drive10447| |agent| |_Person1120|)
  (|_Drive10447| |object| |_Car1121|)
  (|_Speed1123| |instance-of| |Speed-Value|)
  (|_Speed1123| |speed-of| |_Drive10447|)
  (|_Speed1123| |value| (:|pair| 60 |*kilometer-per-hour|))
  (|_Duration1126| |instance-of| |Duration-Value|)
  (|_Duration1126| |duration-of| |_Drive10447|)
  (|_Duration1126| |value| (:|pair| 30 |*minute|))
  (|_Person1129| |instance-of| |Person|)
  (|_Walk10455| |agent| |_Person1129|)
  (|_Speed1131| |instance-of| |Speed-Value|)
  (|_Speed1131| |speed-of| |_Walk10455|)
  (|_Speed1131| |value| (:|pair| 4 |*kilometer-per-hour|))
  (|_Duration1136| |instance-of| |Duration-Value|)
  (|_Duration1136| |duration-of| |_Walk10455|)
  (|_Duration1136| |value| (:|pair| 30 |*minute|))
  (|_Distance1140| |instance-of| |Length-Value|)
  (|_Distance1142| |instance-of| |Length-Value|)
  (|_Sum1145| |instance-of| |Sum|)
  (|_Distance1140| |distance-of| |_Drive10447|)
  (|_Distance1142| |distance-of| |_Walk10455|)
  (|_Sum1145| |input| |_Distance1140|)
  (|_Sum1145| |input| |_Distance1142|)
  (|_What1144| |instance-of| |Thing|)
  (|_Drive10447| |instance-of| |Move|)
  (|_Walk10455| |instance-of| |Walk|)
  (|_Drive10447| |instance-of| |Motion-with-constant-acceleartion|)
  (|_Walk10455| |instance-of| |Motion-with-constant-acceleartion|))
'(|_What1144|)
nil))

(trace collate-equation-system)

(let ((*logging* t)
      (*CONTROLLER-TRIPLE-INSTANCES* 
       (list (ps-instantiate-concept '|Velocity-Vector-Value|)
	     (ps-instantiate-concept '|Velocity-Vector-Value|))))
  (reset-eq-solver)
  (collate-equation-system)))


(let ((*logging* t)
      (question "There is a move.
                 The acceleration magnitude of the move is 2 m/s^2.
                 There is a second move.
                 The acceleration magnitude of the second move is 5 m/s^2."))
(multiple-value-bind
    (scenario compute-question yn-questions cpl-nl-triples)
    (ps-parse-question question)
  (let ((*CONTROLLER-TRIPLE-INSTANCES* 
	 (extract-all-km-instances scenario)))
  (reset-eq-solver)
  (collate-equation-system))))

Here's a new testcase for vectors:
(defun testcase ()
  (let* ((scenario
           '((|_Displacement129_c28| |distance| |_X130_c28|)
             (|_X130_c28| |value|
             (:|pair| 10 |*meter|))
             (|_Displacement129_c28| |angle| |_Angle132_c28|)
             (|_Angle132_c28| |value|
             (:|pair| 45 |*degree|))
             (|_Duration135_c28| |value|
             (:|pair| 5 |*second|))
             (|_Move127_c28| |duration| |_Duration135_c28|)
             (|_Move127_c28| |displacement| |_Displacement129_c28|)
             (|_X130_c28| |instance-of| |Length-Value|)
             (|_Angle132_c28| |instance-of| |Angle-Value|)
             (|_Duration135_c28| |instance-of| |Duration-Value|)
             (|_Move127_c28| |instance-of| |Vector-Move|)
             (|_Displacement129_c28| |instance-of| |Displacement-Vector-Value|)))
         (*controller-km-instances* (extract-all-km-instances scenario))
         (*logging* t)
        )
     (delete-frames-in-triple-lst scenario)
     (PS-ASSERT-TRIPLES scenario t)
     (collate-equation-system)))

|#

#|
;;deprecated code

;;deprecated? (ps-assert-triples ...) now eagerly eval slots in scenario to install unifications
;;Eagerly evaluate slots associated with scenario and km-instances. 
;;This is required for eq-solver to correctly associate variable bindings.
(defun touch-all-properties-for-instances-in-scenario(&optional(additional-instances nil))
  (let ((debug nil)
	(*EQ-FLAG* nil)
	(touch-list ;;additional instances must always be at end of list.
	 (append
	  (get-triple-instances-for-eq-solver)
	  additional-instances)))
    (if debug (format t "Touch list: ~a~%" touch-list))
    (dolist (km-instance touch-list)
      (progn
	(if (not (member km-instance *EQ-SOLVER-TOUCHED-INSTANCES*))
	    (eq-solver-query-immediate-slots km-instance))))
    (setq *controller-triple-instances* touch-list)
    (setq 
     *EQ-SOLVER-TOUCHED-INSTANCES* 
     (remove-if 'km-constantp 
		(remove-duplicates 
		 (append touch-list 
			 *EQ-SOLVER-TOUCHED-INSTANCES*))))
    touch-list))
|#

