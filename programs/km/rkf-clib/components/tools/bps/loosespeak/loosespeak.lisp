
;;; File: loosespeak.lisp
;;; Author: James Fan

(in-package :km)

(defparameter *use-loosespeak* nil)
(defparameter *ls-generated-triples* nil)
(defparameter *ls-remark* nil)
(defparameter *ls-remark-nn* nil)
(defparameter *ls-remark-coref* nil)

#|
TOP LEVEL API
(loosespeak sentence triples)
RETURNS: A list of pairs of the form:
    ((orig-expr1 . choice11) (orig-expr1 . choice12) ... (orig-exprN . choiceN1) (orig-exprN . choiceN2))

(apply-loosespeak-choices sentence triples loosespeak-choices)
INPUT: The loosespeak choices. If not interactive, the system should just pick the first.
	    ((orig-expr1 . choice1) ... (orig-exprN . choiceN) ...)
RETURNS: updated triples

The code currently assume orig-expr1 and choice1 are strings (generated princ-to-string).
|#

;; given a sentence and triples, return the interpreted triples
;; call ls-generate-path-triples and return a set of interpreted triples
(defun apply-loosespeak-choices (sentence triples loosespeak-choices)
  (declare (ignore sentence))
  (km-format t "User's loosespeak choices were: ~a~%" loosespeak-choices)
  (let ((old-triple nil)
	(interpretation nil)
	(equalities (find-equalities triples))
	(inst-of-triples nil)
	(replaced-insts nil))
    (dolist (choice loosespeak-choices)
;	    (setf old-triple (km2triples (read-from-string (car choice))))
;	    (setf interpretation (km2triples (read-from-string (cdr choice))))
      (when (not (equal (car choice)
			(second choice)))
	(setf old-triple (km2triples (car choice)))
					;      (setf interpretation (km2triples (cdr choice)))
	(setf interpretation (km2triples (second choice)))		; seem to need this PEC
	(push (list old-triple interpretation) *ls-generated-triples*)
	(setf replaced-insts (mapcar #'car equalities))
	(setf inst-of-triples
	      (remove-if-not #'(lambda (triple) (and (eql (second triple) '|instance-of|)
						     (member (first triple) replaced-insts :test #'eql)))
			     triples))
	(setf triples (apply-equalities triples equalities))
	(setf triples (append triples (mapcar #'(lambda (equality) `(,(car equality) |equal| ,(cdr equality))) equalities)))
	(setf triples (append triples inst-of-triples))
	(setf triples (append (set-difference triples old-triple :test #'triple-equal) interpretation))
	))
    ;; then, restore KB
    ;;(put-kb *loosespeak-kb-state*)
    triples))

;;; ======================================================================
(defun set-append-element (l e)
  (cond
   ((and
     (consp e)
     (equal (first l) ':|set|)
     (equal (first e) ':|set|))
    (append l (rest e))
    )
   (t
    (append-element l e))
   ))

;; set the direction of the triples based on the head/tail's popularity
(defun redirect-triples (triples)
  (let ((inst-triples (remove-if-not #'(lambda (triple) (equal '|instance-of| (second triple))) triples))
	(useful-triples (remove-if #'(lambda (triple) (equal '|instance-of| (second triple))) 
				   (remove-if-not #'ls-real-triplep triples)))
	(counter (make-hash-table :test #'equal))
	(triple-head nil)
	(slot-class nil)
	(retval nil))
    ;; first reset the kb
    (start-logging)
    (set-checkpoint)
    ;; second assert the instances
    (dolist (triple inst-triples)
	    (if (consp (third triple))
		(km `(,(first triple) |has| (|instance-of| ,(third triple))))
	      (km `(,(first triple) |has| (|instance-of| (,(third triple)))))))
    ;; third count the assertions
    (dolist (triple useful-triples)
	    ;;(format t "triple is ~S~%" triple)
	    (setf slot-class (extract-slot-group (second triple)))
	    (when (triple-exists triple)
	      (setf triple-head (first triple))
	      (incf (gethash `(,triple-head ,slot-class) counter 0)))
	    (when (triple-exists (inverse-triple triple))
	      (setf triple-head (third triple))
	      (incf (gethash `(,triple-head ,slot-class) counter 0)))
	    )
    ;; fourth set the direction of the triples
    (dolist (triple (remove-duplicates useful-triples :test #'triple=))
	    (setf slot-class (extract-slot-group (second triple)))
	    #|(format t "Triple: ~S hash(~S ~S) = ~A hash(~S ~S) = ~A ~%"
		    triple 
		    (third triple) slot-class (gethash `(,(third triple) ,slot-class) counter 0)
		    (first triple) slot-class (gethash `(,(first triple) ,slot-class) counter 0))|#
	    (if (> (gethash `(,(third triple) ,slot-class) counter 0)
		   (gethash `(,(first triple) ,slot-class) counter 0))
		;; need to redirect the triple
		(setf retval (append-element retval (inverse-triple triple)))
	      ;; no need to redirect the triple
	      (setf retval (append-element retval triple))))
    ;; fifth revert kb
    (undo)
    (stop-logging)
    retval
    ))

(defun extract-slot-group (slot)
  (let ((slot-classes (km `(|the| |instance-of| |of| ,slot))))
    (cond ((equal slot-classes '(|Slot|))
	   '|Slot|)
	  (t
	   (first (remove '|Slot| slot-classes :test #'equal))))
    ))

;; a triple exists if the value of the head is a super/subclass of the tail
(defun triple-exists (triple)
  (let ((filler (km `(|the| ,(second triple) |of| ,(first triple)))))
    (if filler
	(is_super_or_subclass0 (list (third triple)) filler))
    ))

(defun km-assertions-from-triples (triples)
  (let ((inst-triples (remove-if-not #'(lambda (triple) (equal '|instance-of| (second triple))) triples)))
    ;; first assert the instance-of
    
    ;; then call triples2km
    (km (triples2km (redirect-triples triples)))
    (dolist (triple inst-triples)
	    (if (consp (third triple))
		(km `(,(first triple) |has| (|instance-of| ,(third triple))))
	      (km `(,(first triple) |has| (|instance-of| (,(third triple)))))))
    ))

(defun triples2km (triples)
  ;; first filter all single scalar constants in triples
  (setf triples
	(mapcar #'(lambda (triple)
		    (let ((x (first triple))
			  (y (second triple))
			  (z (third triple)))
		      (if (scalar-const? x)
			  (setf x `(:|pair| ,x |Thing|)))
		      (if (scalar-const? z)
			  (setf z `(:|pair| ,z |Thing|)))
		      (list x y z)))
		triples))
  ;; now run unification on all "equal" relation triples
  (setf triples (apply-equalities triples (find-equalities triples)))
  (setf triples
	(remove nil
		(mapcar #'(lambda (triple)
			    (cond
			     #|((eql '|equal| (second triple))
			      (km `(,(first triple) & ,(third triple)))
			      nil)|#
			     ((eql '|instance-of| (second triple))
			      (km `(,(first triple) |has| (|instance-of| (,(third triple)))))
			      nil)
			     (t
			      triple)))
			triples)))
  (cond
   ;; if is a query
   #|((ls-find-all-triples-by-pattern '(?x |query-varp| |t|) triples)
    (ls-extract-question triples))|#
   ;; it is an assertion
   (t
    (triples2km-assertion nil triples)))
  )

;; input: a root instance and a set of triples
;; output: a KM expression and the triples not covered by the root instance
;; example: (nil ((_A r1 _B) (_B r2 _C))) => (_A has (r1 ((_B has (r2 (_C)))))) nil
;; example: (_A ((_A r1 _B) (_C r2 _D))) => (_A has (r1 (_B))) (_C r2 _D)
(defun triples2km-assertion (root triples)
  (let ((nodes nil)
	(km-expr nil)
	(retval nil)
	(dependent-triples nil)
	(dependent-nn nil)
	)
    (cond 
     ;; no root node is specified, then
     ;; find all the top level nodes
     ((null root) 
      (setf nodes (remove-duplicates (extract-root-nodes triples)))
      (setf retval '(:|set|))
      (dolist (node nodes)
	      (multiple-value-setq (km-expr triples)(triples2km-assertion node triples))
	      (setf retval (set-append-element retval km-expr))
	      )
      (if (= (length retval) 2)
	  (setf retval (second retval)))
      )
     ;; if root not used as a head in triples
     ((not (find root (mapcar #'first triples) :test #'equal))
      (setf retval root))
     ;; else
     (t
      (setf dependent-triples (remove-if-not #'(lambda (triple) (and (equal root (first triple))
								     (not (equal '|related-to| (second triple))))) triples))
      (setf dependent-nn (remove-if-not #'(lambda (triple) (and (equal root (first triple))
								(equal '|related-to| (second triple)))) triples))
      (setf triples (set-difference triples (append dependent-triples dependent-nn) :test #'equal))
      (when dependent-triples
	(setf retval `(,root |has| )))
      (dolist (triple dependent-triples)
	      (multiple-value-setq (km-expr triples)
				   (triples2km-assertion (third triple) 
					       triples))
	      ;;(format t "triples = ~S~%" triple)
	      (if (and (equal (second triple) '|instance-of|)
		       (consp (third triple)))
		  (setf retval (append-element retval `(,(second triple) ,km-expr)))
		(setf retval (append-element retval `(,(second triple) (,km-expr)))))
	      )
      (when dependent-nn
	(when (or (> (length dependent-nn) 1)
		  dependent-triples)
	  (setf retval (list ':|set| retval)))
	(dolist (nn dependent-nn)
		(multiple-value-setq (km-expr triples)
				     (triples2km-assertion (third nn)
						 triples))
		(if (atom km-expr)
		    (setf retval (append-element retval `(:|nn| ,root ,(third nn))))
		  (setf retval (append-element
				(append-element retval `(:|nn| ,root ,(third nn)))
				km-expr))))
	(when (= (length retval) 1)
	  (setf retval (first retval)))
	)))
    (values retval triples)
    ))

(defun triple= (triple1 triple2)
  (if (or (equal triple1 triple2)
	  (equal triple1 (inverse-triple triple2)))
      t)
  )

(defun inverse-triple (triple)
  (let ((inverse-slot (first (km `(|the| |inverse| |of| ,(second triple))))))
    (list (third triple) inverse-slot (first triple))
    ))

(defun ls-temp-sym (sym)
  (let ((sym-str (format nil "~a" sym)))
    (excl::match-regexp "_\\(\\w+\\)_ls_\\([0-9]+\\)" sym-str)
    ))

(defun ls-triple-contain-one-temp-sym (triple)
  (let ((first-temp? (ls-temp-sym (first triple)))
	(third-temp? (ls-temp-sym (third triple))))
    (or (and first-temp? (not third-temp?))
	(and (not first-temp?) third-temp?))
    ))

;; assume triple contains 1 temp sym
;; find out if the temp sym can be replaced
(defun ls-triple-temp-sym-replaceable (triple triples)
  (let* ((temp-sym (find-if #'ls-temp-sym triple))
	 (slot (second triple))
	 (perm-sym (find-if #'(lambda (sym) (and (not (eql sym slot)) (not (eql sym temp-sym)))) triple))
	 (slot-fillers (if (eql (first triple) perm-sym)
			   (ls-km `(|the| ,slot |of| ,perm-sym))
			 (ls-km `(|the| ,(get-inverse-slot slot) |of| ,perm-sym))))
	 (temp-sym-classes (remove 
			    nil 
			    (mapcar #'(lambda (tr) 
					(cond 
					 ((and
					   (eql (second tr) '|instance-of|)
					   (eql (first tr) temp-sym))
					  (third tr))
					 ((and
					   (eql (second tr) '|instance|)
					   (eql (third tr) temp-sym))
					  (first tr)))) triples))))
    (find-if #'(lambda (filler)
		 (let ((filler-classes (all-classes filler)))
		   (and (not (ls-temp-sym filler))
			(subsetp temp-sym-classes filler-classes))))
	     slot-fillers)
    ))

(defun ls-replace-temp-sym (triple replacement triples)
  (let* ((temp-sym (find-if #'ls-temp-sym triple))
	 (retval (remove-if #'(lambda (tr)
				(or
				 (and (eql (second tr) '|instance-of|)
				      (eql (first tr) temp-sym))
				 (and (eql (second tr) '|instance|)
				      (eql (third tr) temp-sym))))
			    triples)))
    (deep-substitute
     replacement temp-sym retval :test #'eql)
    ))

(defun km2triples-postprocessing (triples)
  (let ((current-triple nil)
	(replacement nil)
	(updates? nil)
	(done? nil)
	(i 0))
    (when triples
      (do nil
	  (done? triples)
	  (when (= i 0) (setf updates? nil))
	  (setf current-triple (nth i triples))
	  (when (and (ls-triple-contain-one-temp-sym current-triple)
		     (not (eql '|instance-of| (second current-triple)))
		     (not (eql '|instance| (second current-triple))))
	    (when (setf replacement (ls-triple-temp-sym-replaceable current-triple triples))
	      (setf updates? t)
	      (setf triples (ls-replace-temp-sym current-triple replacement triples)))
	    )
	  (setf i (mod (+ 1 i) (length triples)))
	  (when (and (= i 0) (null updates?)) (setf done? t))
	  ))
    ))

;; input: a km expression
;; output: a set/list of triples and a list of roots
;; example: input: (|the| |subevent| |of| (|_Elongation2826| |has| (|instance-of| (|Elongation|))))
;; output: 
(defun km2triples (km-expr)
  (km2triples-postprocessing (km2triples0 km-expr)))
    #|(cond 
     ((km-queryp km-expr)
       (setf retval triples)
       (dolist 
	(root roots)
	(setf what-var (gen-instance-and-number '|What|))
	(setf retval
	      (append (list `(,what-var |query-varp| |t|)
			    `(,what-var |equal| ,root))
		      retval))
	)
       retval)
			    (t
			     triples))|#

(defun km2triples0 (km-expr)
  (cond
   ((km-queryp km-expr)
    ;; if it is query in the form of (the slot of ...), then 
    (km2triples-query km-expr)
    )
   (t
    ;; else
    (km2triples-assertion km-expr)
    ))
  )

;; given a km query in the form of (the slot of ...), return
;; ((_x slot ...) (_what equal _x) (_what query-varp t) )
;; what happens if a long query path is used? (the has-part of (the has-part of (_Thing3 has (instance-of (Thing)))))
;; is it equivalent of (the has-part of (_Thing3 has (instance-of (Thing)) (has-part ((a Part)))))??
;; input: a query km expression
;; output: a list of triples and a list of roots
(defun km2triples-query (km-expr)
  (let ((slot nil)
	(range nil)
	(minimatches (minimatch km-expr '(|the| ?slot |of| ?rest)))
	(rest-triples nil)
	(query-vars nil)
	(query-var nil)
	(roots nil)
	)
    (setf slot (first minimatches))
    (setf range (first (ls-km `(|the| |range| |of| ,slot))))
    (cond 
     ((atom (second minimatches))
      (setf query-var (gen-instance-and-number range))
      (values (list (list (second minimatches) slot query-var)
		    (list query-var '|instance-of| range))
	      (list query-var))
      )
     ((consp (second minimatches))
      (multiple-value-setq (rest-triples roots) (km2triples0 (second minimatches)))
      (dolist (root roots)
	      (setf query-var (gen-instance-and-number range))
	      (setf query-vars 
		    (append-element query-vars query-var))
	      (setf rest-triples
		    (cons
		     (list root slot query-var)
		     (cons
		      (list query-var '|instance-of| range)
		      rest-triples)))
	      )
      (values rest-triples query-vars)
      )
     )
    ))

(defun gen-instance-and-number (sym)
  (my-gentemp (concat (concat "_" (string sym)) "_ls_"))
  )

;; input: a km assertion (x has (slot ((y has ...))))
;; output: a set of triples ((x slot y) ...) and a list of roots
;; example: (x has (slot (y))) => ((x slot y))
;; example: (x has (slot1 ((y has (slot2 (z)))))) => ((x slot1 y) (y slot2 z))
(defun km2triples-assertion (km-expr)
  (let ((retval nil)
	(head nil)
	(slot nil)
	(filler nil)
	(new-filler nil)
	(slot-filler-pairs nil)
	(triples nil)
	(roots nil)
	(retval-roots nil))
    (cond ((atom km-expr)
	   nil)
	  ((minimatch km-expr '(:|nn| ?x ?y))
	   (values `((,(second km-expr) |related-to| ,(third km-expr)))
		   (list (second km-expr))))
	  ((equal (first km-expr) ':|set|)
	   (dolist (element (rest km-expr))
		   (multiple-value-setq (triples roots) (km2triples0 element))
		   (setf retval
			 (append retval triples))
		   (setf retval-roots
			 (append retval-roots roots))
		   )
	   (values retval retval-roots)
	   )
	  (t
	   (cond ((minimatch km-expr '(?x |has| &rest))
		  (setf head (first km-expr))
		  (setf slot-filler-pairs (cddr km-expr)))
		 ((minimatch km-expr '(|a| ?x |with| &rest))
		  (setf head (gen-instance-and-number (second km-expr)))
		  (setf retval (append-element retval `(,head |instance-of| ,(second km-expr))))
		  (setf slot-filler-pairs (cdddr km-expr))))
	   (dolist (slot-filler slot-filler-pairs)
		   (setf slot (first slot-filler))
		   (setf fillers (second slot-filler))
		   (dolist (filler fillers)
		     (cond
		      ((atom filler)
		       (setf retval (append retval (list `(,head ,slot ,filler)))))
		      ((minimatch filler '(?x |has| &rest))
		       (setf retval (append retval (list `(,head ,slot ,(first filler)))
					    (km2triples0 filler))))
		      ((minimatch filler '(|a| ?x |with| &rest))
		       (setf new-filler (gen-instance-and-number (second filler)))
		       (setf retval (append retval (list `(,head ,slot ,new-filler))
					    (list `(,new-filler |instance-of| ,(second filler)))
					    (km2triples0 (append (list new-filler '|has|) (cdddr filler))))))
		      ((minimatch filler '(:|set| &rest))
		       (setf retval (append retval (list `(,head ,slot ,filler))
					    (km2triples0 filler)))
		       )
		      ((minimatch filler '(:|nn| ?x ?y))
		       (setf retval (append retval (list `(,head ,slot ,(third filler)))
					    (km2triples0 filler))))
		      (t ;; others like pair or anything else
		       (setf retval (append retval (list `(,head ,slot ,filler)))))
		      ))
		   )
	   (values retval
		   (list head))
	   ))
    ))
#|
to do:
1. how to filter noise slots? anything other than relation property + a list of reserved ones? -- ls-separate-triples function
3. what's the best way to interpret triples using existing lsi? -- run-ls-triple function
6. how to deal with specialization cases?
8. how can domain/range constraint violation be solved?
9. prompt for answer for multiple prompt? prompt-for-clarification chooses orig expr, and store replacements in a global var in the form of ((orig-expr . choice1) (orig-expr . choice2) ...) if auto selection, then just store choice one
|#
#|
INPUT: a sentence and a list of triples
OUTPUT: either a list of options for user to choose from 
        or a list of choices chosen by the system
|#
(defun loosespeak (sentence triples)
  (declare (ignore sentence))
  (progn
    (format t "*use-loosespeak*(~a)~%" *use-loosespeak*)
  (cond 
   (*use-loosespeak*
    (let ((kb-triples nil))
      (setf *ls-cpl-mode* t)
      (setf *ls-cpl-choices* nil)
      (setf *ls-auto-selection* nil)
      (ls-parse-triples
       (second 
	(multiple-value-list (ls-separate-triples triples))))))
   (t triples))))

;;; ----------------------------------------------------
;;; split the triples into useful triples and noisy ones
(defun ls-separate-triples (triples)
  (let ((noisy nil)
	(real nil))
    (setf noisy (remove-if-not #'ls-noisy-triplep triples))
    (setf real (remove-if #'ls-noisy-triplep triples))
    (values noisy real)
    ))
;; noisy slots are anything other than relation/properties/instance-of
(defun ls-noisy-triplep (triple)
  (not (ls-real-triplep triple)))
(defun ls-real-triplep (triple)
  (let ((slot (second triple)))
    (ls-real-slot slot)
    ))
(defun ls-real-slot (slot)
  (or (km `(,slot |isa| |Relation|))
      (km `(,slot |isa| |Property|))
      (equal slot '|related-to|)
;     (equal slot '|unknown-relation-of|)
      (equal slot '|instance-of|)
      (equal slot '|instance|)
      (equal slot '|value|)
      (equal slot '|value-of|)
      (equal slot '|equal|)
      (equal slot '|query-varp|)
      (equal slot '|context|)
      (equal slot '|context-of|))
  )

(defun ls-parse-triples (triples)
  (let ((km-expr (triples2km triples))
	;;(kb-state (get-kb))
	;;(instances-of-triples (remove-if-not #'(lambda (triple) (equal (second triple) '|instance-of|)) triples))
	)
    ;;(setf *loosespeak-kb-state* (get-kb))
    (setf *ls-cpl-mode* t)
    (setf *ls-cpl-choices* nil)
    ;;(dolist (inst instances-of-triples)
    ;;(km `(,(first inst) |has| (|instance-of| (,(third inst))))))
    (parse-for-ls km-expr)
    ;;(put-kb kb-state)
    (cond (*ls-cpl-choices*
	     (remove-duplicates *ls-cpl-choices* :test #'equal))
	  (t ;; no loosespeak found
	   (list (list km-expr km-expr))
	   ))
    ))

;;; given a set of triples, run ls interpreter on them one by one
(defun ls-parse-triples-defunct (triples)
  (declare (special *loosespeak-kb-state*))
  (let ((retval triples) instances-of-triples)
    ;; first, save kb
    (setf *loosespeak-kb-state* (get-kb))
    ;; then assert the instance-of triples
    (setf instances-of-triples (remove-if-not #'(lambda (triple) (equal (second triple) '|instance-of|)) triples))
    (dolist (inst instances-of-triples)
	    (km `(,(first inst) |has| (|instance-of| (,(third inst))))))
    ;; next, call ls-parse-triple one by one
    (setf retval (mapcar #'ls-parse-triple triples))
    ;; remove the nil results
    (setf retval (remove-if #'(lambda (result)
				(or (null result)
				    (null (second result))))
			    retval))
    retval
    ))

;;; return a set of options for user to choose from
;;; or return a single choice chosen by the system
(defun ls-parse-triple (triple)
  (let ((head (first triple))
	(slot (second triple))
	(tail (third triple))
	)
    (when (and (not (is-an-class head))
	       (not (is-an-class tail)))
      (ls-pick-result (ls-search head slot tail) triple)
      )))

(defun ls-pick-result (results triple)
  (cond 
   ((not *ls-auto-selection*)
    (mapcar #'(lambda (result)
		(list triple (third result)))
	    results))
   (t
    (list triple (third (first results))))))

;; given a result in the form of (_X1 slot _Y2 cost slot _Z3 cost)
;; return ((X slot Y) (Y slot Z))
;; ignore the specialization cases for the time being
(defun ls-generate-path-triples (result head tail)
  (let ((retval nil)
	;;(intermediate-instances nil)
	instances inst-type
	(sym-tab nil)
	new-head new-triple
	)
    ;; first, collect a list of instances in the result
    (setf instances (remove-if #'numberp (remove-if #'ls-slotp result)))
    ;; exclude the first and last element because they correspond to head and tail insts
    (setf sym-tab
	  (append sym-tab
		  (list
		   (list (first instances) head (km `(|the| |classes| |of| ,(first instances))))
		   (list (last-element instances) tail (km `(|the| |classes| |of| ,(last-element instances)))))))
    (setf instances (butlast (cdr instances)))
    
    ;; second, build a look up table of instances and their temp symbols and types
    (dolist (inst instances)
	    (setf inst-type (km `(|the| |classes| |of| ,inst)))
	    (setf sym-tab
		  (append-element sym-tab
				  (list inst 
					(gen-instance-and-number (first inst-type))
					inst-type))))
    ;; third, replace the instances by temp symbols in the search result path
    (dolist (sym sym-tab)
	    (setf result
		  (substitute (second sym) (first sym) result :test #'equal)))
    ;; fourth, generate triples to declare the types of the temp symbols
    (dolist (sym sym-tab)
	    (setf retval 
		  (append-element retval
				  (list (second sym) '|instance-of| (third sym)))))
    ;; fifth, convert result path into sets of triples
    (setf new-head head)
    (dotimes (i (/ (length (cdr result)) 3))
	     (setf new-triple (list new-head (nth (* 3 i) (cdr result)) (nth (+ 1 (* 3 i)) (cdr result))))
	     (setf retval
		   (append-element
		    retval new-triple))
	     (setf new-head (nth (+ 1 (* 2 i)) (cdr result)))
	     )
    retval
    ))

;; given a set of triples
;; return the heads of the triples that are not the tail of any triples
(defun extract-root-nodes (triples)
  (let ((tails (mapcar #'third triples))
	(heads (mapcar #'first triples)))
    (remove-duplicates (set-difference heads tails :test #'equal))
    ))

(defun triple-equal (t1 t2)
  (and (triple-element-equal (first t1) (first t2))
       (triple-element-equal (second t1) (second t2))
       (triple-element-equal (third t1) (third t2))
       )
  )
(defun triple-element-equal (e1 e2)
  (or (equal e1 e2)
      (or (and (scalar-const? e1)
	       (equal e2 `(:|pair| ,e1 |Thing|)))
	  (and (scalar-const? e2)
	       (equal e1 `(:|pair| ,e2 |Thing|))))
      )
  )
#| 
  question-extraction.lisp -- given a CPL encoding of a question, translate it to (the slot of X) form of KM 
|#

(defun ls-find-all-triples-by-pattern (match-pattern triples)
  (remove-if-not #'(lambda (triple) (minimatch triple match-pattern)) triples)
  )

#| sample input:     
    (_Duration2363 instance-of Duration-Value) 
    (_Fall2361 instance-of Fall) 
    (_What2362 equal _Duration2363) 
    (_Duration2363 duration-of _Fall2361) 
    (_What2362 input-word (:pair "what" n)) 
    (_What2362 det det-what) 
    (_Fall2361 input-word (:pair "fall" n)) 
    (_Fall2361 det det-the) 
    (_Duration2363 input-word (:pair "duration" n)) 
    (_Duration2363 det det-the) 
    (_What2362 query-varp t) |#
#| sample output:
    ((the value of (the duration of _Fall2361)))
|#
(defun ls-extract-question (triples)
  (let ((query-vars (mapcar #'first (ls-find-all-triples-by-pattern  '(?x |query-varp| |t|) triples)))
	(retval nil)
	)
    (dolist (query-var query-vars)
	    (setf retval
		  (append retval
			  (ls-extract-1-question-var query-var triples)))
	    )
    ;; assume there's only 1 question in the triples
    (first retval)
    ))

(defun ls-extract-1-question-var (query-var triples)
  (let* ((queried-value (third (first (ls-find-all-triples-by-pattern `(,query-var |equal| ?x) triples)))) ;; assume it only equals to one thing
	 ;(queried-value query-var)
	 (interesting-triples (remove-if-not #'(lambda (triple)
						 (and (ls-real-triplep triple)
						      (not (find (second triple) '(|equal| |instance| |instance-of| |query-varp|) :test #'equal))))
					     triples)) ;; now filter all noisy slots
	 (query-triples nil)
	 )
    ;; convert rest of them to query paths
    (setf query-triples 
	  (append
	   (ls-find-all-triples-by-pattern `(?x ?y ,queried-value) interesting-triples)
	   (mapcar #'inverse-triple (ls-find-all-triples-by-pattern `(,queried-value ?x ?y) interesting-triples))))
    (mapcar #'(lambda (triple)
		(ls-triple2query triple (set-difference triples query-triples :test #'equal)))
	    query-triples)
    ))
(defun ls-triple2query (triple triples)
  `(|the| ,(second triple) |of| ,(triples2km-assertion (first triple) triples)))

;;; end of question-extraction.lisp

(defun ls-slot-for-noun-word (word) 
  (let ((slots (remove nil 
		       (mapcar #'(lambda (syn-slot)
				   (if (string= (first syn-slot) word)
				       (cdr syn-slot)))
			       *noun-slots*))))
    (setf slots
	  (sort slots
		#'(lambda (slot1 slot2)
		    (< (levenshtein-string-distance word (string slot1))
		       (levenshtein-string-distance word (string slot2))))))
    (invert-slot (first slots))
    ))

;; INPUT: triple
;; OUTPUT: NIL if triple is not generated by LS, the original CPL triple otherwise
(defun ls-generated-triple? (triple)
  (first (find-if #'(lambda (generated-triple)
	       (and (member triple (second generated-triple) :test #'equal)
		    (not (member triple (first generated-triple) :test #'equal))))
	   *ls-generated-triples*))
  )

;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; Copyright © David B. Lamkins 1999
;;;
;;; Permission is granted for use, provided that author's name and copyright
;;; are retained in this file. The author makes no warranties with respect to
;;; this work.
;;;
;;; File: levenshtein.lisp
;;; Author: David B. Lamkins <dlamk...@teleport.com>
;;; Organization: NIL
;;; Created: 1999/01/29 00:31:20 (UTC-8)
;;; Implementation: Macintosh Common Lisp Version 4.2
;;; Target: same
;;; Purpose: Calculate Levenshtein distance between two strings.
;;; Keywords: string similarity
;;; Requires:
;;; Resources:
;;; Dependencies:
;;; References: See Description.
;;; Standards:
;;; Unit-Test: See examples at end of file.
;;; Limitations:
;;; To-Do:
;;; Bugs: Code is ugly and conses too much.
;;;
;;; Description:
;;;
;;; Compute the Levenshtein distance between a pair of strings.
;;;
;;; I no longer have the article in which I found this algorithm,
;;; but it appears to find the minimal-cost sequence of edits that
;;; can be applied to both strings to make them identical.  In
;;; other words, this algorithm does not assume that one string
;;; is "correct". Edits can be any of match (no change), insert,
;;; delete, or substitute. Each kind of edit has an associated
;;; cost.
;;;
;;; Despite the fact that neither string is presumed "authoritative",
;;; the Levenshtein distance metric is surprisingly useful for
;;; qualifying fuzzy matches from a dictionary; see the DWIM lookup
;;; examples at the end of the file.
;;;
;;; This code was translated from a THINK Pascal adaptation of a
;;; C algorithm I found in the article "Finding String Distances",
;;; Ray Valdés, Dr. Dobb¹s Journal, April 1992, ppg. 56\u2039 62, 107.
;;;
;;; This was a "reasonably" straightforward translation from the
;;; THINK Pascal source, which was itself somewhat obfuscated by
;;; a lot of idioms for managing dynamic storage on the Mac heap.
;;; I could probably make the code more readable if I built a Lisp
;;; version from scratch, but that would mean that I'd have to
;;; dig up the original references, and I don't have the inclination
;;; to do that just now.
;;;
;;; Algorithm due to V.I. Levenshtein, as presented in "Time Warps,
;;; String Edits, and MacroMolecules: The Theory and Practice of
;;; Sequence Comparison", Sankoff and Kruskal, eds., Addison­Wesley,
;;; 1983.

(defstruct (lev-op
            (:print-function lev-op-printer))
  index1
  index2
  op)

(defun lev-op-printer (object stream depth)
  (declare (ignore depth))
  (print-unreadable-object (object stream :type t)
    (format stream "~D ~D ~A"
            (lev-op-index1 object)
            (lev-op-index2 object)
            (lev-op-op object))))

(defstruct matrix-cell
  distance
  op)

(defstruct move
  row-dist
  col-dist)

(defun levenshtein-string-distance (string-1 string-2
					     &key
					     (match-cost 0)
					     (insert-cost 1)
					     (delete-cost 1)
					     (substitute-cost 2))
  (declare (optimize (speed 3)))
  (let* ((moves (list
		 (cons 'match (make-move :row-dist -1
					 :col-dist -1))
		 (cons 'insert (make-move :row-dist 0
					  :col-dist -1))
		 (cons 'delete (make-move :row-dist -1
					  :col-dist 0))
		 (cons 'substitute (make-move :row-dist -1
					      :col-dist -1))))
	 (num-rows (1+ (length string-1)))
	 (num-cols (1+ (length string-2)))
	 (matrix (make-array (list num-rows num-cols))))
    ;; initialize matrix
    (do ((i 1 (1+ i)))
	((= i num-cols))
        (setf (aref matrix 0 i)
              (make-matrix-cell :distance i
                                :op 'insert)))
    (do ((i 0 (1+ i)))
	((= i num-rows))
        (setf (aref matrix i 0)
              (make-matrix-cell :distance i
                                :op 'delete)))
    ;; calculate matrix
    (let ((linearized-matrix (make-array (* num-rows num-cols)
					 :displaced-to matrix))
	  (c (1+ num-cols))
	  (n 1)
	  (w num-cols)
	  (nw 0)
	  (op-costs `((match . ,match-cost)
		      (insert . ,insert-cost)
		      (delete . ,delete-cost)
		      (substitute . ,substitute-cost))))
      (labels ((advance-indices ()
				(incf c)
				(incf n)
				(incf w)
				(incf nw))
	       (op-cost (op)
			(cdr (assoc op op-costs)))
	       (distance (index)
			 (matrix-cell-distance (aref linearized-matrix index)))
	       (set-c-entry (ref-index op)
			    (setf (aref linearized-matrix c)
				  (make-matrix-cell :distance (+ (distance ref-index)
								 (op-cost op))
						    :op op))))
	      (do ((row 0 (1+ row)))
		  ((= row (1- num-rows)))
		  (do ((col 0 (1+ col)))
		      ((= col (1- num-cols)))
		      ;; calculate cell
		      (cond ((< (distance w) (distance n))
			     (cond ((< (distance w) (distance nw))
				    (set-c-entry w 'insert))
				   ((char= (char string-1 row) (char string-2 col))
				    (set-c-entry nw 'match))
				   (t
				    (set-c-entry nw 'substitute))))
			    ((< (distance n) (distance nw))
			     (set-c-entry n 'delete))
			    ((char= (char string-1 row) (char string-2 col))
			     (set-c-entry nw 'match))
			    (t
			     (set-c-entry nw 'substitute)))
		      ;; advance indices - inner
		      (advance-indices))
		  ;; advance indices - outer
            (advance-indices))))
    ;; backtrack matrix
    (let ((distance (matrix-cell-distance
		     (aref matrix (1- num-rows) (1- num-cols))))
	  (row (1- num-rows))
	  (col (1- num-cols))
	  (edit-count 0)
	  (ops ()))
      (loop
       (unless (and (not (zerop row))
		    (not (zerop col)))
	 (return-from levenshtein-string-distance
		      (values distance edit-count (nreverse ops))))
       (let* ((which-op (matrix-cell-op (aref matrix row col)))
	      (move (cdr (assoc which-op moves))))
	 (unless (eq which-op 'match)
	   (incf edit-count)
	   (push (make-lev-op :index1 (1- row)
			      :index2 (1- col)
			      :op which-op)
		 ops))
	 (incf row (move-row-dist move))
	 (incf col (move-col-dist move)))))))