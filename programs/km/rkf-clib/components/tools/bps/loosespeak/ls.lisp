;;;; ============================================================================
;;;; ====                        LS.lisp                                     ====
;;;; ============================================================================

#|
Copyright (C) 2000-2006 James Fan

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact information:
James Fan m/s C0500, Dept Computer Science, 
Univ Texas at Austin, Austin, TX 78712, USA. jfan@cs.utexas.edu

If you would like a copy of this software issued under a different license
(e.g., with different redistribution conditions) please contact the author.

A copy of the GNU Lesser General Public Licence can be found at the end of 
this file.

======================================================================

The source code, manuals, and a test suite of examples for the most
recent version of loose speak are available at 

	http://www.cs.utexas.edu/users/jfan/ls/

Check this site for RELEASE NOTES and the CURRENT VERSION of loose speak.

======================================================================
		USING THIS FILE:
======================================================================

Save this file as (say) ls.lisp, then load it into your favorite Lisp 
environment:
	% lisp
        > (load "km")
	> (load "ls")

For greatly increased efficiency, make a compiled version of this file:
	% lisp
        > (load "km")
	> (compile-file "ls")
	> (load "ls")		
will load the faster, compiled version in future.

To start the query interpreter running, type (ls):
	> (ls)
	LS> 

See the Manual for instructions on using the loose-speak interpreter. 
The manuals are available at:

	http://www.cs.utexas.edu/users/jfan/ls/

James Fan
jfan@cs.utexas.edu

|#


;;(load "~/load") ;; need the KB & KM
(in-package :km)

(defparameter *ls-version* "1.0.4")
(defparameter *ls-start-time* (get-internal-run-time))
(defparameter *ls-time-bound* (* 15000 internal-time-units-per-second))
					;(defparameter *ls-time-bound* (* 120 internal-time-units-per-second))
(defparameter *ls-auto-selection* t);; decides if always picks the first answer
(defparameter *ls-auto-selection-method* 'first)
(defparameter *ls-debug-mode* nil)
(defparameter *ls-cpl-mode* nil)
(defparameter *ls-cpl-choices* nil)

(defparameter *ls-search-depth-bound* 2.4)
(defparameter *ls-min-search-depth* 0)
(defparameter *ls-stop-criteria* 'is_super_or_subclass0)
(defparameter *ls-head-specialization-bound* 1)
(defparameter *ls-tail-specialization-bound* 1)
(defparameter *ls-max-specialization* 10) 
(defparameter *ls-exhaustive-search* nil)

(defparameter *ls-generated-exprs* (make-hash-table :test #'equal))
(defparameter *ls-found* nil)
(defparameter *ls-search-cache* (make-hash-table :test #'equal))
(defparameter *ls-slot-domains-cache* (make-hash-table :test #'equal))
(defparameter *ls-slot-val-cache* (make-hash-table :test #'equal)) ;; the val of each entry is a class plus additional own-properties list
(defparameter *ls-slot-property-cache* nil)

(defparameter *ls-current-self* nil)
(defparameter *ls-current-replaced-self* nil) ;with things such as (every ... has ...) converted to (a ... with ...)
(defparameter *ls-current-self-instance* nil)
(defparameter *ls-current-self-instance-tmp* nil)

(defconstant *ls-pair-seq-class* '|Pair-Seq-Class|)

;; used to record what type of search was used for the last parsing action
;; set to appropriate value when an answer is found
;; data structure requirements:
;; 1. records only meaningful actions, 2. reset at the appropriate times
;; proposal: stack, actions pushed on top; popped only when the value is no longer needed, such as in case1, and partially in case2
;; con: only the top few levels of *ls-last-parsing-action* useful, bottom of stack useless
(defparameter *ls-last-parsing-action* nil) 

;; problem: case 1 & 2 overlapping.
;; ie. when solving for case 1, *ls-specialized-heads* may be full of results from doing A-r1-B and A-r2-C, etc
;; in other words how to implement the "has-new-value *ls-specialized-heads*"? -- by using *ls-last-parsing-action*?
(defparameter *ls-specialized-heads* nil)
(defparameter *ls-specialized-tails* nil)

;; a stack of values for (instance-of ...)
(defparameter *ls-instance-of* nil)

;; a list of slots as indicated by valency theory
;; default values are based on CLib slot vocabulary
;; should be changed for different KBs
(defparameter *valency-slots* '(|agent| |object| |base| |instrument| |donor| |recipient| |result| |raw-material| |in-event| |causes| |defeats| |enables| |inhibits| |by-means-of| |first-subevent| |next-event| |prevents| |subevent| |has-part| |has-basic-structural-unit| |has-basic-functional-unit| |plays| |element-type| ))
(defparameter *extended-valency-slots* nil)
					;(defparameter *valency-slots* nil)
(defparameter *valency-cost* 0.55)

(defparameter *ls-print-depth-limit* nil)

(defparameter *ls-existing-objs* nil)

;;; --------------- profiling variables -----------
;; symbols in the hash table includes: 
;; 0. total-number-ls-found, total-number-ls-called -- done
;; 1. total-answer-len, -- done
;; 2. cons-vio-detected, non-resemblance-detected, non-resemblance-unresolved, -- done
;; 3. search-head-used, search-tail-used, specialize-head-used, specialize-tail-used, domain-sat, range-sat, domain-specialize, range-specialize, domain-unsat, range-unsat
;; 4. from-nn, from-indirect-anaphora, from-overall, from-matcher, from-indirect-anaphora-nn  -- 
;; 5. total-search-depth -- done
;; 6. total-cache-lookup, total-cache-hit, total-cache-miss -- done
;; 7. superclass-sc-used subclasses-used -- done for stopping criteria

(defparameter *ls-profiling-data* (make-hash-table :test #'equal))
(defparameter *ls-old-profiling-data* (make-hash-table :test #'equal))
(defparameter *ls-class-usage* (make-hash-table :test #'equal))
(defparameter *ls-report-class-usage* nil)

;;;;; ------------- macros ------------------------
(defmacro ls-intern (x)
  `(intern ,x :km))

(defmacro ls-inst? (inst)
  `(and (atom ,inst)
	(and (not (numberp ,inst))
	     (not (stringp ,inst)) ;; inst shouldn't be string
	     (or (is-an-instance ,inst)
		 (ls-named-instance? ,inst)))))

(defmacro ls-km (expr &key (fail-mode 'fail))
  `(let ((ls-km-result                       nil)
	 (*prototype-classification-enabled* nil)  ;Temporarily disable KM classification
	 (*classification-enabled*           nil))  ;Temporarily disable KM classification
    ;;(format t "*ls-current-self* = ~A *ls-current-replaced-self* = ~A~%" *ls-current-self* *ls-current-replaced-self*)
     ;(disable-classification)
     (setf ls-km-result (km ,expr :fail-mode (quote ,fail-mode)))
     ;(enable-classification)
     ls-km-result)
  )

(defmacro aggregatep (expr)
  `(and (listp ,expr)
	(member (first ,expr)
		'(:|pair| :|seq| :|set| :|bag| :|triple|)
		:test #'eql))
  )

(defmacro km-seq-queryp (expr)
  `(and
    (consp ,expr)
    (km-queryp ,expr)
    (member (first ,expr)
	    '(|the1| |the2| |the3| |theN|)))
  )

(defmacro km-queryp (expr)
  `(or
    ;;(boolean-expr expr)
    (and (consp ,expr)
	 (member (first ,expr) '(|the| |the1| |the2| |the3| |theN|) :test #'eql))
    ))

;;;;; ------------- main routines -----------------
;;;; do a bottom up processing of all the sexprs
;;;; check with the Meta-KB knowledge, and see if any LS-type triggers would fire off
;;;; if so, checks with the user to see if the alternatives are the intended meanings
(defun init-ls ()
  (when (null *ls-slot-property-cache*)
    (setf *ls-slot-property-cache*
	  (mapcar #'(lambda (slot)
		      (list slot 
			    (first (ls-km `(|the| |categorical-constant-class| |of| ,slot)))
			    (first (ls-km `(|the| |scalar-constant-class| |of| ,slot)))
			    (first (ls-km `(|the| |cardinal-unit-class| |of| ,slot)))))
		  (ls-km `(|the| |all-instances| |of| |Slot|)))))
  )

(defun ls (&optional (lsexpr 'ask-user))
  (cond ((eql lsexpr 'ask-user)
	 (ls-read-eval-print))
	(t
	 (km (parse-for-ls lsexpr)))
	)
  )

;; relevant triples have the following characteristics:
;; the slot is not any of the following: input-word, instance-of |det|, 
;; the slot should be an instance of Slot
(defun extract-relevant-triples (triples)
  (remove-if-not 
   #'(lambda (triple)
       (let ((slot (second triple)))
	 (and (not (member slot '(|input-word| |det| |instance-of|) :test #'eql))
	      (ls-slotp slot))
	 ))
   triples)
  )

;;;; the following was coded for compound LS, ie. 1 input w/ multiple LS
;;;; not working because it may trying to interpret non-LS as LS, e.g
;;;; (a Entity with (mass ((:pair 1 *gram)))) =>
;;;; (a Entity with (mass ((a Mass-Value with (value ((:pair 1 *gram))))))) =>
;;;; (a Entity with (mass ((a Mass-Value with (same-as ((:pair 1 *gram))) (value ((:pair 1 *gram))))))) =>
;;;; .... on and on
;;;; for the time being ignore compound LS, 
;;;; in the future, may mark the section generated by LIPS because it doesn't contain LS for sure,
;;;; and no need to recheck those sections.
(defun parse-for-ls (expr)
  (init-ls)
  (incf (gethash 'total-number-ls-called *ls-profiling-data* 0))
  (setf *ls-last-parsing-action* nil)
  (setf *ls-specialized-heads* nil)
  (setf *ls-specialized-tails* nil)
  ;;(setf *ls-search-cache* nil)
  (start-logging)			; starts recording KB changes to they can be rolled back
  (set-checkpoint)
  (setf *ls-existing-objs* (ls-km `(|the| |all-instances| |of| |Thing|)))
  ;;(format t "obj stack is ~S~%" (obj-stack))
  (let ((retresult nil))
    (clrhash *ls-generated-exprs*)
    (setf *ls-found* nil);; commented out... see the commented out section on compound LS
    ;; now reorder the slots if expr is an assertion
    (setf expr
	  (ls-reorder-triples expr))
    (let ((oldexpr expr))
      (catch 'ls-retry
	(setf retresult
	      (do ((newexpr (revert-self (parse-and-flush expr))))
		  ((not *ls-found*)
		   newexpr)
		  ;;(format t "ls-found = ~a new expr = ~a oldexpr = ~a~%" *ls-found* newexpr oldexpr)
		  (setf *ls-found* nil)
		  
		  (setf *ls-last-parsing-action* nil)
		  (setf *ls-specialized-heads* nil)
		  (setf *ls-specialized-tails* nil)
		  
		  (if (equal oldexpr newexpr)
		      (setf *ls-found* nil)
		    (progn
		      (setf oldexpr newexpr)
		      (setf newexpr (revert-self (parse-and-flush newexpr)))))
		  (setf *ls-found* nil);; make sure parse it only once or twice, does it still work??????
		  )
	      )
	)
      (undo)				; will restore KB back to how it was at the last checkpoint
      (stop-logging)
      (setf *ls-existing-objs* nil)
      retresult
      )))


;;;; example: (a Reaction w/ (raw-material ((a NaOH with  with (base-of ((a Solution))) (quantity ((:pair 1 *mole)))) (a HCl))) (result ((a H2O) (a NaCl))))
;;;; converted to a tree: Reaction 
;;;;                    r-m /    \ result
;;;;                 NaOH  HCl   H2O  NaCl
;;;;        quantity /   \base-of
;;;;        (:pair..)    Solution

;;;; given an assertion, treat it like a tree
;;;; and try all the paths from root to leaves for ls
;;;; not considering how to merge the results yet
(defun parse-for-ls-one-iteration (expr)
  ;; first save the current KB state
  ;; reset the global vars
  (push (first
	 (retrieve-property-from-assertion expr '|instance-of|))
	*ls-instance-of*)
  (push nil *ls-last-parsing-action*)
  (push nil *ls-specialized-heads*)
  (push nil *ls-specialized-tails*)
  ;;(format t "pushed last action now it is ~S~%" *ls-last-parsing-action*)
  (let* (		
	 (result
	  (cond ((and 
		  (consp expr)
		  (= (length expr) 2)
		  (eql (second expr) '|&|)
		  (gethash (first expr) *ls-generated-exprs*))
		 (progn
		   (ls-debug-msg 'parse-for-ls-one-iteration 
				 (format nil "the whole expr ~S is found in has~%" (first expr)))
		   expr))
		;; queries of the form (the1 of (:seq ...)) are treated the same as normal queries
		;; this is inefficient b/c querying subclasses etc, but easy to develop
		;; note this has the restriction on queries such as (the1 slot of ...) 
		;; but it can be rephrased as (the1 of (the slot of ...)) anyway
		((km-seq-queryp expr)
		 (parse-km-seq-query-one-iteration expr))
		((km-queryp expr)
		 (parse-for-query-one-iteration expr))
		(t
		 (parse-for-assertion-one-iteration expr))))
	 (actions nil)
	 (heads nil)
	 (tails nil)
	 )
    (pop *ls-instance-of*)
    (setf actions (pop *ls-last-parsing-action*))
    (setf heads (pop *ls-specialized-heads*))
    (setf tails (pop *ls-specialized-tails*))
    (values result actions heads tails)
    ))

(defun parse-for-assertion-one-iteration (expr)
  (let ((result nil))
    (if (and (km-assertionp expr)
	     (consp expr))
	(progn
	  ;; save the self here since self only refers to assertions
	  ;; save the current expr for replacing Self
	  (if (not *ls-current-self*)
	      (progn;; if this is the 1st time this function is called
		(push expr *ls-current-self*)
		(push (clone-instance (first *ls-current-self*)) *ls-current-replaced-self*)
		)
	    (progn;; else replicate the top of the stack
	      (push (car *ls-current-self*) *ls-current-self*)
	      (push (car *ls-current-replaced-self*) *ls-current-replaced-self*)))
	  ;;(setf expr (replace-self expr)) no need to replace-self for assertions because assertions won't mention self
	  )
      )
    (setf 
     result
     (cond ((gethash expr *ls-generated-exprs*)
	    (ls-debug-msg 'parse-for-assertion-one-iteration
			  (format nil "Expr ~s found in the hash~%" expr))
	    expr)
	   ((atom expr)
	    expr)
	   ((null expr)
	    expr)
	   ((ls-chemical-name-expr expr)
	    (ls-interpret-chemical-name expr))
	   ((ls-property-expr expr)
	    (ls-interpret-property expr))
	   (;; of the form (:nn a b)
	    (ls-nn-seq-expr expr)
	    (format t "LS DEBUG: resolving noun compounds ~S~%" expr)
	    (ls-interpret-nn expr)
	    )
	   (;; of the form (forall ...), etc
	    (forall-allofp expr)
	    (interpret-allof expr))
	   (;; of the forma (x isa y), need to do role checking
	    (km-isap expr)
	    (interpret-isa expr))
	   ((condition-expr expr)
	    (interpret-condition-expr expr))
	   ((not (nested-list expr))
	    expr)
	   (;; of the form ((:nn a b) with ...)
	    (and (ls-nn-seq-expr (first expr))
		 (rest expr))
	    (append
	     (parse-and-flush (first expr))
	     (rest (rest expr)))
	    )
	   (;; of the form ((...) and (...)), etc
	    (boolean-expr expr)
	    (mapcar #'(lambda (element)
			(if (consp element)
			    (parse-and-flush element)
			  element))
		    expr))
	   ;; if :seq or :set
	   ((aggregatep expr)
	    (check-for-ls expr))
	   ;; now process every slot
	   (t
	    (parse-for-assertion-one-iteration0 expr))
	   ))
    (if (and (km-assertionp expr)
	     (consp expr))
	(progn
	  ;;(setf result (revert-self result))
	  (pop *ls-current-self*)
	  (pop *ls-current-replaced-self*)
	  )
      )
    result
    ))

(defun remove-by-index (list index)
  (remove (nth index list) list :test #'equal)
  )

(defun append-if-new-slot (slot-fillers assertion-body)
  (let ((olditem nil)
	(oldfillers nil))
    (cond 
     ((setf olditem (first (member-if #'(lambda (x) (equal (first slot-fillers) (first x)))
				      assertion-body)))
      ;; slot already exists in the body, just add the filler
      (setf oldfillers
	    (second olditem))
      (substitute
       (list (first olditem) (append oldfillers (second slot-fillers)))
       olditem
       assertion-body
       :test #'equal)
      )
     (t
      (append-element assertion-body slot-fillers))
     )))

(defun parse-for-assertion-one-filler (head slot filler)
  (let (actions specialized-heads tmp)
    (multiple-value-setq (tmp actions specialized-heads) 
			 (parse-for-ls-one-iteration filler))
    ;; this is for CPL only!!!
    (when (and *ls-cpl-mode*
	     (null *ls-auto-selection*)
	     (ls-chemical-name-expr filler))
      (setf *ls-cpl-choices* (butlast *ls-cpl-choices*))
      (prompt-for-clarification (append head `((,slot (,filler)))) (list (append head  `((,slot (,tmp)))))))
    ;; (format t "pre case1 heads = ~A~%" *ls-specialized-heads*)
    ;; check for Case 1 in bug #68 here!
    (solve-linear-dependency head slot filler tmp actions specialized-heads)
    ))

;; return a new body based on the slot-fillers and old-body
(defun parse-for-assertion-one-slot-fillers-pair (slot-fillers old-body head i)
  (let (
	(slot (first slot-fillers))
	(fillers (second slot-fillers))
	(tmp nil)
	(tmp-body nil)
	(new-body old-body)
	(additional-result-slot-fillers nil)
	)
    (dolist (filler fillers)
	    ;; returns a new expr
	    (setf tmp (parse-for-assertion-one-filler head slot filler))
	    (setf tmp-body (extract-concept-body tmp))
	    (dolist (tmp-slot-fillers tmp-body)
		    (setf additional-result-slot-fillers
			  (append-if-new-slot tmp-slot-fillers additional-result-slot-fillers))
		    )
	    )
    ;; now we process 1 slot, we should update the *ls-current-self* accordingly
    ;; first get the latest expr
    (setf new-body
	  (substitute-n
	   (first additional-result-slot-fillers)
	   i
	   old-body
	   ))
    (setf new-body (append new-body (cdr additional-result-slot-fillers)))
    ;; then replace accordingly from the old *ls-current-self*
					;(format t "current-eslf is ~s replaced -by ~s olditem  = ~S~%"
					;*ls-current-self* (append head new-body) expr)
    (push 
     (deep-substitute
      (append head new-body)
      (append head old-body)
      (pop *ls-current-self*) :test #'equal) *ls-current-self*)
    (pop *ls-current-replaced-self*)
    (push (clone-instance (first *ls-current-self*)) *ls-current-replaced-self* )
    new-body
    ))

(defun parse-for-assertion-one-iteration0 (expr)
  (let* ((head (extract-concept-head expr))
	 (body (extract-concept-body expr))
	 (new-body body)
	 (i 0))
    (dolist (slot-fillers body)
	    ;; return new-body
	    (setf new-body (parse-for-assertion-one-slot-fillers-pair slot-fillers new-body head i))
	    (setf i (+ 1 i)))
    ;; prepare for checking Case 2 in bug #68 here
    ;; pop all the heads on the stack
    ;; now modify the specialized-heads to update the new-body
    (if (> (length (car *ls-specialized-heads*)) 1) 
	;; the head has been specialized by more than 1 slot-fillers pair
	(push 
	 (mapcar #'(lambda (head-exprs)
		     (mapcar #'(lambda (specialized-head)
				 ;; this lambda function puts the new head onto the new body
				 (let ((specialized-new-head 
					(extract-concept-head specialized-head)))
				   ;; need to check if the specialized-head has (instance-of ...) or not, 
				   (if (retrieve-property-from-assertion specialized-head '|instance-of|)
				       (add-new-property-to-assertion
					(append
					 specialized-new-head new-body)
					(list '|instance-of|
					      (retrieve-property-from-assertion specialized-head '|instance-of|)))
				     (append 
				      specialized-new-head new-body))))
			     head-exprs))
		 (pop *ls-specialized-heads*))
	 *ls-specialized-heads*)
      )
    ;;(format t "after = ~S~%" *ls-specialized-heads*)
    (append head new-body)
    ))

(defun parse-for-query-one-iteration (expr)
  (cond 
   ((aggregatep (extract-head expr))
    (replace-km-expr-head 
     expr
     (cons
      (first (extract-head expr))
      (mapcar #'(lambda (element)
		  (let ((interpretation (parse-for-ls-one-iteration (replace-km-expr-head expr element))))
		    (extract-head interpretation)))
	      (rest (extract-head expr)))))
    )
   (t
    (parse-for-query-one-iteration0 expr))
   )
  )

(defun parse-for-query-one-iteration0 (expr)
  ;;(format t "self = ~S~%" *ls-current-replaced-self*)
  (let ((result nil)
	(alternative nil)
	(new-expr nil)
	(actions nil)
	(specialized-heads nil)
	(search-results nil))
    (multiple-value-setq (result actions specialized-heads)
			 (parse-for-ls-one-iteration (extract-head expr)))
        ;; (format t "specialized-heads = ~S and stack = ~S~%" specialized-heads *ls-specialized-heads*)
    (setf result (replace-self result))
    ;; (format t "result = ~S~%" result)
    (setf new-expr (replace-km-expr-head expr result))

    (setf result
	  (cond 
	   ((or
	     (gethash (revert-self new-expr) *ls-generated-exprs*)
	     (boolean-expr new-expr)
	     (minimatch new-expr '(|the| |elements| |of| &rest)))
	    (revert-self new-expr))
	   ((km-class-queryp new-expr)
	    (parse-class-query-one-iteration new-expr)
	    )
	   ((setf alternative (constraint-violation new-expr))
	    ;; profiling
	    (incf (gethash 'cons-vio-detected *ls-profiling-data* 0))
	    (dolist (violation alternative);; increment individual type of violations
		    (incf (gethash violation *ls-profiling-data* 0)))
	    (setf alternative (constraint-satisfaction new-expr alternative));; this is needed for case2 query form
	    ;;(format t "ls-specialized-heads ~s~%" *ls-specialized-heads*)
	    (if (not (does-last-action-include 'specialize-head))
		;;(prompt-for-clarification new-expr (revert-self alternative))
		(prompt-for-clarification (revert-self new-expr) (revert-self alternative))
	      (revert-self new-expr)
	      ;;new-expr
	      )
	    )
	   ((preexisting-path new-expr)
	    (revert-self new-expr))
	   ((setf search-results 
		  (mapcar #'(lambda (one-result)
			      (cons
			       (first one-result)
			       (list 
				(remove-if #'contains-conjugate-slot-pairs-in-query-result
					   (second one-result)))))
			  (specialize-head new-expr)))
	    (format t "LS DEBUG: no prior path of ~S found but an alternative(s) is found ~S.~%" new-expr search-results)
	    (setf alternative (generate-expr-from-specialize-head new-expr search-results))
					; profiling
	    (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))

	    (if nil 
		;; the following condition commented out because
		;; there's no linear or tree dependency for query
		;;(has-specialized search-results)		
		(progn
					; profiling
		  (incf (gethash 'specialize-head-used *ls-profiling-data* 0))
		  (ls-add-last-action 'specialize-head)
		  ;;(ls-add-specialized-head (revert-self alternative))
		  (ls-add-specialized-head alternative)
		  ;; no need to prompt yet
		  (revert-self new-expr)
		  )
	      (prompt-for-clarification new-expr (revert-self alternative)))
	    )
	   (t
	    (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
	    (incf (gethash 'non-resemblance-detected-unresolved *ls-profiling-data* 0))
	    new-expr
	    )))
    ;; now call case2
    (setf new-expr
	  (solve-tree-dependency result 
				 (append actions 
					 (car *ls-last-parsing-action*))
				 (append (car *ls-specialized-heads*) specialized-heads)))
    (setf (gethash
	   new-expr
	   *ls-generated-exprs*)
	  t)
    new-expr
    ))

;; parses queries of the form (the Class slot of ...)
(defun parse-class-query-one-iteration (expr)
  (let ((class (second expr))
	(alternative nil)
	(search-results nil))
    (cond ((preexisting-path expr)
	   (revert-self expr)
	   )
	  ((setf search-results (specialize-head expr
						 nil
						 ;; two examples:
						 ;; (the O2 raw-material of ...)
						 ;; (the Base-Role plays of ...)
						 #'(lambda (inst goals)
						     (declare (ignore goals))
						     (let ((slotval nil)
							   (slot (extract-first-slot expr)))
						       (and (kb-objectp inst)
							    (setf slotval (ls-km `(|the| ,slot |of| ,inst)))
							    (ls-km `(,slotval |isa| ,class))))
						 )))
	   (setf alternative (generate-expr-from-specialize-head
			      expr
			      search-results))
					; profiling
	   (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
	   (if nil
	       ;; the following condition commented out because
	       ;; there's no linear or tree dependency for query
	       ;; (has-specialized search-results)
	       (progn
					; profiling
		 (incf (gethash 'specialize-head-used *ls-profiling-data* 0))
		 (ls-add-last-action 'specialize-head)
		 ;;(ls-add-specialized-head (revert-self alternative))
		 (ls-add-specialized-head alternative)
		 ;; no need to prompt yet
		 (revert-self alternative)
		 )
	     (prompt-for-clarification expr (revert-self alternative))))
	  (t
					; profiling
	   (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
	   (incf (gethash 'non-resemblance-detected-unresolved *ls-profiling-data* 0))
	   expr))
    ))

(defun parse-km-seq-query-one-iteration (expr)
  (let ((new-expr 
	 (replace-km-expr-head
	  expr
	  (parse-for-ls-one-iteration (extract-head expr))))
	;; the actions are ignored here because neither linear nor tree dependency is possible here
	(search-results nil)
	(alternative nil))
    (cond 
     ((minimatch new-expr '(?head ?slot |of| &rest))
      (parse-km-seq-query-one-iteration
       (list (first new-expr)
	     (parse-for-query-one-iteration
	      `(|the| ,(second new-expr) |of| ,(fourth (revert-self new-expr)))))))
     ((or
       (aggregatep (extract-head new-expr))
       (aggregatep (ls-km (extract-head new-expr))))
      (revert-self new-expr))

     ((setf search-results
	    (specialize-head new-expr 
			     nil
			     #'(lambda (inst goals)
				 (declare (ignore goals))
				 (aggregatep inst))))
      (setf alternative 
	    (generate-expr-from-specialize-head 
	     new-expr search-results))
					; profiling
      (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
      (if nil
	  ;; the following condition commented out because
	  ;; there's no linear or tree dependency for query
	  ;; (has-specialized search-results)
	  (progn
					; profiling
	    (incf (gethash 'specialize-head-used *ls-profiling-data* 0))
	    (ls-add-last-action 'specialize-head)
	    ;;(ls-add-specialized-head (revert-self alternative))
	    (ls-add-specialized-head alternative)
	    ;; no need to prompt yet
	    (revert-self alternative)
	    )
	(prompt-for-clarification new-expr (revert-self alternative))))
     (t
					; profiling
      (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
      (incf (gethash 'non-resemblance-detected-unresolved *ls-profiling-data* 0))
      new-expr)
     )
    ))

;; to see if a list contains another list
(defun nested-list (expr)
  (if (not (consp expr))
      nil
    (if (consp (car expr))
	t
      (nested-list (cdr expr)))
    ))

;; given a new expr, compare to the orig, so that mark the ls generated portion
;; example1: given (a H2O with (quantity ((:pair 1 *mole)))) and (a H2O with (quantity ((a Quantity-Value with (value ((:pair 1 *mole))))))), mark (a Quantity-Value with (value ((:pair 1 *mole)))) as ls-generated
;; example2: given (a Reaction with (raw-material ((a H2O)))) and (a Reaction with (raw-material ((a Chemical with (has-basic-structural-unit ((a H2O))))))), mark (a Chemical with (has-basic-structural-unit ((a H2O))))
(defun mark-ls-generated-expr (orig-expr new-expr)
  (cond
   ((equal (extract-first-slot orig-expr)
	   (extract-first-slot new-expr))
    (setf (gethash (extract-tail new-expr) *ls-generated-exprs*) t))
   (t
    (setf (gethash new-expr *ls-generated-exprs*) t)))
  )
;;;; given the orig expr and a list of possible replacements
;;;; ask the user to pick one
;;;; and return it
(defun prompt-for-clarification (orig-expr replacements)
  ;; take the simplest approach for the time being
  ;;(first replacements)
  (cond 
   ((= (length replacements) 0)
    orig-expr)
   ((and (= (length replacements) 1)
	 (equal orig-expr (first replacements)))
    orig-expr)
   ((member orig-expr replacements :test #'equal)
    orig-expr)
   (t
    ;; assume that a different expression is chosen as the input
    (setf *ls-found* t);; commented out... see the commented out section on compound LS
    ;; profiling
    #|(format t "unique replacements are ~S~%"
	    (remove-duplicates replacements :test #'equal))|#
    (setf replacements (remove-duplicates replacements :test #'equal))
    (incf (gethash 'total-answer-len *ls-profiling-data* 0) 
	  (length replacements))
    (incf (gethash 'total-number-ls-found *ls-profiling-data* 0))
    (incf (gethash 'total-search-depth *ls-profiling-data* 0)
	  (eval (cons '+ (mapcar #'ls-path-literal-length replacements))))
    ;; now remove duplicates and sort them
    (when (not (ls-property-expr orig-expr))
      (setf replacements
	    (rank-ls-generated-exprs
	     replacements
	     (extract-first-slot orig-expr)
	     ;; required slots
	     (remove-duplicates
	      (cons (extract-first-slot orig-expr)
		    (append
		     (ls-km `(|the| |all-subslots| |of| ,(extract-first-slot orig-expr)))
		     (ls-km `(|the| |inverse| |of| (|the| |all-subslots| |of| (|the| |inverse| |of| ,(extract-first-slot orig-expr))))))
		    ))
	     (extract-head-instance orig-expr)
	     (extract-tail-instance orig-expr))))
    (cond ((and *ls-auto-selection* *ls-cpl-mode*)
	   (append-cpl-choices (list orig-expr (funcall *ls-auto-selection-method* replacements)))
	   (ls-debug-msg 'prompt-for-clarification 
			 (format nil "ls-cpl-choices are ~S~%" *ls-cpl-choices*))
	   ;;(mark-ls-generated-expr orig-expr (first replacements))
	   orig-expr
	   )
	  (*ls-auto-selection*
	   ;; the chose expr shouldn't contain ls, therefore is ls-generated
	   (let ((chosen-one (funcall *ls-auto-selection-method* replacements)))
	     (mark-ls-generated-expr orig-expr chosen-one)
	     chosen-one)
	   )
	  (*ls-cpl-mode*
	   (append-cpl-choices (cons orig-expr replacements))
	   (ls-debug-msg 'prompt-for-clarification
			 (format nil "ls-cpl-choices are ~S~%" *ls-cpl-choices*))
	   orig-expr)
	  (t
	   ;; else do the prompt & dialogue to get the right selection from user
	   (prompt-user orig-expr replacements)))
    ))
  )

(defun prompt-user (orig-expr replacements)
  (let ((answer 0))
    (print-choices orig-expr replacements)
    (setf answer (case-sensitive-read))
    (loop while (or (not (numberp answer))
		    (< answer 0)
		    (> answer (+ (length replacements) 1)))
	  do
	  (format t "Please choose between 0 and ~a~%" (length replacements))
	  (print-choices orig-expr replacements)
	  (setf answer (case-sensitive-read)))
    (cond ((= answer (length replacements))
	   orig-expr)
	  ((= answer (+ (length replacements) 1))
	   ;; return nil
	   (throw 'ls-retry nil))
	  (t
	   ;; the chose expr shouldn't contain ls, therefore is ls-generated
	   (mark-ls-generated-expr orig-expr (nth answer replacements))
	   (nth answer replacements))
	  )
    ))
    
(defun print-choices (orig-expr replacements)
  (format t "Does ~s mean: ~%" orig-expr)
  (dotimes (i (length replacements))
	   (format t "~a> ~S~%" i (nth i replacements)))
  (format t "~A> Use the expression as it is.~%" (length replacements))
  (format t "~A> Let me re-enter the expression.~%" (+ (length replacements) 1))
  )
;;;; given a single "branch" of a complicated KM expression
;;;; check all the LS types to see if anything matches
;;;; if nothing found, try the KB searches
;;;; 1. try search given expr to see if existing
;;;; 2. search for any related concept whose paths exists
;;;; 3. search for any subclasses whose paths exists
;;;; 4. search for any concepts whose assertions may include sub or superclasses of suspectec concept and whose paths exists, e.g. Chemical
(defun check-for-ls (expr)
  ;; first reset the last parsing-action to nil
  ;;(setf *ls-last-parsing-action* nil) ;;why?
  (let ((alternative nil) (classes nil))
    (cond 
     ((gethash expr *ls-generated-exprs*)
      (ls-debug-msg 'check-for-ls
		    (format nil "Expr ~s found in the hash~%" expr))
      expr)
     ((atom expr)
      expr)
     ((and (consp expr)
	   (= (length expr) 1));; cases like (It)
      expr)
     ((ls-property-value-value-pair expr) ;; special case2 for CPL
       (ls-interpret-property-value-value-pair expr))
     ((aggregatep expr)
      (dolist (element expr)
	      (when (minimatch element '(?x |has| &rest))
		;; first find all the instance-of 
		;; second assert them
		(setf classes (retrieve-property-from-assertion element '|instance-of|))
		(km `(,(first element) |has| (|instance-of| ,classes)))
		))
      (cons
       (first expr)
       (mapcar #'parse-for-ls-one-iteration 
	       (rest expr))))
     (;; of the form (:nn a b)
      (ls-nn-seq-expr expr)
      (ls-interpret-nn expr)
      )
     (;; of the form (forall ...), etc
      (forall-allofp expr)
      (interpret-allof expr))
     ;; check for constraint violations
     ((setf alternative (constraint-violation expr))
      ;; profiling
      (incf (gethash 'cons-vio-detected *ls-profiling-data* 0))
      (dolist (violation alternative);; increment individual type of violations
	      (incf (gethash violation *ls-profiling-data* 0)))

      (setf alternative (constraint-satisfaction expr alternative))
      (if (not (does-last-action-include 'specialize-head)) 
	  ;; only need to worry about specialize-head because specialize-tail will be handled by solve-linear-dependency only
	  (prompt-for-clarification 
	   expr
	   alternative)
	;; expr
	;; this is for case like "(a Reaction w/ (ki ((:pair 3 nil))))" where both head and tail are modified
	;; need to keep the tail modification and leave head for solve-tree-dependency
	;; this problem only occurs in constraint violation, so no need for prior path case below
	(prompt-for-clarification 
	 expr
	 (mapcar #'(lambda (one-alternative) (replace-km-expr-head-with-class
					      one-alternative (extract-head-class expr)))
		 alternative))
	))
     ((preexisting-path expr)
      expr)
     ((setf alternative (search-prior-path expr))
      (format t "LS DEBUG: no prior path of ~S found but an alternative(s) is found ~S.~%" expr alternative)
      ;; profiling
      (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
      (if (not (or (does-last-action-include 'specialize-head)
		   (does-last-action-include 'specialize-tail)))
	  (prompt-for-clarification expr alternative)
	expr
	)
      )
     (t
      ;; profiling
      (incf (gethash 'non-resemblance-detected *ls-profiling-data* 0))
      (incf (gethash 'non-resemblance-detected-unresolved *ls-profiling-data* 0))

      expr)
     )
    ))

(defun generate-expr-from-search-head (expr search-results)
  (cond 
   ((and (aggregatep (extract-head expr))
	 (km-queryp expr)
	 (aggregate-search-results? search-results))
    ;; so the search-results will be a bit different
    (let* ((aggregate (extract-head expr))
	   (current-result (list (first aggregate)))
	   (query-expr (remove aggregate expr))
	   (one-search-result nil)
	   (newexprs nil)
	   (aggregate-element nil))
      (dotimes (i (length search-results))
	       ;; need to prompt the user for an answer
	       #|(setf one-search-result (mapcar #'(lambda (result) ;; need to reverse the path so that the result
						   (list (first result)
							 (second result)
							 (reverse-path (third result))))
					       (nth i search-results)))|#
	       (setf one-search-result (nth i search-results))
	       ;;(format t "current-result = ~S~%" current-result)
	       (setf aggregate-element (nth (+ 1 i) aggregate))
	       (setf newexprs
		     (mapcar #'(lambda (interpretation)
				 (append
				  query-expr
				  (list (append-element current-result
							interpretation))))
			     (if (km-queryp aggregate-element)
				 (mapcar #'extract-head
					 (generate-query-expr-from-search-head
					  `(|the| |slot| |of| ,aggregate-element)
					  one-search-result))
			       (mapcar #'extract-tail
				       (generate-assertion-expr-from-search-tail
					`(|a| |Thing| |with| (|has-part| (,aggregate-element)))
					one-search-result)))))
	       (setf current-result
		     (extract-head
		      (prompt-for-clarification
		       ;; make sure the orig expr is nicely presented
		       (append
			query-expr 
			(list (append current-result (list (nth (+ 1 i) aggregate)))))
		       newexprs)
		      ))
	       )
      
      ;; must make it list because rest of gen-exp cases return a list of interpretations
      (list (append-element query-expr current-result))
      ))
   ((aggregatep (extract-head expr))
    (format t "WARNING: Head of a triple is an aggregate!! Unexpected case. ~%")
    (list expr);; must make it list because rest of gen-exp cases return a list of interpretations
    )
   ((km-queryp expr)
    (generate-query-expr-from-search-head expr search-results))
   (t
    (generate-assertion-expr-from-search-head expr search-results)))
  )

;; expands the head of a triple to something else
;; e.g. A-slot-B => A-slot'-A'-slot-B
(defun generate-assertion-expr-from-search-head (expr search-results)
  (let* ((orig-head-extracted (extract-head expr))
	 )
    (cond 
     ((null search-results);; nothing found
      nil)
     (t
      (mapcar 
       #'(lambda (search-result)
	   (let* (
		  ;;(reversed-path (reverse-path path))
		  (access-path 
		   (gen-access-path search-result))
		  (newexpr;; replace the new head w/ orig in case the orig is an instance
		   (if (ls-inst? orig-head-extracted)
		       (replace-km-expr-head-with-inst
			(gen-km-expr-from-access-path access-path)
			orig-head-extracted)
		     (replace-km-expr-head
		      (gen-km-expr-from-access-path access-path)
		      orig-head-extracted)))
		  (retresult
		   ;; now append it to the orig expr
		   (replace-km-expr-tail
		    newexpr
		    (append (list '|a| (extract-tail-class newexpr) '|with|)
			    (extract-concept-body expr)))))
	     (setf (gethash retresult *ls-generated-exprs*) t)
	     (ls-debug-msg 'generate-assertion-expr-from-search-head
			   (format nil "Expr ~s set in the hash in search-head~%" retresult))
	     retresult
	     ))
       search-results)))
    ))

(defun generate-query-expr-from-search-head (expr search-results)
  (let* (
	 (orig-head-extracted (extract-head expr))
	 )
    (mapcar 
     #'(lambda (search-result)
	 (let* ((path (third search-result))
					;(reversed-path (reverse-path path))
		(slots (reverse (remove-if-not #'ls-slotp path))))
	   ;; the following condition commented out for the case
	   ;; '(|the| |Base-Role| |plays| |of| |_Chemical6937|)
	   ;; '((NIL ((SUCCESS |_Chemical6960| (|_Chemical6937| |raw-material-of| |_Equilibrium-Reaction6936| 0.55 |result| |_Chemical6960| 1.1)))))
	   #|(if (km-class-queryp expr)
	       (append `(|the| ,(second expr))
		       (cdr (generate-access-path slots orig-head-extracted)))|#
	     (substitute
	      (generate-access-path slots orig-head-extracted)
	      orig-head-extracted expr :test #'equal)
	     ;;)
	   ))
     search-results)
    ))

;; given the result of a search
;; regenerate the expression path
;; e.g. (success '(success (:|pair| 0 |*electronic-charge|) (|_NaOH15333| |charge| |_Charge-Value15855| 4 |value| (:|pair| 0 |*electronic-charge|) 5)))
(defun generate-assertion-expr-from-search-tail (expr search-results)
  (cond
   ((null search-results)
    nil)
   (t
    (remove
     nil
     (mapcar
      #'(lambda (search-result)
	  (cond ((= (length (third search-result)) 1)
		 ;; path of length 1 indicates no need to look further, orig expr is good
		 nil)
		(t
		 (let* (
			(access-path
			 (gen-access-path search-result))
			(new-path (gen-km-expr-from-access-path access-path))
			(reverse-path (gen-km-expr-from-access-path (reverse-path access-path)))
			(specific-path 
			 (replace-km-expr-end
			  reverse-path
			  ;; the orig expr-tail
			  ;; no need to extract-concept-header for the (|a| |Reaction| |with| (|raw-material| ((|*a2e| |has| (|instance-of| (h2o)))))) case
			  (extract-tail expr)
			  ))
			;; final path depends on 2 cases
			;; 1> if reverse-path exists in expr, move any attachment from it 
			;; 2> else replace-km-whole-tail
			(final-path
			 (if (find-access-path (extract-tail expr) new-path)
			     (fold-expr-inside-out (extract-tail expr) new-path)
			   nil))
			)
		   (if final-path
		       (progn
			 (replace-km-expr-tail
			  expr
			  final-path))
		     (progn
		       (setf (gethash specific-path *ls-generated-exprs*) t)
		       (ls-debug-msg 'generate-assertion-expr-from-search-tail
				     (format nil "Expr ~s set in the hash in search-tail~%" specific-path))
		       (replace-km-expr-tail
			expr
			specific-path)
		       ))))))
      search-results))))
  )

(defun generate-expr-from-specialize-head (expr search-results)
  (cond ((km-queryp expr)
	 (generate-query-expr-from-specialize-head expr search-results))
	(t
	 (generate-assertion-expr-from-specialize-head expr search-results)))
  )

(defun generate-query-expr-from-specialize-head (expr search-results)
  (cond ((and
	  (aggregatep (extract-head expr))
	  (aggregate-specialize-search-results? search-results))
					;(ls-objectp (extract-head expr)))
	 (let* ((aggregate (extract-head expr))
		(tmpexpr nil)
		(tmpanswers nil)
		(final-aggregate (list (first aggregate))))
	   (dotimes (i (length (cdr aggregate)));; the first elemnt of aggregate is :set so ignore
		    ;; first generate the expr one at a time
		    (setf tmpexpr
			  (replace-km-expr-head
			   expr
			   (nth (+ 1 i) aggregate)))
		    (setf tmpanswers
			  (generate-expr-from-specialize-head
			   tmpexpr (nth i search-results))
			  )
		    (setf final-aggregate
			  (append-element
			   final-aggregate
			   (extract-head
			    (prompt-for-clarification
			     tmpexpr
			     tmpanswers))))
		    )
	   (list (replace-km-expr-head expr final-aggregate))
	   ))
	(t
	 (append-list
	  (mapcar #'(lambda (search-result)
		      (let ((new-class (first search-result)))
			(mapcar #'(lambda (one-expr)
				    (if new-class
					(replace-km-expr-head-with-class
					 one-expr
					 new-class)
				      one-expr))
				(generate-expr-from-search-head expr (second search-result)))))
		  search-results
		  )))
	))

;; FUNCTION: creates nicely formatted assertions
;; PRE: search-results in the format ((subclass ((search-result1) (search-result2)...)) (subclass ...))
;; Note: no need to worry about aggregates b/c heads are not aggregates unless in queries
(defun generate-assertion-expr-from-specialize-head (expr search-results)
  (append-list 
   (mapcar #'(lambda (search-result)
	       (let ((new-class (first search-result))
		     )
		 (mapcar #'(lambda (one-expr)
			     (if new-class
				 (replace-km-expr-head-with-class 
				  one-expr
				  new-class)
			       one-expr))
			 (generate-expr-from-ls-search expr
						       (second search-result)))
		 ))
	   search-results)
   ))

;; there are two types of results: aggregate-search-results and non-aggregate
;; treat them differently
(defun generate-expr-from-specialize-tail (expr search-results)
  (cond ((and (aggregatep (extract-tail expr))
	      (aggregate-specialize-search-results? search-results))
					;(ls-objectp (extract-tail expr)))
	 (let* ((aggregate (extract-tail expr))
		(final-aggregate (list (first aggregate)))
		(tmpexpr nil)
		(tmpanswers nil))
	   (dotimes (i (length (cdr aggregate)));; the first elemnt of aggregate is :set so ignore
		    ;; first generate the expr one at a time
		    (setf tmpexpr
			  (replace-km-expr-tail
			   expr
			   (nth (+ 1 i) aggregate)))
		    (setf tmpanswers
			  (generate-expr-from-specialize-tail
			   tmpexpr (nth i search-results))
			  )
		    (setf final-aggregate
			  (append-element
			   final-aggregate
			   (extract-tail
			    (prompt-for-clarification
			     tmpexpr
			     tmpanswers))))
		    )
	   ;; must make it list because rest of gen-exp cases return a list of interpretations
	   (list (replace-km-expr-tail expr final-aggregate))
	   ))
	(t
	 (append-list
	  (mapcar #'(lambda (search-result)
		      (generate-expr-from-ls-search expr
						    (second search-result)
						    (first search-result)
						    t))
		  search-results)))
	))

;; extract the filler for slot
;; PRE: expr is an assertion and it's pure assertional and doesn't contain multiple value filler
(defun search-path-after-slot (path slot)
  (cdr (member slot path))
  )

(defun search-path-til-slot (path slot)
  (let ((retval nil))
    (dotimes (i (length path))
	     (if (equal (nth i path)
			slot)
		 (return (append
			  retval
			  (list
			   (nth i path)
			   (nth (+ 1 i) path))))
	       (setf retval
		     (append-element retval (nth i path)))))
    ))

;; deals w/ assertion expr only		 
(defun generate-expr-from-ls-search (expr search-results &optional (new-tail-class nil) (from-tail? nil))
  (let* ((orig-head-extracted (extract-head expr))
	 (orig-tail (extract-tail expr))
	 (orig-head-inst (extract-head-instance expr))
	 (orig-tail-inst (extract-tail-instance expr))
	 )
    (when (not (null new-tail-class))
      (setf orig-tail
	    (replace-km-expr-head-with-class
	     orig-tail new-tail-class)))
    (mapcar #'(lambda (search-result)
		(cond
		 ((and (< (length (third search-result)) 3)
		       (null new-tail-class))
		  expr 
		  )
		 ((< (length (third search-result)) 3)
		  (replace-km-expr-tail 
		   expr orig-tail))
		 (t
		  (let* ((access-path 
			  (gen-access-path search-result))
			 (newexpr;; replace the new head w/ orig in case the orig is an instance 
			  ;; why always head? b/c replace tail in the next few lines
			  (if;;if it is an instance
			      (and (not (consp orig-head-extracted))
				   orig-head-extracted);; orig-head not nil
			      (replace-km-expr-head-with-inst
			       (gen-good-km-expr-from-access-path access-path orig-head-inst orig-tail-inst from-tail?)
			       orig-head-extracted)
			    (replace-km-expr-head
			     (gen-good-km-expr-from-access-path access-path orig-head-inst orig-tail-inst from-tail?)
			     orig-head-extracted)))
			 (partial-reversed-tail (if (and (km-assertionp orig-tail) 
							 (= (length (extract-concept-body orig-tail)) 1))
						    (partial-reverse-assertion-expr orig-tail))))
		    ;; need to do the find-access-path thing
		    (if (and
			 (not (null partial-reversed-tail))
			 (find-access-path 
			  (replace-km-expr-end 
			   newexpr
			   (extract-head orig-tail))
			  partial-reversed-tail))
			(progn
			  ;; bring the partially reversed tail's properties into the new one
			  (setf access-path;; first cut the last part of gen path
				(butlast (gen-access-path search-result) 2))
			  (setf newexpr;; replace the new head w/ orig in case the orig is an instance 
				;; why always head? b/c replace tail in the next few lines
				(if;;if it is an instance
				    (and (not (consp orig-head-extracted))
					 orig-head-extracted);; orig-head not nil
				    (replace-km-expr-head-with-inst
				     (gen-good-km-expr-from-access-path access-path orig-head-inst orig-tail-inst from-tail?)
				     orig-head-extracted)
				  (replace-km-expr-head
				   (gen-good-km-expr-from-access-path access-path orig-head-inst orig-tail-inst from-tail?)
				   orig-head-extracted)))
			  (replace-km-expr-end
			   newexpr
			   partial-reversed-tail)
			  )
		      (replace-km-expr-end
		       newexpr
		       orig-tail))
		    ))))
	    search-results)
    ))

(defun has-value (inst goals slot)
  (declare (ignore goals))
  (and (kb-objectp inst)
       (ls-km `(|the| ,slot |of| ,inst)))
  )

(defun filter-ls-search (head slot tail search-results excluded-slots required-slots)
  (let ((is-query? (null tail)))
    (when (and (not is-query?);;assertion
	       slot);; not :nn case when slots is unspecified
      ;; now filter out the ones that don't contain the required slot
      (setf search-results
	    (remove-if-not #'(lambda (element)
			       (intersection required-slots
					     (third element) :test #'equal))
			   search-results)))
    (when (not is-query?)
      ;; here's a strange filter: search result's tail is named-inst, and it diffs from tail
      ;; then ignore
      ;; for cases like: given *breakfast--before--Time-Interval3 find Time-Interval3 ? *lunch
      ;; and remove found path Time-Interval3--before--*breakfast
      (setf search-results
	    (remove-if #'(lambda (element)
			   (or
			    (and (is-named-instance (second element))
				 (is-named-instance tail)
				 (not (equal (second element)
					     tail)))
			    (and (is-named-instance (first (third element)))
				 (is-named-instance head)
				 (not (equal (first (third element))
					     head)))))
		       search-results)))
    (when excluded-slots
      ;; this filter is to exclude circular paths, e.g. looking for X-r-Y, and find, X-r-Z-r'-X, where r' is the inverse of r
      (setf search-results
	    (remove-if #'(lambda (element)
			   (and (ls-slotp (second (third element)))
				(member (second (third element)) excluded-slots :test #'equal)))
		       search-results)))
    #|(when (null slot)
      ;; remove reflexive paths for NN interpretation
      (setf search-results
	    (remove-reflexive-search-results search-results))
      )|#
    search-results
    ))

(defun ls-search (head slot tail &key (search-constraints? nil) (excluded-slots nil) (query-stop-criteria slot))
  (let ((cached-results (ls-lookup-search-cache head slot tail search-constraints? excluded-slots query-stop-criteria)))
    (if (eql cached-results 'not-cached)
	(ls-search-wo-cache head slot tail :search-constraints? search-constraints? :excluded-slots excluded-slots :query-stop-criteria query-stop-criteria)
      cached-results
      )))

;; given a path, add the weights to it
(defun ls-add-cost-to-path (path)
  (let ((retval (list (car path))))
    (setf path (cdr path))
    (dotimes (n (/ (length path) 2))
      (setf retval
	    (append retval
		    (list (nth (* 2 n) path)
			  (nth (+ 1 (* 2 n)) path)
			  1))))
    retval
    ))

(defun find-search-cache (head slot tail search-constraints? excluded-slots query-stop-criteria)
  (let* ((head-class (first (ls-km `(|the| |instance-of| |of| ,head))))
	 (tail-class (first (ls-km `(|the| |instance-of| |of| ,tail))))
	 (head-classes (if (aggregatep head) (list head) (cons head-class (all-superclasses head-class))))
	 (tail-classes (if (aggregatep tail) (list tail) (cons tail-class (all-superclasses tail-class))))
	 (cross-products (get-cross-prod head-classes tail-classes #'list))
	 (search-results nil)
	 (retval nil)
	 )
    (setf search-results
	  (mapcar #'(lambda (pair)
		      (list (first pair) slot (second pair) search-constraints? excluded-slots query-stop-criteria
			    (gethash (list (first pair) slot (second pair) search-constraints? excluded-slots query-stop-criteria)
				     *ls-search-cache* 'not-cached)))
		  cross-products))
    ;;(format t "search-results = ~S~%" search-results)
    (cond
     ;; as long as any of its superclasses' previous search is successful, we return the success results
     ((setf retval
	    (find-if #'(lambda (element)
			 (and (not (eql (last-element element) 'not-cached))
			      (successful-search? (last-element element))))
		     search-results))
      retval)
     ;; the exact head+slot+tail's previous search is failure, we return the failed results
     ((setf retval
	    (find-if #'(lambda (element)
			 (and (eql (first element) head-class) (eql (third element) tail-class)
			      (null (last-element element))))
		     search-results))
      retval)
     (t
      nil))
    ))

(defun ls-lookup-search-cache (head slot tail search-constraints? excluded-slots query-stop-criteria)
  (let* ((cache-entry
	  (find-search-cache head slot tail search-constraints? excluded-slots query-stop-criteria))
	 (cache-entry-head (first cache-entry))
	 (cache-entry-tail (third cache-entry))
	 (cached-results (last-element cache-entry)))
    ;;(format t "cached-entry = ~S~%" cache-entry)
    (incf (gethash 'total-cache-lookup *ls-profiling-data* 0))
    (cond
     (cached-results
      (incf (gethash 'total-cache-hit *ls-profiling-data* 0))
      ;; now need to turn the paths into instances
      (if (and (ls-inst-isa-class head cache-entry-tail)
	       (ls-inst-isa-class tail cache-entry-head))
	  ;; reverse the results paths
	  (setf cached-results
		(mapcar #'(lambda (search-result)
			    (list (first search-result)
				  (second search-result)
				  (ls-add-cost-to-path (remove-if #'numberp (reverse-path (third search-result))))))
			cached-results)))
      (mapcar #'(lambda (search-result)
		  (ls-cached-result-to-path search-result))
	      cached-results))
     (cache-entry
      (incf (gethash 'total-cache-hit *ls-profiling-data* 0))
      cached-results)
     (t
      (incf (gethash 'total-cache-miss *ls-profiling-data* 0))
      'not-cached))
    ))

;; each cached result is of the form: (SUCCESS |Chemical-Entity| (|Chemical-Entity| |is-basic-structural-unit-of| |Chemical| 0 |raw-material-of| |Reaction| 1)), head, tail look like: |_NaOH4159| |_Reaction4160|
;; return
(defun ls-cached-result-to-path (cached-result)
  (list (first cached-result)
	(first (ls-km `(|a| ,(second cached-result))))
	(mapcar #'(lambda (path-element)
		    (if (and (clib-concept? path-element)
			     (not (ls-slotp path-element))
			     (not (numberp path-element)))
			(first (ls-km `(|a| ,path-element)))
		      path-element))
		(third cached-result)))
  )

;; element1 is an instance, element2 is a class
;; returns true is element1 isa element2
(defun ls-inst-isa-class (element1 element2)
  (cond ((or (null element2) (null element1))
	 (eql element1 element2))
	(t
	 (ls-km `(,element1 |isa| ,element2))))
  )

;; element1 is an instance, element2 is a class
;; returns true is element2 is-subsumed-by the class of element1
(defun ls-class-isa-inst (element1 element2)
  (let ((classes1 nil)
	(superclasses2 nil))
    (cond ((or (null element2) (null element1))
	   (eql element1 element2))
	  (t
	   (if (aggregatep element2)
	       (setf superclasses2 '(|Aggregate|))
	     (setf superclasses2 (cons element2 (ls-km `(|the| |all-superclasses| |of| ,element2)))))
	   (setf classes1 (ls-km `(|the| |classes| |of| ,element1)))
	   (intersection classes1 superclasses2 :test #'eql)))
    ))

;; a cache is hit if one of the conditions is true (1 is the given inst, and 2 is the one in cache)
;; 1. 1 is a superclass of a specific search that failed (i.e. head2 isa head1 slot1=slot2 and tail2 isa tail1 or head2 isa tail1 slot1= inverse slot2 and tail2 isa head1)
;; 2. 1 is a subclass of a general search that worked (i.e. [head1 isa head2 slot1=slot2 and tail1 isa tail2] or [head1 isa tail2 slot1= inverse slot2 and tail1 isa head2] and results are successful)
(defun ls-compare-searches (search1 search2)
  (or
   (and ;; condition 1
    (eql (fourth search1) (fourth search2))
    (eql (fifth search1) (fifth search2))
    (eql (sixth search1) (sixth search2))
    (not (successful-search? (seventh search2)))
    (or (and (ls-class-isa-inst (first search1) (first search2))
	     (ls-class-isa-inst (third search1) (third search2))
	     (eql (second search1) (second search2)))
	(and (ls-class-isa-inst (first search1) (third search2))
	     (ls-class-isa-inst (third search1) (first search2))
	     (eql (second search1) (first (ls-km `(|the| |inverse| |of| ,(second search2)))))))
    )
   (and ;; condition 2
    (eql (fourth search1) (fourth search2))
    (eql (fifth search1) (fifth search2))
    (eql (sixth search1) (sixth search2))
    (successful-search? (seventh search2))
    (or (and (ls-inst-isa-class (first search1) (first search2))
	     (ls-inst-isa-class (third search1) (third search2))
	     (eql (second search1) (second search2)))
	(and (ls-inst-isa-class (first search1) (third search2))
	     (ls-inst-isa-class (third search1) (first search2))
	     (eql (second search1) (first (ls-km `(|the| |inverse| |of| ,(second search2)))))))
    )
   )
  )

(defun ls-clear-cache ()
  (clrhash *ls-search-cache*)
  (clrhash *ls-slot-domains-cache*)
  (clrhash *ls-slot-val-cache*)
  )

(defun ls-store-cache (head slot tail search-constraints? excluded-slots query-stop-criteria search-results)
  (cond
   ((and (successful-search? search-results)
	 (> (length (third (first search-results))) 2))
    (ls-store-successful-long-search-cache head slot tail search-constraints? excluded-slots query-stop-criteria search-results))
   ((not (successful-search? search-results))
    (ls-store-failed-search-cache head slot tail search-constraints? excluded-slots query-stop-criteria search-results))
   ))

;; given two lists
;; returns a list of the cross products after an operation is performed on the pairs of elements 
(defun get-cross-prod (X Y operator)
  (reduce #'append 
	  (mapcar 
	   #'(lambda (x1) 
	       (mapcar #'(lambda (y1) 
			   (funcall operator x1 y1)) 
		       Y)) 
	   X)))

(defun ls-store-failed-search-cache (head slot tail search-constraints? excluded-slots query-stop-criteria search-results)
  (let* (
	 (head-classes (if (aggregatep head) (list head) (all-classes head)))
	 (tail-classes (if (aggregatep tail) (list tail) (all-classes tail)))
	 (cross-products (get-cross-prod head-classes tail-classes #'list))
	 )
    (dolist (pair cross-products)
      (setf 
       (gethash
	(list (first pair) slot (second pair) search-constraints? excluded-slots query-stop-criteria)
	*ls-search-cache*)
       search-results))
    ))

(defun ls-class1-subsumes-class2 (c1 c2)
  (or 
   (eql c2 c1)
   (member c1 (all-superclasses c2) :test #'eql))
  )

;; given the search results,
;; extract it into class-slot-class-slot paths
;; and save it to the cache
(defun ls-store-successful-long-search-cache (head slot tail search-constraints? excluded-slots query-stop-criteria search-results)
  (let ((new-results (extract-class-slot-path search-results))
	(head-classes (ls-km `(|the| |instance-of| |of| ,head)))
	(tail-classes (ls-km `(|the| |instance-of| |of| ,tail)))
	(new-head nil) ;; new-head and tail are based on the input search
	(new-tail nil)
	(new-head2 nil) ;; new-head2 and tail2 are based on the actual found path
	(new-tail2 nil))
    ;; new-head is the beginning of the search path
    ;; new-tail is the end of the search path
    (setf new-head2 (first (third (first new-results))))
    (if (= 1 (length head-classes))
	(setf new-head (first head-classes))
      (setf new-head (find-if #'(lambda (c)
				  (or ;; c subsumes result's head
				   (ls-class1-subsumes-class2 c
							      (first (third (first new-results))))
				   (ls-class1-subsumes-class2  (first (third (first new-results)))
							       c)))
			      head-classes)))

    (setf new-tail2 (first (last (third (first new-results)) 2)))
    (if (= 1 (length tail-classes))
	(setf new-tail (first tail-classes))
      (setf new-tail (find-if #'(lambda (c)
				  (or ;; c subsumes result's tail
				   (ls-class1-subsumes-class2 c
							      (first (last (third (first new-results)) 2)))
				   (ls-class1-subsumes-class2  (first (last (third (first new-results)) 2))
							       c)))
			      tail-classes)))
    ;;(format t "new-head = ~S new-tail = ~S~%" new-head new-tail)
    ;;should replace the found path with the orig input classes
    (setf
     (gethash
      (list new-head slot new-tail search-constraints? excluded-slots query-stop-criteria)
      *ls-search-cache*)
     (mapcar #'(lambda (one-result)
		 (list (first one-result)
		       new-tail
		       (cons new-head
			     (substitute new-tail new-tail2 (cdr (third one-result)) :from-end t :test #'equal :count 1))))
      new-results))
    (setf
     (gethash
      (list new-head2 slot new-tail2 search-constraints? excluded-slots query-stop-criteria)
      *ls-search-cache*)
      new-results)
    ))

;; each result is of the format (SUCCESS |_Chemical-Entity4139| (|_Chemical-Entity4139| |is-basic-structural-unit-of| |_Chemical4136| 0 |raw-material-of| |_Reaction4132| 1))
;; return (SUCCESS |Chemical-Entity| (|Chemical-Entity| |is-basic-structural-unit-of| |Chemical| 0 |raw-material-of| |Reaction| 1))
(defun extract-one-class-slot-path (search-result)
  (list
   (first search-result)
   (first (ls-km `(|the| |instance-of| |of| ,(second search-result))))
   (mapcar #'(lambda (inst)
	       (if (and (ls-inst? inst)
			(not (ls-slotp inst)))
		   (first (ls-km `(|the| |instance-of| |of| ,inst)))
		 inst)
	       )
	   (third search-result)))
  )

(defun extract-class-slot-path (search-results)
  (mapcar #'extract-one-class-slot-path
	  search-results)
  )

(defun ls-search-wo-cache (head slot tail &key (search-constraints? nil) (excluded-slots nil) (query-stop-criteria slot))
  (let ((search-results nil))
    (cond 
     #|((and (null slot) ;; if both the slot and either head/tail is nil, then nothing to search for
     (or (null head)
     (null tail)))
     nil)|#
     ((and 
       (not (null tail))
       (not (null head))
       (or (null slot) 
	   (property-slot? slot))
       (or 
	(property-concept? tail)
	(property-concept? head)) ;; for cases like _Velocity-Value3 nil _Entity5
       ;; (property-concept? head)) ;; why the head cannot be property?
       )
      (if (property-concept? tail)
	  (setf search-results (ls-search-by-property head tail slot))
	(setf search-results 
	      (mapcar #'(lambda (search-result)
			  (list (first search-result) ;; success or failure
				(last-element (remove-if-not #'kb-objectp (third search-result))) 
				(reverse-search-path (third search-result) 0)))
		      (ls-search-by-property tail head slot))))
      (ls-store-cache head slot tail search-constraints? excluded-slots query-stop-criteria search-results)
      search-results
      )
     (t
      (setf search-results (ls-search0 head slot tail :search-constraints? search-constraints? :excluded-slots excluded-slots :query-stop-criteria query-stop-criteria))
      (ls-store-cache head slot tail search-constraints? excluded-slots query-stop-criteria search-results)
      search-results))
    ))

(defun ls-search0 (head slot tail &key (search-constraints? nil) (excluded-slots nil) (query-stop-criteria slot))
  (let* ((init-unit (cond ((null head) head)
			  ((consp head)
			   (ls-km head))
			  (t
			   (list head))))
	 (goal-unit (cond ((null tail)
			   tail)
			  ((consp tail)
			   (ls-km tail))
			  (t
			   (list tail))))
	 (orig-slot slot)
	 (required-slots 
	  (remove-duplicates
	   (cons orig-slot
		 (append
		  (ls-km `(|the| |all-subslots| |of| ,orig-slot))
		  (ls-km `(|the| |inverse| |of| (|the| |all-subslots| |of| (|the| |inverse| |of| ,orig-slot)))))
		 )))
	 (searchable-slots (all-searchable-slots))
	 (search-results nil)
	 (search-head nil)
	 (stop-criteria #'is_super_or_subclass)
	 (is-query? (null tail))
	 (min-depth (if is-query? 1 *ls-min-search-depth*)));; queries shouldn't find itself as the answer, so min depth 1
    (when is-query?
      ;; it's a query
      (if (functionp query-stop-criteria)
	  (setf stop-criteria query-stop-criteria)
	(setf stop-criteria #'(lambda (inst goals) (has-value inst goals query-stop-criteria)))))
    (when (and (clib-inst? (first goal-unit))
	       (clib-inst? (first init-unit)))
      ;; first try to search from 1 end to the other
      (setf search-results
	    (ls-kb-search goal-unit searchable-slots init-unit stop-criteria :min-depth min-depth :multiple-answers? t :search-constraints? search-constraints?))
      (setf search-results
	    (successful-search? search-results))
      ;; also need to reverse the results
      (setf search-results
	    (mapcar #'(lambda (search-result)
			(list (first search-result)
			      (last-element (remove-if-not #'kb-objectp (third search-result)))
			      (reverse-search-path (third search-result) 0)))
		    search-results))
      (setf search-results 
	    (filter-ls-search head slot tail search-results excluded-slots required-slots))
      (if search-results
	  (incf (gethash 'search-tail-used *ls-profiling-data* 0)))
      (setf search-head
	    (ls-kb-search init-unit searchable-slots goal-unit stop-criteria :min-depth min-depth :multiple-answers? t :search-constraints? search-constraints?))
      (setf search-head
	    (successful-search? search-head))
      (setf search-head (filter-ls-search head slot tail search-head excluded-slots required-slots))
      (if search-head
	  (incf (gethash 'search-head-used *ls-profiling-data* 0)))
      (setf search-results
	    (append search-results search-head))
      (when (null search-results)
	(setf search-results
	      (bi-dir-search
	       init-unit
	       searchable-slots
	       goal-unit
	       stop-criteria
	       :multiple-answers? t
	       :search-constraints? search-constraints?))
	(setf search-results
	      (successful-search? search-results))
	(setf search-results (filter-ls-search head slot tail search-results excluded-slots required-slots))
	(when search-results
	  (incf (gethash 'bi-dir-search-used *ls-profiling-data* 0)))
	)
      #|(rank-ls-search-results
     search-results orig-slot required-slots (first init-unit) (first goal-unit))|#
      search-results
      )))

;; similar to ls-path-length, but give no discount to valency slots
(defun ls-path-literal-length (path)
  (let ((slots (remove-if-not #'ls-slotp (flatten-list path))))
    (length slots))
  )

;; given a path
;; return a length value of the path
(defun ls-path-length (path)
					;(length path)
  (let ((slots (remove-if-not #'ls-slotp (flatten-list path))))
    (cond
     ((null slots)
      0)
     (t
      (eval
       (cons '+
	     (mapcar #'(lambda (slot)
			 (if (is-valency slot)
			     *valency-cost*
			   1)) slots))
       )))
    ))

(defun rank-ls-generated-exprs (exprs orig-slot required-slots init-unit goal-unit)
  (sort exprs
	#'(lambda (expr1 expr2)
	    (compare-two-search-results
	     expr1
	     expr2
	     orig-slot
	     required-slots
	     init-unit
	     goal-unit)
	    ))
  )

;; given 2 search paths or exprs, compare which is better
(defun compare-two-search-results (path1 path2 orig-slot required-slots init-unit goal-unit &optional (found-goal1 0) (found-goal2 0))
  (declare (ignore init-unit))
  (let* (
	 (slot-used1 (intersection (flatten-list path1) required-slots))
	 (slot-used2 (intersection (flatten-list path2) required-slots))
	 slot-dist1 slot-dist2 path-len1 path-len2 taxonomical-dist1 taxonomical-dist2)
    (cond 
     ((and goal-unit;; this sorting is only valid for assertions
	   orig-slot;; and not valid for :nn types
	   (/= (setf slot-dist1 
		     (eval (cons 'min
				 (mapcar #'(lambda (slot)
					     (slot-dist orig-slot slot))
					 slot-used1)
				 )))
	       (setf slot-dist2 
		     (eval (cons 'min
				 (mapcar #'(lambda (slot)
					     (slot-dist orig-slot slot))
					 slot-used2)
				 ))
		     )))
      (< slot-dist1 slot-dist2))
     ((/= (setf path-len1 (ls-path-length path1))
	  (setf path-len2 (ls-path-length path2)))
      (< path-len1 path-len2))
     ((and goal-unit;; only valid for assertions, queries are excluded
	   found-goal1
	   found-goal2)
      (setf taxonomical-dist1
	    found-goal1)
      (setf taxonomical-dist2
	    found-goal2)
      (< taxonomical-dist1 taxonomical-dist2))
     )
    ))

;; now rank search results: based on 3 criteria:
;; 1> contains the slot itself instead of subslots
;; 2> short paths
;; 3> taxonomical distance
(defun rank-ls-search-results (results orig-slot required-slots init-unit goal-unit)
  (mapcar #'(lambda (result)
	      (list (first result)
		    (second result)
		    (third result)));; remove the tax dist info
	  (sort (remove-duplicates results :test #'equal)
		#'(lambda (result1 result2)
		    (compare-two-search-results
		     (third result1)
		     (third result2)
		     orig-slot
		     required-slots
		     init-unit
		     goal-unit 		    
		     (fourth result1)
		     (fourth result2)
		     ))
		)
	  ))

(defun slot-dist (orig-slot descendant)
  (let ((counter 0)
	(open (list orig-slot))
	(one-level nil)
	(done nil)
	)
    (loop 
     while (not done)
     do
     (setf one-level open)
     (setf open nil)
     (cond 
      ((null one-level)
       (setf done 'failed))
      ((member descendant one-level)
       (setf done t))
      (t
       (progn
	 (dolist (node one-level)
		 (setf open
		       (append open
			       (ls-km `(|the| |subslots| |of| ,node)))))
	 (incf counter))
       )))
    (if (equal done 'failed)
	(slot-dist
	 (first (ls-km `(|the| |inverse| |of| ,orig-slot)))
	 (first (ls-km `(|the| |inverse| |of| ,descendant))))
      counter)
    ))

;; test: 
;; (search-head '(|a| |H2O-Substance| |with| (|has-chemical-formula| ((|a| |Chemical-Formula|)))))
;; (search-head '(|the| |has-chemical-formula| |of| (|a| |H2O-Substance|)))
(defun search-head (expr &key (stop-condition #'search-head-stop-condition))
  (cond 
   ((aggregatep (extract-head expr))
    (mapcar #'(lambda (element)
		(search-head0 (replace-km-expr-head expr element) stop-condition)
		)
	    (rest (extract-head expr)))
    )
   (t
    (search-head0 expr stop-condition)))
  )

(defun search-head0 (expr stop-condition)
  (let* ((init-unit (extract-head-instance expr))
	 ;; (slots (cons '|value| (append (all-instances '|Relation|) (all-instances '|Property|))))
	 (slots (all-searchable-slots))
	 (result nil)
	 )
    (setf result
	  (ls-kb-search (list init-unit)
			slots
			nil;; goal units are not relevant here
			;; check to see if an instance have preexisting path
			;; have to be a lambda function here to access expr
			#'(lambda (current-inst goals) 
			    ;; if the current-inst is not a (:seq ..) type then 
			    ;; first replace the expr head with curr inst
			    ;; and then check to see if new expr has preexisting path
			    (funcall stop-condition current-inst goals expr)
			    ) :multiple-answers? t)
	  )
    (setf result
	  (successful-search? result))
    (setf result
	  (remove-reflexive-search-results result))
					;profiling
    (if result
	(incf (gethash 'search-head *ls-profiling-data* 0)))
    ))

;; given the results of specialize-head or specialize-tail
;; recognized if any subclasses are used
(defun has-specialized (search-results)
  (if (aggregate-specialize-search-results? search-results)
      (not (null (car (first (first search-results)))))
    (not (null (car (first search-results))))
    )
  )

;;(defun specialize-head (expr &optional (forced-property? nil) (query-stop-criteria #'(lambda (inst goals) (has-value inst goals (extract-first-slot expr)))))
(defun specialize-head (expr &optional (forced-property? nil) (query-stop-criteria (extract-first-slot expr)))
  (let ((search-results nil))
    (cond 
     ((and (aggregatep (extract-head expr))
	   (ls-value-type (extract-head expr))
	   (setf search-results
		 (specialize-head0 expr forced-property? query-stop-criteria)))
      search-results)
     ((and (aggregatep (extract-head expr))
	   (ls-objectp (extract-head expr)))
      (mapcar #'(lambda (element)
		  (specialize-head0 (replace-km-expr-head expr element) forced-property? query-stop-criteria))
	      (rest (extract-head expr)))
      )
     (t
      ;; includes cases like (the quantity-of of (:pair 1 *mole))
      (specialize-head0 expr forced-property? query-stop-criteria))
     )
    ))
  
;; test: (specialize-head '(|a| |Reaction| |with| (|ki| ((|a| |Equilibrium-Constant-Value|)))))
(defun specialize-head0 (expr forced-property? query-stop-criteria)
  (let* ((init-class (extract-head-class expr))
	 (orig-slot (extract-first-slot expr))
	 (tail (extract-tail expr))
	 (slots (list '|subclasses|))
	 (domain (first (get-domain-of-slot orig-slot)))
	 (domain-inst nil)
	 (final-result nil)
	 (classes-examined nil)
	 (subclasses-searched 0))
    (if init-class
	(ls-kb-search
	 (list init-class)
	 slots
	 nil
	 #'(lambda (current-class goals)
	     (declare (ignore goals))
	     (cond
	      ((member current-class classes-examined :test #'equal)
	       )
	      ((< subclasses-searched *ls-max-specialization*)
	       (incf subclasses-searched)
	       (setf classes-examined
		     (cons current-class classes-examined))
	       (let* ((new-head
		       ;; use the orig head if the class is the init class
		       (if (equal current-class init-class)
			   (extract-head-instance expr)
			 (first (ls-km `(|a| ,current-class)))))
		      (search-result nil)
		      (specialized-class
		       (if (equal current-class init-class)
			   nil;; no specialization takes place
			 current-class)
		       )
		      (proto-instances nil))
		 (setf search-result (ls-search new-head orig-slot tail :query-stop-criteria query-stop-criteria))
		 ;; added for the cases of anonymous class as in airplane KB
		 #|(when (and (not (successful-search? search-result))
			    (atom current-class)) ;; current class should not be an aggregate
		   (setf proto-instances
			 (mapcar #'(lambda (prototype)
				     (clone prototype))
				 (ls-km `(|the| |prototypes| |of| ,current-class))))
		   (loop while (and (not (successful-search? search-result))
				    (not (null proto-instances)))
			 do
			 (setf search-result (ls-search (car proto-instances) orig-slot tail :query-stop-criteria query-stop-criteria))
			 (setf proto-instances (cdr proto-instances)))
		   )|#
		 (when (and forced-property?
			    (not (successful-search? search-result)))
		   ;; force the head inst to have the property of the domain of the slot
		   ;; and have a desperate attempt to find something
		   (setf domain-inst (first (ls-km `(|a| ,domain))))
		   ;; in case domain is a subclass of current-class)
		   (if (ls-km `(,domain-inst |isa| ,current-class))
		       (setf specialized-class domain))
		   (ls-km `(,domain-inst |has| (,orig-slot (,tail))))
		   (setf search-result
			 (mapcar #'(lambda (result)
				     ;; need to distinguish between query and assertion
				     (cond ((null tail);; query
					    result)
					   (t;; assertion
					    (list 
					     (first result)
					     tail
					     (append
					      (third result)
					      (list orig-slot tail))))
					   ))
				 (ls-search new-head nil domain-inst 
					    :search-constraints? forced-property?
					    :excluded-slots (when (ls-1-to-n-slotp orig-slot) (ls-km `(|the| |inverse| |of| ,orig-slot)))
					    :query-stop-criteria query-stop-criteria
					    ));; any slot will do
			 ))
		 (if (successful-search? search-result)
		     (setf final-result
			   (append-element final-result
					   (list specialized-class 
						 search-result)))
		   )
		 ))))
	 :max-depth *ls-head-specialization-bound*
	 :multiple-answers? t
	 :min-depth 0
	 ))
    (sort-based-on-prosperity final-result)
    ))

(defun specialize-tail (expr &optional (forced-property? nil))
  (let ((search-results nil))
    (cond 
     ((and (aggregatep (extract-tail expr))
	   (ls-value-type (extract-tail expr))
	   (setf search-results
		 (specialize-tail0 expr forced-property?)))
      search-results)
     ((and (aggregatep (extract-tail expr))
	   (ls-objectp (extract-tail expr)))
      (mapcar #'(lambda (element)
		  (specialize-tail0 (replace-km-expr-tail expr element) forced-property?))
	      (rest (extract-tail expr))))
     (t
      ;; includes cases like (a Chemical with (quantity ((:pair 1 *mole))))
      (specialize-tail0 expr forced-property?)))
    ))

(defun specialize-tail0 (expr forced-property?)
  (let* ((init-class (extract-tail-class expr))
	 (orig-slot (extract-first-slot expr))
	 (head (extract-head-instance expr))
	 (slots (list '|subclasses|))
	 (range (first (ls-km `(|the| |range| |of| ,orig-slot))))
	 (range-inst nil)
	 (final-result nil)
	 (classes-examined nil)
	 (subclasses-searched 0))
    (if init-class
	(ls-kb-search
	 (list init-class)
	 slots
	 nil
	 #'(lambda (current-class goals)
	     (declare (ignore goals))
	     (cond
	      ((member current-class classes-examined :test #'equal)
	       )
	      ((< subclasses-searched *ls-max-specialization*)
	       (incf subclasses-searched)
	       (setf classes-examined 
		     (cons current-class classes-examined))
	       ;; (format t "current-class = ~S init-class = ~S~%" current-class init-class)
	       (let* ((new-tail
		       (if (equal current-class init-class)
			   (extract-tail-instance expr)
			 (first (ls-km `(|a| ,current-class)))
			 #|(extract-tail-instance ;; the old code adds multiple classes to the orig tail
			    (replace-km-expr-tail-with-class ;; as each one is being examined
			   expr current-class))|#))
		      (search-result nil)
		      (specialized-class
		       (if (equal current-class init-class)
			   nil;; no specialization takes place
			 current-class))
		      (proto-instances nil)
		      )
		 (setf search-result
		       (ls-search head orig-slot new-tail))
		 ;; added for the cases of anonymous class as in airplane KB
		 #|(when (and (not (successful-search? search-result))
			    (atom current-class)) ;; current class should not be an aggregate
		   (setf proto-instances
			 (mapcar #'(lambda (prototype)
				     (clone prototype))
				 (ls-km `(|the| |prototypes| |of| ,current-class))))
		   (loop while (and (not (successful-search? search-result))
				    (not (null proto-instances)))
			 do
			 (setf search-result (ls-search head orig-slot (car proto-instances)))
			 (setf proto-instances (cdr proto-instances)))
		   )|#
		 (when (and forced-property?
			    (not (successful-search? search-result)))
		   ;; now very desperate, just assert that 
		   (setf range-inst (first (ls-km `(|a| ,range))))
		   ;; in case range is a subclass of current-class)
		   (if (ls-km `(,range-inst |isa| ,current-class))
		       (setf specialized-class range))
		   (ls-km `(,head |has| (,orig-slot (,range-inst))))
		   (setf search-result
			 ;; need to append the path
			 (mapcar #'(lambda (result)
				     (list
				      (first result)
				      (second result)
				      (append 
				       ;; insert a number in it to make the format right for non-specialization cases
				       (if (> (length (third result)) 2)
					   (list head orig-slot (car (third result)) 0)
					 (list head orig-slot (car (third result))))
				       (cdr (third result))))) 
				 (successful-search? (ls-search range-inst nil new-tail 
								:search-constraints? forced-property?
								:excluded-slots (when (ls-1-to-n-slotp orig-slot) (ls-km `(|the| |inverse| |of| ,orig-slot))))))))
		 (if (successful-search? search-result)
		     (setf final-result
			   (append-element final-result
					   (list specialized-class
						 search-result)))
		   )
		 ))
	      ))
	 :max-depth *ls-tail-specialization-bound*
	 :multiple-answers? t
	 :min-depth 0
	 ))
    (sort-based-on-prosperity final-result)
    ))

(defun ls-search-property (x y)
  (let* ((property (cond
		    ((and (property-concept? x)
			  (property-concept? y)
			  (property-constant? y))
		     y ;; if both property and one is a pair
		     )
		    ((and (property-concept? x)
			  (property-concept? y)
			  (property-constant? x))
		     x ;; if both property and one is a pair
		     )
		    ((property-concept? x)
		     x)
		    (t
			y)))
	 (concept (if (not (equal property x)) x y))
	 (swapped? (if (not (equal property x)) t nil))
	 (concept-inst nil)
	 (property-inst nil)
	 (interpretations nil)
	 )
    (format t "LS DEBUG: resolving property value ~S ~S~%" x y)
    (if (ls-inst? concept)
	(setf concept-inst concept)
      (setf concept-inst (first (ls-km `(|a| ,concept)))))
    (if (is-an-instance property)
	(setf property-inst property)
      (setf property-inst (first (ls-km `(|a| ,property))))) ;; in case property is Velocity-Value
    (setf interpretations (ls-search concept-inst nil property-inst))
    (setf interpretations
	  (mapcar #'(lambda (search-result)
		      (list (first search-result)
			    (last-element (remove-if-not #'kb-objectp (third search-result)))
			    (if swapped?
				(reverse-search-path (third search-result) 0)
			      (third search-result))))
		  interpretations))
    (remove-duplicates
     (gen-patterns interpretations)
     :test #'equal)
    ))

;; this happens when one is a non property, and the other is a generical property
;; e.g. _Entity3, _Property-Value4
(defun ls-cpl-entity-property-val (x y)
  (or (and (not (property-concept? x))
	   (equal (ls-km `(|the| |instance-of| |of| ,y)) '(|Property-Value|)))
      (and (not (property-concept? y))
	   (equal (ls-km `(|the| |instance-of| |of| ,x)) '(|Property-Value|))))
  )

;; this happens when x is a specific property val, y is a generic val
;; e.g x = _Height123 y = _Property-Value123 or vice versa
(defun ls-cpl-property-val-situation (x y)
  (and (ls-inst? x) (ls-inst? y)
       (ls-km `(,x |isa| |Property-Value|))
       (ls-km `(,y |isa| |Property-Value|))
       (or (and (equal (ls-km `(|the| |instance-of| |of| ,x)) '(|Property-Value|))
		(not (equal (ls-km `(|the| |instance-of| |of| ,y)) '(|Property-Value|))))
	   (and (equal (ls-km `(|the| |instance-of| |of| ,y)) '(|Property-Value|))
		(not (equal (ls-km `(|the| |instance-of| |of| ,x)) '(|Property-Value|))))
	   (equal 
	    (ls-km `(|the| |instance-of| |of| ,y)) (ls-km `(|the| |instance-of| |of| ,x)))))
  )

;; things such as (a Property-Value with (value ((:pair 1 *meter))))
(defun ls-property-value-value-pair (expr)
  (and 
   ;; assumed expr to be an assertion
   (consp expr)
   (not (eql (first expr) ':|nn|))
   (equal '(|value|) (extract-slots expr))
   (equal (extract-head-class expr) '|Property-Value|)
   ;;(ls-km `(,(extract-head-instance expr) |isa| |Property-Value|))
   ))

;; given X Y, interpret x y
(defun ls-search-nn (x y &optional (search-func 'search-3))
  (let ((search-results nil)
	(subclasses-searched 0))
    (cond ((and (not (ls-inst? x))
		(not (ls-inst? y)));; both x & y are classes
	   ;; do the seach by specializing x
	   ;; the following code is suspicious because it uses a side effect
	   ;; it doesn't present all possible specializations to user
	   (successful-search?
	    (ls-kb-search
	     (list x)
	     '(|subclasses|)
	     nil
	     #'(lambda (class goals)
		 (declare (ignore goals))
		 (when (< subclasses-searched *ls-max-specialization*)
		   (incf subclasses-searched)
		   (setf search-results (funcall search-func class y)))
		 )
	     :min-depth 0
	     :max-depth *ls-head-specialization-bound*
	     ))
	   (if (not search-results)
	       (successful-search?
		(ls-kb-search
		 (list y)
		 '(|subclasses|)
		 nil
		 #'(lambda (class goals)
		     (declare (ignore goals))
		     (when (< subclasses-searched *ls-max-specialization*)
		       (incf subclasses-searched)
		       (setf search-results (funcall search-func x class))))
		 :min-depth 0
		 :max-depth *ls-tail-specialization-bound*
		 )))
	   search-results
	   )
	  (t
	   (funcall search-func x y))
	  )
    ))

;; the form: concept is a P-Value, and property is a p-value
;; example: concept is _Property-Value3 property is (:pair 3 *meter)
(defun ls-interpret-property-P-value (concept property)
  (and 
   (and (property-concept? concept)
	(or (clib-concept? concept)
	    (clib-inst? concept)))
   (and (property-concept? property)
	(consp property)))
  )

(defun ls-interpret-property-non-value (property)
  (and (property-concept? property)
       (or (km `(,property |isa| |Property-Value|))
	   (km `(,property |is-subsumed-by| |Property-Value|)))
       )
  )

(defun ls-interpret-property-categorical (property)
  (and 
   (atom property)
   (ls-km `(,property |isa| |Constant|)))
  )

(defun ls-interpret-property-scalar (property)
  (and 
   (listp property)
   (= (length property) 3)
   (ls-km `(,(second property) |isa| |Constant|)))
  )

(defun ls-generate-property-head (inst)
  (cond
   ((ls-inst? inst)
    `(,inst |has|))
   (t
    `(|a| ,inst |with|)))
  )

;; implementation of the categorical property values
;; assume the first element is a categorical constant
;; the second element is a class or an instance
(defun ls-interpret-property (lsexpr)
  (cond 
   ((ls-cpl-entity-property-val (second lsexpr) (third lsexpr)) ;; special case3 for CPL
    (prompt-for-clarification lsexpr nil))
   (t
    (let ((interpretations (ls-search-property (second lsexpr) (third lsexpr))))
      (dolist (interpretation (ls-output-nn interpretations lsexpr))
	(setf (gethash interpretation *ls-generated-exprs*) t))
      (prompt-for-clarification lsexpr (ls-output-nn interpretations lsexpr))
      ))))

;; very similar to search-3, except different return results 
(defun ls-search-3-without-expr-returned (x y)
  (let* ((i1 (if (ls-inst? x) (list x) (ls-create-inst `(|a| ,x))))
	 (i2 (if (ls-inst? y) (list y) (ls-create-inst `(|a| ,y))))
	 (results (ls-search (first i1) nil (first i2))))
    (setf results (successful-search? results))
    (cond 
     ;; only the pick result with most specific
     ;; if x y has an isa relation
     ((and (= (length results) 2)
	   (= (length (third (first results))) 1)
	   (= (length (third (second results))) 1))
      (if (is-more-specific (second (first results)) (second (second results)))
	  (list (first results))
	(list (second results))))
     (t
      results))
    ))

;; given a domain find the slots whose domain include the given one
(defun ls-pick-slots-based-on-domains (slots domain)
  (remove 
   nil
   (mapcar #'(lambda (slot)
	       (if (member domain (get-domain-of-slot slot) :test #'eql)
		   slot))
	   slots))
  )
;; given a concept-inst and a list of domains, try to find how the concepts can be related to the domains
;; return a list of search paths relating the concept inst to the property through the domain instances
;; example: given _Chemical-Entity1 (Chemical Event) quantity _Quantity-Value2
;; return: ((success _Chemical-Entity1 (_Chemical-Entity1 is-basic-structural-unit-of _Chemical quantity _Quanitty-Value2)))
(defun ls-search-concept-inst-to-domains (concept-inst slots property)
  (let ((results nil)
	(tmp-results nil)
	(domains (ls-km `(|the| |domain| |of| ,(cons ':|set| slots))))
	(matching-slots nil)
	;;(concept-class (first (ls-km `(|the| |instance-of| |of| ,concept-inst))))
	)
    (dolist (domain domains)
      (when
	  (match-domains-or-ranges (list domain) concept-inst)
	(setf matching-slots (ls-pick-slots-based-on-domains slots domain))
	(setf results
	      (append results
		      (apply #'append
			     (mapcar #'(lambda (slot)
					 (list `(SUCCESS ,concept-inst (,concept-inst ,slot ,property 0.55))))
				     matching-slots))))))
    (when (null results)
	(dolist (domain domains)
	  (if (not (find '|Property-Group| (all-superclasses domain) :test #'eql))
	      (setf tmp-results (ls-search-nn concept-inst domain #'ls-search-3-without-expr-returned)))
	  (setf tmp-results (successful-search? tmp-results))
	  (setf matching-slots (ls-pick-slots-based-on-domains slots domain))
	  ;; add the slot paths
	  (dolist (slot matching-slots)
	    (setf results
		  (append results
			  (mapcar #'(lambda (result)
				      (list
				       (first result);;success
				       (second result);; concept-inst
				       (append 
					(third result)
					(if (> (length (third result)) 1)
					    `(,slot ,property ,(+ (last-element (third result)) 1)) ;; normal case
					  `(,slot ,property))))) ;; specialization case
				  tmp-results)))))
	)
    results
    ))

;; this handles the simple cases such as
;; 1. figure out the slot (:nn Entity Mass-Value)
;; 2. fill in property value (a Entity with (mass ((:pair 1 *gram))))
(defun ls-search-by-property (concept-inst property slot)
  (let ((results (ls-search-by-property0 concept-inst property))
	(primary-slot nil))
    (if (not (null slot))
	;; a slot has been specified, so filter results that don't contain slot
	(setf results (remove-if-not #'(lambda (result)
					 (intersection (flatten-list result) (list slot)))
				     results)))
    (if (and (ls-km `(,property |isa| |Property-Value|))
	     (setf primary-slot (first (ls-km `(|the| |primary-slot| |of| ,property)))))
	(setf results (sort results
			    #'(lambda (result1 result2)
				(if (member primary-slot
					    (flatten-list result1) :test #'eql)
				    t))
			    )))
    results
    ))

;; given a concept-inst, return the right property 
(defun ls-find-property-relation-by-constant (property)
  (let (const property-slots const-class)
    (cond
     ((ls-interpret-property-categorical property);; Entity->*red
      (setf const property)
      ;; get the right property slot
      (setf const-class
	    (first (ls-km `(|the| |instance-of| |of| ,const))))
      (setf property-slots
	    (remove nil
		    (mapcar #'(lambda (slot-x)
				(if (eql const-class (second slot-x)) (first slot-x)))
			    *ls-slot-property-cache*)))
      )
     ((ls-interpret-property-scalar property);; Entity->(:pair *hot Drink)
      (setf const (second property))
      ;; get the right property slot
      (setf const-class
	    (first (ls-km `(|the| |instance-of| |of| ,const))))
      (setf property-slots
	    (remove nil
		    (mapcar #'(lambda (slot-x)
				(if (eql const-class (third slot-x)) (first slot-x)))
			    *ls-slot-property-cache*)))
      )
     (t;; cardinal value
      (setf const (third property))
      ;; get the right property slot
      (setf const-class
	    (if (equal const '|nil|)
		'|UoM-Unitless|
	      (first (ls-km `(|the| |instance-of| |of| ,const)))))
      (setf property-slots
	    (if t
		;;(not (eql const-class '|UoM-Unitless|)) ;; let's not do anything for unitless values--why not?
		(remove nil
		    (mapcar #'(lambda (slot-x)
				(if (eql const-class (fourth slot-x)) (first slot-x)))
			    *ls-slot-property-cache*))))
      ))
    property-slots
    ))


;; it handles: Entity->(:pair 1 *meter), Entity->_Length-Value, _Property-Value->(:pair 1 *meter) cases
(defun ls-search-by-property0 (concept-inst property)
  (let (
	(property-slots nil)
	(concept-class (ls-km `(|the| |instance-of| |of| ,concept-inst)))
	;;(inst-superclasses nil)
	)
    ;;(setf inst-superclasses (ls-km `(|the| |all-superclasses| |of| (|the| |instance-of| |of| ,concept-inst)))) ;; inst is an inst
    ;;(setf inst-superclasses (cons ':|set| inst-superclasses))
    (cond
     ((ls-interpret-property-non-value property);; Entity->_Length-Value
      (if (not (ls-inst? property))
	  (setf property (first (ls-km `(|an| |instance| |of| ,property)))))
      (setf property-slots
	    (ls-km 
	     `(|the| |property-slot| |of| ,property)))
      )
     (t
      (setf property-slots (ls-find-property-relation-by-constant property)))
     )
    ;; create the return paths in the form of
    ;; ((SUCCESS Y (X1 slot1 Y1 0.55 value Z1 1.1)) ...)
    (cond 
     ((ls-interpret-property-P-value concept-inst property);; _Property-Value->(:pair 1 *meter) or _Length-Value->(:pair 1 *meter) 
      ;; return ((SUCCESS Y (newclass value Y 0.55)))
      (remove nil
	      (mapcar #'(lambda (property-slot)
			  (let ((range (first (ls-km `(|the| |range| |of| ,property-slot))))
				(range-inst nil))
			    (cond
			     ((equal concept-class '(|Property-Value|)) ;; for _Property-Value->(:pair 1 *meter)
			      (setf range-inst (first (ls-km `(|a| ,range))))
			      `(SUCCESS ,range-inst (,range-inst |value| ,property 0.55)))
			     ((ls-km `(,concept-inst |isa| ,range)) ;; for _Length-Value->(:pair 1 *meter) 
			      `(SUCCESS ,concept-inst (,concept-inst |value| ,property 0.55))))))
		      property-slots))
      )
     ((ls-interpret-property-non-value property)
      (ls-search-concept-inst-to-domains concept-inst property-slots property)
      #|(apply #'append
	     (mapcar #'(lambda (property-slot)
			 (let ((domains (get-domain-of-slot property-slot)))
			   (if (match-domains-or-ranges domains concept-inst)
			       (list `(SUCCESS ,concept-inst (,concept-inst ,property-slot ,property 0.55)))
			     (ls-search-concept-inst-to-domains concept-inst domains property-slot property)))
			 )
		     property-slots))|#
      )
     (t
      (remove nil
	      (mapcar #'(lambda (property-slot)
			  (let ((domains (get-domain-of-slot property-slot))
				(property-val
				 (first (ls-km `(|an| |instance| |of| (|the| |range| |of| ,property-slot))))))
			    (if (match-domains-or-ranges domains concept-inst)
				`(SUCCESS ,concept-inst (,concept-inst ,property-slot ,property-val 0.55 |value| ,property 1.1)))))
		      property-slots)))
     )
    ))

;; a function demanded by Peter so that both LS and matcher can be used as an ensemble
;; and use bagging to remove extra matches, etc
;; return a set of expressions instead of just 1
(defun ls-interpret-nn-all (lsexpr)
  (ls-output-nn (ls-search-nn (second lsexpr) (third lsexpr)) lsexpr)
  )

(defun ls-interpret-property-value-value-pair (expr)
  (let ((search-results (ls-search (extract-head-instance expr) '|value| (extract-tail expr)))
	(new-classes nil)
	(interpretations nil))
    (when (successful-search? search-results)
      (setf new-classes (mapcar #'(lambda (result) (first (ls-km `(|the| |instance-of| |of| ,(second result))))) search-results))
      (if (minimatch expr '(|a| ?x |with| &rest))
	  (setf interpretations
		(mapcar #'(lambda (class)
			    (replace-km-expr-head-with-class expr class))
			new-classes))
	(setf interpretations
	      (mapcar #'(lambda (class)
			  (add-new-property-to-assertion expr `(|instance-of| (,class))))
		      new-classes))))
    ;;(prompt-for-clarification expr interpretations)
    (ls-add-last-action 'specialize-head)
    (ls-add-specialized-head interpretations)
    expr
    ))

;; this happens when x is a specific property val, y is a generic val
;; e.g x = _Height123 y = _Property-Value123 or vice versa
(defun ls-interpret-cpl-property-val (lsexpr x y)
  (let ((y-class (ls-km `(|the| |instance-of| |of| ,y)))
	(x-class (ls-km `(|the| |instance-of| |of| ,x))))
  (cond
   ((equal x-class '(|Property-Value|))
    (ls-km `(,x |has| (|instance-of| (,y-class))))
    (prompt-for-clarification lsexpr 
			      `((,y |has| (|equal| ((,x |has| (|instance-of| ,y-class))))))))
			      ;;`((,x |has| (|equal| (,y))))))
   ((and (equal x-class y-class) ;; indirect anaphora
	 (equal (km0 `(|the| |value| |of| ,x))
		(km0 `(|the| |value| |of| ,y))))
    (prompt-for-clarification lsexpr `((,y |has| (|equal| (,x))))))
   (t
    (ls-km `(,y |has| (|instance-of| (,x-class))))
    (prompt-for-clarification lsexpr 
			      `((,x |has| (|equal| ((,y |has| (|instance-of| ,x-class))))))))
   ;;`((,y |has| (|equal| (,x)))))
   )
  ))

(defun ls-interpret-nn (expr)
  (cond
   ((ls-nn-chemical-expr expr)
    (ls-interpret-chemical-nn expr))
   ((and (ls-nn-seq-expr expr)
	 (ls-cpl-property-val-situation (second expr) (third expr))) ;; special case1 for CPL
    (ls-interpret-cpl-property-val expr (second expr) (third expr)))
   ((ls-cpl-entity-property-val (second expr) (third expr)) ;; special case3 for CPL
    (prompt-for-clarification expr nil))
   (t
    (ls-interpret-nn0 expr))))

(defun ls-ionized? (x-class)
  (member '|Ion| (all-superclasses x-class) :test #'eql)
  )

(defun ls-interpret-chemical-nn (expr)
  (let ((x (second expr))
	(y (third expr)))
    (prompt-for-clarification expr (list `(,x |has| (|context| (,y)))))))

(defun ls-interpret-chemical-name (expr)
  (let (matches	x x-class y y-class x-expr y-expr x-ion y-ion ret-val)
    (setf x (first expr))
    (setf x-class (extract-head-class expr))
    (setf matches (first (or (retrieve-property-from-assertion expr '|context|)
			     (retrieve-property-from-assertion expr '|context-of|))))
    (setf y 
	  (if (consp matches)
	      (first matches) ;; in case the matches are of the form (_Y2 |has| (|instance-of| (|Y-class|)))
	    matches)) ;; in case the matches are of the form _Y2
    (setf y-class (extract-head-class y))
    (cond
     ((ls-ionized? x-class)
      (setf x-expr `(,x |has| (|instance-of| (,x-class)))))
     (t
      (setf x-ion (first (km0 `(|the| |instance-of| |of| ,(first (compute-ion-from-defs (first (km0 `(|the| |has-chemical-name| |of| (|a| ,x-class)))) nil nil nil nil))))))
      (if (null x-ion)
	  (setf x-ion '|Ion|))
      (setf x-expr `(|a| ,x-ion |with|  (|is-ion-of| ((,x |has| (|instance-of| (,x-class)))))))))
    (cond
     ((ls-ionized? y-class)
      (setf y-expr `(,y |has| (|instance-of| (,y-class)))))
     (t
      (setf y-ion (first (km0 `(|the| |instance-of| |of| ,(first (compute-ion-from-defs (first (km0 `(|the| |has-chemical-name| |of| (|a| ,y-class)))) nil nil nil nil))))))
      (if (null y-ion)
	  (setf y-ion '|Ion|))
      (setf y-expr `(|a| ,y-ion |with| (|is-ion-of| ((,y |has| (|instance-of| (,y-class))))))))
     )
    (setf ret-val `(|a| |Ionic-Compound| |with| (|has-part| (,x-expr ,y-expr))))
    (prompt-for-clarification expr (list ret-val))
    ret-val
    ))

;; Given a KM expr in the form of a list of 2 nouns, 
;; returns a interpretation
(defun ls-interpret-nn0 (lsexpr)
  (let (
	(interpretations (ls-search-nn (second lsexpr) (third lsexpr))))
    ;; now add the interpretations to the hash table
    (dolist (interpretation (ls-output-nn interpretations lsexpr))
	    ;;(format t "storing ~s in hash~%" interpretation)
	    (setf (gethash interpretation *ls-generated-exprs*) t))
    (prompt-for-clarification lsexpr (ls-output-nn interpretations lsexpr))
    ))

;;; INPUT: a list of interpretations
;;; OUTPUT: a list of interpretations. if *ls-shaken-input*, then not a singleton list, else a singleton list
(defun ls-output-nn (interpretations nnexpr)
  (remove-duplicates
   (mapcar #'(lambda (interpretation)
	       (ls-output-nn-1-interpretation interpretation nnexpr))
	   interpretations)
   :test #'equal))

(defun ls-output-nn-1-interpretation (interpretation nnexpr)
  ;; format SME's pick
  (cond 
   ;; for inputs like "Apple smell"
   ((minimatch interpretation '(|the| ?x |of| &rest))
    interpretation)
   ;; for inputs like "pine tree", always return (a tree)
   ((minimatch interpretation '(|a| ?x))
    `(|a| ,(third nnexpr)))
   (t
    ;; for situations such as (a X with ...)
    (let* ((X (second nnexpr))
	   (Y (third nnexpr))
	   (head (extract-head-class interpretation))
	   ;; (new-X (last-element (flatten-list interpretation))) ;; no longer want to present the specialized result
	   (new-X X)
	   (X-replacement 
	    (cond 
	     ((property-constant? X)
	      X)
	     ((and (is-an-class X)
		   (find new-X (all-superclasses X) :test #'eql))
	      (list `|a| X))
	     ((and (is-an-class X))
	      (list '|a| new-X))
	     ((and (is-an-instance X)
		   (is-an-class new-X)
		   (ls-km `(,X |isa| ,new-X)))
	      X)
	     ((is-an-class new-X)
	      `(,X |has| (|instance-of| (,new-X))))
	     (t X)))
	   (after-X-replacement 
	    (replace-km-expr-end
	     interpretation
	     X-replacement)))
      (cond ((and (not (ls-inst? Y))
		  (not (property-constant? Y))
		  (is_super_or_subclass (ls-km `(|a| ,Y)) (ls-km `(|a| ,head))))
	     (replace-km-expr-head-with-class
	      after-X-replacement (extract-head-class interpretation)))
	    ((and (ls-inst? Y)
		  (is_super_or_subclass Y (ls-km `(|a| ,head))))
	     (replace-km-expr-head-with-inst
	      after-X-replacement Y)
	     )
	    ((ls-inst? Y)
	     (replace-km-expr-head-with-inst
	      after-X-replacement Y))
	    ((or (classp Y)
		 (atom Y)) ;; if just an atom then assume it's a class because it's not an inst
	     (replace-km-expr-head-with-class
	      after-X-replacement Y))
	    (t ;; for example (:pair 1 *meter)
	     after-X-replacement)
	    )
      ))
   ))

;; given an allof structure 
;; and how the structure is used,
;; do the following search:
;; first make sure the allof structure is good
;; then make sure the output of allof structure fits its usage
(defun interpret-allof (expr)
  (let* ((range (forall-range expr))
	 (condition (forall-condition expr))
	 (transformation (forall-transformation expr))
	 (range-set nil)
	 (it nil)
	 (itdegree (if (forall-allof2p expr) 2 1));; decide if to deal w/ It or It2
	 )
    ;; check range
    (setf range (parse-and-flush range))
    ;; run range
    (setf range-set (ls-km (replace-self range)))
    ;;(format t "range-set = ~s~%" range-set)
    
    (setf it (first range-set))
    ;; the condition speculation has been commented out because
    ;; we don't have enough evidence to guess one way or another
    (setf condition 
	  (revert-it
	   (parse-for-ls-one-iteration 
	    (replace-it condition 
			it itdegree))
	   it itdegree))
    
    ;; apply condition on range
    (setf range-set (remove-if #'(lambda (inst)
				   (not (ls-km (replace-it condition inst itdegree))))
			       range-set))
    (setf it (first range-set))
    (if transformation
	(setf transformation 
	      (revert-it
	       (parse-for-ls-one-iteration 
		(replace-it transformation it itdegree))
	       it itdegree
	       )))

    (if transformation
	(replace-forall-transformation
	 (replace-forall-condition
	  (replace-forall-range expr range)
	  condition)
	 transformation)
      (replace-forall-condition (replace-forall-range expr range)
				condition))
    ))

(defun interpret-isa (expr)
  (declare (special *km-code-keywords*))
  (let ((head (parse-for-ls-one-iteration (first expr)))
	(tail (parse-for-ls-one-iteration (third expr)))
	(search-slots nil)
	(search-results nil)
	(interpretations nil))
    (cond 
     ((member head *km-code-keywords* :test #'equal)
      expr)
     ((ls-km expr)
      expr)
     ((is_super_or_subclass
       (first (ls-km head))
       (ls-km `(|a| ,tail)))
      expr)
     (t
      (setf search-slots
	    (ls-km `(|allof| (|the| |all-instances| |of| |Slot|) |where| (((|the| |range| |of| |It|) /= |Class|) |and|
									  (|oneof2| (|the| |range| |of| |It|) |where|
									   (,tail |is-subsumed-by| |It2|))))))
      
      (setf search-results
	    (ls-kb-search
	     (ls-km head)
	     search-slots
	     (ls-km `(|a| ,tail))
	     #|#'(lambda (current-inst goals)
	       (remove nil
		       (mapcar #'(lambda (goal)
				   (and 
				    (ls-inst? current-inst)
				    (not (km-structured-list-valp current-inst))
				    (ls-km `(,current-inst |isa| ,goal))))
			       goals)))|#
	     #'is_super_or_subclass
	     :min-depth *ls-min-search-depth*
	     :multiple-answers? t
	     ))
      (setf search-results
	    (sort search-results
		  #'(lambda (x y)
		      (if (and x y)
			  (< (absolute-taxonomical-distance-between-instances x (first (ls-km `(|a| ,tail))))
			     (absolute-taxonomical-distance-between-instances y (first (ls-km `(|a| ,tail)))))))
		  :key #'second))
      (setf interpretations 
	    (generate-expr-from-search-head `(|the| |slot| |of| ,head) search-results))
      (setf interpretations
	    (mapcar #'(lambda (interpretation)
			`(|oneof2| ,(extract-head interpretation) |where| (|It2| |isa| ,tail))
			)
		    interpretations))
      (prompt-for-clarification
       expr
       interpretations)
      ))
    ))

(defun interpret-condition-expr (expr)
  ;; 
  (cond
   ((not (consp expr))
    expr)
   ((= (length expr) 4);; "if ... then ..." form
    `(|if| ,(parse-for-ls-one-iteration (second expr))
      |then|
      ,(parse-for-ls-one-iteration (fourth expr)))
    )
   ((= (length expr) 6);; "if ... then ... else ..." form
    `(|if| ,(parse-for-ls-one-iteration (second expr))
      |then|
      ,(parse-for-ls-one-iteration (fourth expr))
      |else|
      ,(parse-for-ls-one-iteration (sixth expr)))
    )
   (t
    expr)
   )
  )

;;;;; ------------- utility routines --------------

;;;; given a KM expression
;;;; see if it follows a preexisting path in the KB
;;;; assume only 1 slot is involved, and that slot has only 1 filler
(defun preexisting-path (expr &key (assertion-criterion #'Is_super_or_subclass))
  (let ((result (preexisting-path-wo-comments expr :assertion-criterion assertion-criterion)))
    (when (null result)
      (ls-debug-msg 'preexisting-path (format nil "non-existing path ~S" expr)))
    result
    ))
(defun preexisting-path-wo-comments (expr &key (assertion-criterion #'Is_super_or_subclass))
  (let ((retval nil))
    ;; if it's query, then execute it to see if anything returns
    (if (km-queryp expr)
	(if (ls-km expr :fail-mode 'fail)
	    (setf retval t))
      ;; if it's assertion, then do a search using the slots and depth restriction and see if anything returns
      (if (preexisting-assertion expr assertion-criterion)
	  (setf retval t)))
    retval
    ))

(defun km-class-queryp (expr)
  (minimatch expr '(|the| ?class ?slot |of| &rest))
  )

(defun km-assertionp (expr)
  (or 
   (atom expr)
   (and (consp expr)
	(= (length expr) 1))
   (minimatch expr '(|a| ?x &rest))
   (minimatch expr '(?x |has| &rest))
   (minimatch expr '(|every| ?x |has| &rest)))
  )

(defun km-isap (expr)
  (minimatch expr '(?x |isa| ?y))
  )

;; need to check for both expr and reversed expr
(defun preexisting-assertion (expr stop-criterion)
  (cond 
   ((aggregatep (extract-head expr))
    ;; assertion exists if every one of the aggregate exists
    (eval (cons 'and
		(mapcar #'(lambda (element)
			    (not (null (preexisting-assertion (replace-km-expr-head expr element) stop-criterion))))
			(rest (extract-head expr)))))
    )
   ((aggregatep (extract-tail expr))
    ;; assertion exists if every one of the aggregate exists
    (eval (cons 'and
		(mapcar #'(lambda (element)
			    (not (null (preexisting-assertion (replace-km-expr-tail expr element) stop-criterion))))
			(rest (extract-tail expr)))))
    )
   #|((pure-assertion expr)
    (or (preexisting-assertion0 expr stop-criterion)
	(preexisting-assertion0 (partial-reverse-assertion-expr expr) stop-criterion)))|#
   (t
    (preexisting-assertion0 expr stop-criterion)))
  )

;; if it's assertion, then do a search using the slots and depth restriction and see if anything returns
;; because we check for ls from beginning to the end 1 step at a time, 
;; we only need to check to see if the path 1 step away exists or not!
(defun preexisting-assertion0 (expr stop-criterion)
  (let* ((slots 
	  (list (extract-first-slot expr)));; changed from extract-unique-slot b/c only care about the first slot
	 (depth-limit
	  1)
	 (init-unit
	  (extract-head-instance expr))
	 (goal-unit
	  (if;; the following conjunction condition test to see
	      ;; if the expr is of the form 
	      ;; (a X with (slot ((:pair ...))))
	      ;; if its slot filler is a pair or seq, then
	      ;; always return not preexisting
	      (or
	       (and
		(consp expr)
		(consp (last expr))
		(consp (last-element expr))
		(consp (second (last-element expr)))
		(consp (first (second (last-element expr))))
		(aggregatep (first (second (last-element expr)))))
	       ;; or if of the form ((:pair 1 *gram) has (value-of ((a Mass-Value))))
	       (and
		(consp expr)
		(consp (first expr))
		(aggregatep (first expr))))
	      nil
	    (extract-tail-instance expr)))
	 (search-result
	  (if (and init-unit goal-unit
		   )
	      (ls-kb-search
	       (list init-unit)
	       slots
	       (list goal-unit)
	       stop-criterion
	       :max-depth depth-limit
	       :search-subslot? t
	       ))))
    (cond 
     ;; no head is specified, so assume it's preexisting
     ((not init-unit)
      t)
     ;; no slots specified, hence no additional properties specified in an assertion
     ;; assume it's preexisting
     ((not slots)
      t)
     ((successful-search? search-result)
      t)
     (t
      ;; do the inverse search
      (successful-search?
       (ls-kb-search
	(list goal-unit)
	(mapcar #'(lambda (slot)
		    (first (ls-km `(|the| |inverse| |of| ,slot))))
		slots)
	(list init-unit)
	stop-criterion
	:max-depth depth-limit
	:search-subslot? t
	)))
     )
    ))


;; try to satisfy the constraint violation in the expr
(defun constraint-satisfaction (expr violation)
  (let ((results nil))
    (setf results
	  (if (km-queryp expr)
	      (constraint-satisfaction-query expr)
	    (constraint-satisfaction-assertion expr violation)))
    (sort-based-on-preexisting-or-not results)
    ))

(defun constraint-satisfaction-query (expr)
  ;; note: query only has domain mismatch problem
  (let ((search-results nil)
	(generated-exprs (list expr))
	(alternatives nil)
	(specialization nil)
	)
    (append-list
     (mapcar #'(lambda (newexpr)
		 (cond ((setf search-results (search-for-subclass-domain-match newexpr))
			(setf alternatives (generate-expr-from-specialize-head newexpr search-results))
					; profiling
			(incf (gethash 'domain-sat *ls-profiling-data* 0))

			;; add specialize-head as the last action
			(if (aggregatep (extract-head expr))
			    (setf specialization
				  (eval (cons 'or
					      (mapcar #'has-specialized search-results))))
			  (setf specialization (has-specialized search-results)))
			(if specialization
			    (progn
					; profiling
			      (incf (gethash 'domain-specialize *ls-profiling-data* 0))
			      (ls-add-last-action 'specialize-head)
			      (ls-add-specialized-head alternatives)
			      (list newexpr))
			  alternatives)
			)
		       (t
			(format t "WARNING: ~a contains domain mismatch, but no interpretation is found!~%" expr)
					; profiling
			(incf (gethash 'domain-unsat *ls-profiling-data* 0))
			
			(if (not (equal expr newexpr))
			    (list newexpr)
			  ;; else nothing new, so return nil
			  )
			)))
	     generated-exprs))
    ))

;; extract the part of the input expr whose body matches with an given body
(defun extract-concept-part-with-body (expr body)
  (let ((retcode nil))
    (apply-action-to-km-expr
     expr
     #'(lambda (code relationship filler depth)
	 (declare (ignore relationship filler depth))
	 (equal (extract-concept-body code) body))
     #'(lambda (code relationship filler)
	 (declare (ignore filler relationship))
	 (setf retcode code)
	 (list nil nil nil)))
    retcode
    ))
;; similar to the function extract-concept-part-with-body
(defun replace-concept-part-with-new-body (expr body newcode)
  (apply-action-to-km-expr
   expr
   #'(lambda (code relationship filler depth)
       (declare (ignore relationship filler depth))
       (equal (extract-concept-body code) body))
   #'(lambda (code relationship filler)
       (declare (ignore filler relationship code))
       (list (extract-head newcode) 
	     (first (first (extract-concept-body newcode)))
	     (first (second (first (extract-concept-body newcode)))))))
  )

;; given an expr (either query or assertion), return a list of possible interpretation or a list of the orig expr
(defun constraint-satisfaction-assertion-range-violation (expr)
  (let (search-results alternatives)
    (mapc #'(lambda (expr)
	      (cond 
	       ((setf search-results (search-for-subclass-range-match expr))
		(setf alternatives (generate-expr-from-specialize-tail expr search-results))
					; profiling
		(incf (gethash 'range-sat *ls-profiling-data* 0))
		
		;; add specialize-tail as the last action
		(when (has-specialized search-results)
		  (ls-add-last-action 'specialize-tail)
		  (ls-add-specialized-tail alternatives)
					; profiling
		  (incf (gethash 'range-specialize *ls-profiling-data* 0))
		  )
		alternatives)
	       (t
					; profiling
		(incf (gethash 'range-unsat *ls-profiling-data* 0))
		
		(format t "WARNING: ~a contains range mismatch, but no interpretation is found!~%" expr))))
	  (list expr))
    alternatives
    ))

;; given an expr (either query or assertion), return a list of possible interpretation or a list of the orig expr
(defun constraint-satisfaction-assertion-domain-violation (expr)
  (let (search-results alternatives)
    (mapc #'(lambda (newexpr)
	      (cond 
	       ((setf search-results (search-for-subclass-domain-match newexpr))
		(setf alternatives (generate-expr-from-specialize-head newexpr search-results))
					; profiling
		(incf (gethash 'domain-sat *ls-profiling-data* 0))
		;;(format t "current last action = ~S~%" *ls-last-parsing-action*)
		(when (has-specialized search-results)
		  ;; add specialize-head as the last action
		  (ls-add-last-action 'specialize-head)
		  (ls-add-specialized-head alternatives)
					; profiling
		  (incf (gethash 'domain-specialize *ls-profiling-data* 0))
		  )
		alternatives)
	       (t
					; profiling
		(incf (gethash 'domain-unsat *ls-profiling-data* 0))
		
		(format t "WARNING: ~a contains domain mismatch, but no interpretation is found!~%" expr)
		(if (not (equal expr newexpr))
		    (list newexpr)
		  ;; else nothing new, so return nil
		  )
		)))
	  (list expr))
    alternatives))

;; what to do if both domain & range are violated?
(defun constraint-satisfaction-assertion (expr violation)
  (let (
	(generated-exprs (list expr))
	(alternatives nil))
    (when (member 'range-mismatch violation :test #'equal)
      (setf alternatives (constraint-satisfaction-assertion-range-violation expr))      
      ;; if an answer has been found
      ;; then call self again in case there are other types of violation
      (when alternatives
	(setf violation
	      (remove 'range-mismatch violation :test #'eql))
	(cond
	 ((first violation) ;; there are other violations
	  (multiple-value-setq (generated-exprs violation)
			       (constraint-satisfaction-assertion
				(prompt-for-clarification expr alternatives)
				violation)))
	 (t
	  (setf generated-exprs 
		(list
		 (prompt-for-clarification expr alternatives)))))
	(setf alternatives nil) ;; clear it for the domain case
	)
      )
    (when (member 'domain-mismatch violation :test #'equal)
      (setf alternatives
	    (constraint-satisfaction-assertion-domain-violation (first generated-exprs)))
      ;; if an answer has been found
      ;; then call self again in case there are other types of violation
      (when alternatives
	(setf violation (remove 'domain-mismatch violation :test #'eql))
	(cond 
	 ((first violation) ;; there are other violations
	  (let ((new-expr (prompt-for-clarification expr alternatives)))
	    (multiple-value-setq 
	     (generated-exprs violation)
	     (constraint-satisfaction-assertion
	      (extract-concept-part-with-body new-expr (extract-concept-body expr))
	      violation))
	    (setf generated-exprs
		  (mapcar #'(lambda (subexpr)
			      (replace-concept-part-with-new-body new-expr (extract-concept-body expr) subexpr))
			  generated-exprs ))))
	 (t
	  (setf generated-exprs
		(list (prompt-for-clarification (first generated-exprs) alternatives)))))
	)
      )
    (if (and 
	 (first violation)
	 (not (member 'range-mismatch violation :test #'eql))
	 (not (member 'domain-mismatch violation :test #'eql)))
	(format t "WARNING: ~a contains unknown type of constraint violation ~a~%" expr violation)
      )
    (values generated-exprs violation)
    ))

(defun constraint-violation (expr)
  (let ((result (constraint-violation-wo-comments expr)))
    (when result
      (ls-debug-msg 'constraint-violation (format nil "constraint violated for ~S" expr))
      (format t "LS DEBUG: constraint violation detected for ~S violation type ~S~%" expr result))
    result
    ))

(defun constraint-violation-wo-comments (expr)
  (cond ((aggregatep (extract-head expr))
	 (remove-duplicates
	  (append-list
	   (mapcar #'(lambda (element)
		       (constraint-violation (replace-km-expr-head expr element)))
		   (rest (extract-head expr)))))
	 )
	((aggregatep (extract-tail expr))
	 (remove-duplicates
	  (append-list
	   (mapcar #'(lambda (element)
		       (constraint-violation (replace-km-expr-tail expr element)))
		   (rest (extract-tail expr)))))
	 )
	(t
	 (constraint-violation0 expr))
	))

;; check to see if the assertion or query violates any constraints, such as domain/range or must-be-a, etc
(defun constraint-violation0 (expr)
  (if (km-queryp expr)
      (constraint-violation-query expr)
    (constraint-violation-assertion expr))
  )

;; only check 1 step
(defun constraint-violation-query (expr)
  ;; query constraints are limited to domain/range only
  ;; (the slot of Y) for example (the has-chemical-formula of (a Chemical))
  (let* (
	 (slot (first (decompose-query expr)))
	 (domains (get-domain-of-slot slot))
	 (head-inst
	  (extract-head-instance expr))
	 (result nil)
	 )
    (if (not (match-domains-or-ranges domains head-inst))
	(setf result (cons 'domain-mismatch result))
      nil)
    result
    ))

;; only check 1 step, which means the expr has only 1 slot listed
(defun constraint-violation-assertion (expr)
  ;; currently check only the domain/range constraints
  ;; develop the other constraint checkings later
  (let* ((slot (extract-first-slot expr))
	 (head-inst (extract-head-instance expr))
	 (body-inst (extract-tail-instance expr))
	 (domains (get-domain-of-slot slot))
	 (ranges (ls-km `(|the| |range| |of| ,slot)))
	 (result nil))
    (if;; head doesn't match the domain
	(and head-inst
	     (not (match-domains-or-ranges domains head-inst)))
	(setf result (cons 'domain-mismatch result)))
    ;; tail doesn't match the range
    (if (and body-inst
	     (not (match-domains-or-ranges ranges body-inst)))
	(setf result (cons 'range-mismatch result)))

    result
    ))

(defun search-for-subclass-domain-match (expr)
  (let ((search-results nil))
    (cond
     ((and (aggregatep (extract-head expr))
	   (ls-value-type (extract-head expr))
	   (setf search-results
		 (search-for-subclass-domain-match0 expr)))
      search-results)
     ((and (aggregatep (extract-head expr))
	   (ls-objectp (extract-head expr)))
      (mapcar #'(lambda (element)
		  (search-for-subclass-domain-match0 (replace-km-expr-head expr element)))
	      (rest (extract-head expr)))
      )
     (t
      ;; includes cases like (the quantity-of of (:pair 1 *mole))
      (search-for-subclass-domain-match0 expr))
     )
    ))

(defun search-for-subclass-domain-match0 (expr)
  (let* ((slot (extract-first-slot expr))
	 (domains (get-domain-of-slot slot))
	 (head (extract-head-class expr))
	 (good-domains nil)
	 (result nil))
    (cond
     ((setf good-domains
	    (ls-km `(|allof| ,(cons ':|set| domains) |where|
		     ((|a| |It|) |isa| ,head))))
      ;; if all it takes is just specialization
      ;; then do it
      (dolist (domain good-domains)
	      (setf result
		    (append-element
		     result
		     `(,domain
		       ((success ,domain
				 (,domain)))))))
      (sort-based-on-prosperity result)
      )
     ;; else
     (t
      (specialize-head expr t)
      ))
    ))

(defun search-for-subclass-range-match (expr)
  (let ((search-results nil))
    (cond 
     ((and (aggregatep (extract-head expr))
	   (ls-value-type (extract-head expr))
	   (setf search-results
		 (search-for-subclass-range-match0 expr)))
      search-results)
     ((and (aggregatep (extract-head expr))
	   (ls-objectp (extract-head expr)))
      (mapcar #'(lambda (element)
		  (search-for-subclass-range-match0 (replace-km-expr-head expr element)))
	      (rest (extract-head expr)))
      )
     (t
      ;; includes cases like (the quantity-of of (:pair 1 *mole))
      (search-for-subclass-range-match0 expr))
     )
    )
  )

(defun search-for-subclass-range-match0 (expr)
  (let* ((slot (extract-first-slot expr))
	 (ranges (ls-km `(|the| |range| |of| ,slot)))
	 (tail (extract-tail-class expr))
	 (good-ranges nil)
	 (result nil))
    (cond
     ((setf good-ranges
	    (ls-km `(|allof| ,(cons ':|set| ranges) |where|
		     ((|a| |It|) |isa| ,tail))))
      ;; if all it takes is just specialization
      ;; then do it
      (dolist (range good-ranges)
	      (setf result
		    (append-element
		     result
		     `(,range
		       ((success ,range
				 (,range)))))))
      (sort-based-on-prosperity result)
      )
     ;; else
     (t
      (specialize-tail expr t)
      ))
    ))

;; find an interpretation based on prior paths in the KB
;; based on the 4 search routines
(defun search-prior-path (expr)
  (let ((alternative nil)
	(search-results nil))
    (cond
     ((setf search-results (specialize-tail expr))
      (setf alternative 
	    (generate-expr-from-specialize-tail expr search-results))
      
      ;; add specialize-tail as the last action
      (when (has-specialized search-results)
					; profiling
	(incf (gethash 'specialize-tail-used *ls-profiling-data* 0))
	(ls-add-last-action 'specialize-tail)
	(ls-add-specialized-tail alternative))
      alternative)
     ((setf search-results (specialize-head expr))
      (setf alternative 
	    (generate-expr-from-specialize-head expr search-results))
      ;; add specialize-head as the last action
      (when (has-specialized search-results)
					; profiling
	(incf (gethash 'specialize-head-used *ls-profiling-data* 0))
	(ls-add-last-action 'specialize-head)
	(ls-add-specialized-head alternative))
      alternative)
     (t
      nil))
    ))


;;;;; ------------- tool box --------------
;; given an instance, return the list of slots 
;; that have filler
(defun ls-specified-slots (inst)
  (let ((classes (all-classes inst))
	(member-property-slots nil)
	(proto-property-slots nil)
	(inst-specific-slots nil)
	(proto-insts nil)
	)
    (setf inst-specific-slots
	  (mapcar #'first
		  (get-slotsvals inst :facet 'own-properties)))
    (setf member-property-slots
	  (apply #'append
		 (mapcar #'(lambda (class)
			     (extract-slots-from-member-properties
			      (get-slotsvals class :facet 'member-properties)))
			 classes)))
    (setf proto-insts
	  (apply #'append
		 (mapcar #'(lambda (class)
			     (get-vals class '|prototypes| :situation *global-situation*))
			 classes)))
    (setf proto-property-slots
	  (apply #'append
		 (mapcar #'(lambda (proto)
			     (extract-slots-from-proto-properties
			      (get proto 'definition)))
			 proto-insts
			 )))
    (remove-duplicates 
     (append inst-specific-slots member-property-slots proto-property-slots))
    ))

;; given the member-properties of a class
;; return all the slots which have fillers
(defun extract-slots-from-member-properties (properties)
  (mapcar #'first properties)
  )
;; given the prototype specs
;; return all the slots
;; example: ((|a-prototoype| |Car|) ((|the| |Car|) |has| (|parts| ((|a| |Engine|)))) (|end-prototype|))
(defun extract-slots-from-proto-properties (proto-properties)
  (let ((proto-class nil)
	(true-proto-properties nil))
    (setf proto-class
	  (second
	   (find-if #'(lambda (assertion)
			(minimatch assertion '(|a-prototoype| ?x)))
		    proto-properties)))
    (setf true-proto-properties
	  (apply #'append
		 (remove nil
			 (mapcar 
			  #'(lambda (assertion)
			      (if (minimatch assertion `((|the| ,proto-class) |has| &rest))
				  (cddr assertion)))
			  proto-properties))
		 ))
    (mapcar #'first true-proto-properties)
    ))

(defun scalar-const? (x)
  (and 
   (atom x)
   (property-concept? x)
   (ls-km `(|the| |scalar-constant-class-of| |of| (|the| |instance-of| |of| ,x))))
  )
;; things such as (:pair 3 *meter) shouldn't be a clib-inst
(defun clib-inst? (inst)
  (let ((classes (ls-km `(|the| |instance-of| |of| ,inst))))
    (if (remove-if #'clib-concept? classes)
	;; at least one of the classes is not a clib concept
	nil
      t)
    ))


(eval-when (compile load eval)
	   (defun ls-named-instance? (inst)
	     (and (symbolp inst)
		  (atom inst)
		  (char= (first-char (symbol-name inst)) '#\*))
	     ))

(defun property-slot? (slot)
  (or (equal slot '|value|)
      (ls-km `(,slot |isa| |Property|)))
  )

;; e.g. *red, (:pair 1 *foot) (:pair *hot Drink)
(defun property-constant? (element)
  (and (property-concept? element)
       (or (aggregatep element)
	   (ls-km `(,element |isa| |Constant|))))
  )

;; concept is property if it: *red, (:pair 3 *gram), (:pair *hot drink) or _Property-Value3 or _Length-Value2
(defun property-concept? (concept)
  (when (not (null concept))
    (or (and 
	 (atom concept);; such as *red
	 (ls-km `(,concept |isa| |Constant|)))
	(and
	 (km-pairp concept)
	 (numberp (second concept))
	 (or (equal (third concept) '|nil|)
	     (ls-km `(,(third concept) |isa| |Unit-of-Measurement|))
	     ));; such as (:pair 3 *gram)
	(and
	 (km-pairp concept)
	 (ls-km `(,(second concept) |isa| |Constant|)));; such as (:pair *cold drink)
	(and
	 (ls-inst? concept)
	 (ls-km `(,concept |isa| |Property-Value|)))
	 ;;(not (ls-km `((|the| |instance-of| |of| ,concept) |includes| |Property-Value|))));; e.g. _Temperature-Value3 but not Property-Value itself -- commented out for ls-interpret-property-P-value
	(and
	 (clib-concept? concept) 
	 ;;(not (equal concept '|Property-Value|)) -- commented out for ls-interpret-property-P-value
	 (ls-km `(,concept |is-subsumed-by| |Property-Value|)));; e.g Temperature-Value
	)
    ))

(defun clib-concept? (concept)
  (declare (special *component-index*))
  ;; it is a clib-concept if it either listed in *component-index* or defined in clib
  (and
   (kb-objectp concept)
   (or
    (if (find-symbol "*component-index*")
	(find concept (mapcar #'second *component-index*) :test #'equal))
    (numberp (string/= (write-frame0 concept) ""))
    (numberp (string/= (write-frame0 (ls-intern (string-downcase (string concept)))) ""))
    (numberp (string/= (write-frame0 (convert-to-const concept)) "")))
   (not (equal concept '|Thing|));; if the concept is Thing, then it's too general
   ;; or the concept is just a newly created class
   ;; [1] but the following line causes Time-Interval not working (such as the *breakfast examples in coverage test
   ;;(not (equal '|Thing| (first (ls-km `(|the| |superclasses| |of| ,concept)))))
   )
  )

(defun safe-ldiff (x y)
  (cond ((null y)
	 x)
	((null x)
	 x)
	(t
	 (ldiff x y)))
  )

;; get the domain of a slot
;; input: a slot name
;; output: a list of domains
(defun get-domain-of-slot (slot)
  (let ((val (gethash slot *ls-slot-domains-cache*)))
    (cond (val
	   val)
	  (t
	   (setf val (ls-km `(|the| |domain| |of| ,slot)))
	   (setf (gethash slot *ls-slot-domains-cache*) val)
	   val))
    ))

(defun get-inverse-slot (slot)
  (first (ls-km `(|the| |inverse| |of| ,slot)))
  )
(defun last-slot-in-path (path)
  (last-element
   (remove-if-not #'ls-slotp path))
  )
;; generate a list of inverse slots (including the subslots) of a given slot
(defun all-inverse-slots (slot)
  (let ((candidates (cons slot 
			  (append
			   (ls-km `(|the| |all-subslots| |of| ,slot))
			   (ls-km `(|the| |all-superslots| |of| ,slot)))))
	)
    (append
     (ls-km `(|the| |all-subslots| |of| (|the| |inverse| |of| ,slot)))
     (ls-km `(|the| |all-superslots| |of| (|the| |inverse| |of| ,slot)))
     (append-list
      (mapcar #'(lambda (s)
		  (ls-km `(|the| |inverse| |of| ,s)))
	      candidates)))
    ))

(defun cardinality-of-slot (slot)
  (first (ls-km `(|the| |cardinality| |of| ,slot)))) ;; assume a slot has only 1 cardinality

(defun last-element (l)
  (first (last l))
  )

(defun append-element (l x)
  (append l (list x))
  )

;; test to see if inst1 is more specific than inst2
(defun is-more-specific (inst1 inst2)
  (cond 
   ((null inst1);; nil is the most generic
    nil)
   ((null inst2);; nil is the most generic inst
    t) 
   ((and (aggregatep inst1)
	 (aggregatep inst2))
    (is-more-specific (second inst1)
		      (second inst2)))
   ((and (aggregatep inst1)
	 (not (aggregatep inst2)))
    nil)
   ((and (not (aggregatep inst1))
	 (aggregatep inst2))
    t)
   (t
    (not (null
	  (remove-if-not #'(lambda (class)
			     (ls-km `(,inst1 |isa| ,class))
			     )
			 (ls-km `(|the| |classes| |of| ,inst2)))))
    )))

;; test an expr/km object is a class or not
(defun is-an-class (expr)
  (and (kb-objectp expr)
       (known-frame expr)
       (atom expr)
       (not (ls-inst? expr)))
  )

;; check to see if the given expr is part of the top-level expr in "self" definition
(defun top-level-expr (expr)
  ;; (format t "self = ~S~% " (car *ls-current-self*))
  ;; (format t "~s ~s ~s ~s~%" (km-assertionp expr) (km-assertionp (car *ls-current-self*))  (same-head expr (car *ls-current-self*)) (subsetp (extract-concept-body expr) (extract-concept-body (car *ls-current-self*)) :test #'equal))
  (and 
   (km-assertionp expr)
   (km-assertionp (car *ls-current-self*))
   (same-head expr (car *ls-current-self*))
   #|(subsetp (extract-concept-body expr) ;; temp commented out b/c the new expr may contain changes not reflected in *ls-current-self*
	    (extract-concept-body (car *ls-current-self*)) :test #'equal)|#
   )
  )

;; excludes query results such as (the output of (the input-of of (the base of (the input of (a Compute with ...)))))
;; maybe not be a good idea, let's try it out first
(defun contains-conjugate-slot-pairs-in-query-result (result)
  ;; the query path starts from the last assertion
  ;; so extract all the slots and see if there's any inverses
  (let ((slots (remove-if-not #'ls-slotp (third result))))
    ;; if an slot whose inverse if found in the rest, then 
    (dolist (slot slots)
	    (if (member
		 (get-inverse-slot slot)
		 (remove slot slots :test #'equal)
		 :test #'equal)
		(return t)))
    ))

;; a predicate that is true if the unit is the self instance
;; unit is of the form (slot successor cost)
(defun exclude-self-in-search (unit self-inst)
  (equal (second unit)
	 self-inst)
  )

;; given a list of slots to denote a search path
;; find out if the path is reflexive
(defun reflexive-path (path)
  (cond ((atom path)
	 t)
	((and (consp path)
	      (= (length path) 1))
	 t)
	(t
	 (and
	  (equal (first (km `(|the| |inverse| |of| ,(car path))))
		 (last-element path))
	  (reflexive-path
	   (butlast (cdr path))))
	 )
	)
  )
;; given a list of search results, remove the ones that find themselves
(defun remove-reflexive-search-results (search-results)
  (remove-if #'(lambda (result)
		 (and (equal (first result) 'success)
		      (reflexive-path (remove-if-not #'ls-slotp (third result)))
		      (> (length (remove-if-not #'ls-slotp (third result))) 1)
		      )
		 )
	     search-results
	     )
  )

;; several different levels of slot listings
(defun all-searchable-slots ()
  (let ((relation-instances (all-instances '|Relation|))
	(property-instances (all-instances '|Property|)))
    (cond 
     ((> (length relation-instances) 1)
      (append '(|element-type| |feature-slot| |value|) relation-instances property-instances))
     (t
      (append '(|element-type| |feature-slot| |value|)
	      (set-difference
	       (all-instances '|Slot|)
	       (all-instances '|KM-Slot-Group|)))))
    ))

(defun all-relation-slots ()
  (append '(|element-type| |feature-slot| |value|)
	  (set-difference
	   (all-instances '|Slot|)
	   (append (all-instances '|Property|)
		   (all-instances '|KM-Slot-Group|))))
  )

(defun ls-read-eval-print ()
  (let ((old-selection-mode *ls-auto-selection*))
    (setf *ls-auto-selection* nil)
    (loop
     (print-ls-prompt)
     (let ((query (case-sensitive-read)))
       (cond 
	((equal query '|q|) 
	 (setf *ls-auto-selection* old-selection-mode)
	 (return))
	(t 
	 (format t "~S~%" (km (parse-for-ls query)))))
       ))
    ))

;; identical to print-km-prompt, 
;; except replace the "KM" string with "LS" string
(defun print-ls-prompt (&optional (stream t))
  (cond ((and (am-in-local-situation) (am-in-prototype-mode))
	 (report-error 'user-error "You are in both prototype-mode and in a Situation, which isn't allowed!~%"))
	((and (am-in-local-theory) (am-in-prototype-mode))
	 (report-error 'user-error "You are in both prototype-mode and in a Theory, which isn't allowed!~%"))
					;  (cond ((and (am-in-prototype-mode) (am-in-local-situation)) (km-format stream "[prototype-mode, ~a] KM> " (curr-situation)))
	((am-in-prototype-mode) (km-format stream "[prototype-mode] LS> "))
	((am-in-local-situation) (km-format stream "[~a] LS> " (curr-situation)))
	((am-in-local-theory) (km-format stream "{~a} LS> " (curr-situation)))
	(t (km-format stream "LS> "))))

;; given a lisp file name
;; returns a list of sexprs found in the file
(defun read-sexprs (filename)
  (let ((stream (open filename
		      :direction :input))
	(sexprs nil)
	(sexpr nil))
    (loop
     (setf sexpr (read stream nil  nil nil))
					;(format t "sexpr = ~S~%" sexpr)
     (if (not sexpr)
	 (return nil)
       (setf sexprs
	     (append-element sexprs 
			     sexpr)))
     )
    (close stream)
    sexprs
    ))

;; analyzes the dependency of functions listed in filename
(defun fun-dep-analysis (filename)
  (let ((sexprs (read-sexprs filename))
	(fun-hash (make-hash-table :test #'equal))
	(fun-name nil)
	(fun-body nil)
	(foo nil)
	(funs nil)
	)
    (setf funs
	  (remove-if-not
	   #'(lambda (sexpr)
	       (equal (first sexpr)
		      'defun))
	   sexprs))
    (dolist (fun funs)
	    (setf fun-name (second fun))
	    (setf fun-body (cdr (cdr fun)))
	    ;; register the function names
	    (setf (gethash fun-name fun-hash)
		  nil)
	    )
    ;; traverse the list again to register all the functions that uses the fun
    (dolist (fun-1 funs)
	    (setf foo (second fun-1))
	    ;;(format t "working on ~S~%" foo)
	    (dolist (fun funs)
		    (setf fun-name (second fun))
		    (setf fun-body (cdr (cdr fun)))
		    ;;(format t "deep-find function ~a in ~a~%" foo fun-name)
		    (if (and
			 (fun-used foo fun-body)
			 (not (equal foo fun-name)))
			(setf (gethash foo fun-hash)
			      (cons fun-name 
				    (gethash foo fun-hash))))
		    )
	    )

    ;; report the results
    (dolist (fun funs)
	    (format t "~S: ~S~%"
		    (second fun)
		    (gethash (second fun) fun-hash)))
    ))

;; checks to see if fun-name is called in fun-body
(defun fun-used (fun-name fun-body)
  (deep-find fun-body
	     #'(lambda (code-segment)
		 (or (and (consp code-segment)
			  (equal fun-name (first code-segment)))
		     (and (atom code-segment)
			  (equal code-segment
				 (ls-intern (format nil "#'~S" fun-name)))))
		 )
	     )
  )


(defun deep-find (sequence test)
  (cond ((atom sequence)
	 (funcall test sequence))
	(t
	 (or
	  (funcall test sequence)
	  (funcall test (car sequence))
	  (deep-find (car sequence) test)
	  (deep-find (cdr sequence) test)))
	)
  )

(defun is-named-instance (instance)
  (and (ls-inst? instance)
       (not (anonymous-instancep instance)))
  )

(defun report-class-usage ()
  ;; first count the total usage b/c we use relative frequency
  (let ((total 0.0)
	)
    (maphash #'(lambda (key val) (declare (ignore key)) (setf total (+ total val))) *ls-class-usage*)
    (maphash #'(lambda (key val)
		 (format t "~s frequency: ~a~%" key (/ val total)))
	     *ls-class-usage*)
    ))

(defun report-profiling ()
  (format t "Current profiling: ")
  (maphash #'(lambda (key val) (format t "~S: ~S~T" key 
				       (- val
					  (gethash key *ls-old-profiling-data* 0)))) 
	   *ls-profiling-data*)
  (format t "~%")
  (format t "Total profiling: ")
  (maphash #'(lambda (key val) (format t "~S: ~S~T" key val)) *ls-profiling-data*)
  (format t "~%")
  (setf *ls-old-profiling-data* (make-hash-table :test #'equal))
  (maphash #'(lambda (key val) 
	       (setf (gethash key *ls-old-profiling-data* 0) 
		     val))
	   *ls-profiling-data*)
  )

;; FUNCTION: given 2 assertions, check to see if they've gotten the same head
;; PRE: inputs have to be assertions
(defun same-head (expr path)
  (equal
   (extract-head expr)
   (extract-head path))
  )

;; FUNCTION: given a path & expr
;; determine if the expr contains the path
;; example: (a Reaction with (raw-material ((a Ca with (is-basic-structural-unit-of ((a Chemical with (quantity ((a Quantity-Value)))))))))) (a Ca with (is-basic-structural-unit-of ((a Chemical))))
;; return (a Ca with (is-basic-structural-unit-of ((a Chemical with (quantity ((a Quantity-Value)))))))
;; PRE: the part being matched is pure assertion
(defun find-access-path (expr path)
  ;; use recursion
  (cond 
   ((or (not (km-assertionp expr))
	(not (km-assertionp path)))
    nil)
   ((not path)
    expr)
   ((same-head expr path)
    ;; now go through all the slots & fillers to find a match
    (let*
	((expr-body (extract-concept-body expr))
	 (path-body (extract-concept-body path))
	 (path-slot (first (first path-body)))
	 (path-filler (first (second (first path-body))))
	 (tmpresult nil)
	 (result nil)
	 )
      (dolist (slot-fillers expr-body)
	      (if (equal path-slot 
			 (first slot-fillers))
		  (dolist (filler (second slot-fillers))
			  (setf tmpresult
				(find-access-path filler path-filler))
			  (if tmpresult
			      (setf result 
				    `(,path-slot (,tmpresult))))))
	      )
      (if (not path-body)
	  (setf result expr)
	(if result
	    (append-element (safe-ldiff expr (extract-concept-body expr))
			    result)))
      )
    )
   (t
    ;; now go through all the slots & fillers to find a match
    (let*
	((expr-body (extract-concept-body expr))
	 (tmpresult nil)
	 (result nil)
	 )
      (dolist (slot-fillers expr-body)
	      (dolist (filler (second slot-fillers))
		      (setf tmpresult
			    (find-access-path filler path))
		      (if tmpresult
			  (setf result 
				tmpresult))))
      result))
   ))

;; assumption: expr is an assertion with only 1 slot-fillers pair
(defun partial-reverse-assertion-expr (expr)
  (let* ((head (extract-concept-header expr))
	 (slot (first (extract-slots expr)))
	 (whole-tail (first (second (first (extract-concept-body expr)))))
	 (inverse (first (km `(|the| |inverse| |of| ,slot)))))
    (cond
     ((atom whole-tail)
      (list whole-tail '|has| `(,inverse (,head))))
     ((minimatch whole-tail '(|a| ?x))
      (append whole-tail
	      (list '|with| `(,inverse (,head)))))
     (t
      (append
       (extract-concept-head whole-tail)
       (cons 
	`(,inverse (,head))
	(extract-concept-body whole-tail))
       )))))

(defun complete-reverse-assertion-expr (expr)
  (fold-expr-inside-out expr expr)
  )

;; FUNCTION: given an expr & path
;; rearrange the expr to bring out the path
;; example: (a Ca with (is-basic-structural-unit-of ((a Chemical with (raw-material-of ((a Reaction with (result ((a Ca_OH_2)))))) (quantity ((a Quantity-Value)))))) (charge ((a Charge-Value)))) (a Ca with (is-basic-structural-unit-of ((a Chemical with (raw-material-of ((a Reaction)))))))
;; return: (a Reaction with (raw-material ((a Chemical with (quantity ((a Quantity-Value))) (has-basic-structural-unit ((a Ca with (charge ((a Charge-Value)))))))) (result ((a Ca_OH_2))))
;; PRE: inputs are pure assertions, and expr can have multiple slot-filler pairs
(defun fold-expr-inside-out (expr path &optional (result nil))
  (cond ((not path)
	 (if (not (member '|with| expr))
	     (append expr '(|with|) result)
	   (append expr result)))
	((simple-fillerp path);; already at the bottom of this
	 (if (not (member '|with| expr))
	     (append expr '(|with|) result)
	   (append expr result)))
	(t
	 (let ((newresult (first (prepare-result expr path result)))
	       (filler (second (prepare-result expr path result)))
	       (path-filler (first (second (last-element path)))));;get the (a Y ...) part of (a X w/ (slot ((a Y ...))))
	   (fold-expr-inside-out filler path-filler newresult)
	   ))
	)
  )

;; FUNCTION: this is a utility function used for fold-expr-inside-out
;; given an expr, a path & a result
;; return the new folded result and the filler to be unfolded by unfolding the expr 1 level
;; PRE: inputs are pure assertions
(defun prepare-result (expr path result)
  ;; go through the slot-fillers listing to find the right one
  (let* ((body (extract-concept-body expr))
	 (slot nil)
	 (fillers nil)
	 (head (safe-ldiff expr body))
	 (newresult nil)
	 (newslot (first (ls-km `(|the| |inverse| |of| ,(first (extract-slots path))))))
	 (picked-filler nil))
    (dolist (slot-fillers body)
	    (setf slot (first slot-fillers))
	    (setf fillers (second slot-fillers))
	    (if (equal slot
		       (first (extract-slots path)))
		(let ((newfillers nil))
		  ;; slot match check the fillers 
		  ;; here the setf runs into problem if 2 slot-fillers have the same slot
		  ;; then overwrites the first slot-fillers
		  (dolist (filler fillers)
			  (if (not
			       (same-head 
				filler
				(first (second (last-element path)))));; the (a Y ...) part of (a X with (slot ((a Y ...))))
			      (setf newfillers (append-element newfillers filler))
			    (setf picked-filler (merge-filler picked-filler filler)));; not working yet bug w/ Q33
			  ;;(setf picked-filler filler))
			  )
		  (if newfillers
		      (setf newresult (append-element result 
						      (list slot newfillers))))
		  )
	      ;; slot mismatch, so carry the results on
	      (setf newresult (append newresult (list slot-fillers)))
	      ))
    ;;(format t "newreult = ~s result = ~s~%" newresult result)
    (if (and (not newresult)
	     (not result))
	(setf head (remove '|with| head)));; in case no slot/fillers/prev results is avaiable
    (setf newresult (append newresult result))
    (list
     (list (list
	    newslot
	    (list (append head
			  newresult))))
     picked-filler)
    ))

;; FUNCTION: merge to fillers found from prepare-result function
;; PRE: 1. fillers are from the same slot 2. hopefully they have the same head
(defun merge-filler (filler1 filler2)
  (cond ((or (not filler1)
	     (not filler2))
	 (or filler1 filler2))
	((not (equal
	       (extract-concept-head filler1)
	       (extract-concept-head filler2)))
	 (format t "WARNING: ~a and ~a are both fillers of the same slot, yet they are unifiable~%" filler1 filler2)
	 )
	(t
	 (append
	  filler1
	  (extract-concept-body filler2)))
	)
  )

;; FUNCTION: extracts the slots in an expr (slots are returned in the direction of from head to tail
;; PRE: expr is either query or assertion
;; test:
;; (a Reaction with (raw-material ((a CaCO3 with (quantity ((a QV))))))) => (raw-material quantity)
;; (the quantity of (the raw-material of _Reaction3)) => (quantity raw-material)
;; (the quantity of (the raw-material of (a Reaction with (result ((a CaCO3 with (color ((a Color))))))))) => (quantity raw-material result color)
;; (a Reaction with (result ((a CaCO3 with (color ((the color of (the raw-material of Self)))))))) => (result color color raw-material
(defun extract-slots (expr)
  (let ((triples (convert-km-to-set-code expr)))
    (remove nil (mapcar #'second triples))
    ))

;; FUNCTION: exract the slot in the immediate triple
;; i.e. the one links the head & tail
;; PRE: expr either query or assertion
(defun extract-first-slot (expr)
  (cond 
   ((km-seq-queryp expr)
    nil)
   ((km-queryp expr)
    (first (decompose-query expr)))
   ((ls-nn-seq-expr expr)
    nil)
   ((ls-property-expr expr)
    nil)
   (t
    (first (extract-slots expr)))))

;; FUNCTION: extract the "head" of an assertion/query triple
;; may return either "type" or "instance" or expression
;; PRE: expr either query or assertion
(defun extract-head (expr)
  (cond 
   ((km-queryp expr)
    ;; this does NOT work with "called tag"!!
    (last-element expr))
   ((ls-nn-seq-expr expr)
    (second expr))
   ((minimatch expr '(|a| ?x |with| &rest));; this works with the form (a X with ...)
    `(|a| ,(second expr)))
   ((minimatch expr '(|every| ?x |has| &rest));; this works with the form (every X has ...)
    `(|a| ,(second expr)))
   ((minimatch expr '(?x |has| ?y));; this works with the form (*X has ...)
    (first expr))
   ((not (extract-concept-body expr));; case like (a H2O) or (a H2O called "new")
    expr)
   (t 
    (extract-concept-name expr))
   ))

;; FUNCTION: make sure always returns the head class or the class of the head instance
;; PRE: expr either query or assertion
(defun extract-head-class (expr)
  (let ((init-head (extract-head expr)))
    (cond ((retrieve-property-from-assertion expr '|instance-of|)
	   (first (retrieve-property-from-assertion expr '|instance-of|)))
	  ((car *ls-instance-of*)
	   (car *ls-instance-of*))
	  ((ls-inst? init-head)
	   (first (ls-km `(|the| |classes| |of| ,init-head) :fail-mode 'fail)))
	  ((equal init-head '|Self|)
	   (first (ls-km `(|the| |classes| |of| ,(first *ls-current-replaced-self*)) :fail-mode 'fail)))
	  ((atom init-head)
	   init-head)
	  ;; what if init-head is a query!! => it'll work just as well
	  (t
	   (first (ls-km `(|the| |classes| |of| ,init-head) :fail-mode 'fail))))
    
    ))

;; FUNCTION: make sure always returns the head instance or an instance of the head type 
;; PRE: expr either query/assertion
(defun extract-head-instance (expr)
  (declare (special *km-code-keywords*))
  (let ((init-head (extract-head expr))
	)
    (cond 
     ((ls-value-type init-head)
      init-head)
     ((aggregatep init-head)
      (first (ls-km `(|a| ,*ls-pair-seq-class*))))
     ((retrieve-property-from-assertion init-head '|instance-of|)
      (first (ls-km `(|a| ,(first (retrieve-property-from-assertion init-head '|instance-of|))))))
     ((car *ls-instance-of*)
      (first (ls-km `(|a| ,(car *ls-instance-of*)))))
     ((ls-inst? init-head) 
      init-head)
     ((equal init-head '|Self|)
					;(format t "current self = ~s~%" *ls-current-replaced-self*)
      (first (ls-km (first *ls-current-replaced-self*)))
      )
     ((or (not init-head )
	  (member init-head *km-code-keywords* :test #'equal))
      nil)
     ((atom init-head)
      (first (ls-km `(|a| ,init-head) :fail-mode 'fail)))
     ((km-queryp init-head)
      (if (ls-km init-head :fail-mode 'fail)
	  (first (ls-km init-head :fail-mode 'fail))
	(cond ((minimatch init-head '(|the| ?slot |of| &rest))
	       (first (ls-km `(|an| |instance| |of| (|the| |range| |of| ,(second init-head))))))
	      (t
	       (first (ls-km `(|an| |instance| |of| (|the| |range| |of| ,(third init-head)))))))))
     (t
      (first (ls-km init-head)))
     )
    ))

;; FUNCTION: get the tail part of a triple (nil for query)
;; PRE: expr either query/expression
(defun extract-tail (expr)
  (cond 
   ((km-queryp expr)
    nil)
   ((ls-nn-seq-expr expr)
    (third expr))
   ((minimatch expr '(|a| ?x |with| (&rest)));; this works with the form (a X with ...)
    (first (second (last-element expr))))
   ((minimatch expr '(|every| ?x |has| &rest));; this works with the form (every X has ...)
    (first (second (last-element expr))))
   ((minimatch expr '(?x |has| ?y));; this works with the form (*X has ...)
    (first (second (last-element expr))))
   (t
    (let ((triples (convert-km-to-set-code expr)))
      (car (third (first triples)))))
   ))

;; FUNCTION: get the class of the tail of a triple (nil for query)
;; PRE: expr either query/expression
(defun extract-tail-class (expr)
  (let ((init-tail (replace-self (extract-tail expr)))
	(tail-expr (first (second (first (extract-concept-body expr))))))
    (cond ((aggregatep init-tail);; limited interpretation of pair-seq !! why? examples?
	   *ls-pair-seq-class*)
	  ((retrieve-property-from-assertion tail-expr '|instance-of|)
	   (first (retrieve-property-from-assertion tail-expr '|instance-of|)))
	  ((ls-inst? init-tail)
	   (first (ls-km `(|the| |classes| |of| ,init-tail) :fail-mode 'fail)))
	  ((atom init-tail)
	   init-tail)
	  (t
	   (first (ls-km `(|the| |classes| |of| ,init-tail) :fail-mode 'fail))))
    )
  )

;; FUNCTION: get an instance of the tail of a triple (nil for query)
;; PRE: expr either query/expression
(defun extract-tail-instance (expr)
  (declare (special *KM-CODE-KEYWORDS*))
  (let* (
	 (init-tail (replace-self (extract-tail expr) nil)))
    ;; replace self is needed 
    ;; (format t "init-tail = ~S and self is like ~A replaced is like ~A~%" init-tail *ls-current-self* *ls-current-replaced-self*)
    (cond 
     ((or (not init-tail)
	  (equal init-tail '|nil|))
      nil)
     ((or (not init-tail )
	  (member init-tail *km-code-keywords* :test #'equal))
      nil)
     ((ls-inst? init-tail) 
      init-tail)
     ((and
       (atom init-tail)
       (classp init-tail))
      (first (ls-km `(|a| ,init-tail) :fail-mode 'fail)))
     ((ls-value-type init-tail)
      init-tail)
     ((aggregatep init-tail)
      (first (ls-km `(|a| ,*ls-pair-seq-class*))))
     ((equal init-tail '|Self|)
      ;;(format t "current self = ~s~%" *ls-current-replaced-self*)
      (first (ls-km (first *ls-current-replaced-self*)))
      )
     ((km-queryp init-tail)
      ;;(format t "query result = ~A~%" (ls-km init-tail))
      (if (ls-km init-tail :fail-mode 'fail)
	  (first (ls-km init-tail :fail-mode 'fail))
	(cond ((minimatch init-tail '(|the| ?slot |of| &rest))
	       (ls-km `(|an| |instance| |of| (|the| |range| |of| ,(second init-tail)))))
	      (t
	       (ls-km `(|an| |instance| |of| (|the| |range| |of| ,(third init-tail))))))))
     ((retrieve-property-from-assertion init-tail '|instance-of|)
      ;;(first (ls-km `(|a| ,(first (retrieve-property-from-assertion init-tail '|instance-of|)))))
      (first (ls-km init-tail))
      )
     (t
      (first (ls-km init-tail)))
     )
    )
  )

;; FUNCTION: replace the very last part of an assertion expr
;; PRE: oldexpr is assertion 
;; example (a Reaction with (raw-material ((a H2O-Substance) (a Chemical with (has-basic-structural-unit ((a Ca))))))) (a Na with (charge ((a Charge-Value)))) => (a Reaction with (raw-material ((a H2O-Substance) (a Chemical with (has-basic-structural-unit ((a Na with (charge ((a Charge-Value))))))))))
(defun replace-km-expr-end (oldexpr newend)
  (cond 
   ((simple-fillerp oldexpr)
    newend)
   ((minimatch oldexpr '(|a| ?x |with| &rest))
    (let* ((body (extract-concept-body oldexpr))
	   (head (safe-ldiff oldexpr body))
	   (last-slot-filler (last-element body))
	   (newbody (butlast body)));; remove the last element
      (append head 
	      (append newbody
		      (list (list (first last-slot-filler)
				  (replace-km-expr-end (second last-slot-filler) newend)))))
      
      ))
   ((minimatch oldexpr '(|a| ?x))
    newend)
   (t
    (if (and (consp oldexpr)
	     (> (length oldexpr)))
	(append
	 (butlast oldexpr)
	 (list (replace-km-expr-end (last-element oldexpr) newend)))
      (list (replace-km-expr-end (first oldexpr) newend))))
   ))

;; FUNCTION: replace the whole tail section of an triple
;; PRE: oldexpr is assertion (b/c query's tail is nil)
(defun replace-km-expr-tail (oldexpr newtail)
  (let ((head (safe-ldiff oldexpr (extract-concept-body oldexpr)))
	(slot (first (first (extract-concept-body oldexpr)))))
    (append
     head 
     (list (list slot (list newtail))))
    ))

;; FUNCTION: replace the tail with a new class
;; PRE: expr is assertion newtail isa class
;; test: (a Reaction with (raw-material ((a CaCO3)))) (a CaCO3-Substance) => (a Reaction with (raw-material ((a CaCO3-Substance))))
;; test: (the raw-material of (a Reaction with (raw-material ((a CaCO3))))) (a CaCO3-Substance) => (a Reaction with (raw-material ((a CaCO3-Substance))))
(defun replace-km-expr-tail-with-class (expr newtail)
  (let* ((body (extract-concept-body expr))
	 (header (safe-ldiff expr body))
	 (slot (first (first body)))
	 (tail (extract-tail expr)))
    (append
     header
     (list (list slot
		 (list (replace-km-expr-head-with-class tail newtail)))))
    ))

;; FUNCTION: replace the head with a new expr, if the new expr is an instance, then use replace-km-expr-head with inst
;; if the newhead starts with "every", then use "every X has", else assume it's "a X with"
;; PRE: expr is either assertion or query
(defun replace-km-expr-head (expr newhead)
  (cond ((km-queryp expr)
	 (append (safe-ldiff expr (last expr))
		 (list newhead)))
	;; else it's an assertion
	(t
	 (replace-km-expr-head-assertion expr newhead))))

(defun replace-km-expr-head-assertion (expr newhead)
  (let (head-replacement)
    (cond ((ls-inst? newhead)
	   (setf head-replacement newhead))
	  ((and (consp newhead)
		(equal '|every| (first newhead))
		(not (equal (last-element newhead) '|has|)))
	   (setf head-replacement (append newhead (list '|has|)))
	   )
	  ((and (consp newhead)
		(equal '|a| (first newhead))
		(not (equal (last-element newhead) '|with|)))
	   (setf head-replacement (append newhead (list '|with|))))
	  (t
	   (setf head-replacement newhead)))
    (cond 
     ((not newhead)
      expr)
     ((ls-inst? newhead)
      (replace-km-expr-head-with-inst expr head-replacement))
     ((or
       (minimatch expr '(|a| ?class &rest))
       (minimatch expr '(|every| ?class &rest)))
      (append head-replacement (cdr (cdr (cdr expr)))))
     ((consp newhead)
      ;; assume expr to be (inst has ...) case and newhead is of (a Class) form
      (replace-km-expr-head-with-class expr (second newhead))
      )
     (t
      (replace-km-expr-head-with-class expr newhead))
     )
    ))

;; FUNCTION: replace the head with a new class, so it really should be named replace-km-expr-head-class
;; PRE: expr either query or assertion & oldhead can an instance, newhead is a class
;; test: (a Reaction with (keq ((:pair 3e-15 nil)))) => (a Equilibrium-Reaction with (keq ((:pair 3e-15 nil))))
;; test: (the ki of (a Reaction)) => (the ki of (a Equilibrium-Reaction))
(defun replace-km-expr-head-with-class (expr newhead &optional (recursive t))
  (cond
   ((not expr)
    `(|a| ,newhead |with|))
   ((and (ls-inst? (extract-head expr))
	 (km-assertionp expr))
    (add-new-property-to-assertion (remove-property-from-assertion expr '|instance-of|)
				   (list '|instance-of| (list newhead))))
   ((km-queryp expr)
    (if recursive
	(append (safe-ldiff expr (last expr))
		`(,(replace-km-expr-head-with-class (extract-head expr) newhead nil)))
      `(|forall| ,expr (|It| |has| (|instance-of| (,newhead)))))
    )
   ((or
     (minimatch expr '(|a| ?class &rest))
     (minimatch expr '(|every| ?class &rest)))
    (append `(|a| ,newhead |with|) (cdr (cdr (cdr (remove-property-from-assertion expr '|instance-of|))))))
   ((atom expr)
    newhead)
   ((aggregatep expr)
    ;; just replace the whole thing
    `(|a| ,newhead))
   (t
    ;; assume to be (inst has ...) case
    (append `(|a| ,newhead |with|) (cdr (cdr (remove-property-from-assertion expr '|instance-of|)))))
   )
  )

;; FUNCTION: replace the head with a new instance
;; PRE: expr either query or assertion
;; test: (a Reaction with (keq ((:pair 3e-15 nil)))) => (_Equilibrium-Reaction3 has (keq ((:pair 3e-15 nil))))
(defun replace-km-expr-head-with-inst (expr newhead)
  (cond ((km-queryp expr)
	 (append
	  (remove (last-element expr) expr :test #'equal)
	  (list newhead)))
	((or
	  (minimatch expr '(|a| ?class &rest))
	  (minimatch expr '(|every| ?class &rest)))
	 (append `(,newhead |has|) (cdr (cdr (cdr expr)))))
	(t
	 ;; assume to be (inst has ...) case
	 (cons newhead (cdr expr))))
  )

;; FUNCTION: given an expr, a predicate and an action
;; if the predicate is true on a part of the expr, then apply the action on that part of the expr
;; PRE: predicate takes 4 arguments: code (the input of apply-action-to-km-expr) relationship (the current slot being examined), filler (the current filler being examined), depth (the current depth) returns t or nil
;;      action takes 3 arguments: code (the input of apply-action-to-km-expr) relationship (the current slot being examined), filler (the current filler being examined), always returns in the following form (frame slot newfiller)
(defun apply-action-to-km-expr (km-code test action &optional (depth 0))
  ;; if km-code is query, then
  ;; then skip to the end
  (cond ((km-queryp km-code)
	 (let* ((query-part-2 (last-element km-code))
		(query-part-1 (remove query-part-2 km-code :test #'equal)))
	   (append query-part-1 (list (apply-action-to-km-expr (last-element km-code) test action depth)))))
	(t
	 (apply-action-to-km-assertion-expr km-code test action depth))))

;; this has the bug of if km-code = (a H2O-Substance), then it'll never replace it with something else
(defun apply-action-to-km-assertion-expr (km-code test action depth)
  (let* ((concept-body (extract-concept-body km-code))
	 (retconcept 
	  (safe-ldiff km-code concept-body))
	 (header (extract-concept-header km-code))
	 (retbody nil)
	 (newtriple nil)
	 (new-filler nil)
	 )
    (if (funcall test header nil nil depth)
	(progn
	  (setf newtriple (funcall action header nil nil))
	  (if (first newtriple)
	      (setf retconcept
		    (first newtriple)))
	  (if (third newtriple)
	      (setf new-filler
		    (append
		     new-filler
		     (list
		      (third newtriple)))))))
    (dolist (slot-filler-pair concept-body)
	    (let ((new-filler nil)
		  (relationship (first slot-filler-pair))
		  newtriple
		  )
	      (dolist (filler (first (rest slot-filler-pair)))
		      (if (funcall test km-code relationship filler depth)
			  (progn
			    ;; action always returns in the following form
			    ;; (frame slot newfiller)
			    (setf newtriple (funcall action km-code relationship filler))
			    (if (first newtriple)
				(setf retconcept
				      (first newtriple)))
			    (if (third newtriple)
				(setf new-filler
				      (append
				       new-filler
				       (list
					(third newtriple))))))
			(setf new-filler
			      (append
			       new-filler
			       (list
				(apply-action-to-km-expr
				 filler test action (+ depth 1)))))
			)
		      )
	      (setf retbody
		    (append
		     retbody
		     (list
		      (list (first slot-filler-pair)
			    new-filler))))
	      ))
    (if retbody
	(progn
	  (ls-debug-msg 'apply-action-to-km-assertion-expr 
			(format nil "retconcept = ~s retbody = ~s~%" retconcept retbody))
	  (cond ((and (not (consp retconcept));; not a list
		      retconcept);; not nil either
		 (append (list retconcept '|has|) retbody))
		((and retconcept
		      (consp retconcept)
		      (not (member '|with| retconcept :test #'equal))
		      (not (member '|has| retconcept :test #'equal)));; if the header not include "with", add "with" to the header
		 (append (append retconcept (list '|with|)) retbody))
		(t
		 (append
		  retconcept
		  retbody))))
      retconcept)
    ))
	   

					;(defun ordered-convert-km-to-set-code (expr)
					;  (reverse (convert-km-to-set-code expr))
					;  )

(defun out-of-time (&optional (bound *ls-time-bound*))
  (>= (- (get-internal-run-time)
	 *ls-start-time*)
      bound))

(defun flatten-list (x)
  (cond
   ((null x) nil)
   ((atom x) (list x))
   ((and (consp x) (or (eql (car x) 'quote) (eql (car x) 'function))) nil)
   (t (append (flatten-list (car x)) (flatten-list (cdr x))))))

;; first check to see if the inst is canned version or has its own properties
(defun ls-get-slot-val (inst slot)
  (let ((own-properties (remove-if #'(lambda (property) (eql (first property) '|instance-of|))
				   (get-slotsvals (dereference inst) :facet 'own-properties))))
    ;; own-properties is a list of properties specific to the inst
    ;; it doesn't include (instance-of (Class)) type of properties
    (if (not (find slot own-properties :test #'eql :key #'first))
	;; try the cached version
	(ls-cached-slot-val inst slot)
      (ls-get-slot-val0 inst slot)
      )
    ))

(defun ls-cached-slot-val (inst slot)
;(defun ls-get-slot-val (inst slot)
  (let ((classes (ls-km `(|the| |instance-of| |of| ,inst)))
	(val nil))
    (setf val (ls-lookup-slot-val-cache inst classes slot))
    (when (eql val 'not-found)
      (setf val (ls-get-slot-val0 inst slot))
      (ls-store-slot-val-cache classes inst slot val))
    val
    ))

(defun ls-lookup-slot-val-cache (inst classes slot)
  (let ((cache-entry (desource (gethash (append-element classes slot) *ls-slot-val-cache* 'not-found))))
    (cond
     ((eql cache-entry 'not-found)
      cache-entry)
     (t
      (mapcar #'(lambda (entry)
		  (ls-generate-inst-from-slot-val-cache entry inst slot))
	      cache-entry))
     )))

(defun ls-generate-inst-from-slot-val-cache (entry inst slot)
  (declare (ignore slot inst))
  (let (;;(own-properties (second (find slot (get inst 'own-properties) :test #'eql :key #'first)))
	;;(all-properties (deep-substitute inst '|Self| (second entry) :test #'eql))
	(return-inst nil)
	)
    ;;(setf own-properties (deep-substitute inst '|Self| own-properties :test #'eql))
    (setf return-inst (create-instance '|Thing| `((|instance-of| ,(first entry)))))
    (cond
     #|((and (inherit-with-overrides-slotp slot)
	   (not (null own-properties)))
      own-properties)
     ((and
       (not (null own-properties))
       (multivalued-slotp slot))
      (ls-km `(,own-properties && ,return-inst))
      )
     ((and
       (not (null own-properties))
       (not (multivalued-slotp slot)))
      (ls-km `(,own-properties & ,return-inst))
      )|#
     (t
      return-inst))
    ))

(defun ls-store-slot-val-cache (classes inst slot vals)
  (if (not (find-if-not #'(lambda (val) (ls-inst? val)) vals)) ;; to see if there's an non-inst fillers
      (setf (gethash (append-element classes slot) *ls-slot-val-cache*)
	    (mapcar #'(lambda (val) (ls-prepare-slot-val-cache-one-val inst val))
		    vals)))
    )

(defun ls-prepare-slot-val-cache-one-val (inst val)
  ;; if the val does contain own-properties, then 
  (let ((own-properties (remove-if #'(lambda (property) (eql (first property) '|instance-of|))
				   (desource (get val 'own-properties))))
	(val-classes (ls-km `(|the| |instance-of| |of| ,val))))
    (setf own-properties
	  (deep-substitute
	   '|Self| inst own-properties :test #'eql))
    (list val-classes own-properties)
    ))

(defun ls-get-slot-val0 (inst slot)
  (when (consp inst)
      (setf inst (first inst)))
  (ls-km `(|the| ,slot |of| ,inst) :fail-mode 'fail)
  )

(defun ls-kb-search (init-unit-list slots goal-unit-list terminate-with-success-criteria
				    &key 
				    (control-strategy #'(lambda (candidates open)
							  (append open candidates)))
				    (min-depth 1)
				    (max-depth *ls-search-depth-bound*)
				    (search-subslot? nil)
				    (search-constraints? nil)
				    (multiple-answers? nil)
				    (pruning-function nil)
				    )
  (let ((result nil))
    (setf result
	  (kb-search init-unit-list slots goal-unit-list terminate-with-success-criteria
		     :control-strategy control-strategy 
		     :min-depth min-depth 
		     :max-depth max-depth 
		     :search-subslot? search-subslot?
		     :search-constraints? search-constraints? :multiple-answers? multiple-answers? :pruning-function pruning-function))
    result
    ))

;; assume X subsumes Y, determine the # of steps away Y is from X
;; X & Y are supposed to be classes
(defun taxonomical-distance (X Y)
  (let ((distance 0)
	(target-class (list X))
	(found nil)
	(currentclasses nil)
	)
    (setf currentclasses (list Y))
    (setf found
	  (and (intersection target-class currentclasses)
	       (not (set-difference target-class (intersection target-class currentclasses)))))
    (loop while
	  (and (not found)
	       (not 
		(and (not (equal target-class '(|Thing|)))
		     (equal currentclasses '(|Thing|)))))
	  do
	  #|(format t "c = ~S target-class = ~s intersection = ~s difference = ~s~%" 
		  currentclasses 
		  target-class 
		  (intersection target-class currentclasses) 
		  (set-difference target-class 
				  (intersection target-class currentclasses)))|#
	  (if (and (intersection target-class currentclasses)
		   (not (set-difference target-class (intersection target-class currentclasses))))
	      (setf found t)
	    (progn
	      (setf currentclasses 
		    (ls-km `(|forall| ,(cons ':|set| currentclasses) |where| |t| (|the| |superclasses| |of| |It|))))
	      
	      (setf distance (+ distance 1)))))
    ;;(format t "taxonomical-distance x ~s y ~S = ~a~%" x y distance)
    (if found
	distance
      1000000)
    ))

;; x and y are instances, which may have multiple classes
;; find the minimum distance between 
(defun absolute-taxonomical-distance-between-instances (X Y)
  (let ((x-classes 
	 (if (aggregatep x)
	     (list *ls-pair-seq-class*)
	   (ls-km `(|the| |classes| |of| ,X))))
	(y-classes 
	 (if (aggregatep y)
	     (list *ls-pair-seq-class*)
	   (ls-km `(|the| |classes| |of| ,Y)))))
    ;; get all the distances and find the min
    (apply #'min
	   (append
	    (mapcar #'(lambda (y-class)
			(cond 
			 ((member y-class x-classes :test #'equal)
			  0)
			 ((null X)
			  (taxonomical-distance '|Thing| y-class))
			 ((ls-km `(,X |isa| ,y-class))
			  (absolute-taxonomical-distance-between-inst-class X y-class))
			 (t
			  1000000)))
		    y-classes)
	    (mapcar #'(lambda (x-class)
			(cond
			 ((member x-class y-classes :test #'equal)
			  0)
			 ((null Y)
			  (taxonomical-distance '|Thing| x-class))
			 ((ls-km `(,Y |isa| ,x-class))
			  (absolute-taxonomical-distance-between-inst-class Y x-class))
			 (t
			  1000000)))
		    x-classes)))
    ))

(defun absolute-taxonomical-distance-between-inst-class (inst class)
  (let ((x-classes 
	 (cond ((aggregatep inst)
		(list *ls-pair-seq-class*))
	       ((ls-km `(|the| |classes| |of| ,inst))
		(ls-km `(|the| |classes| |of| ,inst)))
	       ((null inst)
		'(|Thing|))
	       (t
		'(|Thing|)))))
    ;; assume inst isa class
    (apply #'min
	   (mapcar #'(lambda (x-class)
		       (taxonomical-distance class x-class))
		   x-classes))
    ))

;; FUNCTION: replace all the slots w/ their inverses
;; PRE: result is in the form of search-result path
(defun reverse-path (result)
  ;;(format t "reverse this ~s~%" result)
  (reverse (mapcar #'(lambda (element)
		       ;;(format t "element = ~s~%" element)
		       (if (and (kb-objectp element)
				(ls-slotp element))
			   (progn
			     ;;(format t "element =~S is slot!~%" element)
			     (first (ls-km `(|the| |inverse| |of| ,element))))
			 element)) result)))

(defun gen-slot-filler-patterns (slot-fillers)
  (cond ((atom slot-fillers)
	 nil)
	(;;(consp slot-fillers)
	 (and (= (length slot-fillers) 2)
	      ;;(ls-inst? (second slot-fillers))
	      (not (is-named-instance (second slot-fillers))))
	 ;; assume the first is slot
	 `(,(first slot-fillers) ((|a| ,(second slot-fillers)))))
	((= (length slot-fillers) 2) ;; assume it's named inst filler or aggregate filler
	 `(,(first slot-fillers) (,(second slot-fillers))))
	((is-named-instance (second slot-fillers))
	 `(,(first slot-fillers) (,(list (second slot-fillers) '|has| (gen-slot-filler-patterns (cddr slot-fillers))))))
	(t
	 `(,(first slot-fillers) (,(list '|a| (second slot-fillers) '|with| (gen-slot-filler-patterns (cddr slot-fillers))))))
	))
  


;; property exprs are of the form 
;; (:nn *red PhysicalObject) or
;; (:nn *red _PhysicalObject3) or
;; (:nn (:pair *light Molecule) Molecule) or
;; (:nn (:pair 1 *mole) _Chemical3) or
;; (:nn (:pair 1 *mole) _Quantity-Value3)
(defun ls-property-expr (lsexpr)
  (and
   (listp lsexpr)
   (= (length lsexpr) 3)
   (equal (first lsexpr) ':|nn|)
   (not (and (property-concept? (second lsexpr))
	     (property-concept? (third lsexpr))
	     (or (clib-concept? (second lsexpr))
		 (clib-inst? (second lsexpr)))
	     (or (clib-concept? (third lsexpr))
		 (clib-inst? (third lsexpr)))))
   (or 
    (and
     ;; the first element is either a constant or a pair
     (property-concept? (second lsexpr))
     ;; the 
     ;; the second element must exist in the current KM
     (or (clib-concept? (third lsexpr))
	 (clib-inst? (third lsexpr))))
    (and
     ;; the second element is either a constant or a pair
     (property-concept? (third lsexpr))
     ;; the first element must exist in the current KM
     (or (clib-concept? (second lsexpr))
	 (clib-inst? (second lsexpr)))))
   ))

(defun ls-chemical-entity (inst)
  (or (km0 `(,inst |isa| |Atom|))
      (km0 `(,inst |isa| |Ion|)))
  )

(defun ls-chemical-entity-class (class)
  (or (member '|Atom| (all-superclasses class) :test #'eql)
      (member '|Ion| (all-superclasses class) :test #'eql))
  )

(defun ls-chemical-name-expr (lsexpr)
  (let (matches)
    (and 
     ;;(retrieve-property-from-assertion lsexpr '|instance-of|)
     (extract-head-class lsexpr)
     (setf matches (first 
		    (or (retrieve-property-from-assertion lsexpr '|context|)
			(retrieve-property-from-assertion lsexpr '|context-of|))))
     ;;(retrieve-property-from-assertion matches '|instance-of|))
     (extract-tail-class lsexpr))
  ))

(defun ls-nn-chemical-expr (lsexpr)
  (and (ls-chemical-entity (second lsexpr))
       (ls-chemical-entity (third lsexpr)))
  )

(defun ls-nn-seq-expr (lsexpr)
  (and
   ;; must be a list of 2 nouns
   (listp lsexpr)
   (= (length lsexpr) 3)
   (atom (first lsexpr))
   (atom (second lsexpr))
   (atom (third lsexpr))
   (equal (first lsexpr) ':|nn|)
   ;; the nouns cannot be KM reserved keywords
   (not (member (second lsexpr) *reserved-keywords*))
   (not (member (third lsexpr) *reserved-keywords*))
   ;; the nouns must exist in the current KM
   (or
    (numberp (string/= (write-frame0 (second lsexpr)) ""))
    (numberp (string/= (write-frame0 (ls-intern (string-downcase (string (second lsexpr))))) ""))
    (numberp (string/= (write-frame0 (convert-to-const (second lsexpr))) ""))
    )
   (or
    (numberp (string/= (write-frame0 (third lsexpr)) ""))
    (numberp (string/= (write-frame0 (ls-intern (string-downcase (string (third lsexpr))))) ""))
    (numberp (string/= (write-frame0 (convert-to-const (third lsexpr))) ""))
    )
   ))

(defun ls-slotp (s)
  (declare (special *km-code-keywords*))
  (and 
    (kb-objectp s)
    (not (member s (append '(:|pair| :|set|) *km-code-keywords*) :test #'eql))
    (or (slotp s)
	(member s '(|name| |name-of| |instance-of| |instances| |classes| |number| |Slot|) :test #'eql))
    ))

(defun ls-1-to-n-slotp (s)
  (or 
   (equal (ls-km `(|the| |cardinality| |of| ,s)) '(|1-to-N|))
   (equal (ls-km `(|the| |cardinality| |of| ,s)) '(|1-to-1|))
   )
  )

(defun forall-allofp (expr)
  (and (consp expr)
       (member (first expr) '(|forall| |forall2| |allof| |allof2| |oneof| |oneof2| |theoneof| |theoneof2| |forall-seq| |forall-seq2| |forall-bag| |forall-bag2|)))
  )

(defun forall-allof2p (expr)
  (and (consp expr)
       (member (first expr) '(|forall2| |allof2| |oneof2| |theoneof2| |forall-seq2| |forall-bag2|)))
  )

(defun boolean-expr (expr)
  (and 
   expr
   (consp expr)
   (or 
    (and
     (> (length expr) 2)
     (member (second expr) '(|and| |or| |=| |/=| |>| |<| |>=| |<=| |includes| |is-superset-of| |-| |+| |*| |==|) :test #'equal));; == is included as a hack to parse variables
    (and
     (= (length expr) 2)
     (member (first expr) '(|not| |numberp| |exists| |has-value|) :test #'equal))
    )
   )
  )

(defun condition-expr (expr)
  (and
   expr
   (consp expr)
   (>= (length expr) 4)
   (equal (first expr) '|if|)
   ;;(minimatch expr '(|if| ?x |then| &y))
   )
  )

;; (forall expr1 where expr2 expr3) => expr1
;; assume the input is (forall ...) format
(defun forall-range (expr)
  (second expr)
  )

(defun replace-forall-range (expr newrange)
  (substitute newrange (forall-range expr) expr :test #'equal)
  )


;; (forall expr1 where expr2 expr3) => expr2
;; assume the input is (forall ...) format
(defun forall-condition (expr)
  (if (member '|where| expr :test #'equal)
      (fourth expr)
    t)
  )

(defun replace-forall-condition (expr newcondition)
  (if (member '|where| expr :test #'equal)
      (substitute-n newcondition 3 expr)
    expr)
  )

;; (forall expr1 where expr2 expr3) => expr3
;; assume the input is (forall ...) format
(defun forall-transformation (expr)
  (if (member '|where| expr :test #'equal)
      (if (>= (length expr) 5)
	  (fifth expr))
    (if (>= (length expr) 3)
	(third expr)))
  )

(defun replace-forall-transformation (expr newtransformation)
  (if (member '|where| expr :test #'equal)
      (if (>= (length expr) 5)
	  (substitute-n newtransformation 4 expr))
    (if (>= (length expr) 3)
	(substitute-n newtransformation 2 expr)))
  )

;; given an KM assertion 
;; if it's in the form of 
;; (every X has ...)
;; change it to (a Y has ...)
(defun definition-to-instance-creation (expr)
  (let ((class (extract-head expr))
	(superclass nil))
    (cond ((equal (first expr) '|every|)
	   (setf superclass (first (ls-km `(|the| |superclasses| |of| ,class))))
	   ;; now do the substitution
	   (substitute '|with| '|has| (substitute '|a| '|every| (substitute superclass class expr :test #'equal) :test #'equal :count 1) :test #'equal :count 1)
	   )
	  (t
	   expr))
    ))

;; if the head of an assertion is a named instance, replace it with an anonymous one
;; PRE: expr is an assertion
(defun named-instance-removal (expr)
  (if (and (ls-inst? (extract-head expr))
	   (not (anonymous-instancep (extract-head expr))))
      (let ((class (if (retrieve-property-from-assertion expr '|instance-of|)
		       (first (retrieve-property-from-assertion expr '|instance-of|))
		     (first (ls-km `(|the| |classes| |of| ,(extract-head expr)))))))
	(append (list '|a| class '|with|)
		(extract-concept-body expr))
	)
    expr)
  )

;; if the given expression has instance
;; replace them with (a Class) expr
(defun instance-removal (expr)
  (apply-action-to-km-expr 
   expr
   #'(lambda (code relationship filler depth)
       (declare (ignore filler relationship depth))
       ;;(format t "code = ~A~%" code)
       (or 
	(and (consp code)
	     (= (length code) 1)
	     (ls-inst? (first code)))
	(and (ls-inst? code)
	     (not (aggregatep code)))))
   #'(lambda (code relationship filler)
       (declare (ignore relationship filler))
       (let ((class (first (ls-km `(|the| |classes| |of| ,code)))))
	 ;;(format t "classe = ~A~%" class)
	 (list (list '|a| class) nil nil))))
  )

;; replace "Self" with an instance 
(defun replace-self (expr &optional (save-self t))
					;(format t "*ls-current-replaced-self* = ~S~%" *ls-current-replaced-self*)
  (let ((self-instance nil))
    (cond ((member '|Self| (flatten-list expr) :test #'equal)
	   (setf self-instance (first (ls-km (first *ls-current-replaced-self*))))
	   (setf *ls-current-self-instance-tmp* self-instance)
	   (if save-self
	       (setf *ls-current-self-instance* self-instance))
	   (deep-substitute self-instance '|Self| expr :test #'equal))
	  (t
	   expr))
    ))

;; revert the "instance" with '|Self|
(defun revert-self (expr)
  ;;(format t "x = ~a~%" *ls-current-self-instance*)
  (if (and *ls-current-self-instance*
	   (anonymous-instancep *ls-current-self-instance*))
      (progn
	(deep-substitute '|Self| *ls-current-self-instance* expr :test #'equal))
    expr)
  )

;; replace the "It" in an expression with an instance
(defun replace-it (expr new-it degree)
  (cond 
   ((= degree 1)
    (deep-substitute new-it '|It| expr :test #'equal))
   (t
    (deep-substitute new-it '|It2| expr :test #'equal))
   )
  )

(defun revert-it (expr it-inst degree)
  (cond
   ((not it-inst);; if it-inst is nil
    expr)
   ((= degree 1)
    (deep-substitute '|It| it-inst expr :test #'equal))
   (t
    (deep-substitute '|It2| it-inst expr :test #'equal)))
  )

(defun deep-substitute (newitem olditem sequence &key test)
  (if (funcall test olditem sequence)
      newitem
    (if (consp sequence)
	(cons
	 (deep-substitute newitem olditem (car sequence) :test test)
	 (deep-substitute newitem olditem (cdr sequence) :test test))
      sequence))
  )

;; replace the nth item in the sequence with newitem
(defun substitute-n (newitem index sequence)
  (substitute-if newitem #'(lambda (x) (declare (ignore x)) t) sequence :start index :count 1)
  )

;; given a list of slots and a head
;; generate an access path
;; example: (atomic-mass has-basic-structural-unit) (a NaOH-Substance)
;; returns (the atomic-mass of (the has-basic-structural-unit of (a NaOH-Substance)))
(defun generate-access-path (slots head)
  (if (consp slots)
      (if (= (length slots) 1)
	  `(|the| ,(first slots) |of| ,head)
	`(|the| ,(car slots) |of| ,(generate-access-path (cdr slots) head)))
    head))

(defun simple-fillerp (filler)
					;(and (not (km-queryp filler))
					;     (not (nested-list filler)))
  (or (atom filler)
      (minimatch filler '(|a| ?class))
      (minimatch filler '(|a| ?class |with|))
      (minimatch filler '(?inst |has|))
      (aggregatep filler)
      )
  )

;; given (a Reaction) & (raw-material ((a Chemical))) => (a Reaction with (raw-material ((a Chemical))))
;; given (a Reaction with (raw-material ((a Chemical)))) & (result ((a Chemical))) => (a Reaction with (raw-material ((a Chemical))) (result ((a Chemical))))
;; given (every Reaction has (raw-material ((a Chemical)))) & (result ((a Chemical))) => (every Reaction with (raw-material ((a Chemical))) (result ((a Chemical))))
(defun add-new-property-to-assertion (expr property)
  (cond ((ls-inst? expr)
	 (cons expr 
	       (list '|has|
		     property)))
	((or (member '|has| expr :test #'equal)
	     (member '|with| expr :test #'equal))
	 (append expr (list property)))
	(t
	 (append expr (list '|with| property)))
	)
  )

;; remove one slot from an assertion
(defun remove-property-from-assertion (expr slotname)
  (let* ((body (extract-concept-body expr))
	 (head (extract-concept-head expr)))
    (if body
	(append head 
		(remove-if #'(lambda (slot-filler) 
			       (equal (first slot-filler) slotname))
			   body))
      expr)
    ))

;; remove the fillers of a slot
(defun retrieve-property-from-assertion (expr slotname)
  (if (not (km-assertionp expr))
      nil
    (let* ((body (extract-concept-body expr)))
      (append-list
       (mapcar #'(lambda (slot-filler)
		   (if (equal (first slot-filler) slotname)
		       (second slot-filler)))
	       body))
      )))

;; given a slot and a new property
;; replace the old property, if orig exp doesn't contain the property, append it
;; NOTE: property order is not retained
(defun replace-property-from-assertion (expr slotname newproperty)
  (let ((tmpexpr expr))
    (if (retrieve-property-from-assertion expr slotname)
	(setf tmpexpr (remove-property-from-assertion expr slotname)))
    (add-new-property-to-assertion tmpexpr newproperty)
    ))

;; given an assertion and a concept body
;; replace the properties of the assertion by the properties in the given body one by one
(defun replace-properties-from-assertion (expr concept-body)
  (if (not (km-assertionp expr))
      expr
    (let ((current-expr expr))
      (dolist (slot-filler concept-body)
	      (setf current-expr 
		    (replace-property-from-assertion current-expr 
						     (first slot-filler)
						     slot-filler)))
      current-expr
      )))

;; given a query of the form (the slot of ...) or (the Class slot of ...)
;; decompose it into: (slot head), where head is the "..." of the expr
(defun decompose-query (expr)
  (cond ((minimatch expr '(|the| ?class ?slot |of| ?expr))
	 `(,(third expr) ,(fifth expr)))
	(t
	 ;; assume its of the form (the slot of ...)
	 `(,(second expr) (fourth expr))))
  )

(defun match-domains-or-ranges (domains inst-x)
  (cond ((not domains)
	 t)
	((aggregatep inst-x)
	 (let ((violation t))
	   (dolist (domain domains)
		   (if (ls-km `(,domain |subsumes| ,*ls-pair-seq-class*))
		       (setf violation nil)))
	   (not violation)))
	(t
	 (let ((violation t))
	   (dolist (domain domains)
		   (if (ls-km `(,inst-x |isa| ,domain))
		       (setf violation nil)))
	   (not violation)
	   )))
  )

;; determine if a search from ls-search/kb-search is successful or not
;; it also works for results from (mapcar #'ls-search aggregate)
(defun successful-search? (search-results)
  (remove nil
	  (mapcar #'(lambda (result)
		      (cond ((consp (first result))
			     (successful-search? result));; in case of aggregate search result
			    ((equal (first result) 
				    'success)
			     result)
			    (t
			     nil)))
		  search-results))
  )

;; given a current instance and goals
;; return a boolean answer to see if the current instance satisfies the goal
;; by default, it uses preexisting-path as the test condition
(defun search-head-stop-condition (current-inst goals expr &optional (test-condition #'preexisting-path))
  (declare (ignore goals))
  ;; if the current-inst is not a (:seq ..) type then 
  ;; first replace the expr head with curr inst
  ;; and then check to see if new expr has preexisting path
  (if (not (aggregatep current-inst))
      (let* ((new-expr
	      (replace-km-expr-head-with-inst
	       expr current-inst))
	     (slot (extract-first-slot new-expr))
	     (domains (get-domain-of-slot slot)))
	(if (remove-if #'(lambda (domain) (not (ls-km `(,current-inst |isa| ,domain)))) domains)
	    ;; removes any replacements that violates domain constraint
	    (if				;(or (km-queryp new-expr) ;; why is it any different for query?
		(and (not (member 'domain-mismatch (constraint-violation (extract-head new-expr)) :test #'equal))
		     (specializable new-expr));; for cases like (_H2O-Substance with (instance-of (Hydrolysis)))
		(funcall test-condition new-expr)))
	)
    )
  )

;; FUNCTION: replace the nn exprs in a KM expr so that the resulting expr can be evaluated by KM
;; PRE: expr is a KM expr
;; useful for creating *ls-current-replaced-self*, 
(defun remove-nn (expr)
  (deep-substitute
   nil
   nil;; old item not relevant because use the lambda function for testing anyway
   expr :test #'(lambda (x y)
		  (declare (ignore x))
		  (ls-nn-seq-expr y)))
  )

;; given a list, append it to the stack top
(defun append-stack-top (item stack)
  (let ((top (pop stack)))
    (push (cons item top) stack)
    ))

(defun ls-add-last-action (action)
  (setf *ls-last-parsing-action*
	(append-stack-top action *ls-last-parsing-action*))
  )

(defun does-last-action-include (action &key (actions nil) (check-stack t))
  ;;(format t "ls-last action = ~S~%" *ls-last-parsing-action*)
  (if (not check-stack)
      (member action actions :test #'equal)
    (member action (car *ls-last-parsing-action*) :test #'equal))
  ;; this is OK because:
  ;; if action = specialize tail, then it'll be immediately check after parsing
  ;; if action = specialize head, then the tails should be popped from the stack already
  )

(defun ls-add-specialized-head (head)
  (setf *ls-specialized-heads* (append-stack-top head *ls-specialized-heads*))
  )

(defun ls-add-specialized-tail (tail)
  (setf *ls-specialized-tails* (append-stack-top tail *ls-specialized-tails*))
  )

;; given a list of expr, extract the classes from each expr using extract-func function
;; return a list of (class index) pairs
(defun extract-class-index-pairs (exprs extract-func)
  (let ((ret-list nil))
    ;; extract the (class index) pairs for the given exprs
    (dotimes (i (length exprs))
	     (setf ret-list
		   (append ret-list
			   `((,(funcall extract-func (nth i exprs)) ,i)))))
    ret-list
    ))


;; given two classes, return the more specific one if one subsumes another
;; otherwise nil
(defun semantic-equal (class1 class2)
  (cond
   ((equal class1 class2)
    t)
   ((km `(,class1 |subsumes| ,class2))
    class2)
   ((km `(,class2 |subsumes| ,class1))
    class1)
   (t
    nil)
   ))

;; given 2 sets of classes
;; find the intersection of the sets
;; return a list of the pairs with the most specific class as the 2nd in the pair
(defun semantic-intersection (set1 set2)
  (let ((result nil)
	(comparison nil))
    (dolist (element1 set1)
	    (dolist (element2 set2)
		    (setf comparison 
			  (semantic-equal element1 element2))
		    (cond ((equal comparison t)
			   (setf result (append result 
						(list (list element1 element2)))))
			  ((equal comparison element1)
			   (setf result (append result
						(list (list element2 element1)))))
			  ((equal comparison element2)
			   (setf result (append result
						(list (list element1 element2)))))
			  (t 
			   nil))))
    result
    ))

;; need a function that takes in 2 sets pairs or (class index) pairs
;; do a semantic-intersection on the classes, and returns two new sets of (class index) pairs
;; assume there's no subsumption relation among the classes in each input set
(defun solve-linear-dependency-0 (heads tails)
  (let ((head-classes nil)
	(tail-classes nil)
	(common-classes nil)
	(class-index-pair nil)
	(index nil)
	(new-heads nil)
	)
    ;; extract the (class index pairs) for the heads
    (setf head-classes (extract-class-index-pairs heads #'extract-head-class))
    ;; extract the (class index pairs) for the tails
    (setf tail-classes (extract-class-index-pairs tails #'extract-tail-class))
    ;; common-classes are the semantic intersection of heads & tails
    (setf common-classes (semantic-intersection 
			  (mapcar #'first head-classes)
			  (mapcar #'first tail-classes)))
    ;; common-classes is a list of pairs (class1 class2), where class1 subsumes class2
    (dolist (common-class common-classes)
	    (cond ((setf class-index-pair (find-if #'(lambda (c) (equal (first common-class) (first c))) head-classes))
		   ;; need to replace the expr with more specific class
		   ;; first find the index
		   (setf index (second class-index-pair))
		   ;; then do the replacement
		   (setf new-heads
			 (append new-heads
				 (list (replace-km-expr-head-with-class (nth index heads) (second common-class)))))
		   )
		  ((setf class-index-pair (find-if #'(lambda (c) (equal (second common-class) (first c))) head-classes))
		   ;; no replacement needed
		   )
		  )
	    )
    new-heads
    ))

;; input: head, slot, filler & a new filler, last action, specialized-heads, specialized-tails
;; output: a new expr
;; case1: A-r1-B-r2-C, and when B is specialized into B', it causes problem
;; A == new-head, r1 == slot, filler = B-r2-C, newfiller = B', actioins = SPECIALIZE-HEAD specialized-heads = B'-r2-C
(defun solve-linear-dependency (new-head slot filler newfiller actions specialized-heads)
  (let (
	(specialized-filler-exprs nil)
	(specialized-tails nil)
	(output-expr nil)
	(specialized-filler nil)
	)
    ;; specialized-filler is set to the filler on the stack if available
    (setf specialized-filler
	  (if (first specialized-heads)
	      (replace-properties-from-assertion 
	       newfiller
	       (extract-concept-body (first (first specialized-heads))))
	    newfiller))
    (setf output-expr (check-for-ls (append new-head (list (list slot (list specialized-filler))))))
    ;; this could be: 1> old filler doesn't have any LS, therefore nothing is changed
    ;; 2> newfiller is the result of specialize-head, therefore nothing is changed yet
    (cond 
     ((and (does-last-action-include 'specialize-head :actions actions :check-stack nil)
	   (does-last-action-include 'specialize-tail))
      (format t "1~%")
      
      (setf specialized-tails (car *ls-specialized-tails*))
					;(format t "head ~A tail popped ~A~%" specialized-heads specialized-tails)
      ;; do the semantic intersection of the 2 sets
      (setf specialized-filler-exprs (solve-linear-dependency-0 (first specialized-heads)  
						    ;; because top of *ls-specialized-heads* is a list of heads now
						    (first specialized-tails)))

      ;; because don't care about specialize-tail again, remove the specialize-tail
      (pop *ls-last-parsing-action*)
      (pop *ls-specialized-tails*)

      ;; prompt user for the right choice.
      (append new-head 
	      (list 
	       (list slot 
		     (list 
		      (prompt-for-clarification filler specialized-filler-exprs))))))
     ((does-last-action-include 'specialize-head :actions actions :check-stack nil)
      (format t "2~%")
      ;; no need to repush the heads on because the specialized head is applied below
      (setf specialized-filler-exprs 
	    (mapcar #'(lambda (specialized-head)
			(replace-km-expr-head
			 ;;newfiller
			 specialized-filler
			 (extract-head specialized-head)
			 ))
		    (first specialized-heads)))
      ;; prompt user for the right choice.
      (append new-head 
	      (list 
	       (list slot 
		     (list 
		      (prompt-for-clarification filler specialized-filler-exprs))))))
     ((does-last-action-include 'specialize-tail)
					;(format t "3~%")

      ;; because don't care about specialize-tail again, remove the specialize-tail
      (pop *ls-last-parsing-action*)
      (setf specialized-tails (pop *ls-specialized-tails*))

      (prompt-for-clarification 
       ;; orig expr
       (append new-head `((,slot (,filler))))
       (car specialized-tails))
      )
     (t output-expr))
    ))

;; expr is assertion and has multiple (instance-of ...)
;; reduce it to as few classes as possible
(defun reduce-assertion-class (expr)
  (let ((class-definitions nil))
    (cond
     ((and (km-assertionp expr)
	   (setf class-definitions (retrieve-property-from-assertion expr '|instance-of|))
	   (> (length class-definitions) 1))
      ;; remove any class that has a more specific class definition in it
      (setf class-definitions (remove-duplicates class-definitions))
      (setf class-definitions
	    (remove-if #'(lambda (class)
			   (member-if
			    #'(lambda (compared-class)
				(let ((equality	(semantic-equal compared-class class)))
				  (and equality
				       (not (equal equality class))))
				)
			    (remove class class-definitions))
			   )
		       class-definitions))
      (add-new-property-to-assertion
       (remove-property-from-assertion expr '|instance-of|)
       `(|instance-of| ,class-definitions))
      )
     (t
      expr))
    ))
;; FUNCTION: given a list of a list of expressions, find the semantic intersection of the head classes of the exprs
;; prompt the user for clarification, and returns the new expression
;; Problem: the list of expressions may not be so valid anymore because early case1 problems. (let's ignore this problem for now)
;; problem solved by using new-body
(defun solve-tree-dependency-0 (head1 head2)
  (let ((head1-classes nil)
	(head2-classes nil)
	(common-classes nil)
	(new-exprs nil)
	(index nil)
	(class-index-pair nil)
	)
    ;; extract the (class index pairs) for the heads
    (setf head1-classes (extract-class-index-pairs head1 #'extract-head-class))
    (setf head2-classes (extract-class-index-pairs head2 #'extract-head-class))
    
    ;; do the intersection
    (setf common-classes (semantic-intersection 
			  (mapcar #'first head1-classes)
			  (mapcar #'first head2-classes)))
    (dolist (common-class common-classes)
	    (cond ((setf class-index-pair (find-if #'(lambda (c) (equal (first common-class) (first c))) head1-classes))
		   ;; need to replace the expr with more specific class
		   ;; first find the index
		   (setf index (second class-index-pair))
		   ;; then do the replacement
		   (setf new-exprs
			 (append new-exprs
				 (list (replace-km-expr-head-with-class (nth index head1) (second common-class)))))
		   )
		  ((setf class-index-pair (find-if #'(lambda (c) (equal (second common-class) (first c))) head1-classes))
		   ;; no replacement needed
		   )
		  )
	    )
    new-exprs
    ))

;; input: an expr
;; output: refined expr
;; case2: A-r1-B and A-r2-C, and when A is specialized into A', it causes problem
(defun solve-tree-dependency (expr actions specialized-heads)
  ;; (format t "ls-specialized-heads = ~s~%" *ls-specialized-heads*)
  ;; (format t "last parsing action = ~S~%" *ls-last-parsing-action*)
  (setf expr
	(reduce-assertion-class expr))
  (setf specialized-heads
	(mapcar #'(lambda (sh-list)
		    (mapcar #'reduce-assertion-class sh-list))
		specialized-heads))
  (cond 
   ((and
     (km-queryp expr)
     (km-queryp 
      (extract-head expr)))
    ;; for cases like (|the| |value| |of| (|the| |solubility| |of| ((|the| |result| ...) |has| (|instance-of| (|Ionic-Compound-Substance|)))))
    ;; no need to process it even if the head specializes its head
    expr)
   ((and
     (km-queryp expr)
     (aggregatep (extract-head expr)))
    expr) ;; for cases like (the ki of (:set (a Reaction) (a Hydrolysis)))
   ((does-last-action-include 'specialize-head :actions actions :check-stack nil)
    (let ((specialized-new-result nil)
	  (ret-result nil))
      ;; keep a note that the resulting expr is the result of specialize head
      ;; for cases like (X-r1-(Y-r2-Z and Y-r2'-Z')), such that case2 occur in (Y-r2-Z and Y-r2'-Z'), and case1 occur in X-r1-...
      (if (> (length specialized-heads) 1)
	  (progn
	    (setf specialized-new-result 
		  (solve-tree-dependency-0 (first specialized-heads) (second specialized-heads)))
	    (dolist (other-head (cdr (cdr specialized-heads)));; what if the expr has more than what *ls-specialized-heads* has??
		    (setf specialized-new-result
			  (solve-tree-dependency-0 specialized-new-result other-head))))
	(setf specialized-new-result 
	      (mapcar #'(lambda (head)
			  (replace-km-expr-head-with-class
			   expr
			   (extract-head-class head)))
		      (first specialized-heads))))
      ;; shouldn't use nil as input for clarification !!??
      (setf ret-result (prompt-for-clarification nil specialized-new-result))
      ret-result
      ))
   (t
    expr))
  )

(defun sort-based-on-preexisting-or-not (exprs)
  (sort exprs
	#'(lambda (x y)
	    (if (and (preexisting-path x)
		     (not (preexisting-path y)))
		t)))
  )

;; given an assertion
;; reorder the triples based on whether a triple depends on another or not
(defun ls-reorder-triples (expr)
  (cond
   ((or 
     (not expr)
     (atom expr)
     (km-queryp expr)
     (simple-fillerp expr)
     (forall-allofp expr)
     (boolean-expr expr))
    expr)
   (t
    ;; now extract the head, and go through all the slots
    (let* ((body (extract-concept-body expr))
	   (head (safe-ldiff expr body))
	   )
      (append
       head
       (sort-based-on-dependency body))
      ))
   )
  )

;; given the body of an assertion
;; sort the slots based on which slot's value depends on which
(defun sort-based-on-dependency (body)
  (let ((independent-list nil)
	(dependent-list nil))
    (dolist (slot-fillers body)
	    (if (not (contains-self-reference slot-fillers))
		(setf independent-list 
		      (append independent-list (list slot-fillers)))
	      (setf dependent-list 
		    (append dependent-list (list slot-fillers)) )))
    
    ;; now order the dependent-list
    ;; algorithm:
    ;; do until the dependent-set is empty
    ;; if an element from the dependent-set not depends on the dependent-set at all,
    ;; then append it to independent-set, and remove it from dependent-set 
    (do ()
	((not dependent-list)
	 independent-list)
	(dolist (dependent dependent-list)
		(let ((dependencies (contains-self-reference dependent)))
		  (cond 
		   ((not
		     (intersection dependencies
				   (mapcar #'first dependent-list)
				   :test #'equal))
		    (setf dependent-list
			  (remove dependent dependent-list :test #'equal))
		    (setf independent-list
			  (append independent-list
				  (list dependent))))
		   (t
		    nil)))
		)
	)
    ))

;; checks to see if the contains self-reference
;; if so returns a list of the slots being referenced
(defun contains-self-reference (slot-fillers)
  (let ((fillers (second slot-fillers)))
    (apply
     #'append
     (mapcar #'contains-self-reference0
	     fillers))
    ))

(defun contains-self-reference0 (filler)
  (cond
   ((not filler)
    nil)
   ((not (consp filler))
    nil)
   ((and (km-queryp filler)
	 (not (nested-list filler)))
    (minimatch filler '(|the| ?slot |of| |Self|)))
   ((and (km-queryp filler)
	 (nested-list filler))
    (contains-self-reference0 (extract-head filler)))
   ((forall-allofp filler)
    (append
     (contains-self-reference0 (forall-range filler))
     (contains-self-reference0 (forall-condition filler))
     (contains-self-reference0 (forall-transformation filler))))
   ((boolean-expr filler)
    (append-list
     (mapcar #'(lambda (x)
		 (if (consp x)
		     (contains-self-reference0 x)))
	     filler)))
   (t
    (append-list
     (mapcar #'contains-self-reference
	     (extract-concept-body filler))))
   ))

;; given an expr, replicate it to be an anonymous instance declaration
(defun clone-instance (expr)
  (instance-removal (named-instance-removal (remove-nn (definition-to-instance-creation expr))))
  )

;; given an expr, check to see if there is instance-of property and if there is, then check to see the head can be specialized
(defun specializable (expr)
  (cond
   ((km-queryp expr)
    (specializable (extract-head expr)))
   ((retrieve-property-from-assertion expr '|instance-of|)
    (ls-km `(,(extract-head-instance expr) |&?| (|a| ,(first (retrieve-property-from-assertion expr '|instance-of|)))))
    )
   (t
    t)
   ))

;; possibly value type of things
(defun ls-value-type (expr)
  (and (aggregatep expr)
       (equal (first expr)
	      ':|pair|)
       ;; and every one of the elements are atoms
       (not
	(remove-if #'atom
		   (cdr expr)))
       ;; and not every one of the elements are insts
       (remove-if #'(lambda (instance)
		      (or (and 
			   (kb-objectp instance)
			   (or (get-vals instance '|instance-of| :facet 'own-properties :situation *global-situation*)		
			       (get-vals instance '|instance-of| :facet 'own-definition :situation *global-situation*)))))
		  (cdr expr))
       )
  )

(defun ls-objectp (expr)
  (or (kb-objectp expr)
      (and (aggregatep expr) 
	   ;; every element in the aggregate is either kb-object or aggregate
	   (not
	    (remove-if
	     #'(lambda (element)
		 (or (kb-objectp element)
		     (consp element)
		     (aggregatep element)))
	     (cdr expr)
	     )))
      )
  )

;; check to see if the search results are for aggregates
;; i.e. if the result is of the form (((success ...) (fail ..)) ((fail ...))) instead of ((success ..) (fail ..))
(defun aggregate-search-results? (expr)
  (and (consp expr)
       (consp (first expr))
       (consp (first (first expr)))
       )
  )

;; recognizes if the search-results are from (mapcar #'specialize-head/tail aggregates)
;; if the result is of the form (((nil ((success ...) (fail ..)) ((fail ...))))  ((subclass ((success ...) (fail ..)) ((fail ...))))) instead of ((success ..) (fail ..))
(defun aggregate-specialize-search-results? (search-results)
  ;; first it's a list
  (and (consp search-results)
       ;; all the elments are lists
       (consp (first search-results))
       ;; and each element is made of a list of subclass & search-result pairs
       (consp (first (first search-results)))
       (minimatch (first (first search-results))
		  '(?x &rest))
       (minimatch (second (first (first search-results)))
		  '(?x &rest))
       )
  )

;; calls solve-tree-dependency after parse-for-ls-one-iteration
(defun parse-and-flush (expr)
  (multiple-value-bind
   (result actions heads)
   (parse-for-ls-one-iteration expr)
   (solve-tree-dependency result actions heads))
  )

;; given a list of search results from specialize-head/tail, 
;; sort them based on which one has more subclasses
(defun sort-based-on-prosperity (search-results)
  (sort search-results
	#'(lambda (result1 result2)
	    (> (first (km `(|the| |number| |of| (|the| |all-subclasses| |of| ,(first result1)))))
	       (first (km `(|the| |number| |of| (|the| |all-subclasses| |of| ,(first result2)))))
	       )))
  )

(defun ls-debug-msg (level msg)
  (when (or (equal *ls-debug-mode* 'all)
	    (equal *ls-debug-mode* level)
	    (and (consp *ls-debug-mode*)
		 (member level *ls-debug-mode* :test #'equal)))
    (format t "~A~%" msg))
  )

(defun append-cpl-choices (choices)
  (let ((preexists (find-if #'(lambda (choice) (equal (first choice) (first choices))) *ls-cpl-choices*)))
    (cond (preexists
	   (setf *ls-cpl-choices* (remove preexists *ls-cpl-choices* :test #'equal))
	   (setf *ls-cpl-choices*
		 (append-element *ls-cpl-choices* (append preexists (cdr choices))))
	   )
	  (t
	   (setf *ls-cpl-choices* (append-element *ls-cpl-choices* choices))))
    ))
;;;; ============================================================================
;;;; ====			   kb-search.lisp                            ====
;;;; ============================================================================


;;;------------------- Parameters & Constants --------------
(defparameter *ls-default-max-depth* 4)
(defparameter *ls-default-min-depth* 0)
(defparameter *ls-search-21-open* nil)

(defconstant *ls-max-search-21-depth* 2)
(defconstant *ls-max-search-21-answers* 10)
(defconstant *ls-max-search-21-trials* 30)
(defconstant *max-answer-length* 4)

;;(defparameter *ls-kb-search-open-list* nil)
;;(defparameter *ls-kb-search-closed-list* nil)

(defparameter *ls-candidate-types* 
  '((|Intangible-Entity| 1) (|Aggregate| 1) (|Object| 3) (|Substance| 3) (|Action| 1) (|State| 1) (|Role| 0) (|Tangible-Entity| 2) (|Spatial-Entity| 1) (|Place| 2)))
(defparameter *ls-categories* 
  '(
    (|Y| |with| (|element-type-of| ((|a| |X|)))) ;0
    (|Y| |with| (|element-type| ((|a| |X|)))) ;1
    (|Y| |with| (|possessed-by| ((|a| |X|)))) ;2
    (|Y| |with| (|is-part-of| ((|a| |X|)))) ;3
    (|Y| |with| (|has-part| ((|a| |X|)))) ;4
    (|Y| |with| (|is-basic-functional-unit-of| ((|a| |X|)))) ;5
    (|Y| |with| (|possesses| ((|a| |X|)))) ;6
    (|Y| |with| (|material| ((|a| |X|)))) ;7
    (|Y| |with| (|instrment-of| ((|a| |X|)))) ;8
    (|Y| |with| (|plays| ((|a| |X|))))	;9
    (|Y| |with| (|played-by| ((|a| |X|)))) ;10
    (|Y| |with| (|in-event-of| ((|a| |X|)))) ;11
    (|Y| |with| (|purpose| ((|a| |X|)))) ;12
    (|Y| |with| (|purpose-of| ((|a| |X|)))) ;13
    (|Y| |with| (|is-at| ((|a| |X|))))	;14
    (|Y| |with| (|inside| ((|a| |X|)))) ;15
    (|Y| |with| (|during| ((|a| |X|)))) ;16
    (|Y| |with| (|object| ((|a| |X|)))) ;17
    (|Y| |with| (|location-of| ((|a| |X|)))) ;18
    (|Y| |with| (|result-of| ((|a| |X|)))) ;19
    (|Y| |with| (|result-of| ((|a| |Create| |with|
			       (|agent| ((|a| |X|))))))) ;20
    (|Y| |with| (|agent-of| ((|a| |Create| |with|
			      (|result| ((|a| |X|))))))) ;21
    (|Y| |with| (|result-of| ((|a| |Create| |with|
			       (|raw-material| ((|a| |X|))))))) ;22
    (|Y| |with| (|object-of| ((|a| |Move| |with|
			       (|source| ((|a| |X|))))))) ;23
    (|Y|)				;24
    (|Y| |with| (|object| ((|a| |Entity| |with| (|plays| ((|a| |X|))))))) ;25
    (nil)				;26
    (|the| |Y| |of| (|a| |X|))		;27
    (|X|)				;28
    (|Y| |with| (|agent-of| ((|a| |Event| |with| (|object| ((|a| |X|))))))) ;29
    (|Y| |with| (|agent| ((|a| |X|))))	;30
    (|Y| |with| (|topic-of| ((|a| |X|)))) ;31
    (|Y| |with| (|age| ((|a| |Age-Value| |with|
			 (|value| ((:|pair| |X| |Y|)))))));;32
    (|Y| |with| (|animacy| ((|a| |Animacy-Value| |with|
			     (|value| ((:|pair| |X| |Y|)))))));;33
    (|Y| |with| (|area| ((|a| |Area-Value| |with|
			  (|value| ((:|pair| |X| |Y|)))))));;34
    (|Y| |with| (|breakability| ((|a| |Breakability-Value| |with|
				  (|value| ((:|pair| |X| |Y|)))))));;35
    (|Y| |with| (|brightness| ((|a| |Brightness-Value| |with|
				(|value| ((:|pair| |X| |Y|)))))));;36
    (|Y| |with| (|capacity| ((|a| |Capacity-Value| |with|
			      (|value| ((:|pair| |X| |Y|)))))));;37
    (|Y| |with| (|color| ((|a| |Color-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;38
    (|Y| |with| (|consistency| ((|a| |Consistency-Value| |with|
				 (|value| ((:|pair| |X| |Y|)))))));;39
    (|Y| |with| (|density| ((|a| |Density-Value| |with|
			     (|value| ((:|pair| |X| |Y|)))))));;35
    (|Y| |with| (|depth| ((|a| |Depth-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;36
    (|Y| |with| (|direction| ((|a| |Direction-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;37
    (|Y| |with| (|distance| ((|a| |Distance-Value| |with|
			      (|value| ((:|pair| |X| |Y|)))))));;38
    (|Y| |with| (|duration| ((|a| |Duration-Value| |with|
			      (|value| ((:|pair| |X| |Y|)))))));;39
    (|Y| |with| (|frequency| ((|a| |Frequency-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;40
    (|Y| |with| (|height| ((|a| |Height-Value| |with|
			    (|value| ((:|pair| |X| |Y|)))))));;41
    (|Y| |with| (|integrity| ((|a| |Integrity-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;42
    (|Y| |with| (|intensity| ((|a| |Intensity-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;43
    (|Y| |with| (|length| ((|a| |Length-Value| |with|
			    (|value| ((:|pair| |X| |Y|)))))));;44
    (|Y| |with| (|manner| ((|a| |Manner-Value| |with|
			    (|value| ((:|pair| |X| |Y|)))))));;45
    (|Y| |with| (|mass| ((|a| |Mass-Value| |with|
			  (|value| ((:|pair| |X| |Y|)))))));;46
    (|Y| |with| (|pH| ((|a| |PH-Value| |with|
			(|value| ((:|pair| |X| |Y|)))))));;47
    (|Y| |with| (|polarity| ((|a| |Polarity-Value| |with|
			      (|value| ((:|pair| |X| |Y|)))))));;48
    (|Y| |with| (|property| ((|a| |Property-Value| |with|
			      (|value| ((:|pair| |X| |Y|)))))));;49
    (|Y| |with| (|rate| ((|a| |Rate-Value| |with|
			  (|value| ((:|pair| |X| |Y|)))))));;50
    (|Y| |with| (|sentience| ((|a| |Sentience-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;51
    (|Y| |with| (|sex| ((|a| |Sex-Value| |with|
			 (|value| ((:|pair| |X| |Y|)))))));;52
    (|Y| |with| (|shape| ((|a| |Shape-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;53
    (|Y| |with| (|size| ((|a| |Size-Value| |with|
			  (|value| ((:|pair| |X| |Y|)))))));;54
    (|Y| |with| (|slope| ((|a| |Slope-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;55
    (|Y| |with| (|smell| ((|a| |Smell-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;56
    (|Y| |with| (|taste| ((|a| |Taste-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;57
    (|Y| |with| (|temperature| ((|a| |Temperature-Value| |with|
				 (|value| ((:|pair| |X| |Y|)))))));;58
    (|Y| |with| (|texture| ((|a| |Texture-Value| |with|
			     (|value| ((:|pair| |X| |Y|)))))));;59
    (|Y| |with| (|thickness| ((|a| |Thickness-Value| |with|
			       (|value| ((:|pair| |X| |Y|)))))));;60
    (|Y| |with| (|trait| ((|a| |Trait-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;61
    (|Y| |with| (|truth| ((|a| |Truth-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;62
    (|Y| |with| (|volume| ((|a| |Volume-Value| |with|
			    (|value| ((:|pair| |X| |Y|)))))));;63
    (|Y| |with| (|wetness| ((|a| |Wetness-Value| |with|
			     (|value| ((:|pair| |X| |Y|)))))));;64
    (|Y| |with| (|width| ((|a| |Width-Value| |with|
			   (|value| ((:|pair| |X| |Y|)))))));;65
    ))

(defparameter *ls-template-rules*
  #| this approach has the benefit of not constrained by Horn's clause, ie. because of Horn's clause restriction, we have to have a single type as the type restriction of the range. This forces us to bundle several types into one, and we may have to relax the range restriction so that it may include types that should not be included, such as range = entity, even though "place" should not be included |#
  #| this approach has the disadvantage of not allow 2nd order restrictions:, ie. cannot restrict things like "protein molecules" because cannot express X is a supertype of Y|#
  '(
    ((|Aggregate| |Entity|) 0)
    ((|Entity| |Aggregate|) 1)
    ((|Aggregate| |Entity|) 2)
    ((|Spatial-Entity| |Entity|) 2)
    ;;((|Tangible-Entity| |Tangible-Entity|) 3) -- handled by search 19
    ((|Tangible-Entity| |Tangible-Entity|) 4)
    ;;((|Entity| |Entity|) 5) -- handled by search 19
    ((|Entity| |Aggregate| ) 6)
    ((|Entity| |Spatial-Entity|) 6)
    ((|Tangible-Entity| |Object|) 7)
    ((|Event| |Tangible-Entity|) 8)
    ((|Event| |Aggregate|) 8)
    ((|Entity| |Role|) 9)
    ((|Role| |Entity|) 10)
    ((|Event| |Role|) 11)
    ((|Role| |Entity|) 12)
    ((|Entity| |Role|) 13)
    ((|Spatial-Entity| |Tangible-Entity|) 14)
    ;;15, 16 not implemented yet
    ((|Entity| |Event|) 17)
    ((|Place| |Spatial-Entity|) 18)
    ((|Event| |Entity|) 19)
    ((|Entity| |Entity|) 20)
    ((|Entity| |Entity|) 21)
    ((|Entity| |Entity|) 22)
    ((|Spatial-Entity| |Entity|) 23)
    ))

(defparameter *ls-precise-rules*
  '(
    ((|Aggregate| |Object|) 0)
    ((|Object| |Aggregate|) 1)
    ((|Object| |Object|) 4)
    ((|Substance| |Object|) 7)
    ((|Event| |Tangible-Entity|) 8)
    ((|Event| |Aggregate|) 8)
    ((|Entity| |Role|) 9)
    ((|Role| |Entity|) 10)
    ((|Event| |Role|) 11)
    ((|Role| |Entity|) 12)
    ((|Entity| |Role|) 13)
    ((|Entity| |Event|) 17)
    ((|Place| |Spatial-Entity|) 18)
    ((|Event| |Entity|) 19)
    ;;((|Object| |Entity|) 22)
    ((|Substance| |Entity|) 22)
    ((|Object| |Object|) 20)
    ((|Bond| |Substance|) 20)
    ((|Chemical-Entity| |Object|) 20)
    ((|Chemical-Entity| |Substance|) 20)
    ;;((|Entity| |Object|) 21)
    ))



;;;----------------Prepackaged Searches ---------------------
;; given an input instance 
(defun is_super_or_subclass (input-insts input-goals)
					;(is_equal input-insts input-goals)
					;(is_subclass input-insts input-goals)
					;(is_superclass input-insts input-goals)
					;(is_super_or_subclass0 input-insts input-goals)
  (funcall *ls-stop-criteria* input-insts input-goals)
  )

(defun is_equal (input-insts input-goals)
  (let ((inst 
	 (if (consp input-insts)
	     (mapcar #'(lambda (input-inst)
			 (if (is-an-class input-inst)
			     (first (ls-km `(|a| ,input-inst)))
			   input-inst))
		     input-insts)
	   (if (is-an-class input-insts)
	       (first (ls-km `(|a| ,input-insts)))
	     input-insts)))
	(goals (mapcar #'(lambda (goal)
			   (if (is-an-class goal)
			       (first (ls-km `(|a| ,goal)))
			     goal))
		       input-goals)))
    (ls-debug-msg 'is_equal 
		  (format nil "inst = ~A goals = ~A~%" inst goals))
    (member inst goals :test #'(lambda (candidate goal)
				 (let (result)
				   (cond 
				    ((not goal);; if the goal is nil then anything is subclass/superclasses of goal
				     t)
				    ((not (ls-inst? goal))
				     ;; if goal is a class, then nil b/c this function is for inst vs inst
				     nil)
				    ((aggregatep goal)
				     (or (aggregatep candidate);; both of them are :pair
					 (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))));; or 1 is just a Thing
				    ((aggregatep candidate)
				     (or (aggregatep goal);; both of them are :pair
					 (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
				    (t
				     (setf result 
					   (intersection
					    (ls-km `(|allof| (|the| |classes| |of| ,goal) |where| |t|))
					    (ls-km `(|allof| (|the| |classes| |of| ,candidate) |where| |t|))))
				     (when result
					; profiling
				       (incf (gethash 'subclass-sc-used *ls-profiling-data* 0)))
				     result)
				    ))))
    )
  )

(defun is_subclass (input-insts input-goals)
  (let ((inst 
	 (if (consp input-insts)
	     (mapcar #'(lambda (input-inst)
			 (if (is-an-class input-inst)
			     (first (ls-km `(|a| ,input-inst)))
			   input-inst))
		     input-insts)
	   (if (is-an-class input-insts)
	       (first (ls-km `(|a| ,input-insts)))
	     input-insts)))
	(goals (mapcar #'(lambda (goal)
			   (if (is-an-class goal)
			       (first (ls-km `(|a| ,goal)))
			     goal))
		       input-goals)))
    (ls-debug-msg 'is_subclass (format nil "inst = ~A goals = ~A~%" inst goals))
    (or (member inst goals :test #'(lambda (candidate goal)
				     (let (result)
				       (cond 
					((not goal);; if the goal is nil then anything is subclass/superclasses of goal
					 t)
					((not (ls-inst? goal))
					 ;; if goal is a class, then nil b/c this function is for inst vs inst
					 nil)
					((aggregatep goal)
					 (or (aggregatep candidate);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))));; or 1 is just a Thing
					((aggregatep candidate)
					 (or (aggregatep goal);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					(t
					 (setf result (ls-km `(|allof| (|the| |classes| |of| ,goal) |where| ((,candidate |isa| |It|)))))
					 (when result
					; profiling
					   (incf (gethash 'subclass-sc-used *ls-profiling-data* 0)))
					 result)
					))))
	#|(member inst goals :test #'(lambda (candidate goal)
				     (let (result)
				       (cond
					((not candidate)
					 ;; if the candidate is nil then anything is subclass/superclasses of goal
					 t)
					((not (ls-inst? candidate))
					 ;; if the candidate is not an instance, then nil
					 nil)
					((aggregatep goal)
					 (or (aggregatep candidate);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))))
					((aggregatep candidate)
					 (or (aggregatep goal);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					
					(t
					 (setf result (ls-km `(|allof| (|the| |classes| |of| ,candidate) |where| ((,goal |isa| |It|)))))
					 ; profiling
					 (incf (gethash 'superclass-sc-used *ls-profiling-data* 0))
					 result))
				       )
				     ))|#
	)
    )
  )


(defun is_superclass (input-insts input-goals)
  (let ((inst 
	 (if (consp input-insts)
	     (mapcar #'(lambda (input-inst)
			 (if (is-an-class input-inst)
			     (first (ls-km `(|a| ,input-inst)))
			   input-inst))
		     input-insts)
	   (if (is-an-class input-insts)
	       (first (ls-km `(|a| ,input-insts)))
	     input-insts)))
	(goals (mapcar #'(lambda (goal)
			   (if (is-an-class goal)
			       (first (ls-km `(|a| ,goal)))
			     goal))
		       input-goals)))
    (ls-debug-msg 'is_superclass (format nil "inst = ~A goals = ~A~%" inst goals))
    (or #|(member inst goals :test #'(lambda (candidate goal)
				     (let (result)
				       (cond 
					((not goal);; if the goal is nil then anything is subclass/superclasses of goal
					 t)
					((not (ls-inst? goal))
					 ;; if goal is a class, then nil b/c this function is for inst vs inst
					 nil)
					((aggregatep goal)
					 (or (aggregatep candidate);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))));; or 1 is just a Thing
					((aggregatep candidate)
					 (or (aggregatep goal);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					(t
					 (setf result (ls-km `(|allof| (|the| |classes| |of| ,goal) |where| ((,candidate |isa| |It|)))))
					 (when result
					; profiling
					   (incf (gethash 'subclass-sc-used *ls-profiling-data* 0)))
					 result)
					))))|#
					 (member inst goals :test #'(lambda (candidate goal)
								      (let (result)
									(cond
									 ((not candidate)
									  ;; if the candidate is nil then anything is subclass/superclasses of goal
									  t)
									 ((not (ls-inst? candidate))
									  ;; if the candidate is not an instance, then nil
									  nil)
									 ((aggregatep goal)
									  (or (aggregatep candidate);; both of them are :pair
									      (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))))
									 ((aggregatep candidate)
									  (or (aggregatep goal);; both of them are :pair
									      (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					
									 (t
									  (setf result (ls-km `(|allof| (|the| |classes| |of| ,candidate) |where| ((,goal |isa| |It|)))))
					; profiling
									  (incf (gethash 'superclass-sc-used *ls-profiling-data* 0))
									  result))
									)
								      )))
    )
  )

;; this search uses super/superclasses
;; limits the max-depth to 1
(defun Is_super_or_subclass0 (input-insts input-goals)
  (let ((inst 
	 (cond
	  ((aggregatep input-insts)
	   input-insts)
	  ((consp input-insts)
	   (first (mapcar #'(lambda (input-inst)
			      (if (is-an-class input-inst)
				  (first (ls-km `(|a| ,input-inst)))
				input-inst))
			  input-insts)))
	  ((is-an-class input-insts)
	   (first (ls-km `(|a| ,input-insts))))
	  (t
	   input-insts)))
	(goals (mapcar #'(lambda (goal)
			   (if (is-an-class goal)
			       (first (ls-km `(|a| ,goal)))
			     goal))
		       input-goals)))
    (ls-debug-msg 'is_super_or_subclass0 (format nil "inst = ~A goals = ~A~%" inst goals))
    (or (member inst goals :test #'(lambda (candidate goal)
				     (let (result)
				       (cond 
					((not goal);; if the goal is nil then anything is subclass/superclasses of goal
					 t)
					((not (ls-inst? goal))
					 ;; if goal is a class, then nil b/c this function is for inst vs inst
					 nil)
					((aggregatep goal)
					 (or (aggregatep candidate);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))));; or 1 is just a Thing
					((aggregatep candidate)
					 (or (aggregatep goal);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					(t
					 (setf result (ls-km `(|allof| (|the| |classes| |of| ,goal) |where| ((,candidate |isa| |It|)))))
					 (when result
					; profiling
					   (incf (gethash 'subclass-sc-used *ls-profiling-data* 0)))
					 result)
					))))
	(member inst goals :test #'(lambda (candidate goal)
				     (let (result)
				       (cond
					((not candidate)
					 ;; if the candidate is nil then anything is subclass/superclasses of goal
					 t)
					((not (ls-inst? candidate))
					 ;; if the candidate is not an instance, then nil
					 nil)
					((aggregatep goal)
					 (or (aggregatep candidate);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,candidate) |includes| |Thing|))))
					((aggregatep candidate)
					 (or (aggregatep goal);; both of them are :pair
					     (ls-km `((|the| |classes| |of| ,goal) |includes| |Thing|))));; or 1 is just a Thing
					
					(t
					 (setf result (ls-km `(|allof| (|the| |classes| |of| ,candidate) |where| ((,goal |isa| |It|)))))
					; profiling
					 (incf (gethash 'superclass-sc-used *ls-profiling-data* 0))
					 result))
				       )
				     )))
    ))

(defun search-result-ranking-distance (result target-inst)
  (if (equal (first result) 'fail)
      0
    (absolute-taxonomical-distance-between-instances target-inst (second result)))
  )

;; same as search-2, except use ls-search instead of breadth-first-search
;; 
(defun search-3 (t1 t2) 
  (cond ((is-slot t2)
	 `((|the| ,t2 |of| (|a| ,t1))))
	((is-slot t1)
	 `((|the| ,t1 |of| (|a| ,t2))))
	(t
	 (let* (
		(i1 (if (ls-inst? t1)
			(list t1)
		      (ls-create-inst `(|a| ,t1))))
		(i2 (if (ls-inst? t2)
			(list t2)
		      (ls-create-inst `(|a| ,t2))))
		(result1 
		 (ls-search (first i1) nil (first i2))))
	   ;; need to reverse the results because
	   ;; ls-search results are from i1 to i2
	   ;; we need it from i2 to i1
	   (setf result1
		 (mapcar #'(lambda (search-result)
			     (list (first search-result)
				   (last-element (remove-if-not #'kb-objectp (third search-result)))
				   (reverse-search-path (third search-result) 0)))
			 result1))
	   (remove-duplicates
	    (gen-patterns result1)
	    :test #'equal))))
  )

(defun search-2 (t1 t2)
  (cond ((is-slot t2)
	 `((|the| ,t2 |of| (|a| ,t1))))
	((is-slot t1)
	 `((|the| ,t1 |of| (|a| ,t2))))
	(t
	 (let* (
		(i1 (if (ls-inst? t1)
			(list t1)
		      (ls-create-inst `(|a| ,t1))))
		(i2 (if (ls-inst? t2)
			(list t2)
		      (ls-create-inst `(|a| ,t2))))
		(result1 
		 (mapcar #'(lambda (result)
			     (list result (search-result-ranking-distance result i2)))
			 (kb-breadth-search i1 i2 :terminate-with-success-criteria *ls-stop-criteria* :max-depth *ls-search-depth-bound* :multiple-answers? t)))
		(result2 
		 (mapcar #'(lambda (result)
			     (list result (search-result-ranking-distance result i1)))
			 (kb-breadth-search i2 i1 :terminate-with-success-criteria *ls-stop-criteria* :max-depth *ls-search-depth-bound* :multiple-answers? t))))
	   (remove-duplicates
	    (gen-patterns 
	     (mapcar #'first
		     (sort (append result1 result2) #'<
			   :key #'second)))
	    :test #'equal)))))


(defun kb-breadth-search (source goal
				 &key 
				 (slots nil)
				 (search-subslot? nil)
				 (terminate-with-success-criteria #'Is_Inst_Of)
				 (search-constraints? nil)
				 (min-depth 0)
				 (max-depth *ls-default-max-depth*)
				 (multiple-answers? nil)
				 (free-slots nil) (open nil) (closed nil))
  (if (not slots)
      ;; set slots to be every slot except Properties and KM-Slot-Group
      (setf slots (all-searchable-slots)))
  #|(setf slots
	    (cons '|element-type|
		  (cons '|feature-slot|
			(set-difference
			 (all-instances '|Slot|)
			 (append (all-instances '|Property|)
				 (all-instances '|KM-Slot-Group|)))))))|#
  #|(setf slots (cons '|element-type| 
			(cons '|feature-slot|
			      (append (all-instances '|Interface-Slot|) (all-instances '|Relation|))))))|#
			      ;; now try to search w/o any partonymy slots: ie. has-part, has-structural-part has-functioinal-part element-type
			      ;; or try to search w/o telelogical slots: ie. purpose, purpose-of
			      ;; or try to search w/o action participants: object, object-of, agent, agent-of, instrument, instrument-of, base, base-of, 
			      ;;(setf slots (set-difference slots '(|object| |object-of| |agent| |agent-of| |instrument| |instrument-of| |base| |base-of|) :test #'equal))
  (kb-search source slots goal terminate-with-success-criteria
	     :search-subslot? search-subslot?
	     :search-constraints? search-constraints?
	     :max-depth max-depth
	     :min-depth min-depth
	     :control-strategy #'(lambda (candidates open)
				   (append open candidates))
	     :multiple-answers? multiple-answers?
	     :free-slots free-slots
	     :open open
	     :closed closed)
  )

(defun Is_Inst_Of (inst goals)
  (member inst goals :test #'(lambda (candidate goal)
			       (ls-km `(|allof| (|the| |classes| |of| ,goal) |where| (,candidate |isa| |It|))))
	  ))

;;;----------------The Search Engine/Skeleton---------------------
;;; NOTE: THE FIRST TWO ARGUMENTS ARE LISTS, NOT ATOMS.
;;;       Ditto for the argument Genl-units.
;;;
;;; given: - initial-unit-list: list of units that serve as initial
;;;                             source(s) in the search
;;;
;;;        - slots            : list of slots through which search
;;;                             may pass 
;;;
;;;        - search-subslots?  : if given as t, then searches both
;;;                             slots and their progeny
;;;                             (default: nil)
;;;
;;;        - goal-units       : list of units that serve as final
;;;                             success states in the search
;;;
;;;        - terminate-with-success-criteria
;;;                           : predicate on units that returns t
;;;                             if the unit to which it is applied
;;;                             is to be considered a goal
;;;
;;;        - terminate-with-failure-criteria
;;;                           : predicate on units that returns t
;;;                             if the unit to which it is applied
;;;                             is to be considered a failure
;;;
;;;        - pruning-function : predicate on units that returns t
;;;                             if the unit and all of its successors
;;;                             should be excluded from the search
;;;
;;;        - control-strategy : a function that takes in a open list
;;;                             and a candidate list, adds candidate list
;;;                             to open, and return a new open list
;;;                             default is "append" function, which means breadth-first
;;;
;;;
;;; use of arguments:
;;;      - user must provide initial-unit-list and slots
;;;      - user must provide at least one of
;;;        terminate-with-success-criteria
;;;        or goal-units; he may provide both
;;;      - all other arguments are optional
;;;
;;; returns one of:
;;;      - (fail no-goal-found)
;;;      - (fail terminate-with-failure <failure-unit>)
;;;      - (success <goal-unit> <path>)
;;;
;;; if <failure-unit> is non-null, its value is the unit
;;; at which the terminate-with-failure-criteria returned t
;;;
;;; if <goal-unit> is non-null, its value is either the unit
;;; at which the terminate-with-success-criteria returned t
;;; or is a member of goal-units
;;;
;;; Notes on removing duplicates from open-list during search:
;;;    1) in the case when a path will not be returned from the search,
;;;       duplicates are removed.  Reason: the code doesn't, and can't,
;;;       exploit information about the path to each node on the
;;;       open-list; therefore, different paths to the same node
;;;       are equivalent.
;;;    2) in the case when a path will be returned from the search:
;;;       a) the current implementation of this search program does
;;;          not permit the user to apply predicates on paths (e.g. to 
;;;          measure the length of a path, or to order nodes on open, 
;;;          best-first search, based on characteristics of their paths).
;;;          These are options that might be added later.
;;;       b) so, duplicates are removed from the open-list in this
;;;          implementation.  However, we've tried to document the
;;;          parts of the code that might be modified later.
;;;       c) interestingly, the method for removing duplicates does not
;;;          depend on control strategy.  For both breadth-first and
;;;          depth-first search, duplicates are removed from the 
;;;          end of the open-list.  For depth-first search, this insures
;;;          that newly placed nodes on open overshadow older copies;
;;;          for breadth-first search, this insures that older copies
;;;          overshadow newly placed ones.

;;; Organization of File
;;; --------------------
;;; 1. General KB-Search function
;;; 2. Path-Finding Search functions
;;; 3. Non-Path-Finding Search functions
;;; 4. Auxiliary functions used by both (2) and (3)
;;; 5. Test function calls


;;; this top-level function simply collects all the slots that will
;;; searched (by collecting the progeny of slots, a passed parameter,
;;; when search-progeny?=T) and calling one of two search programs,
;;; based on whether paths are being collected.
(defun kb-search (initial-unit-list slots
				    goal-units
				    terminate-with-success-criteria
				    &key search-subslot?
				    terminate-with-failure-criteria
				    pruning-function
				    (control-strategy #'append)
				    (search-constraints? nil)
				    (max-depth 6)
				    (min-depth 0)
				    (multiple-answers? nil)
				    (free-slots nil)
				    (open nil)
				    (closed nil))
  (setf *ls-start-time* (get-internal-run-time))
  (if (not (or goal-units terminate-with-success-criteria));; need to specify a goal unit
      '(fail no-goal-requested)
    (let ((slots-to-search (if search-subslot?
			       (remove-duplicates 
				(reduce #'append
					(mapcar #'(lambda (slot)
						    (list slot))
						slots))
				:test #'equal)
			     slots)))
      (kb-search-for-path initial-unit-list
			  slots-to-search
			  goal-units
			  terminate-with-success-criteria
			  terminate-with-failure-criteria
			  pruning-function
			  control-strategy
			  search-constraints? max-depth min-depth multiple-answers? free-slots open closed)
      )))
  
;;;-------------------------------------------------------------------
;;; Path-finding functions (return a solution path)
;;;
;;; a solution path is of the form:
;;;      (<initial-unit> s1 u1 s2 u2 ... sn <goal-unit>)
;;; where <initial-unit> occurs in the initial-unit-list
;;; and s1, s2,..., sn are slots that occur in slots-to-search
;;;
;;; the variable Open is a list of pairs of the form
;;;      (<path from initial-unit to unit-i> <unit-i> depth cost)
;;; where initial-unit is in the initial-unit-list and
;;; unit-i is reachable through slots-to-search
(defun kb-search-for-path (initial-unit-list
			   slots-to-search
			   goal-units
			   terminate-with-success-criteria
			   terminate-with-failure-criteria
			   pruning-function
			   control-strategy
			   search-constraints?
			   max-depth min-depth multiple-answers? free-slots open closed )
  (let ((result-depth nil)
	(current-unit nil)
	(current-path nil)
	(current-cost nil)
	(current-depth nil)
	ls-kb-search-closed-list ls-kb-search-open-list
	);; result-depth == the depth of the first result found
    (setf ls-kb-search-open-list (cond 
				  ((null open) (initialize-open-for-paths initial-unit-list))
				  ((equal open 'no-more) nil)
				  (t open)))
    (setf ls-kb-search-closed-list closed)
    (do ((terminated? nil))
	((equal (first terminated?) 'done)
	 (cdr terminated?))
	(cond ((null ls-kb-search-open-list);; nothing else to search
	       (if *ls-print-depth-limit*
		   (format t "current-depth is ~a~%" current-depth))
	       (if terminated?
		   (setf terminated? (cons 'done terminated?))
		 (setf terminated? (list 'done '(fail no-goal-found)))));; change here to (list 'done nil)
	      (t
	       (setf current-unit (second (first ls-kb-search-open-list)))
	       (setf current-path (first (first ls-kb-search-open-list)))
	       (setf current-cost (third (first ls-kb-search-open-list)))
	       (setf current-depth (fourth (first ls-kb-search-open-list)))
	       (if (not result-depth)
		   (setf terminated?
			 (check-termination 
			  current-unit
			  current-path
			  current-depth
			  min-depth
			  goal-units
			  terminate-with-success-criteria
			  terminate-with-failure-criteria ls-kb-search-open-list multiple-answers? terminated?))
		 (setf terminated?
		       (check-termination 
			current-unit
			current-path
			result-depth
			min-depth
			goal-units
			terminate-with-success-criteria
			terminate-with-failure-criteria ls-kb-search-open-list multiple-answers? terminated?)))
	       (if (and (not result-depth)
			terminated?)
		   (setf result-depth current-depth))
	       (ls-debug-msg 'kb-search-for-path
			     (format nil "Current-unit: ~a~%number of opens: ~a~%Open: ~S~%Terminated?: ~a~%~%" current-unit (length ls-kb-search-open-list) ls-kb-search-open-list terminated?))
	       (when (or (and (not terminated?) (not *ls-exhaustive-search*))
			 (not (equal (first terminated?) 'done)))
		 (setf ls-kb-search-closed-list (cons (list current-unit current-path current-cost current-depth) ls-kb-search-closed-list ))
		 ;; update open list only for the following condition
		 ;; terminated? = nil OR
		 ;; multiple-answer = t AND
		 ;; there is something in the open-list that is shallower than the current-depth
		 ;; WRONG!!!
		 (setf ls-kb-search-open-list
		       (update-open  (rest ls-kb-search-open-list)
				     current-unit
				     current-path
				     current-cost
				     current-depth
				     (mapcar #'first ls-kb-search-closed-list)
				     slots-to-search
				     control-strategy
				     pruning-function search-constraints? max-depth free-slots multiple-answers? (not (not terminated?))))
		 ))
	      ))
    ))

;; assume list is sorted, insert element i in at the right spot
(defun in-order-insert (i list &key (predicate #'<=) (key nil))
  (let ((after nil)
	(index 0))
    (setf index
	  (position-if 
	   #'(lambda (x) 
	       (cond 
		(key
		 (funcall predicate
			  (funcall key i)
			  (funcall key x)))
		(t
		 (funcall predicate i x))))
	   list))
    (cond
     ((null index)
      (setf list (append-element list i)))
     ((and (numberp index)
	   (= index 0))
      (setf list 
	    (cons i list)))
     (t
      (setf index
	    (- index 1))
      (setf after (nthcdr index list))
      (setf (cdr after) (cons i (cdr after)))))
    list
    ))
;; assume open is a sorted list and insert the items in additions-to-open to open while preserving the sorted order
;; the sorting is done based on the path length
(defun directed-bi-dir-search-append (additions-to-open open)
  (dolist (i additions-to-open)
	  (setf open
		(in-order-insert i open :key #'third)))
  open
  )

;; same as check-termination, except that returns only when 
(defun direct-bi-dir-search-check-termination (current-unit current-path current-depth min-depth goal-units terminate-with-success-criteria terminate-with-failure-criteria open multiple-answers? prev-results)
  (declare (ignore multiple-answers? open terminate-with-failure-criteria))
  (let ((result prev-results))
    (setf result 
	  (cond ((and 
		  (>= current-depth min-depth)
		  terminate-with-success-criteria
		  (funcall terminate-with-success-criteria current-unit goal-units))
		 (cons (list 'success current-unit current-path)
		       result))
		((out-of-time)
		 (cons 'done result))
		(t result)))
    result
    ))

;; to do: 1> set cost in append DONE2> define an append function that is in-order insert DONE 3> change termination criterion to be unterminated except when out of time 4> change return list
(defun directed-bi-dir-search (init-units slots goal-units terminate-with-success-criteria &key terminate-with-failure-criteria pruning-function (control-strategy #'directed-bi-dir-search-append) (search-constraints? nil) (max-depth 3) (min-depth 0) (multiple-answers? nil) (free-slots nil))
  (if (and (null goal-units)
	   (null init-units)
	   (null terminate-with-success-criteria))
      '(fail no-goal-requested)
    (let
	((terminated? nil)
	 (source-terminated? nil)
	 (dest-terminated? nil)
	 
	 (source-open-list (initialize-open-for-paths init-units))
	 (dest-open-list (initialize-open-for-paths goal-units))
	 
	 (source-goal-list (initialize-open-for-paths goal-units))
	 (dest-goal-list (initialize-open-for-paths init-units))
	 
	 (source-current nil)
	 (dest-current nil)
	 
	 (closed nil);; for avoiding loops
	 )
      (setf *ls-start-time* (get-internal-run-time))
      (loop while (not (equal (first terminated?) 'done))
	    do
	    ;;(format t "open list = ~S ~S~%" source-open-list dest-open-list)
	    (cond ((and (null source-open-list)
			(null dest-open-list))
		   ;; nothing left to search
		   (if terminated?
		       (setf terminated? (cons 'done terminated?))
		     (setf terminated? (list 'done '(fail no-goal-found)))))
		  ;; else check for termination 
		  (t
		   (setf source-current (first source-open-list))
		   (setf dest-current (first dest-open-list))

		   (if source-current
		       (setf source-terminated? 
			     (direct-bi-dir-search-check-termination
			      (second source-current)
			      (first source-current)
			      (fourth source-current)
			      min-depth
			      (remove nil (mapcar #'second source-goal-list));; nil shouldn't be included
			      terminate-with-success-criteria
			      terminate-with-failure-criteria
			      source-open-list
			      multiple-answers?
			      source-terminated?)
			     )
		     )

		   (if dest-current
		       (setf dest-terminated? 
			     (direct-bi-dir-search-check-termination
			      (second dest-current)
			      (first dest-current)
			      (fourth dest-current)
			      min-depth
			      (remove nil (mapcar #'second dest-goal-list));; nil shouldn't be included
			      terminate-with-success-criteria
			      terminate-with-failure-criteria
			      dest-open-list
			      multiple-answers?
			      dest-terminated?)
			     )
		     )

		   ;; now merge the two terminated results
		   (setf terminated?
			 (merge-terminated source-terminated? 
					   dest-terminated? 
					   source-goal-list 
					   dest-goal-list terminate-with-success-criteria))
		   
		   (when (or (not terminated?)
			     (not (equal (first terminated?) 'done)))
		     (setf closed (cons (second source-current)
					(cons (second dest-current)
					      closed)))
		     ;; update open lists
		     (setf source-open-list
			   (update-open (cdr source-open-list)
					(second source-current)
					(first source-current)
					(third source-current)
					(fourth source-current)
					closed
					slots
					control-strategy
					pruning-function
					search-constraints?
					max-depth
					free-slots
					multiple-answers?
					(not (not source-terminated?))
					t
					))
		     (setf dest-open-list
			   (update-open (cdr dest-open-list)
					(second dest-current)
					(first dest-current)
					(third dest-current)
					(fourth dest-current)
					closed
					slots
					control-strategy
					pruning-function
					search-constraints?
					max-depth
					free-slots
					multiple-answers?
					(not (not dest-terminated?))
					t
					))
		     ;; update goal lists
		     (if (not (null dest-current))
			 (setf source-goal-list
			       (append-element source-goal-list
					       dest-current)))
		     (if (not (null source-current))
			 (setf dest-goal-list
			       (append-element dest-goal-list
					       source-current)))
		     )
		   ))
	    )
      (cdr terminated?)
      )
    )
  ) 

;; a bi-directional search
(defun bi-dir-search (init-units slots goal-units terminate-with-success-criteria &key terminate-with-failure-criteria pruning-function (control-strategy #'(lambda (candidate open) (append open candidate))) (search-constraints? nil) (max-depth (/ *ls-search-depth-bound* 2.0)) (min-depth (/ *ls-min-search-depth* 2.0)) (multiple-answers? nil) (free-slots nil))
  (if (and (null goal-units)
	   (null init-units)
	   (null terminate-with-success-criteria))
      '(fail no-goal-requested)
    (let
	((terminated? nil)
	 (source-terminated? nil)
	 (dest-terminated? nil)
	 
	 (source-open-list (initialize-open-for-paths init-units))
	 (dest-open-list (initialize-open-for-paths goal-units))
	 
	 (source-goal-list (initialize-open-for-paths goal-units))
	 (dest-goal-list (initialize-open-for-paths init-units))
	 
	 (source-current nil)
	 (dest-current nil)
	 
	 (closed nil);; for avoiding loops
	 )
      (loop while (not (equal (first terminated?) 'done))
	    do
	    ;;(format t "open list = ~S ~S~%" source-open-list dest-open-list)
	    (cond ((and (null source-open-list)
			(null dest-open-list))
		   (if *ls-print-depth-limit*
		       (format t "search depth1 is ~a search depth2 is ~A~%" (fourth source-current) (fourth dest-current)))
		   ;; nothing left to search
		   (if terminated?
		       (setf terminated? (cons 'done terminated?))
		     (setf terminated? (list 'done '(fail no-goal-found)))))
		  ;; else check for termination 
		  (t
		   (setf source-current (first source-open-list))
		   (setf dest-current (first dest-open-list))
		   ;; update goal lists
		   ;;(format t "dest-current = ~S source-current=~S~%" dest-current source-current)
		   (if (not (null dest-current))
		       (setf source-goal-list
			     (append-element source-goal-list
					     dest-current)))
		   (if (not (null source-current))
		       (setf dest-goal-list
			     (append-element dest-goal-list
					     source-current)))
		   (if source-current
		       (setf source-terminated? 
			     (check-termination
			      (second source-current)
			      (first source-current)
			      (fourth source-current)
			      min-depth
			      (remove nil (mapcar #'second source-goal-list));; nil shouldn't be included
			      terminate-with-success-criteria
			      terminate-with-failure-criteria
			      source-open-list
			      multiple-answers?
			      source-terminated?
			      (mapcar #'(lambda (source-goal)
					  (last-slot-in-path (first source-goal)))
				      source-goal-list)
			      )
			     )
		     )

		   (if dest-current
		       (setf dest-terminated? 
			     (check-termination
			      (second dest-current)
			      (first dest-current)
			      (fourth dest-current)
			      min-depth
			      (remove nil (mapcar #'second dest-goal-list));; nil shouldn't be included
			      terminate-with-success-criteria
			      terminate-with-failure-criteria
			      dest-open-list
			      multiple-answers?
			      dest-terminated?
			      (mapcar #'(lambda (dest-goal)
					  (last-slot-in-path (first dest-goal)))
				      dest-goal-list)
			      )
			     )
		     )

		   ;; now merge the two terminated results
		   (setf terminated?
			 (merge-terminated source-terminated? 
					   dest-terminated? 
					   source-goal-list 
					   dest-goal-list terminate-with-success-criteria))
		   
		   (when (or (not terminated?)
			     (not (equal (first terminated?) 'done)))
		     (setf closed (cons (second source-current)
					(cons (second dest-current)
					      closed)))
		     ;; update open lists
		     (setf source-open-list
			   (update-open (cdr source-open-list)
					(second source-current)
					(first source-current)
					(third source-current)
					(fourth source-current)
					closed
					slots
					control-strategy
					pruning-function
					search-constraints?
					max-depth
					free-slots
					multiple-answers?
					(not (not source-terminated?))
					))
		     (setf dest-open-list
			   (update-open (cdr dest-open-list)
					(second dest-current)
					(first dest-current)
					(third dest-current)
					(fourth dest-current)
					closed
					slots
					control-strategy
					pruning-function
					search-constraints?
					max-depth
					free-slots
					multiple-answers?
					(not (not dest-terminated?))
					))
		     )
		   ))
	    )
      (cdr terminated?)
      )
    )
  )

(defun merge-terminated (sources dests source-goals dest-goals terminate-with-success-criteria)
  ;; need to invert all the path to from source to dest
  (let* 
      (
       (outcome (if (or (equal 'done (first sources))
			(equal 'done (first dests)))
		    'done))
       (new-sources
	(mapcar #'(lambda (source)
		    (merge-search-path-with-goal-path
		     source source-goals terminate-with-success-criteria))
		(remove-if-not #'consp (successful-search? sources))))
       (new-dests
	(mapcar #'(lambda (dest)
		    (merge-search-path-with-goal-path
		     dest dest-goals terminate-with-success-criteria))
		(remove-if-not #'consp (successful-search? dests)))))
    (setf new-dests;; revese the path
	  (mapcar #'(lambda (dest)
		      (list 
		       (first dest)
		       (second dest)
		       (reverse-search-path (third dest) 0)
		       (fourth dest)
		       ))
		  new-dests))
    (setf new-dests
	  (mapcar #'(lambda (dest)
		      (list
		       (first dest)
		       (first (last (third dest) 2))
		       (third dest)
		       (fourth dest)
		       ))
		  new-dests))
    (append
     (list outcome)
     new-sources
     new-dests) 
    ))

;; given a search result, append the path with the one from the correct goal list
(defun merge-search-path-with-goal-path (search-result goals terminate-with-success-criteria)
  (let* ((goal-used (find (second search-result) goals :test #'(lambda (inst goal)
								 (funcall terminate-with-success-criteria 
									  inst (list (second goal))))))
	 (search-path (third search-result))
	 (offset (if (> (length search-path) 1) (last-element search-path) 0))
	 (goal-path (reverse-search-path (first goal-used) offset))
	 (connecting-inst1 (first (last search-path 2)))
	 (connecting-inst2 (first goal-path))
	 )
    (when goal-path
      (setf search-path
	    (butlast search-path 2));; remove the last number and _Chemical from search-path
      (if (is-more-specific connecting-inst1 connecting-inst2)
	  (setf goal-path
		(append 
		 (list connecting-inst1 offset)
		 (cdr goal-path)))
	(setf goal-path
	      (append 
	       (list connecting-inst2 offset)
	       (cdr goal-path))))
      (setf search-path
	    (append search-path goal-path)))
    (list (first search-result)
	  (first (last search-path 2))
	  search-path
	  (absolute-taxonomical-distance-between-instances connecting-inst1 connecting-inst2))
    ))

;; reverse the search path in the form of (_Chemical has-basic-structural-unit _H2O 1 has-formula _Formula 2)
;; to be (_Formula has-formula-of _H2O 1+offset is-basic-structural-unit-of _Chemical 2+offset)
(defun reverse-search-path (search-path offset)
  (let ((reversed-path (reverse-path (gen-access-path0 search-path)))
	)
    (cons (car reversed-path)
	  (anti-gen-access-path (cdr reversed-path) offset))
    ))

;; the opposite of gen-access-path
;; given an access path, add the numeric numbers to it
(defun anti-gen-access-path (access-path offset)
  (cond ((or (atom access-path)
	     (< (length access-path) 2))
	 nil)
	(t
	 (append
	  (list (first access-path)
		(second access-path)
		offset)
	  (anti-gen-access-path (cdr (cdr access-path)) (+ 1 offset))))
	))
  
;;; initializes the variable *Ls-Kb-Search-Open-List* to be a list of triples of the
;;; form ((initial-unit) initial-unit cost depth)
(defun initialize-open-for-paths (initial-unit-list)
  (mapcar #'(lambda (unit)            
	      (list (list unit) unit 0 0))
	  initial-unit-list))

;;; returns the *ls-kb-search-open-list* list updated by the successors of the current unit,
;;; with duplicates removed
(defun update-open  (open current-unit current-path current-cost current-depth
			  closed  slots
			  control-strategy pruning-function search-constraints?
			  max-depth free-slots multiple-answers? found-answers-yet? &optional (directed-search? nil))
  (if (not (kb-objectp current-unit))
      open
    (if (and found-answers-yet?;; this for the case of already found 1 answer
	     multiple-answers?;; need multiple answers
	     (not *ls-exhaustive-search*)
	     (not;; and the open list does not have more units that are shallower than the current answer
	      (remove-if-not;; therefore no need to search further or update-open list further
	       #'(lambda (x)
		   (< (fourth x) current-depth))
	       open))
	     (not directed-search?))
	open
      (let ((additions-to-open
	     (generate-path-successors-of-unit
	      current-unit
	      current-path
	      current-depth
	      current-cost
	      slots
	      closed
	      pruning-function search-constraints? max-depth)))
	;;(format t "additions-to-open=")
	;;(pprint additions-to-open)
	;;(format t "~%")
	;;(format t "max depth = ~a open=~s ~%" max-depth open)
	(setf additions-to-open
	      (remove nil (mapcar #'(lambda (element-to-add)
				      (let ((full-path (first element-to-add))
					    (new-depth 0))
					(if (member (nth (- (length full-path) 3) full-path) free-slots) ;; this is unused legacy code 
					    (setf new-depth (+ current-depth 0.26))                      ;; thought some slots, such as parts, maybe shallower than others
					  (setf new-depth (+ current-depth 1))) 
					;;(format t "new-dpeth = ~A for ~S~%" new-depth element-to-add)
					(if (> new-depth max-depth) ;(format t "new depth = ~S max-depth = ~A~%" new-depth max-depth)
					    nil
					  (append element-to-add (list new-depth)))))
				  additions-to-open)))
	;;(format t "here2~%")
	(funcall control-strategy additions-to-open open)))))


;;; finds all successors of current that are connected via slots
;;; returns successors that are not on closed and that are not pruned
;;; each successor is a pair of the form (<path> <successor-unit>)
;;; the last step in the process is to append the partial-paths to
;;;    each successor
(defun generate-path-successors-of-unit (current-unit current-path current-depth current-cost
						      slots 
						      closed pruning-function search-constraints? max-depth)
  ;;(format t "here?~%")
  (let* ((successors
	  (if (not (> (+ current-depth 1) max-depth))
	      (generate-unpruned-unit-successors-with-slots current-unit current-cost current-path
							    slots search-constraints?)))
	 (non-dupe-successors
	  (if *ls-exhaustive-search*
	      successors
	    (set-difference successors closed
			    :test #'(lambda (successor-item closed-item)
				      (equal (second successor-item)
					     closed-item)))))
	 (non-pruned-successors
	  (if pruning-function
	      (remove-if pruning-function non-dupe-successors)
	    non-dupe-successors)))
    ;;(format t "successors = ~s non-dupe-successors = ~s non-pruned-succesors~s~%" successors non-dupe-successors non-pruned-successors)
    (add-path non-pruned-successors current-path)))

(defun update-classes-usage-profile (classes)
  (dolist (class classes)
	  (incf (gethash class *ls-class-usage* 0)))
  )

;;; EXTEND-ADDRESS takes in an instance of a frame and a slot
;;; always returns at least 1 slot filler
(defun extend-address (current-unit slot search-constraints? current-cost)
  (let ((results (extend-address1 current-unit slot search-constraints? current-cost)));; 0,1,2 are three version w/ perceived speed advantage, but not proven, and sometimes 0, 2 don't return an answer, but 1 does
    (when 
	(and *ls-report-class-usage* ;; remove all results whose first result is null, if there is one left
	     (find-if #'(lambda (resultx) (not (null (first resultx)))) results))
      (let ((head-classes (ls-km `(|the| |classes| |of| ,current-unit)))
	    )
	(update-classes-usage-profile head-classes)
	(dolist (resulty results)
		(update-classes-usage-profile (ls-km `(|the| |classes| |of| ,(first resulty)))))
	))
    results
    ))

(defun extend-address0 (current-unit slot search-constraints? current-cost)
  (let ((retval nil)
	(new-cost 0)
	(inherited-rules (inherited-rule-sets current-unit slot))
	(local-rules (own-rule-sets current-unit slot))
	(class-filler nil))
    ;;    (format t "search-constraints? = ~A~%" search-constraints?)
    (ls-debug-msg 'extend-address0
		  (format nil "unit = ~A slot = ~A inherited-rules= ~A local-rules = ~A~%" current-unit slot inherited-rules local-rules))
    (if (or inherited-rules local-rules)
	(progn
	  (if (is-valency slot)
	      (setf new-cost (+ current-cost *valency-cost*))
	    (setf new-cost (+ current-cost 1)))
	  (if (and (is-range-class slot)
		   (or (not (ls-get-slot-val current-unit slot))
		       (not (ls-km `(,(ls-get-slot-val current-unit slot) |isa| |String|)))))
	      ;; the slot's range is a class instead of instance
	      (progn
		(setf class-filler (ls-get-slot-val current-unit slot))
		;;(format t "class-filler = ~A the whole thing is ~A~%" class-filler (ls-km `(|the| ,slot |of| ,current-unit)))
		(if class-filler
		    (setf retval (reduce #'append 
					 (mapcar #'(lambda (x)
						     (ls-create-inst `(|a| ,x))) class-filler)))))
	    (progn
	      (if (and search-constraints?
		       (not (ls-get-slot-val current-unit slot)))
		  (ls-km `(,current-unit |has| (,slot ((|a| |Thing|))))))
	      (setf retval (ls-get-slot-val current-unit slot))
	      ))))
    ;;(setf retval (get-vals current-unit slot))))
    (if retval
	(progn
	  (if (is-filler-seq retval)
	      ;; retval starts with :arg, use only the later part
	      (setf retval (list (cadr retval))))
	  (if (listp retval)
	      (mapcar #'(lambda (filler)
			  (list filler new-cost))
		      retval)))
      (list (list retval new-cost)))))

(defun extend-address1 (current-unit slot search-constraints? current-cost)
  (let ((retval nil)
	(new-cost 0)
	(inherited-rules nil)
	(local-rules nil)
	(class-filler nil)
	(slot-val (ls-get-slot-val current-unit slot)))
    (when (and search-constraints?
	       (null slot-val))
      (setf inherited-rules (inherited-rule-sets current-unit slot))
      (setf local-rules (own-rule-sets current-unit slot)))
    (if (or inherited-rules local-rules slot-val)
	(progn
	  (if (is-valency slot)
	      (setf new-cost (+ current-cost *valency-cost*))
	    (setf new-cost (+ current-cost 1)))
	  (if (and (is-range-class slot)
		   (or (null slot-val)
		       (not (ls-km `(,slot-val |isa| |String|)))))
	      ;; the slot's range is a class instead of instance
	      (progn
		(setf class-filler slot-val)
		;;(format t "class-filler = ~A the whole thing is ~A~%" class-filler (ls-km `(|the| ,slot |of| ,current-unit)))
		(if class-filler
		    (setf retval (reduce #'append 
					 (mapcar #'(lambda (x)
						     (ls-create-inst `(|a| ,x))) class-filler)))))
	    (progn
	      (setf retval slot-val)
	      (when (and search-constraints?
			 (null slot-val))
		(ls-km `(,current-unit |has| (,slot ((|a| |Thing|)))))
		(setf retval (ls-get-slot-val current-unit slot))
		(when (equal '(|Thing|) (ls-km `(|the| |superclasses| |of| ,retval)))
		  (setf retval nil)))
	      ))))
    ;;(setf retval (get-vals current-unit slot))))
    (if retval
	(progn
	  (if (is-filler-seq retval)
	      ;; retval starts with :arg, use only the later part
	      (setf retval (list (cadr retval))))
	  (if (listp retval)
	      (mapcar #'(lambda (filler)
			  (list filler new-cost))
		      retval)))
      (list (list retval new-cost)))))

(defun extend-address2 (current-unit slot search-constraints? current-cost)
  (let ((retval nil)
	(new-cost 0)
	(inherited-rules nil)
	(local-rules nil)
	(class-filler nil)
	(slot-val nil))
    (setf local-rules (own-rule-sets current-unit slot))
    (if (null local-rules)
	(setf inherited-rules (inherited-rule-sets current-unit slot)))
    (if (or inherited-rules local-rules)
	(progn
	  (if (is-valency slot)
	      (setf new-cost (+ current-cost *valency-cost*))
	    (setf new-cost (+ current-cost 1)))
	  (setf slot-val (ls-get-slot-val current-unit slot))
	  (if (and (is-range-class slot)
		   (or (not slot-val)
		       (not (ls-km `(,slot-val |isa| |String|)))))
	      ;; the slot's range is a class instead of instance
	      (progn
		(setf class-filler slot-val)
		(if class-filler
		    (setf retval (reduce #'append 
					 (mapcar #'(lambda (x)
						     (ls-create-inst `(|a| ,x))) class-filler)))))
	    (progn
	      (if (and search-constraints?
		       (not slot-val))
		  (ls-km `(,current-unit |has| (,slot ((|a| |Thing|))))))
	      (setf retval (ls-get-slot-val current-unit slot))))))
    ;;(setf retval (get-vals current-unit slot))))
    (if retval
	(progn
	  (if (is-filler-seq retval)
	      ;; retval starts with :arg, use only the later part
	      (setf retval (list (cadr retval))))
	  (if (listp retval)
	      (mapcar #'(lambda (filler)
			  (list filler new-cost))
		      retval)))
      (list (list retval new-cost)))))

;;; GET-LOCAL takes in a list of pairs: (slot fillers, cost)
;;; returns a list of interesting ones
;;; "interesting" is defined as anything more specific than 
;;; a Thing and cannot be a String
(defun get-local (slotvals)
  (remove nil 
	  (mapcar #'(lambda (val)
		      (if (or
			   (not (first val))
			   (stringp (first val)))
			  nil
			val))
		  slotvals)))
  

;;; CONSIDER THIS EFFICIENCY HACK:
;;;    REDUCE THE LIST OF SLOTS BY INTERSECTING IT WITH THE EXPLICIT
;;;    SLOTS ON CURRENT-UNIT, THEN GET-LOCAL'ing ONLY THOSE

;;; finds all successors of current that are connected via slots
;;; current is a unit
;;; returns list of the form:
;;;        ( ... (slot successor cost) ...)
(defun generate-unpruned-unit-successors-with-slots (current-unit current-cost current-path slots search-constraints? )
  (let* 
      (
       (inverse-slots
	;; inverse-slots will be excluded from search if the current slot is 1-to-N or 1-to-1
	(if #|(or (eql (cardinality-of-slot (last-slot-in-path current-path)) '|1-to-N|)
		(eql (cardinality-of-slot (last-slot-in-path current-path)) '|1-to-1|))|#
		t
		(all-inverse-slots (last-slot-in-path current-path))))
       (inst-classes (ls-km `(|the| |all-classes| |of| ,current-unit)))
       (valid-slots
	(remove nil 
		(mapcar #'(lambda (slot)
			    (if (or (not (get-domain-of-slot slot))
				    (find-if #'(lambda (d)
						 ;;(ls-km `(,current-unit |isa| ,d))
						 (find d inst-classes :test #'eql)
						 )
					     (get-domain-of-slot slot)))
				slot
			      nil))
			slots)))
       #|(used-slots 
       (ls-specified-slots current-unit))|#
       (searchable-slots
	(set-difference
	 valid-slots inverse-slots))
       )
    ;;(format t "valid slots include has-part? ~A current-unit = ~s ~%" (member '|has-part| valid-slots) current-unit)
    #|(setf searchable-slots
	  (intersection
	   searchable-slots used-slots))|#
    (remove-if #'(lambda (x) (equal (second x) nil))
	       (reduce #'append 
		       (mapcar #'(lambda (slot)
				       (mapcar #'(lambda (value)
						   (cons slot value))
					       (get-local (extend-address current-unit
									  slot search-constraints? current-cost))))
			       searchable-slots)))))

;;; extends the path of each successor by adding current-path
;;; to the front
;;;
;;; example:
;;;     successors: ((x y) (z w))
;;;     current-path: (a b c)
;;;     result: (((a b c x y) y) ((a b c z w) w))
(defun add-path (successors current-path)
  (mapcar #'(lambda (successor)
	      (list (append current-path successor)
		    (second successor)
		    (third successor)))
	  successors))


(defun check-termination (current-unit current-path current-depth min-depth
				       goal-units
				       terminate-with-success-criteria
				       terminate-with-failure-criteria open multiple-answers? prev-results &optional (excluded-slots nil))
  (let ((result prev-results))
    ;;(format t "multiple-answers ? = ~s terminate-with-success-criteria = ~S failure ~A open = ~s~%" multiple-answers? terminate-with-success-criteria terminate-with-failure-criteria open)
    (if multiple-answers?
	(progn
	  ;; if no more nodes left on open list that has the same depth as current node, then done
	  ;;(format t "result = ~S current depth = ~A open still has anything of current depth?~A~%" result current-depth (member current-depth (mapcar #'fourth open)))
	  (cond ((and result
		      (not *ls-exhaustive-search*)
		      (not (member current-depth
				   (mapcar #'fourth open))))
		 (cons 'done result))
		(t
		 (setf result 
		       (cond ((and 
			       (>= current-depth min-depth)
			       terminate-with-success-criteria
			       ;; the last slot in the current path is not the inverse of any slots to be excluded, i.e. no circular paths
			       (not
				(intersection excluded-slots
					      (all-inverse-slots
					       (get-inverse-slot (last-slot-in-path current-path)))
					      :test #'equal
					      ))
			       (funcall terminate-with-success-criteria current-unit goal-units))
			      (cons (list 'success current-unit current-path)
				    result))
			     ((and 
			       (>= current-depth min-depth)
			       terminate-with-failure-criteria
			       (funcall terminate-with-failure-criteria current-unit))
			      (cons (list 'fail current-unit) result))
			     ((out-of-time)
			      (cons (list 'fail current-unit) result))
			     (t result)))
		 ))
	  )
      ;; single answer is returned
      (cond ((and 
	      (>= current-depth min-depth)
	      terminate-with-success-criteria
	      ;; the last slot in the current path is not the inverse of any slots to be excluded, i.e. no circular paths
	      (not
	       (intersection excluded-slots
			     (all-inverse-slots
			      (get-inverse-slot (last-slot-in-path current-path)))
			     :test #'equal
			     ))
	      (funcall terminate-with-success-criteria current-unit goal-units))
	     (list 'done (list 'success current-unit current-path)))
	    ((and terminate-with-failure-criteria
		  (funcall terminate-with-failure-criteria current-unit))
	     (list 'done (list 'fail current-unit)))
	    ((out-of-time)
	     (list 'done (list 'fail current-unit)))
	    (t nil)))
    ))
  

(defun is-slot (i)
  (let ((lower-i (cond ((stringp i)
			(ls-intern (string-downcase (string i))))
		       ((symbolp i)
			i)
		       (t
			i))))
    (if lower-i
	(ls-slotp lower-i)
      nil)))

;; checks to see if the range of a given slot name 
;; is Class instead of instance
(defun is-range-class (s)
  (member '|Class|  (ls-km `(|the| |range| |of| ,s))))

;; checks to see if a given list of slot filler is a sequence
(defun is-filler-seq (s)
  (if (listp s)
      (if (listp (car s))
	  (equal (caar s) ':|args|))))

;;;;-------------------RDP----search part--------
#| RDP-search.lisp -- resolves the relation of a Noun-Noun phrase using the RDP and KB taxonomy|#

;; given a symbol, convert it into the constant format
(defun convert-to-const (X)
  (let ((x-string (string X)))
    ;; if X starts with a *, then return X
    (if (string= x-string "*" :start1 0 :end1 1)
	x
      ;; else convert it to lower case, prefix with *, and return
      (ls-intern (format nil "*~A" (string-downcase x-string)))
      )
    ))

(defun ls-create-inst (x)
  (ls-km x)
  )

(defun gen-patterns (result-list)
  (remove nil
	  (mapcar #'gen-pattern  
		  result-list))
  )

;; generate the access path based on a search result
(defun gen-access-path (result)
  (let ((path (third result))
	(result nil)
	)
    (setf result (gen-access-path0 path))
    (mapcar #'(lambda (element)
		(cond ((is-named-instance element)
		       element)
		      ((is-slot element)
		       element)
		      ((numberp element)
		       element)
		      ((stringp element)
		       element)
		      ((property-constant? element)
		       element)
		      (t
		       (first (ls-km `(|the| |classes| |of| ,element)))))
		)
	    result)
    ))
(defun gen-access-path0 (path)
  ;;cannot simply remove any non slot, class elements b/c cofficient could be 2
  ;;do the removal based on the pattern Inst slot 
  (if (> (length path) 2)
      (append (list (first path))
	      (gen-access-path1 (cdr path)))
    (list (car path)))
  )

(defun gen-access-path1 (path)
  (if (or (null path)
	  (not (consp path))
	  (< (length path) 2))
      path
    (append
     (list (first path)
	   (second path))
     (gen-access-path1
      (cdr (cdr (cdr path)))))
    ))

;; this function tries to build the KM expr by "recursively" querying the head
;; instead of creating (a Class) type of KM expr
(defun gen-good-km-expr-from-access-path (access-path head tail from-tail?)
  (if from-tail?
      (gen-good-km-expr-from-access-path-through-tail access-path tail)
    (gen-good-km-expr-from-access-path-through-head access-path head)))

(defun gen-good-km-expr-from-access-path-through-tail (access-path tail)
  (complete-reverse-assertion-expr (gen-good-km-expr-from-access-path-through-head (reverse-path access-path) tail))
)

(defun gen-good-km-expr-from-access-path-through-head (access-path head)
  (let ((inst nil))
    (cond
     ((null access-path)
      nill)
     ((null head)
      (gen-km-expr-from-access-path access-path))
     ((= (length access-path) 1)
      (gen-km-expr-from-access-path access-path))
     (;; now query to see if the head instance has existing slot filler of the right type
      (setf inst (existing-inst-in-access-path access-path head))
      `(,head |has| (,(second access-path) (,(gen-good-km-expr-from-access-path-through-head (cddr access-path) inst)))))
     (t
      `(,head |has| (,(second access-path) (,(gen-km-expr-from-access-path (cddr access-path))))))
     )
    ))

(defun existing-inst-in-access-path (access-path head)
  (let ((inst nil)
	(slot (second access-path))
	(class (third access-path)))
    (setf inst (first (ls-km `(|the| ,class ,slot |of| ,head))))
    (if (and inst
	     (is-previous-existing-instance inst))
	inst
      nil)
    ))

;; an heuristic to determine if an instance is generated by LS or exists before LS
(defun is-previous-existing-instance (inst)
  (or (member inst *ls-existing-objs* :test #'equal)
      (member (bound inst) *ls-existing-objs* :test #'equal))
  )

(defun gen-km-expr-from-access-path (access-path)
  (cond 
   ((not access-path)
    nil)
   ((= (length access-path) 1)
    `(|a| ,(first access-path)))
   ((or (is-named-instance (first access-path))
	(aggregatep (first access-path)))
    `(,(first access-path) |has| ,(gen-slot-filler-patterns (cdr access-path)))
    )
   (t
    `(|a| ,(first access-path) |with| ,(gen-slot-filler-patterns (cdr access-path)))
    )
   )
  )

;; given the result of a search
;; generate a path based on the classes, not the instances 
(defun gen-pattern (result)
  (let* (;; (path (coalesce-result result))
	 ;; forgot why we need to coalesce results, commented out for time being
	 (access-path (gen-access-path result)))
    (gen-km-expr-from-access-path access-path)
    ))


;;;; ============================================================================
;;;; ====                       km-to-set.lisp                               ====
;;;; ============================================================================

(defparameter *km-code-keywords* '(
				   |a| |the| |of| |Self| |every| |has| |with| |must-be-a|
				   |the-class| |It| |It2| |allof|
				   ))

(defun convert-km-to-set-code (expr)
  (second (convert-km-to-set-code-0 expr))
  )

;; given a km expr
;; return a list of the form '(head triple-sets)
(defun convert-km-to-set-code-0 (expr &optional (depth 0))
  (cond
   ((and (atom expr)
	 (not (ls-inst? expr)))
    (list (cons expr depth) (list (list (cons expr depth) nil (cons nil (+ 1 depth))))))
   ((and (atom expr)
	 (ls-inst? expr))
    (let ((class (first (ls-km `(|the| |classes| |of| ,expr)))))
      (list (cons class depth) (list (list (cons class depth) nil (cons nil (+ 1 depth))))))
    )
   ((singletonp expr)
    (convert-km-to-set-code-0 (car expr) depth))
   ((aggregatep expr)
    (convert-km-to-set-code-pair-seq-etc expr depth))
   ((km-queryp expr)
    ;; the ... type
    (convert-km-to-set-code-query expr depth)
    )
   (t
    ;; assertion treatment
    ;; of the form (a X with ...)
    (convert-km-to-set-code-assertion expr depth))
   )
  )

;; test cases:
;; (the raw-material of *reaction) => (((|*reaction| . 0) nil (nil . 1)) ((|*reaction| . 0) |raw-material| (nil . 1)))
;; (the quantity of (the raw-material of (a Reaction))) => (((|Reaction| . 0) nil (nil . 1)) ((|Reaction| . 0) |raw-material| (nil . 1)) ((nil . 1) |quantity| (nil . 2)))
;; (the rate of (a Reaction with (keq ((a Value))))) => (((|Reaction| . 0) |keq| (|Value| . 1)) ((|Value| . 1) nil (nil . 2)) ((|Reaction| . 0) |rate| (nil . 1)))
;; (the quantity of (the raw-material of (a Reaction with (raw-material ((a CaCO3) *HCl)) (result ((a H2O with (quantity ((a QV))))))))) => (((|Reaction| . 0) |raw-material| (|CaCO3| . 1)) ((|Reaction| . 0) |raw-material| (|*HCl| . 1)) ((|Reaction| . 0) |result| (h2o . 1)) ((|CaCO3| . 1) nil (nil . 2)) ((|*HCl| . 1) nil (nil . 2)) ((h2o . 1) |quantity| (qv . 2)) ((qv . 2) nil (nil . 3)) ((qv . 2) |raw-material| (nil . 3)) ((nil . 3) |quantity| (nil . 4)))
(defun convert-km-to-set-code-query (expr depth)
  (cond ((equal (first expr) '|the|)
	 (let* ((code-from-rest (convert-km-to-set-code-0 (last expr) depth))
		(slot (if (= (length expr) 4)
			  (second expr)
			(third expr)))
		(new-triple-head (first code-from-rest))
		(new-triple-tail (if (= (length expr) 4);; the slot of frame 
				     (cons nil (+ 1 (cdr new-triple-head)))
				   (cons (second expr) (+ 1 (cdr new-triple-head)))));; the frame slot of frame
		)
	   (list
	    new-triple-tail
	    (append;; for example (the raw-material of (a Reaction with (raw-material ((a CaCO3)))))
	     (second code-from-rest)
	     (list (list new-triple-head slot new-triple-tail ))
	     ))))
	;; else it's (the1 of ...) e
	(t
	 (convert-km-to-set-code-0 (last expr) (+ depth 1)))
	)
  )

;; assertion treatment
;; of the form (a X with ...)

;; test cases: 
;; *Reaction => (((|*Reaction| . 0) nil (nil . 1)))
;; (a Reaction) => (((|Reaction| . 0) nil (nil . 1)))
;; (a Reaction with (raw-material ((a CaCO3)))) => (((|Reaction| . 0) |raw-material| (|CaCO3| . 1)) ((|CaCO3| . 1) nil (nil . 2)))
;; (a Reaction with (raw-material ((a CaCO3) *HCl)) (result ((a H2O with (quantity ((a QV))))))) => '(((|Reaction| . 0) |raw-material| (|CaCO3| . 1)) ((|Reaction| . 0) |raw-material| (|*HCl| . 1)) ((|Reaction| . 0) |result| (h2o . 1)) ((|CaCO3| . 1) nil (nil . 2)) ((|*HCl| . 1) nil (nil . 2)) ((h2o . 1) |quantity| (qv . 2)) ((qv . 2) nil (nil . 3)))

(defun convert-km-to-set-code-assertion (km-code depth)
  (let ((concept-name (cons (extract-head-class km-code) 
			    (extract-called-tag km-code depth)))
	(concept-body (extract-concept-body km-code))
	(retval-from-self nil)
	(retval-from-other nil)
	(filler-triples nil)
	relationship
	)
    (if concept-body
	(dolist (km-frag concept-body)
		(setf relationship (first km-frag))
		(dolist (filler (second km-frag))
			(setf filler-triples (convert-km-to-set-code-0 filler (+ depth 1)))
			(setf retval-from-self
			      (append
			       retval-from-self
			       (list
				(list concept-name relationship
				      (first  filler-triples)))
			       ))
			(setf retval-from-other
			      (append retval-from-other
				      (second filler-triples)))
			)
		)
      (setf retval-from-self 
	    (list (list concept-name nil (cons nil (+ 1 depth))))))
    (list concept-name (append retval-from-self retval-from-other))
    ))


;; treats :pair, :seq, etc differently because 
;; 1. LS parsing deals with single value filler only, have to treat :pair as a single value
;; 2. but can't extract their classes later on in LS
;; problems: if there is embedded assertions in :pair, then won't work, e.g. (:pair (a H2O with ...) (a CaCO3 with ..))
;; test cases:
;; (:pair 1 *mole) => ((|Thing| . 0) nil (nil . 1))
(defun convert-km-to-set-code-pair-seq-etc (expr depth)
  (declare (ignore expr))
  `((,*ls-pair-seq-class* . ,depth) (((,*ls-pair-seq-class* . ,depth) nil (nil . ,(+ depth 1)))))
  )

;;;---------------------------------------------------------------------
;;; Auxiliary function used by various top-level functions.
;;;---------------------------------------------------------------------

(defun extract-concept-name (km-concept)
  (if (consp km-concept)
      (if (aggregatep km-concept)
	  '|Thing|
	(dolist (km-term km-concept)
		(cond 
		 ((or (eql km-term	'|the|) (eql km-term '|Self|))
		  (return nil))
		 ((not (member km-term *km-code-keywords* :test #'eql))
		  (return km-term)))))
    km-concept))

(defun extract-concept-head (km-concept)
  (if (consp km-concept)
      (let ((body (extract-concept-body km-concept)))
	(safe-ldiff km-concept body))
    km-concept
    ))

(defun extract-concept-header (km-concept)
  (if (consp km-concept)
      (let ((body (extract-concept-body km-concept)))
	(remove '|has| (remove '|with| (safe-ldiff km-concept body)))
	)
    km-concept
    ))

(defun extract-concept-body (km-concept)
  (if (km-queryp km-concept)
      (extract-concept-body-query km-concept)
    (extract-concept-body-assertion km-concept)))
      
(defun extract-concept-body-query (km-concept)
  (let ((body km-concept))
    (dolist (km-term km-concept)
	    (cond ((atom km-term)
		   (setf body (rest body)))
		  (t
		   (return body))))
    ))

(defun extract-concept-body-assertion (km-concept)
  (let ((body km-concept)
	(found-marker nil);; markers are "with" or "has" 
	)
    (if (consp km-concept)
	(dolist (km-term km-concept)
		(cond ((or (equal km-term '|with|)
			   (equal km-term '|has|))
		       (setf found-marker t)
		       (setf body (rest body)))
		      ((and found-marker
			    (not (atom km-term)))
		       (return body))
		      (t
		       (setf body (rest body)))
		      )))))

(defun extract-called-tag (km-concept &optional count)
  (let ((start-pos (if (consp km-concept) (member '|called| km-concept :test #'eql))))
    (if start-pos
	(first (rest start-pos))
      count)))

(defun is-valency (slot)
  (if (null *extended-valency-slots*)
      (setf *extended-valency-slots*
	    (append *valency-slots*
		    (ls-km `(|the| |subslots| |of| ,(cons ':|set| *valency-slots*)))
		    (ls-km `(|the| |inverse| |of| ,(cons ':|set| *valency-slots*)))
		    (ls-km `(|the| |inverse| |of| (|the| |subslots| |of| ,(cons ':|set| *valency-slots*))))
		    ) ))
  (member slot *extended-valency-slots* :test #'equal)
  )

(defun trivial-stop-criteria (input-insts input-goals)
  (declare (ignore input-insts input-goals))
  t)

;;;;; ---------- Patches ---------------
;;;;; --- 09/12/2007 by M. R. Glass ----
(defun reverse-map (mapping)
  (let ((reversed nil))
    (dolist (m mapping reversed)
      (push (list (second m) (first m)) reversed))))

(defun replace-instances (mapping expr)
  (let ((new-expr expr))
    (dolist (one-replacement mapping new-expr)
      (setf new-expr (subst (second one-replacement) (first one-replacement) new-expr)))))

(defun create-new-instance (inst)
  (let ((concepts (km0 `(|the| |instance-of| |of| ,inst))))
    (if (rest concepts)
        (km-unique0 `(|a| ,(first concepts) |with| (|instance-of| ,(rest concepts))))
      (km-unique0 `(|a| ,(first concepts))))))
(defun create-new-instances (instances)
  (let ((mapping nil))
    (dolist (inst instances mapping)
      (push (list inst (create-new-instance inst)) mapping))))

(defun gather-instances (expr)
  (if (atom expr)
      (if (anonymous-instancep expr)
	  (list expr)
        nil)
    (let ((inst-list nil))
      (dolist (subexpr expr inst-list)
	(setf inst-list (nconc inst-list (gather-instances subexpr)))))))

(defun clean-check-for-ls (expr)
  (let* ((instances (gather-instances expr))
	 (mapping (create-new-instances instances))
	 (clean-expr (replace-instances mapping expr))
	 (returned-expr (check-for-ls clean-expr))
	 (reverse-mapping (reverse-map mapping))
	 )
    (setf *ls-cpl-choices* (replace-instances reverse-mapping *ls-cpl-choices*))
    (replace-instances reverse-mapping returned-expr)
    ))

(defun parse-for-assertion-one-iteration (expr)
  (let ((result nil))
    (if (and (km-assertionp expr)
	     (consp expr))
	(progn
	  ;; save the self here since self only refers to assertions
	  ;; save the current expr for replacing Self
	  (if (not *ls-current-self*)
	      (progn ;; if this is the 1st time this function is called
		(push expr *ls-current-self*)
		(push (clone-instance (first *ls-current-self*)) *ls-current-replaced-self*)
		)
	    (progn ;; else replicate the top of the stack
	      (push (car *ls-current-self*) *ls-current-self*)
	      (push (car *ls-current-replaced-self*) *ls-current-replaced-self*)))
	  ;;(setf expr (replace-self expr)) no need to replace-self for assertions because assertions won't mention self
	  )
      )
    (setf
     result
     (cond ((gethash expr *ls-generated-exprs*)
	    (ls-debug-msg 'parse-for-assertion-one-iteration
			  (format nil "Expr ~s found in the hash~%" expr))
	    expr)
	   ((atom expr)
	    expr)
	   ((null expr)
	    expr)
	   ((ls-chemical-name-expr expr)
	    (ls-interpret-chemical-name expr))
	   ((ls-property-expr expr)
	    (ls-interpret-property expr))
	   ( ;; of the form (:nn a b)
	    (ls-nn-seq-expr expr)
	    (format t "LS DEBUG: resolving noun compounds ~S~%" expr)
	    (ls-interpret-nn expr)
	    )
	   ( ;; of the form (forall ...), etc
	    (forall-allofp expr)
	    (interpret-allof expr))
	   ( ;; of the forma (x isa y), need to do role checking
	    (km-isap expr)
	    (interpret-isa expr))
	   ((condition-expr expr)
	    (interpret-condition-expr expr))
	   ((not (nested-list expr))
	    expr)
	   ( ;; of the form ((:nn a b) with ...)
	    (and (ls-nn-seq-expr (first expr))
		 (rest expr))
	    (append
	     (parse-and-flush (first expr))
	     (rest (rest expr)))
	    )
	   ( ;; of the form ((...) and (...)), etc
	    (boolean-expr expr)
	    (mapcar #'(lambda (element)
			(if (consp element)
			    (parse-and-flush element)
			  element))
		    expr))
	   ;; if :seq or :set
	   ((aggregatep expr)
	    (clean-check-for-ls expr))
	   ;; now process every slot
	   (t
	    (parse-for-assertion-one-iteration0 expr))
	   ))
    (if (and (km-assertionp expr)
	     (consp expr))
	(progn
	  ;;(setf result (revert-self result))
	  (pop *ls-current-self*)
	  (pop *ls-current-replaced-self*)
	  )
      )
    result
    ))

(defun solve-linear-dependency (new-head slot filler newfiller actions specialized-heads)
  (let (
	(specialized-filler-exprs nil)
	(specialized-tails nil)
	(output-expr nil)
	(specialized-filler nil)
	)
    ;; specialized-filler is set to the filler on the stack if available
    (setf specialized-filler
	  (if (first specialized-heads)
	      (replace-properties-from-assertion
	       newfiller
	       (extract-concept-body (first (first specialized-heads))))
	    newfiller))
    (setf output-expr (clean-check-for-ls (append new-head (list (list slot (list specialized-filler))))))
    ;; this could be: 1> old filler doesn't have any LS, therefore nothing is changed
    ;; 2> newfiller is the result of specialize-head, therefore nothing is changed yet
    (cond
     ((and (does-last-action-include 'specialize-head :actions actions :check-stack nil)
	   (does-last-action-include 'specialize-tail))
      (format t "1~%")

      (setf specialized-tails (car *ls-specialized-tails*))
					;(format t "head ~A tail popped ~A~%" specialized-heads specialized-tails)
      ;; do the semantic intersection of the 2 sets
      (setf specialized-filler-exprs (solve-linear-dependency-0 (first specialized-heads)
								;; because top of *ls-specialized-heads* is a list of heads now
								(first specialized-tails)))

      ;; because don't care about specialize-tail again, remove the specialize-tail
      (pop *ls-last-parsing-action*)
      (pop *ls-specialized-tails*)

      ;; prompt user for the right choice.
      (append new-head
	      (list
	       (list slot
		     (list
		      (prompt-for-clarification filler specialized-filler-exprs))))))
     ((does-last-action-include 'specialize-head :actions actions :check-stack nil)
      (format t "2~%")
      ;; no need to repush the heads on because the specialized head is applied below
      (setf specialized-filler-exprs
	    (mapcar #'(lambda (specialized-head)
			(replace-km-expr-head
			 ;;newfiller
			 specialized-filler
			 (extract-head specialized-head)
			 ))
		    (first specialized-heads)))
      ;; prompt user for the right choice.
      (append new-head
	      (list
	       (list slot
		     (list
		      (prompt-for-clarification filler specialized-filler-exprs))))))
     ((does-last-action-include 'specialize-tail)
					;(format t "3~%")

      ;; because don't care about specialize-tail again, remove the specialize-tail
      (pop *ls-last-parsing-action*)
      (setf specialized-tails (pop *ls-specialized-tails*))

      (prompt-for-clarification
       ;; orig expr
       (append new-head `((,slot (,filler))))
       (car specialized-tails))
      )
     (t output-expr))
    ))

					#|
GNU LESSER GENERAL PUBLIC LICENSE
Version 2.1, February 1999

Copyright (C) 1991, 1999 Free Software Foundation, Inc.
59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
Everyone is permitted to copy and distribute verbatim copies
of this license document, but changing it is not allowed.

[This is the first released version of the Lesser GPL.  It also counts
as the successor of the GNU Library Public License, version 2, hence
the version number 2.1.]

Preamble

The licenses for most software are designed to take away your
freedom to share and change it.  By contrast, the GNU General Public
Licenses are intended to guarantee your freedom to share and change
free software--to make sure the software is free for all its users.

This license, the Lesser General Public License, applies to some
specially designated software packages--typically libraries--of the
Free Software Foundation and other authors who decide to use it.  You
can use it too, but we suggest you first think carefully about whether
this license or the ordinary General Public License is the better
strategy to use in any particular case, based on the explanations below.

When we speak of free software, we are referring to freedom of use,
not price.  Our General Public Licenses are designed to make sure that
you have the freedom to distribute copies of free software (and charge
for this service if you wish); that you receive source code or can get
it if you want it ; that you can change the software and use pieces of
it in new free programs	   ; and that you are informed that you can do
these things.

To protect your rights, we need to make restrictions that forbid
distributors to deny you these rights or to ask you to surrender these
rights.  These restrictions translate to certain responsibilities for
you if you distribute copies of the library or if you modify it.

For example, if you distribute copies of the library, whether gratis
or for a fee, you must give the recipients all the rights that we gave
you.  You must make sure that they, too, receive or can get the source
code.  If you link other code with the library, you must provide
complete object files to the recipients, so that they can relink them
with the library after making changes to the library and recompiling
it.  And you must show them these terms so they know their rights.

We protect your rights with a two-step method: (1) we copyright the
library, and (2) we offer you this license, which gives you legal
permission to copy, distribute and/or modify the library.

To protect each distributor, we want to make it very clear that
there is no warranty for the free library.  Also, if the library is
modified by someone else and passed on, the recipients should know
that what they have is not the original version, so that the original
author's reputation will not be affected by problems that might be
introduced by others.

Finally, software patents pose a constant threat to the existence of
any free program.  We wish to make sure that a company cannot
effectively restrict the users of a free program by obtaining a
restrictive license from a patent holder.  Therefore, we insist that
any patent license obtained for a version of the library must be
consistent with the full freedom of use specified in this license.

Most GNU software, including some libraries, is covered by the
ordinary GNU General Public License.  This license, the GNU Lesser
General Public License, applies to certain designated libraries, and
is quite different from the ordinary General Public License.  We use
this license for certain libraries in order to permit linking those
libraries into non-free programs.

When a program is linked with a library, whether statically or using
a shared library, the combination of the two is legally speaking a
combined work, a derivative of the original library.  The ordinary
General Public License therefore permits such linking only if the
entire combination fits its criteria of freedom.  The Lesser General
Public License permits more lax criteria for linking other code with
the library.

We call this license the \"Lesser\" General Public License because it
does Less to protect the user's freedom than the ordinary General
Public License.  It also provides other free software developers Less
of an advantage over competing non-free programs.  These disadvantages
are the reason we use the ordinary General Public License for many
libraries.  However, the Lesser license provides advantages in certain
special circumstances.

For example, on rare occasions, there may be a special need to
encourage the widest possible use of a certain library, so that it becomes
a de-facto standard.  To achieve this, non-free programs must be
allowed to use the library.  A more frequent case is that a free
library does the same job as widely used non-free libraries.  In this
case, there is little to gain by limiting the free library to free
software only, so we use the Lesser General Public License.

In other cases, permission to use a particular library in non-free
programs enables a greater number of people to use a large body of
free software.  For example, permission to use the GNU C Library in
non-free programs enables many more people to use the whole GNU
operating system, as well as its variant, the GNU/Linux operating
system.

Although the Lesser General Public License is Less protective of the
users' freedom, it does ensure that the user of a program that is
linked with the Library has the freedom and the wherewithal to run
that program using a modified version of the Library.

The precise terms and conditions for copying, distribution and
modification follow.  Pay close attention to the difference between a
\"work based on the library\" and a \"work that uses the library\".  The
former contains code derived from the library, whereas the latter must
be combined with the library in order to run.

GNU LESSER GENERAL PUBLIC LICENSE
TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. This License Agreement applies to any software library or other
program which contains a notice placed by the copyright holder or
other authorized party saying it may be distributed under the terms of
this Lesser General Public License (also called \"this License\").
Each licensee is addressed as \"you\".

A \"library\" means a collection of software functions and/or data
prepared so as to be conveniently linked with application programs
(which use some of those functions and data) to form executables.

The \"Library\", below, refers to any such software library or work
which has been distributed under these terms.  A \"work based on the
Library\" means either the Library or any derivative work under
copyright law: that is to say, a work containing the Library or a
portion of it, either verbatim or with modifications and/or translated
straightforwardly into another language.  (Hereinafter, translation is
							included without limitation in the term \"modification\".)

\"Source code\" for a work means the preferred form of the work for
making modifications to it.  For a library, complete source code means
all the source code for all modules it contains, plus any associated
interface definition files, plus the scripts used to control compilation
and installation of the library.

Activities other than copying, distribution and modification are not
covered by this License	     ; they are outside its scope.  The act of
running a program using the Library is not restricted, and output from
such a program is covered only if its contents constitute a work based
on the Library (independent of the use of the Library in a tool for
			    writing it).  Whether that is true depends on what the Library does
and what the program that uses the Library does.
  
1. You may copy and distribute verbatim copies of the Library's
complete source code as you receive it, in any medium, provided that
you conspicuously and appropriately publish on each copy an
appropriate copyright notice and disclaimer of warranty	; keep intact
all the notices that refer to this License and to the absence of any
warranty	; and distribute a copy of this License along with the
Library.

You may charge a fee for the physical act of transferring a copy,
and you may at your option offer warranty protection in exchange for a
fee.

2. You may modify your copy or copies of the Library or any portion
of it, thus forming a work based on the Library, and copy and
distribute such modifications or work under the terms of Section 1
above, provided that you also meet all of these conditions:

a) The modified work must itself be a software library.

b) You must cause the files modified to carry prominent notices
stating that you changed the files and the date of any change.

c) You must cause the whole of the work to be licensed at no
charge to all third parties under the terms of this License.

d) If a facility in the modified Library refers to a function or a
table of data to be supplied by an application program that uses
the facility, other than as an argument passed when the facility
is invoked, then you must make a good faith effort to ensure that,
in the event an application does not supply such function or
table, the facility still operates, and performs whatever part of
its purpose remains meaningful.

(For example, a function in a library to compute square roots has
     a purpose that is entirely well-defined independent of the
     application.  Therefore, Subsection 2d requires that any
     application-supplied function or table used by this function must
     be optional: if the application does not supply it, the square
     root function must still compute square roots.)

These requirements apply to the modified work as a whole.  If
identifiable sections of that work are not derived from the Library,
and can be reasonably considered independent and separate works in
themselves, then this License, and its terms, do not apply to those
sections when you distribute them as separate works.  But when you
distribute the same sections as part of a whole which is a work based
on the Library, the distribution of the whole must be on the terms of
this License, whose permissions for other licensees extend to the
entire whole, and thus to each and every part regardless of who wrote
it.

Thus, it is not the intent of this section to claim rights or contest
your rights to work written entirely by you ; rather, the intent is to
exercise the right to control the distribution of derivative or
collective works based on the Library.

In addition, mere aggregation of another work not based on the Library
with the Library (or with a work based on the Library) on a volume of
a storage or distribution medium does not bring the other work under
the scope of this License.

3. You may opt to apply the terms of the ordinary GNU General Public
License instead of this License to a given copy of the Library.  To do
this, you must alter all the notices that refer to this License, so
that they refer to the ordinary GNU General Public License, version 2,
instead of to this License.  (If a newer version than version 2 of the
				 ordinary GNU General Public License has appeared, then you can specify
				 that version instead if you wish.)  Do not make any other change in
these notices.

Once this change is made in a given copy, it is irreversible for
that copy, so the ordinary GNU General Public License applies to all
subsequent copies and derivative works made from that copy.

This option is useful when you wish to copy part of the code of
the Library into a program that is not a library.

4. You may copy and distribute the Library (or a portion or
					       derivative of it, under Section 2) in object code or executable form
under the terms of Sections 1 and 2 above provided that you accompany
it with the complete corresponding machine-readable source code, which
must be distributed under the terms of Sections 1 and 2 above on a
medium customarily used for software interchange.

If distribution of object code is made by offering access to copy
from a designated place, then offering equivalent access to copy the
source code from the same place satisfies the requirement to
distribute the source code, even though third parties are not
compelled to copy the source along with the object code.

5. A program that contains no derivative of any portion of the
Library, but is designed to work with the Library by being compiled or
linked with it, is called a \"work that uses the Library\".  Such a
work, in isolation, is not a derivative work of the Library, and
therefore falls outside the scope of this License.

However, linking a \"work that uses the Library\" with the Library
creates an executable that is a derivative of the Library (because it
								   contains portions of the Library), rather than a \"work that uses the
library\".  The executable is therefore covered by this License.
Section 6 states terms for distribution of such executables.

When a \"work that uses the Library\" uses material from a header file
that is part of the Library, the object code for the work may be a
derivative work of the Library even though the source code is not.
Whether this is true is especially significant if the work can be
linked without the Library, or if the work is itself a library.  The
threshold for this to be true is not precisely defined by law.

If such an object file uses only numerical parameters, data
structure layouts and accessors, and small macros and small inline
functions (ten lines or less in length), then the use of the object
file is unrestricted, regardless of whether it is legally a derivative
work.  (Executables containing this object code plus portions of the
		    Library will still fall under Section 6.)

Otherwise, if the work is a derivative of the Library, you may
distribute the object code for the work under the terms of Section 6.
Any executables containing that work also fall under Section 6,
whether or not they are linked directly with the Library itself.

6. As an exception to the Sections above, you may also combine or
link a \"work that uses the Library\" with the Library to produce a
work containing portions of the Library, and distribute that work
under terms of your choice, provided that the terms permit
modification of the work for the customer's own use and reverse
engineering for debugging such modifications.

You must give prominent notice with each copy of the work that the
Library is used in it and that the Library and its use are covered by
this License.  You must supply a copy of this License.  If the work
during execution displays copyright notices, you must include the
copyright notice for the Library among them, as well as a reference
directing the user to the copy of this License.  Also, you must do one
of these things:

a) Accompany the work with the complete corresponding
machine-readable source code for the Library including whatever
changes were used in the work (which must be distributed under
Sections 1 and 2 above)	    ; and, if the work is an executable linked
with the Library, with the complete machine-readable \"work that
uses the Library\", as object code and/or source code, so that the
user can modify the Library and then relink to produce a modified
executable containing the modified Library.  (It is understood
that the user who changes the contents of definitions files in the
Library will not necessarily be able to recompile the application
to use the modified definitions.)

b) Use a suitable shared library mechanism for linking with the
Library.  A suitable mechanism is one that (1) uses at run time a
copy of the library already present on the user's computer system,
rather than copying library functions into the executable, and (2)
will operate properly with a modified version of the library, if
the user installs one, as long as the modified version is
interface-compatible with the version that the work was made with.

c) Accompany the work with a written offer, valid for at
least three years, to give the same user the materials
specified in Subsection 6a, above, for a charge no more
than the cost of performing this distribution.

d) If distribution of the work is made by offering access to copy
from a designated place, offer equivalent access to copy the above
specified materials from the same place.

e) Verify that the user has already received a copy of these
materials or that you have already sent this user a copy.

For an executable, the required form of the \"work that uses the
Library\" must include any data and utility programs needed for
reproducing the executable from it.  However, as a special exception,
the materials to be distributed need not include anything that is
normally distributed (in either source or binary form) with the major
components (compiler, kernel, and so on) of the operating system on
which the executable runs, unless that component itself accompanies
the executable.

It may happen that this requirement contradicts the license
restrictions of other proprietary libraries that do not normally
accompany the operating system.  Such a contradiction means you cannot
use both them and the Library together in an executable that you
distribute.

7. You may place library facilities that are a work based on the
Library side-by-side in a single library together with other library
facilities not covered by this License, and distribute such a combined
library, provided that the separate distribution of the work based on
the Library and of the other library facilities is otherwise
permitted, and provided that you do these two things:

a) Accompany the combined library with a copy of the same work
based on the Library, uncombined with any other library
facilities.  This must be distributed under the terms of the
Sections above.

b) Give prominent notice with the combined library of the fact
that part of it is a work based on the Library, and explaining
where to find the accompanying uncombined form of the same work.

8. You may not copy, modify, sublicense, link with, or distribute
the Library except as expressly provided under this License.  Any
attempt otherwise to copy, modify, sublicense, link with, or
distribute the Library is void, and will automatically terminate your
rights under this License.  However, parties who have received copies,
or rights, from you under this License will not have their licenses
terminated so long as such parties remain in full compliance.

9. You are not required to accept this License, since you have not
signed it.  However, nothing else grants you permission to modify or
distribute the Library or its derivative works.  These actions are
prohibited by law if you do not accept this License.  Therefore, by
modifying or distributing the Library (or any work based on the
Library), you indicate your acceptance of this License to do so, and
all its terms and conditions for copying, distributing or modifying
the Library or works based on it.

10. Each time you redistribute the Library (or any work based on the
Library), the recipient automatically receives a license from the
original licensor to copy, distribute, link with or modify the Library
subject to these terms and conditions.  You may not impose any further
restrictions on the recipients' exercise of the rights granted herein.
You are not responsible for enforcing compliance by third parties with
this License.

11. If, as a consequence of a court judgment or allegation of patent
infringement or for any other reason (not limited to patent issues),
conditions are imposed on you (whether by court order, agreement or
otherwise) that contradict the conditions of this License, they do not
excuse you from the conditions of this License.  If you cannot
distribute so as to satisfy simultaneously your obligations under this
License and any other pertinent obligations, then as a consequence you
may not distribute the Library at all.  For example, if a patent
license would not permit royalty-free redistribution of the Library by
all those who receive copies directly or indirectly through you, then
the only way you could satisfy both it and this License would be to
refrain entirely from distribution of the Library.

If any portion of this section is held invalid or unenforceable under any
particular circumstance, the balance of the section is intended to apply,
and the section as a whole is intended to apply in other circumstances.

It is not the purpose of this section to induce you to infringe any
patents or other property right claims or to contest validity of any
such claims	 ; this section has the sole purpose of protecting the
integrity of the free software distribution system which is
implemented by public license practices.  Many people have made
generous contributions to the wide range of software distributed
through that system in reliance on consistent application of that
system; it is up to the author/donor to decide if he or she is willing
to distribute software through any other system and a licensee cannot
impose that choice.

This section is intended to make thoroughly clear what is believed to
be a consequence of the rest of this License.

12. If the distribution and/or use of the Library is restricted in
certain countries either by patents or by copyrighted interfaces, the
original copyright holder who places the Library under this License may add
an explicit geographical distribution limitation excluding those countries,
so that distribution is permitted only in or among countries not thus
excluded.  In such case, this License incorporates the limitation as if
written in the body of this License.

13. The Free Software Foundation may publish revised and/or new
versions of the Lesser General Public License from time to time.
Such new versions will be similar in spirit to the present version,
but may differ in detail to address new problems or concerns.

Each version is given a distinguishing version number.  If the Library
specifies a version number of this License which applies to it and
\"any later version\", you have the option of following the terms and
conditions either of that version or of any later version published by
the Free Software Foundation.  If the Library does not specify a
license version number, you may choose any version ever published by
the Free Software Foundation.

14. If you wish to incorporate parts of the Library into other free
programs whose distribution conditions are incompatible with these,
write to the author to ask for permission.  For software which is
copyrighted by the Free Software Foundation, write to the Free
Software Foundation	 ; we sometimes make exceptions for this.  Our
decision will be guided by the two goals of preserving the free status
of all derivatives of our free software and of promoting the sharing
and reuse of software generally.

NO WARRANTY

15. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO
WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.
EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR
OTHER PARTIES PROVIDE THE LIBRARY \"AS IS\" WITHOUT WARRANTY OF ANY
KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE
LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME
THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

16. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN
WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY
AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU
FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR
CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE
LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES.

END OF TERMS AND CONDITIONS
|#
