(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)
; rules.lsp               Gordon S. Novak Jr.            ; 17 May 07

; Copyright (c) 2005 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; 11 May 05; 12 May 05; 17 May 05; 18 May 05; 19 May 05; 20 May 05; 27 May 05
; 06 Jun 05; 08 Jun 05; 14 Jul 05; 15 Jul 05; 28 Jul 05; 02 Aug 05; 08 Aug 05
; 09 Aug 05; 15 Aug 05; 17 Aug 05; 19 Aug 05; 06 Sep 05; 07 Sep 05; 09 Feb 06

; Rule Engine for use with KM

; Rules are needed to complete a description of a physics problem,
; e.g. If the destination of a Fall is unknown, assume it is the Ground.

; The rule engine should run in a forward fashion until a fixpoint is
; reached (no further rules can fire).  We will assume that rules are
; indexed by a "primary" class, e.g. Fall.

#|
Rules can be written using "plain Lisp" symbols; these are translated to
KM symbol forms as needed.  For example, (class ?f 'fall) will test
against the class |Fall|.

Rule format:   (name  antecedent  consequent)
   antecedent is:     (and (class ?var 'class) ...)
     The rule will be indexed under the specified class.

   Antecedent forms:
      (and <form> ...)
      (or  <form> ...)
      (not <form>)             ; there is no match to <form>
      (class ?var <class>)     ; ?var has <class> as a superclass
      (input-word ?var "word" ...) ; test for specific input word(s)
      (<prop> ?var <value>)    ; slot <prop> of ?var has value <value>

      (one-of (<prop> ?var <values>)) ; slot <prop> of ?var has at least one value of <values>
      (all-of (<prop> ?var <values>)) ; slot <prop> of ?var has all values of <values>
                                      ; equivalent to (<prop> ?var <value>)
      (must-be (<prop> ?var <values>)) ; slot <prop> of ?var has only values listed in <values>
      (exactly (<prop> ?var <values>)) ; = must-be and all-of

   Consequent forms:
      (progn <form> ...)       ; execute multiple forms
      (create ?var '<class>)   ; create instance of <class> and set ?var to it
      (<prop> ?var <value>)    ; set the slot <prop> of ?var to <value>
;      (change <prop> ?var <value>) ; change the slot <prop> of ?var to <value>
	(negate ?var) ; make negative the numerical value of ?var
      (remove-triple <prop> ?var ?propvar)
; e.g. (_Velocity-Value1 has (value ((:pair 80 *meter-per-second))))

The usual way to run rules is to call rlrunallrules with a list of
all the instance objects in the problem:

   (rlrunallrules '(|_Fall1901| |_Ground1905| |_Cliff1902|))

Note that a list of executed rules is stored for each object (so that a
rule only fires once).

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'while)
    (defmacro while (test &rest forms)
      `(loop (unless ,test (return)) ,@forms))))


(defmacro rlclassrulesm         (x) `(get ,x 'rlclassrulesm)) ; rules for a class
(defmacro rlrule                (x) `(get ,x 'rlrule))        ; rule from name
(defmacro rlrulename            (x) `(first ,x))   ; rule is (name ante conse)
(defmacro rlruleante            (x) `(second ,x))
(defmacro rlruleconse           (x) `(third ,x))
(defmacro rlrulesfired          (x) `(get ,x 'rlrulesfired)) ; rules fired already
(defvar *rlallrules* nil)
(defvar *rlinstlist* nil)    ; place to save inst list for debugging
(defvar *rltriples* nil)
(defvar *aura-trace-rule-engine* nil)
(defvar *rlremovetriples* nil)
(defvar *rlrulesfired* nil)
(defvar *newinst* nil)

; get the list of rules associated with a given class
(defun rlclassrules (class) (rlclassrulesm class))

; define a list of rules
(defun rldefrules (rules)
  (dolist (rule rules) (rldefrule rule)) )

; define a rule
(defun rldefrule (rule)
  (let (classbl)
    (setq classbl (rlmainclass rule))
    (when classbl
      (rlundefrule (rlrulename rule))
      (pushnew rule (rlclassrulesm (cdar classbl)))
      (pushnew (rlrulename rule) *rlallrules*)
      (setf (rlrule (rlrulename rule)) rule) ) ))

; remove definitions of all rules
(defun rlclearrules () (dolist (rulename *rlallrules*) (rlundefrule rulename)) )

; remove definition of a rule
(defun rlundefrule (rulename)
  (let ((rule (rlrule rulename)) classbl old)
    (setq classbl (rlmainclass rule))
    (when classbl
      (if (setq old (assoc rulename (rlclassrulesm (cdar classbl))))
	  (setf (rlclassrulesm (cdar classbl))
		(remove old (rlclassrulesm (cdar classbl))))) )
    (setq *rlallrules* (remove rulename *rlallrules*)) ))

; get main class of a rule
; returns a binding list ((?c . class))
; assumes rule is (name (and (class ?c 'foo) ...) ...)
(defun rlmainclass (rule &optional obj)
  (let (ante classcl)
    (and (consp rule)
	 (consp (setq ante (rlruleante rule)))
	 (eq (car ante) 'and)
	 (consp (setq classcl (cadr ante)))
	 (eq (car classcl) 'class)
	 (varp (cadr classcl))
	 (quotep (third classcl))
	 (list (cons (cadr classcl)
		     (or obj (kmifyclass (cadr (third classcl)))))) ) ))

; 09 Aug 05: add instlist arg (Won)  ; 17 May 07: run rules on new inst (Won)
; Run a rule if possible
; bindings is an initial binding list that defines the main class
; assumes antecedant is (and (class ...) ...)
(defun rlrunrule (rule bindings instlist)
  (let (newbindings class)
    (setq class (cdar bindings))
    (if (not (member (rlrulename rule) (rlrulesfired class)))
		(progn (format *aura-trace-rule-engine* "testing rule: ~a~%"
                               (rlrulename rule))
	(if (setq newbindings (rlevaland (cddr (rlruleante rule)) bindings))
	    (progn (push (rlrulename rule) (rlrulesfired class))
		   (rleval (rlruleconse rule) newbindings instlist)
		   (format *aura-trace-rule-engine* "rule ~a fired~%"
                           (rlrulename rule))
		   (format *aura-trace-rule-engine* "~a~%~%"
                           (km (list '|showme| (caar *rltriples*))))
		   (push (rlrulename rule) *rlrulesfired*)
                       ; if a new instance was created, rerun RE on instances
		   (if *newinst*
			   (progn (rlrunallrules (append *newinst* instlist) t)
					  (setq *newinst* nil)))
		   t) ) ) )))

; evaluate a (and ...) clause
(defun rlevaland (clauses bindings)
  (let (clause)
    (while clauses
      (setq clause (pop clauses))
      (if (consp clause)
	  (setq bindings
		(case (first clause)
		  (and (rlevaland (cdr clause) bindings) )
		  (or (rlevalor (cdr clause) bindings) )
		  (not (rlevalnot (cadr clause) bindings) )
		  (class (rlevalclass clause bindings))
		  (input-word (rlevalinputword clause bindings))
		  ((< > <= >= = /=) (rlevalcompare clause bindings))
		  (t (prog1 (rlevalprop clause bindings clauses)
		            (setq clauses nil)))))))
    bindings))

(defun rlevalcompare (clause bindings)
  (setq clause (sublis bindings clause))
  (print clause)
  (if (member '|Property-Value|
			  (rlinstsupers (cadr clause)))
	  (let (value)
		(setq value (km (append '(|the| |value| |of|) (list (cadr clause)))))
		(if (funcall (car clause) (cadar value) (caddr clause))
			bindings nil))))

; evaluate a property
; This function will do search through multiple property values,
; returning the first set of bindings that makes remaining clauses true.
(defun rlevalprop (clause bindings clauses)
  (let (vals newb op goals)
    (setq op (car clause))
    (if (member op '(one-of all-of must-be exactly))
        (setq clause (sublis bindings (cadr clause)))
        (setq clause (sublis bindings clause)) )
    (and (not (varp (cadr clause)))
         (setq vals (kmget (cadr clause) (car clause)))
         (if (varp (caddr clause))
             (progn
               (while (and (not newb) vals)
                 (setq newb (rlevaland clauses
                                       (cons (cons (caddr clause) (pop vals))
                                             bindings))) )
               newb)
             (if (quotep (caddr clause))
                 (progn
                   (setq goals (cadr (caddr clause)))
                   (case op
                     (one-of  (and (some #'(lambda (val) (rlequal val goals))
                                         vals)
                                   bindings))
                     (all-of  (and (rlequal goals vals) bindings))
                     (must-be (and (rlexclusive vals goals) bindings) )
                     (exactly (and (rlequal goals vals)
                                   (rlexclusive vals goals) bindings))
                     (t       (and (rlequal goals vals) bindings))) ) )) )))

(defun rlexclusive (vals goals)
  (let ((temp t))
    (dolist (val vals)
      (setq temp (and temp (member val goals))) )
    temp))

; 19 Aug 05: Test over multiple words as value of input-word
; evaluate a test of input word:  (input-word ?top "top")
(defun rlevalinputword (clause bindings)
  (let (vals newb)
    (setq clause (sublis bindings clause))
    (and (not (varp (cadr clause)))
	 (setq vals (kmget (cadr clause) (car clause)))
	 (consp vals)
	 (some #'(lambda (val)
		   (and (consp val)
			(eq (car val) :|pair|)
			(member (cadr val) (cddr clause) :test #'string=)))
	       vals)
	 bindings) ))

; 14 Jul 05
; evaluate a single clause
(defun rlevalclause (clause bindings)
  (case (first clause)
    (and (rlevaland (cdr clause) bindings) )
    (or (rlevalor (cdr clause) bindings) )
    (not (rlevalnot (cadr clause) bindings) )
    (class (rlevalclass clause bindings))
    (input-word (rlevalinputword clause bindings))
    (t (rlevalprop clause bindings nil)) ) )

; 08 Aug 08
; evaluate a class clause, (class inst <class>)
; <class> may be a variable or 'class
(defun rlevalclass (clause bindings)
  (let (classes)
    (setq clause (sublis bindings clause))
    (and (not (varp (cadr clause)))
	 (setq classes (rlinstsupers (cadr clause)))  ; Won; was rlclassof
	 (if (varp (caddr clause))
	     (cons (cons (caddr clause) (car classes)) bindings)
	     (if (quotep (caddr clause))
		 (and (member (kmifyclass (cadr (caddr clause))) classes)
		      bindings)))) ))

; 17 Aug 05
; evaluate a (not ...) clause
; A special evaluation is done for a clause such as (not (value ?x ?v))
; to avoid KM creating a Skolem for the value.
(defun rlevalnot (clause bindings)
  (let (inst)
    (and (consp clause)
	 (if (member (car clause) '(and or not class))
	     (if (rlevalclause clause bindings) nil bindings)
	     (if (and (varp (cadr clause))
		      (setq inst (cdr (assoc (cadr clause) bindings)))
		      (varp (caddr clause)))
		 (if (rlkmlookup inst (kmifyprop (car clause)))
		     nil
		     bindings)
                 (if (rlevalclause clause bindings) nil bindings)))) ))
       
; evaluate a (or ...) clause
(defun rlevalor (clauses bindings)
  (let (res)
    (while (and clauses (not res))
      (setq res (rlevalclause (pop clauses) bindings)) )
    res))

; 13 Oct 06: add negate (Won)
; 09 Aug 05: add instlist arg (Won)
; 28 Jul 05: change by Won to runallrules on new inst
; 15 Aug 05: add triples
; 07 Sep 05: add noinit arg to call to rlrunallrules
; 17 May 07: tracing and Won change for remove-triple, runallrules on new inst
; evaluate a rule consequent expression
(defun rleval (expr bindings instlist)
  (let (newinst class val units)
    (and (consp expr)
	 (case (car expr)
	   (progn (dolist (x (cdr expr))
		    (setq bindings (rleval x bindings instlist))))
#|
	   (change (setq expr (sublis bindings expr))
			   (setq val (rlevalb (caddr expr)))
			   (km (list (cadr expr) '|now-has|
                                     (list (kmifyprop (car expr)) (list val)))))
|#
	   (negate 
		(setq expr (sublis bindings expr))
		(setq val (km (append '(|the| |value| |of|) (list (cadr expr)))))
		(setq units (caddar val)) ; store the units of the value
                (push (cons (cadr expr)
                            (cons '|value|
                                  (list (cons ':|pair|
                                              (cons (cadar val)
                                                    (list units))))))
                      *rlremovetriples*)
		(format *aura-trace-rule-engine* "set to remove: ~a~%" (car *rlremovetriples*))
		(setq val (- (cadar val)))
                (push (cons (cadr expr)
                            (cons '|value|
                                  (list (cons ':|pair|
                                              (cons val (list units))))))
                      *rltriples*)
		(format *aura-trace-rule-engine* "added: ~a~%" (car *rltriples*)) )
	   (remove-triple
		(setq expr (sublis bindings expr))
		(push (cons (caddr expr) (cons (cadr expr) (list (cadddr expr))))
			  *rlremovetriples*)
		(format *aura-trace-rule-engine* "set to remove: ~a~%" (car *rlremovetriples*))
		(push (cons (cadddr expr) (cons (car (km (append '(|the| |inverse| |of|) (list (cadr expr))))) (list (caddr expr))))
			  *rlremovetriples*)
		(format *aura-trace-rule-engine* "and/or the inverse: ~a~%" (car *rlremovetriples*))
		bindings)
	   (create (setq expr (sublis bindings expr))
		   (setq newinst
			 (km (list '|a|
			       (setq class (kmifyclass
					     (rlunquote (caddr expr)))))))
;		   (rlrunallrules (append newinst instlist) t)   ; need to finish consequent before calling RE on new inst
		   (setq *newinst* (append newinst *newinst*))
		   (push newinst *rlinstlist*)
		   (push (list (car newinst) '|instance-of| class)
			 *rltriples*)
		   (format *aura-trace-rule-engine* "added: ~a~%" (car *rltriples*))
		   (cons (cons (cadr expr) (car newinst)) bindings) )
	   (unify (setq expr (sublis bindings expr))
		  (km (list (rlunquote (cadr expr)) '&
			    (rlunquote (caddr expr)))))
	   (t (setq expr (sublis bindings expr))
	      (unless (varp (cadr expr))
		(setq val (rlevalb (caddr expr)))
		(rlputprop (cadr expr) (car expr) val)
		(push (list (cadr expr) (kmifyprop (car expr)) val) *rltriples*)
		(format *aura-trace-rule-engine* "added: ~a~%" (car *rltriples*)) )
	      bindings))) ))

; evaluate value part of a conse formula, (<prop> ?var <value>)
; ****** to be expanded *******
(defun rlevalb (expr) (rlunquote expr))

; returns a list of classes of an instance
(defun rlclassof (inst) (km (list '|the| '|instance-of| '|of| inst)))

; set a property of an instance, where val is a single value
(defun rlputprop (inst prop val)
  (kmtr (list inst '|has| (list (kmifyprop prop) (list val)))))

; indirection to allow tracing
(defun kmtr (lst) (km lst))

; set a property of an instance, where vals is a list of values
(defun rlputprops (inst prop vals)
  (km (list inst '|has| (list (kmifyprop prop) vals))))

; remove a quote if present, handle (list ...)
(defun rlunquote (x)
  (if (and (consp x)
	   (eq (car x) 'quote))
      (cadr x)
      (if (and (consp x)
	       (eq (car x) 'list))
	  (mapcar #'rlunquote (cdr x))
          x)))

; 08 Aug 05
; test equality for matching.
; goal might be a symbol or list; vals is a list.
(defun rlequal (goal vals)
  (if (symbolp goal)
      (member goal vals)
      (if (consp goal)
	  (subsetp goal vals))))

; 28 Jul 05: change by Won to include all-superclasses
; get superclasses of an instance.
(defun rlinstsupers (inst)
  (let (classes supers)
    (setq classes (km (append '(|the| |instance-of| |of|) (list inst))))
    (setq supers classes) ; include the instance's immediate class(es)
    (dolist (class classes)
      (setq supers (union (km (append '(|the| |all-superclasses| |of|) (list class))) supers)) )
    (set-difference supers '(|Thing| |Action| |Event|) ) ))

; 09 Aug 05: add instlist arg (Won)
; run rules on an instance
(defun rlrunrules (inst instlist)
  (let ((supers (rlinstsupers inst)) rules progress)
    (dolist (s supers)
      (dolist (rule (rlclassrulesm s))
	(if (rlrunrule rule (rlmainclass rule inst) instlist)
	    (setq progress t)) ) )
    progress))

; 15 Aug 05; 17 Aug 05; 06 Sep 05; 07 Sep 05; 17 May 07
; run rules on a set of instances until no further progress
(defun rlrunallrules (instlist &optional noinit)
  (let ((progress t))
    (unless noinit 
	  (setq *rltriples* nil)
	  (setq *rlremovetriples* nil)
	  (setq *rlrulesfired* nil)
	  (setq *rlinstlist* instlist)      ; save for debugging
	  (setq *newinst* nil)
	  (dolist (inst instlist)
		(setf (get inst 'rlrulesfired) nil)) )
    (while progress
      (setq progress nil)
      (dolist (inst instlist)
	(setq progress (or progress (rlrunrules inst instlist))) ) )
    (reverse *rltriples*) ))

; 17 Aug 05
; see if a slot is defined for an instance in KM, without creating Skolems
; ***** is there a standard KM function that does this?
; ***** doesn't handle inheritance (but probably shouldn't)
(defun rlkmlookup (inst propname)
  (let (pl done val)
    (setq pl (symbol-plist inst))
    (while (and pl (not done))
      (if (string= (symbol-name (car pl)) "OWN-PROPERTIES"
		   :end1 (min (length (symbol-name (car pl))) 14))
	  (if (setq val (cadr (assoc propname (cadr pl))))
	      (setq done t)))
      (setq pl (cddr pl)))
    val))
