;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; match.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains various pattern matchers
;;;   differ in type of binding list
;;;   differ in degree of matching
;;; 
;;; All matchers 
;;;   work on atoms and lists (with @variables occurring in last position only)
;;;   handle quote
;;;   treat variables as free, i.e. quantifiers are ignored
;;;
;;; Implementation note:
;;;   The simple binding list matchers all assume that they are the only ones
;;;   creating their binding lists.  For example, code for MATCHP assumes that
;;;   variables cannot be bound to expressions containing variables with 
;;;   bindings on alist that we want to pursue.  By contrast, the various 
;;;   matchers that work with complex binding lists do not make this assumption.
;;;   They allow that their input binding lists may be arbitrarily complex.
;;;
;;;   Also matchers with complex binding lists assume that variables in their
;;;   distinct arguments are distinct, unless the alist arguments are the same.
;;;   Consequence (IDENTIFY '(P ?X) AL '(P ?X) BL) --> NIL if AL and BL not EQ.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (proclaim '(special traceexpressions alist)))

(defconstant truth '((t . t)))

(defvar *occurcheck* t
 "*OCCURCHECK* determines whether or not various matchers, in considering the
  binding of a variable to a term, first check for occurrences of the variable
  in the term.  The default value is T.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUOTIFY assumes its argument is ground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quotify (x)
  (cond ((numberp x) x)
        ((characterp x) x)
        ((stringp x) x)
        (t (list 'quote x))))

(defun kwotify (x)
  (cond ((numberp x) x)
        ((characterp x) x)
        ((stringp x) x)
        ((member x '(t nil)) x)
        (t (list 'kwote x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNQUOTE assumes its argument is ground
;;; NB: the value returned may not be ground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unquote (x)
  (cond ((numberp x) x)
        ((stringp x) x)
        ((characterp x) x)
        ((atom x) nil)
        ((eq 'quote (car x)) (cadr x))
        ((eq 'listof (car x)) (mapcar 'unquote (cdr x)))))

(defmethod varp (x)
 "(VARP X)
  VARP takes any object as argument and returns T if and only if the
  object is a variable."
  (and (symbolp x)
       (setq x (char (symbol-name x) 0))
       (or (eql #\? x) (eql #\@ x))))

(defmethod indvarp (x)
 "(INDVARP X)
  INDVARP takes any object as argument and returns T if and only if the
  object is an individual variable."
  (and (symbolp x) (eql #\? (char (symbol-name x) 0))))

(defmethod seqvarp (x)
 "(SEQVARP X)
  SEQVARP takes any object as argument and returns T if and only if the
  object is a sequence variable."
  (and (symbolp x) (eql #\@ (char (symbol-name x) 0))))

(defun newindvar () (gensym "?"))

(defun newseqvar () (gensym "@"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (identp x y)
;;;    determines whether x and y are equal but handles quote
;;;    if success, returns t
;;;    if failure, returns nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod identp (x y)
 "(IDENTP X Y)
  IDENTP takes two expressions as arguments and checks whether they are 
  meta-equal.  IDENTP returns T if the check is successful, and it
  returns NIL otherwise."
  (cond ((atom x) (cond ((atom y) (equalp x y))
                        ((eq 'quote (car y)) (descriptionp x (cadr y)))))
	((eq 'quote (car x)) (descriptionp y (cadr x)))
	((eq 'listof (car x)) (identplist x y))
	(t (cond ((atom y) nil)
                 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (mapand 'identp x y))))))

(defun identplist (x y)
  (cond ((atom y) nil)
        ((eq 'quote (car y)) (mapand 'descriptionp (cdr x) (cadr y)))
        ((eq 'listof (car y)) (mapand 'identp (cdr x) (cdr y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (descriptionp x y)
;;;    determines whether x is a description of y
;;;    equivalent to evaluating x and checking for equality with y 
;;;    but cheaper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun descriptionp (x y)
  (cond ((atom x) (if (or (numberp x) (stringp x) (characterp x)) (equalp x y)))
	((eq 'quote (car x)) (equalp (cadr x) y))
	((eq 'listof (car x))
         (cond ((not (listp y)) nil)
               (t (mapand 'descriptionp (cdr x) y))))))

(defun mapand (p l m)
  (do ()
      (nil)
      (cond ((null l) (return (null m)))
            ((null m) (return nil))
            ((funcall p (car l) (car m)) (setq l (cdr l) m (cdr m)))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (samep x y) 
;;;    determines whether x and y are same up to variable renaming
;;;    if success, returns t
;;;    if failure, returns nil
;;;    knows about quote
;;;
;;; NB: binds vars to selves, including seqvars (therefore not bound to lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod samep (x y)
 "(SAMEP X Y)
  SAMEP takes two expressions as arguments and checks whether they have the
  same structure, i.e. they are meta-equal after variable renaming.
  SAMEP returns T if the check is successful, and it returns NIL otherwise."
  (if (samepexp x y truth) t))

(defmethod samelist (x y)
 "(SAMELIST X Y)
  SAMELIST takes two expressions as arguments and checks whether they have the
  same structure, i.e. they are meta-equal after variable renaming.
  SAMELIST returns a list of variable bindings, if the check is successful;
  and it returns NIL otherwise."
  (samepexp x y truth))

(defun samepexp (x y al)
  (cond ((indvarp x) (if (indvarp y) (samepvar x y al)))
        ((seqvarp x) (if (seqvarp y) (samepvar x y al)))
	((atom x) (cond ((atom y) (if (equalp x y) al))
                        ((eq 'quote (car y)) (if (descriptionp x (cadr y)) al))))
	((eq 'quote (car x)) (if (descriptionp y (cadr x)) al))
        ((eq 'listof (car x)) (sameplist x y al))
	(t (cond ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
                 (t (samepexpexp x y al))))))

(defun samepvar (x y al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'eq)) (if (eq (cdr dum) y) al))
	  ((setq dum (dssq y al)) nil)
	  (t (acons x y al)))))

(defun sameplist (x y al)
  (cond ((atom y) nil)
        ((eq 'quote (car y)) (if (mapand 'descriptionp (cdr x) (cadr y)) al))
        ((eq 'listof (car y)) (samepexpexp (cdr x) (cdr y) al))))

(defun samepexpexp (l m al)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l) (return (if (null m) al)))
            ((null m) (return nil))
            ((setq al (samepexp (car l) (car m) al)))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (instp x y)
;;;     determines whether x is an instance of y
;;;     if success, returns alist
;;;     if failure, returns nil
;;;     knows about quote
;;;
;;; (matchp x y)
;;;     reverse of instp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod instp (x y)
 "(INSTP X Y)
  INSTP takes two expressions as arguments and checks whether the first
  expression is an instance of the second, i.e. whether there is a set of
  variable bindings that, when substituted into the second expression,   
  produces an expression meta-equal to the first expression.
  INSTP returns T if the check is successful, and it returns NIL otherwise."
  (if (matchpexp y x truth) t))

(defmethod instantiator (x y)
 "(INSTANTIATOR X Y)
  INSTANTIATOR takes two expressions as arguments and checks whether the first
  expression is an instance of the second, i.e. whether there is an alist of
  variable bindings that, when substituted into the second expression,   
  produces an expression meta-equal to the first expression.  If there is
  such an alist, it is returned as result; otherwise, the result is NIL."
  (matchpexp y x truth))

(defmethod matchp (x y)
 "(MATCHP X Y)
  MATCHP takes two expressions as arguments and checks whether the second
  expression is an instance of the first, i.e. whether there is a set of
  variable bindings that, when substituted into the first expression,   
  produces an expression meta-equal to the second expression.
  MATCHP returns T if the check is successful, and it returns NIL otherwise."
  (if (matchpexp x y truth) t))

(defmethod matcher (x y)
 "(MATCHER X Y)
  MATCHER takes two expressions as arguments and checks whether the second
  expression is an instance of the first, i.e. whether there is an alist of
  variable bindings that, when substituted into the first expression,   
  produces an expression meta-equal to the second expression.  If there is
  such an alist, it is returned as result; otherwise, the result is NIL."
  (matchpexp x y truth))

(defun matchpexp (x y al)
  (cond ((indvarp x) (matchpindvar x y al))
	((atom x) (cond ((atom y) (if (equalp x y) al))
                        ((eq 'quote (car y)) (if (descriptionp x (cadr y)) al))))
	((eq 'quote (car x)) (if (descriptionp y (cadr x)) al))
        ((eq 'listof (car x)) (matchplist x y al))
	(t (cond ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (matchpexpexp x y al))))))

(defun matchpindvar (x y al)
  (let (dum)
       (cond ((eq x '?*) al)
             ((setq dum (assoc x al :test #'eq)) (if (identp (cdr dum) y) al))
	     (t (acons x y al)))))

(defun matchpseqvar (x l al)
  (let (dum)
       (cond ((eq x '@*) al)
             ((setq dum (assoc x al :test #'eq))
              (if (mapand 'identp (cdr dum) l) al))
	     (t (acons x l al)))))

(defun matchplist (x y al)
  (cond ((atom y) nil)
        ((eq 'quote (car y)) (matchpdescriptionplist (cdr x) (cadr y) al))
        ((eq 'listof (car y)) (matchpexpexp (cdr x) (cdr y) al))))

(defun matchpexpexp (l m al)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l) (return (if (null m) al)))
            ((seqvarp (car l)) (return (matchpseqvar (car l) m al)))
            ((null m) (return nil))
            ((setq al (matchpexp (car l) (car m) al)))
            (t (return nil)))))

(defun matchpdescriptionpexp (x y al)
  (cond ((indvarp x) (matchpdescriptionpindvar x y al))
        ((atom x) (if (and (or (numberp x) (stringp x) (characterp x))
                           (equalp x y))
                      al))
	((eq 'quote (car x)) (if (equalp (cadr x) y) al))
	((eq 'listof (car x)) (matchpdescriptionplist (cdr x) y al))))

(defun matchpdescriptionpindvar (x y al)
  (cond ((eq x '?*) al)
        ((assoc x al :test #'eq)
         (if (descriptionp (cdr (assoc x al :test #'eq)) y) al))
	(t (acons x (quotify y) al))))

(defun matchpdescriptionpseqvar (x l al)
  (cond ((eq x '@*) al)
        ((assoc x al :test #'eq)
         (if (mapand 'descriptionp (cdr (assoc x al :test #'eq)) l) al))
	(t (acons x (mapcar 'quotify l) al))))

(defun matchpdescriptionplist (x y al)
  (cond ((not (listp y)) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               (nil)
               (cond ((null l) (return (if (null m) al)))
                     ((seqvarp (car l)) 
                      (return (matchpdescriptionpseqvar (car l) m al)))
                     ((null m) (return nil))
                     ((setq al (matchpdescriptionpexp (car l) (car m) al)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (mgup p q)
;;;    determines whether p and q are unifiable with common variables
;;;    uses single, simple alist
;;;    if success, returns alist
;;;    if failure, returns nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mgup (p q)
 "(MGUP X Y)
  MGUP takes two expressions as arguments and checks whether they are 
  meta-unifiable.  MGUP returns T if the check is successful, and it
  returns NIL otherwise."
  (if (mguexp p q truth) t))

(defmethod mgu (x y)
 "(MGU X Y)
  MGU takes two expressions as arguments and checks whether they are 
  meta-unifiable.  MGU returns the unifier if the check is successful,
  and it returns NIL otherwise."
  (mguexp x y truth))

(defun mguexp (x y al)
  (cond ((eq x y) al)
	((indvarp x) (mguindvar x y al))
	((atom x)
         (cond ((indvarp y) (mguindvar y x al))
               ((atom y) (if (equalp x y) al))
               ((eq 'quote (car y)) (mgudescriptionpexp x (cadr y) al))))
	((eq 'quote (car x)) (mgudescriptionpexp y (cadr x) al))
        ((eq 'listof (car x)) (mgulist x y al))
	(t (cond ((indvarp y) (mguindvar y x al))
		 ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (mguexpexp x y al))))))

(defun mguindvar (x y al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'eq)) (mguexp (cdr dum) y al))
          ((eq x (setq y (mguindval y al))) al)
	  ((and *occurcheck* (mguchkp x y al)) nil)
          (t (acons x y al)))))

(defun mguindval (x al)
  (let (dum)
    (cond ((and (varp x) (setq dum (assoc x al :test #'eq))) (mguindval (cdr dum) al))
          (t x))))

(defun mguseqvar (x l al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'eq)) (mguexpexp (cdr dum) l al))
          ((eq x (car (setq l (mguseqval l al)))) al)
	  ((and *occurcheck* (mguchkplist x l al)) nil)
          (t (acons x l al)))))

(defun mguseqval (l al)
  (let (dum)
    (cond ((and (varp (car l)) (setq dum (assoc (car l) al :test #'eq)))
           (mguseqval (cdr dum) al))
          (t l))))

(defun mguchkp (p q al)
  (cond ((eq p q))
	((indvarp q) (mguchkp p (cdr (assoc q al :test #'eq)) al))
        ((seqvarp q) (mguchkplist p (cdr (assoc q al :test #'eq)) al))
	((atom q) nil)
        ((eq 'quote (car q)) nil)
	(t (mguchkplist p q al))))

(defun mguchkplist (p l al)
  (some #'(lambda (x) (mguchkp p x al)) l))

(defun mgulist (x y al)
  (cond ((indvarp y) (mguindvar y x al))
        ((atom y) nil)
        ((eq 'quote (car y)) (mgudescriptionplist (cdr x) (cadr y) al))
        ((eq 'listof (car y)) (mguexpexp (cdr x) (cdr y) al))))

(defun mguexpexp (l m al)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l)
             (cond ((null m) (return al))
                   ((seqvarp (car m)) (return (mguseqvar (car m) nil al)))
                   (t (return nil))))
            ((seqvarp (car l)) (return (mguseqvar (car l) m al)))
            (t (cond ((null m) (return nil))
                     ((seqvarp (car m)) (return (mguseqvar (car m) l al)))
                     ((setq al (mguexp (car l) (car m) al)))
                     (t (return nil)))))))
            
(defun mgudescriptionpexp (x y al)
  (cond ((indvarp x) (mgudescriptionpindvar x y al))
	((atom x) (if (and (or (numberp x) (stringp x) (characterp x))
                           (equalp x y))
                      al))
	((eq 'quote (car x)) (if (equalp (cadr x) y) al))
	((eq 'listof (car x)) (mgudescriptionplist (cdr x) y al))))

(defun mgudescriptionpindvar (x y al)
  (cond ((assoc x al :test #'eq)
         (mgudescriptionpexp (cdr (assoc x al :test #'eq)) y al))
	(t (acons x (quotify y) al))))

(defun mgudescriptionpseqvar (x l al)
  (cond ((assoc x al :test #'eq)
         (mgudescriptionplist (cdr (assoc x al :test #'eq)) l al))
	(t (acons x (mapcar 'quotify l) al))))

(defun mgudescriptionplist (x y al)
  (cond ((not (listp y)) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               (nil)
               (cond ((null l) (return (if (null m) al)))
                     ((seqvarp (car l)) 
                      (return (mgudescriptionpseqvar (car l) m al)))
                     ((null m) (return nil))
                     ((setq al (mgudescriptionpexp (car l) (car m) al)))
                     (t (return nil)))))))


(defun mgwexp (x y al)
  (cond ((eq x y) al)
	((indvarp x) (mgwindvar x y al))
	((atom x)
         (cond ((indvarp y) (mgwindvar y x al))
               ((equalp x y) al)))
	(t (cond ((atom y) nil)
		 (t (mgwexpexp x y al))))))

(defun mgwindvar (x y al)
  (let (dum)
    (cond ((eq x '?*) al)
          ((setq dum (assoc x al :test #'eq)) (mgwexp (cdr dum) y al))
          ((eq x (setq y (mguindval y al))) al)
          (t (acons x y al)))))

(defun mgwexpexp (l m al)
  (do ((l l (cdr l)) (m m (cdr m)))
      ((null l) (if (null m) al))
      (unless (setq al (mgwexp (car l) (car m) al)) (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utility functions for simple alists
;;;
;;; Format: ((var . val) ... (t . t))
;;;    tail ((t . t)) allows return of empty alist without confusion with nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric plug (x env)
 (:documentation
 "(PLUG X ENV)
  PLUG takes an expression and an alist or environment as arguments, substitutes
  the associated bindings into the expression, and returns the result.  WARNING:
  If the environment is empty, the original expression is returned, i.e. no copy
  is made in this case."))

(defmethod indval (x (al list)) (mguindval x al))

(defmethod plug (x (al list))
  (if (null (cdr al)) x (plugexp x al)))

(defun plugexp (x al)
  (cond ((indvarp x) (plugindvar x al))
	((not (listp x)) x)
	((eq 'quote (car x)) x)
	(t (do ((l x (cdr l)) (nl))
               ((null l) (nreverse nl))
               (if (seqvarp (car l))
                   (return (nreconc nl (plugseqvar (car l) al)))
                   (setq nl (cons (plugexp (car l) al) nl)))))))

(defun plugindvar (x al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'eq)) (plugexp (cdr dum) al))
          (t x))))

(defun plugseqvar (x al)
  (let (dum)
    (cond ((setq dum (assoc x al :test #'eq)) 
           (mapcar #'(lambda (x) (plugexp x al)) (cdr dum)))
	  (t (list x)))))

(defmethod stdize (x)
 "(STDIZE X)
  STDIZE takes an expression as argument and produces an equivalent 
  expression in which all variables have been given new names."
  (let (alist)
    (stdizeexp x)))

(defun stdizeexp (x)
  (cond ((varp x) (stdizevar x))
	((atom x) x)
	((eq 'quote (car x)) x)
	(t (mapcar 'stdizeexp x))))

(defun stdizevar (x)
  (cond ((eq x '?*) '?*)
        ((eq x '@*) '@*)
        ((cdr (assoc x alist :test #'eq)))
        ((indvarp x) (cdar (setq alist (acons x (newindvar) alist))))
        (t (cdar (setq alist (acons x (newseqvar) alist))))))

(defmethod plugstd (x alist)
  (if (null (cdr alist)) x (plugstdize x)))

(defun plugstdize (x)
  (cond ((indvarp x) (plugstdindvar x))
	((atom x) x)
	((eq 'quote (car x)) x)
	(t (do ((l x (cdr l)) (nl))
               ((null l) (nreverse nl))
               (if (seqvarp (car l))
                   (return (nreconc nl (plugstdseqvar (car l))))
                   (setq nl (cons (plugstdize (car l)) nl)))))))

(defun plugstdindvar (x)
  (cond ((eq x '?*) '?*)
        ((cdr (assoc x alist :test #'eq)))
        (t (cdar (setq alist (acons x (newindvar) alist))))))

(defun plugstdseqvar (x)
  (cond ((eq x '@*) (list '@*))
        ((cdr (assoc x alist :test #'eq)))
        (t (cdar (setq alist (acons x (list (newseqvar)) alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (identify x al y bl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod identifyp (x y)
 "(IDENTIFYP X Y)
  IDENTIFYP takes two expressions as arguments and checks whether they are 
  meta-equivalent. IDENTIFYP returns T if the check is successful, and it
  returns NIL otherwise."
 (let ((al (environment)))
   (identify x al y al)))

(defun identify (x al y bl)
  (cond ((and (eq x y) (eq al bl)))
        ((indvarp x) (identifyindvar x al y bl))
	((atom x)
	 (cond ((indvarp y) (identifyindvar y bl x al))
	       ((atom y) (equalp x y))
               ((eq 'quote (car y)) (descriptionize x al (cadr y)))))
	((eq 'quote (car x)) (descriptionize y bl (cadr x)))
        ((eq 'listof (car x)) (identifylist x al y bl))
	(t (cond ((indvarp y) (identifyindvar y bl x al))
		 ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (identifyexpexp x al y bl))))))

(defun identifyindvar (x al y bl)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (identify (cadr dum) (cddr dum) y bl))
          ((and (multiple-value-setq (y bl) (unifyindval y bl))
                (eq x y) (eq al bl))))))

(defun identifyseqvar (x al l bl)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (identifyexpexp (cadr dum) (cddr dum) l bl))
          ((and (multiple-value-setq (l bl) (unifyseqval l bl))
                (eq x (car l)) (eq al bl))))))

(defun identifylist (x al y bl)
  (cond ((indvarp y) (identifyindvar y bl x al))
        ((atom y) nil)
        ((eq 'quote (car y)) (descriptionizelist (cdr x) al (cadr y)))
        ((eq 'listof (car y)) (identifyexpexp (cdr x) al (cdr y) bl))))

(defun identifyexpexp (l al m bl)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l)
             (cond ((null m) (return t))
                   ((seqvarp (car m)) (identifyseqvar (car m) bl nil al))
                   (t (return nil))))
            ((seqvarp (car l)) (return (identifyseqvar (car l) al m bl)))
            ((null m) (return nil))
            ((seqvarp (car m)) (return (identifyseqvar (car m) bl l al)))
            ((identify (car l) al (car m) bl))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (descriptionize x al y bl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun descriptionize (x al y)
  (cond ((indvarp x) (descriptionizeindvar x al y))
	((atom x) (if (or (numberp x) (stringp x) (characterp x)) (equalp x y)))
	((eq 'quote (car x)) (equalp (cadr x) y))
	((eq 'listof (car x)) (descriptionizelist (cdr x) al y))))

(defun descriptionizeindvar (x al y)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (descriptionize (cadr dum) (cddr dum) y)))))

(defun descriptionizeseqvar (x al l)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (descriptionizelist (cadr dum) (cddr dum) l)))))

(defun descriptionizelist (x al y)
  (cond ((not (listp y)) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               (nil)
               (cond ((null l) (return (null m)))
                     ((seqvarp (car l)) 
                      (return (descriptionizeseqvar (car l) al m)))
                     ((null m) (return nil))
                     ((descriptionize (car l) al (car m)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (matchify x al y bl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matchifyp (x y)
 "(MATCHIFYP X Y)
  MATCHIFYP takes two expressions as arguments and checks whether they are 
  meta-unifiable.  MATCHIFYP treats the variables in the second expression
  as constants. MATCHIFYP returns T if the check is successful, and it
  returns NIL otherwise."
  (if (matchify x (environment) y (environment)) t))

(defun matchify (x al y bl)
  (let ((alist truth))
    (cond ((matchifyexp x al y bl) alist)
	  (t (backup alist)))))

(defun matchifyexp (x al y bl)
  (cond ((and (eq x y) (eq al bl)))
        ((indvarp x) (matchifyindvar x al y bl))
	((atom x) (cond ((indvarp y) (identifyindvar y bl x al))
                        ((atom y) (equalp x y))
                        ((eq 'quote (car y)) (descriptionize x al (cadr y)))))
	((eq 'quote (car x)) (descriptionize y bl (cadr x)))
        ((eq 'listof (car x)) (matchifylist x al y bl))
	(t (cond ((indvarp y) (identifyindvar y bl x al))
		 ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (matchifyexpexp x al y bl))))))

(defun matchifyindvar (x al y bl)
  (let (dum)
    (cond ((eq x '?*))
          ((cddr (setq dum (getbdg x al)))
           (identify (cadr dum) (cddr dum) y bl))
          ((and (multiple-value-setq (y bl) (unifyindval y bl))
                (eq x y) (eq al bl)))
	  ((and *occurcheck* (unifychkp x al y bl)) nil)
	  ((null dum) (setnew x al y bl))
	  (t (setold dum y bl)))))

(defun matchifyindval (x al y bl)
  (let (dum)
    (cond ((cddr (setq dum (getbdg y bl)))
	   (matchifyexp x al (cadr dum) (cddr dum))))))

(defun matchifyseqvar (x al l bl)
  (let (dum)
    (cond ((eq x '@*))
          ((cddr (setq dum (getbdg x al)))
           (identifyexpexp (cadr dum) (cddr dum) l bl))
          ((and (multiple-value-setq (l bl) (unifyseqval l bl))
                (eq x (car l)) (eq al bl)))
	  ((and *occurcheck* (unifychkplist x al l bl)) nil)
	  ((null dum) (setnew x al l bl))
	  (t (setold dum l bl)))))

(defun matchifyseqval (x al l bl)
  (let (dum)
    (cond ((cddr (setq dum (getbdg (car l) bl)))
	   (matchifyexpexp x al (cadr dum) (cddr dum))))))

(defun matchifylist (x al y bl)
  (cond ((indvarp y) (matchifyindval x al y bl))
        ((atom y) nil)
        ((eq 'quote (car y)) (matchifydescriptionplist (cdr x) al (cadr y)))
        ((eq 'listof (car y)) (matchifyexpexp (cdr x) al (cdr y) bl))))

(defun matchifyexpexp (l al m bl)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l)
             (cond ((null m) (return t))
                   ((seqvarp (car m)) (identifyseqvar (car m) bl nil al))
                   (t (return nil))))
            ((seqvarp (car l)) (return (matchifyseqvar (car l) al m bl)))
            ((null m) (return nil))
            ((seqvarp (car m)) (return (matchifyseqval l al (car m) bl)))
            ((matchifyexp (car l) al (car m) bl))
            (t (return nil)))))

(defun matchifydescriptionpexp (x al y)
  (cond ((indvarp x) (matchifydescriptionpindvar x al y))
	((atom x) (if (or (numberp x) (stringp x) (characterp x)) (equalp x y)))
	((eq 'quote (car x)) (equalp (cadr x) y))
	((eq 'listof (car x)) (matchifydescriptionplist (cdr x) al y))))

(defun matchifydescriptionpindvar (x al y)
  (let (dum)
    (cond ((eq x '?*))
          ((cddr (setq dum (getbdg x al)))
	   (matchifydescriptionpexp (cadr dum) (cddr dum) y))
	  ((null dum) (setnew x al (quotify y) al))
	  (t (setold dum (quotify y) al)))))

(defun matchifydescriptionpseqvar (x al l)
  (let (dum)
    (cond ((eq x '@*))
          ((cddr (setq dum (getbdg x al)))
	   (matchifydescriptionplist (cadr dum) (cddr dum) l))
	  ((null dum) (setnew x al (mapcar 'quotify l) al))
	  (t (setold dum (mapcar 'quotify l) al)))))

(defun matchifydescriptionplist (x al y)
  (cond ((not (listp y)) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               (nil)
               (cond ((null l) (return (null m)))
                     ((seqvarp (car l)) 
                      (return (matchifydescriptionpseqvar (car l) al m)))
                     ((null m) (return nil))
                     ((matchifydescriptionpexp (car l) al (car m)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (unify x al y bl)
;;;    determines whether x with binder al and y with binder bl are unifiable
;;;    if successful, returns a list of new bindings 
;;;    if successful, sets variables in binders
;;;    if successful, can have side effects in binders other than al and bl
;;;    if fails, returns nil and has no side effects on binders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *unifications* 0
 "The value of *UNIFICATIONS* is the number of attempted unifications in the
  most recent proof effort.")

(defmethod unifyp (x y) 
 "(UNIFYP X Y)
  UNIFYP takes two expressions as arguments and checks whether they are 
  meta-unifiable.  UNIFYP differs from MGUP in that the variables in one
  expression are treated as distinct from the variables in the other
  expression.  UNIFYP returns T if the check is successful, and it
  returns NIL otherwise."
  (if (unify x (environment) y (environment)) t))

(defun unify (x al y bl)
  (let ((alist truth))
    (setq *unifications* (1+ *unifications*))
    (cond ((unifyexp x al y bl) alist)
	  (t (backup alist)))))

(defun unifyexp (x al y bl)
  (cond ((and (eq x y) (eq al bl)))
        ((indvarp x) (unifyindvar x al y bl))
	((atom x)
	 (cond ((indvarp y) (unifyindvar y bl x al))
	       ((atom y) (equalp x y))
               ((eq 'quote (car y)) (unifydescriptionpexp x al (cadr y)))))
	((eq 'quote (car x)) (unifydescriptionpexp y bl (cadr x)))
        ((eq 'listof (car x)) (unifylist x al y bl))
	(t (cond ((indvarp y) (unifyindvdr x al y bl))
		 ((atom y) nil)
		 ((eq 'quote (car y)) nil)
                 ((eq 'listof (car y)) nil)
		 (t (unifyexpexp x al y bl))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transitively closing here okay since these bindings will get backed up
;;; any bindings they are dependent on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unifyindvar (x al y bl)
  (let (dum)
    (cond ((and (eq x y) (eq al bl)))
          ((eq x '?*))
          ((eq y '?*))
          ((cddr (setq dum (getbdg x al)))
	   (unifyexp (cadr dum) (cddr dum) y bl))
          ((and (multiple-value-setq (y bl) (unifyindval y bl))
                (eq x y) (eq al bl)))
	  ((and *occurcheck* (unifychkp x al y bl)) nil)
	  ((null dum) (setnew x al y bl))
	  (t (setold dum y bl)))))

(defun unifyindvdr (x al y bl)
  (let (dum)
    (cond ((and (eq x y) (eq al bl)))
          ((eq x '?*))
          ((eq y '?*))
          ((cddr (setq dum (getbdg y bl)))
	   (unifyexp x al (cadr dum) (cddr dum)))
          ((and (multiple-value-setq (x al) (unifyindval x al))
                (eq x y) (eq al bl)))
	  ((and *occurcheck* (unifychkp y bl x al)) nil)
	  ((null dum) (setnew y bl x al))
	  (t (setold dum x al)))))

(defun unifyindval (y bl)
  (let (dum)
    (cond ((and (indvarp y) (cddr (setq dum (getbdg y bl))))
           (unifyindval (cadr dum) (cddr dum)))
          (t (values y bl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transitively closing here okay since these bindings will get backed up
;;; any bindings they are dependent on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unifyseqvar (x al l bl)
  (let (dum)
    (cond ((and (eq x (car l)) (eq al bl)))
          ((eq x '@*))
          ((eq l '@*))
          ((cddr (setq dum (getbdg x al)))
           (unifyexpexp (cadr dum) (cddr dum) l bl))
          ((and (multiple-value-setq (l bl) (unifyseqval l bl))
                (eq x (car l)) (eq al bl)))
	  ((and *occurcheck* (unifychkplist x al l bl)) nil)
	  ((null dum) (setnew x al l bl))
	  (t (setold dum l bl)))))

(defun unifyseqval (l al)
  (let (dum)
    (cond ((and (seqvarp (car l)) (cddr (setq dum (getbdg (car l) al))))
           (unifyseqval (cadr dum) (cddr dum)))
          (t (values l al)))))

(defun unifychkp (p al q bl)
  (cond ((and (eq p q) (eq al bl)))
	((indvarp q) (if (cddr (setq q (getbdg q bl)))
                         (unifychkp p al (cadr q) (cddr q))))
        ((seqvarp q) (if (cddr (setq q (getbdg q bl)))
                         (unifychkplist p al (cadr q) (cddr q))))
	((atom q) nil)
        ((eq 'quote (car q)) nil)
	(t (unifychkplist p al q bl))))

(defun unifychkplist (p al l bl)
  (some #'(lambda (x) (unifychkp p al x bl)) l))

(defun unifylist (x al y bl)
  (cond ((indvarp y) (unifyindvar y bl x al))
        ((atom y) nil)
        ((eq 'quote (car y)) (unifydescriptionplist (cdr x) al (cadr y)))
        ((eq 'listof (car y)) (unifyexpexp (cdr x) al (cdr y) bl))))

(defun unifyexpexp (l al m bl)
  (do ((l l (cdr l)) (m m (cdr m)))
      (nil)
      (cond ((null l)
             (cond ((null m) (return t))
                   ((seqvarp (car m)) (return (unifyseqvar (car m) bl nil al)))
                   (t (return nil))))
            ((seqvarp (car l)) (return (unifyseqvar (car l) al m bl)))
            ((null m) (return nil))
            ((seqvarp (car m)) (return (unifyseqvar (car m) bl l al)))
            ((unifyexp (car l) al (car m) bl))
            (t (return nil)))))

(defun unifydescriptionpexp (x al y)
  (cond ((indvarp x) (unifydescriptionpindvar x al y))
	((atom x) (if (or (numberp x) (stringp x) (characterp x)) (equalp x y)))
	((eq 'quote (car x)) (equalp (cadr x) y))
	((eq 'listof (car x)) (unifydescriptionplist (cdr x) al y))))

(defun unifydescriptionpindvar (x al y)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (unifydescriptionpexp (cadr dum) (cddr dum) y))
	  ((null dum) (setnew x al (quotify y) al))
	  (t (setold dum (quotify y) al)))))

(defun unifydescriptionpseqvar (x al l)
  (let (dum)
    (cond ((cddr (setq dum (getbdg x al)))
	   (unifydescriptionplist (cadr dum) (cddr dum) l))
	  ((null dum) (setnew x al (mapcar 'quotify l) al))
	  (t (setold dum (mapcar 'quotify l) al)))))

(defun unifydescriptionplist (x al y)
  (cond ((not (listp y)) nil)
        (t (do ((l x (cdr l)) (m y (cdr m)))
               (nil)
               (cond ((null l) (return (null m)))
                     ((seqvarp (car l)) 
                      (return (unifydescriptionpseqvar (car l) al m)))
                     ((null m) (return nil))
                     ((unifydescriptionpexp (car l) al (car m)))
                     (t (return nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ultility functions for environments
;;;
;;; environment has one component:   ((var val . env) ...)
;;;    if no env in cddr, then cadr, if exists, is the standardized variable
;;;
;;; (backup ol)
;;;    undoes the bindings in ol by rplacding those entries to nil.
;;;
;;; (plug x al)
;;;    copies x substituting bindings for variables from al.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass environment ()
  ((alist :accessor alist :initform nil)))

(defmethod environment ()
 "(ENVIRONMENT)
  The subroutine ENVIRONMENT takes no arguments and returns a new variable
  binding environment."
  (make-instance 'environment))

(defmethod alist ((env list)) (cdr env))

(defmethod (setf alist) (al (env list)) (rplacd env al))

(defmethod variables ((env environment))
 "(VARIABLES ENV)
  VARIABLES takes an environment as argument and returns a list of variables
  with bindings in that environment."
  (do ((l (alist env) (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (cdar l) (setq nl (cons (caar l) nl)))))

(defmethod bind (x (e1 environment) y e2)
 "(BIND X E1 Y E2)
  BIND takes a variable, an environment, an expression, and another environment
  as arguments.  It binds the specified variable in its specified environment
  to have as value the sepcified expression with variable values defined in the
  second specified environment."
  (let (dum)
    (cond ((setq dum (assoc x (alist e1) :test #'eq)) (setoldbdg dum y e2))
          (t (setnewbdg x e1 y e2)))))

(defmethod unbind (x (env environment))
 "(UNBIND X ENV)
  UNBIND takes a variable and an environment as arguments and removes the
  binding of the specified variable from the specified environment."
  (let (dum)
    (if (setq dum (getbdg x env)) (rplacd dum nil))))

(defmethod binding (x (env environment))
 "(BINDING X ENV)
  BINDING takes a variable and an environmen as arguments ans returns as values
  the expression associated with the variable and the environment in which its
  variables are bound.  PLUG is a recursive version of BINDING."
  (let (dum)
    (if (setq dum (assoc x (alist env) :test #'eq))
        (values (cadr dum) (cddr dum)))))

(defun getbdg (x env)
  (assoc x (alist env) :test #'eq))

(defun setold (x y bl)
  (rplacd x (cons y bl))
  (setq alist (cons x alist)))

(defun setnew (x al y bl)
  (setq x (list* x y bl))
  (setf (alist al) (cons x (alist al)))
  (setq alist (cons x alist)))

(defun setoldbdg (v y bl)
  (rplacd v (list* y bl))
  y)

(defun setnewbdg (x al y bl)
  (setf (alist al) (cons (list* x y bl) (alist al)))
  y)

(defun backup (bl)
  (do ((l bl (cdr l)))
      ((null (cdr l)) nil)
      (rplacd (car l) nil)))


(defun subolp (l1 l2)
  (do ((l l1 (cdr l)))
      ((null (cdr l)) t)
      (if (not (member (car l) l2 :test #'eq)) (return nil))))


(defmethod indval (x (env environment)) (unifyindval x env))

(defmethod plug (x (alist environment))
  (if (null (alist alist)) x (plugstdexp x alist)))

(defun plugstdexp (x al)
  (cond ((indvarp x) (plugstdexpindvar x al))
	((atom x) x)
	((eq 'quote (car x)) x)
	(t (plugstdexpcdr x al))))

(defun plugstdexpcdr (x al)
  (do ((l x (cdr l)) (nl))
      ((null l) (nreverse nl))
      (if (seqvarp (car l))
          (return (nreconc nl (plugstdexpseqvar (car l) al)))
          (setq nl (cons (plugstdexp (car l) al) nl)))))

(defun plugstdexpindvar (x al)
  (let (dum)
    (cond ((eq '?* x) '?*)
          ((setq dum (getbdg x al))
           (cond ((cddr dum) (plugstdexp (cadr dum) (cddr dum)))
                 ((eq alist al) x)
                 ((cdr dum) (cadr dum))
                 (t (setoldbdg dum (newindvar) alist))))
          ((eq alist al) x)
	  (t (setnewbdg x al (newindvar) alist)))))

(defun plugstdexpseqvar (x al)
  (let (dum)
    (cond ((eq '@* x) (list x))
          ((setq dum (getbdg x al))
           (cond ((cddr dum) (plugstdexpcdr (cadr dum) (cddr dum)))
                 ((eq alist al) (list x))
                 ((cdr dum) (cadr dum))
                 (t (setoldbdg dum (list (newseqvar)) nil))))
          ((eq alist al) (list x))
          (t (setnewbdg x al (list (newseqvar)) nil)))))

(defun groundplugstdexp (x al)
  (cond ((indvarp x) (if (cddr (setq x (getbdg x al)))
                         (groundplugstdexp (cadr x) (cddr x))))
	((atom x) x)
	((eq 'quote (car x)) x)
	(t (groundplugstdcdr x al))))

(defun groundplugstdcdr (x al)
  (do ((l x (cdr l)) (ans) (nl))
      ((null l) (nreverse nl))
      (cond ((seqvarp (car l))
             (return (if (cddr (setq ans (getbdg (car l) al)))
                         (nreconc nl (groundplugstdcdr (cadr ans) (cddr ans))))))
            ((setq ans (groundplugstdexp (car l) al)) (setq nl (cons ans nl)))
            (t (return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unival
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod unival (x (al list))
  (let (dum)
    (cond ((not (varp x)) x)
          ((setq dum (assoc x al :test #'eq)) (unival (cdr dum) al))
          (t x))))

(defmethod unival (x (al environment))
  (let (dum)
    (cond ((not (indvarp x)) x)
          ((and (setq dum (getbdg x al)) (cdr dum))
           (unival (cadr dum) (cddr dum)))
          (t x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
