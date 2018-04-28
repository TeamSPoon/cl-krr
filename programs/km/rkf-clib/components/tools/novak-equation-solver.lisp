(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)
; eqnfix.lsp          Gordon S. Novak Jr.            ; 23 Jan 06

; Copyright (c) 2006 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; extracts and stubs from GLisp

(defmacro glispobjects (&rest stuff) nil)
(defmacro gldefun (&rest stuff) nil)
(defmacro glispconstantflg    (x) `(get ,x 'glispconstantflg));Keep
(defmacro glispconstantval    (x) `(get ,x 'glispconstantval));Keep


(defvar *CONN-USER-LAWS* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'while)
    (defmacro while (test &rest forms)
      `(loop (unless ,test (return)) ,@forms))))

(defmacro subset (fn lst)
  (let ((x (gensym)))
    `(mapcan #'(lambda (,x) (if (funcall ,fn ,x) (cons ,x nil))) ,lst)))

(defmacro op  (x) `(car ,x))
(defmacro lhs (x) `(cadr ,x))
(defmacro rhs (x) `(caddr ,x))
(defmacro eqop (x y) `(and (consp ,x) (eq (car ,x) ,y)))

; 11 Nov 91; 18 Nov 91; 28 Feb 94; 28 Apr 94
; Find variables in an expression
(defun glvarsin (x) (glvarsinb x nil))
(defun glvarsinb (x vars)
  (if (null x) vars
      (if (symbolp x)
	  (if (member x vars) vars (cons x vars))
	  (if (and (consp x)
		   (not (member (car x) '(quote function))))
	      (dolist (z (cdr x) vars) (setq vars (glvarsinb z vars)))
	      vars) )))

; edited:  3-JUN-82
; See if X occurs in STR, using EQ. 
(defun gloccurs (x str)
  (cond ((eq x str) t)
	((atom str) nil)
	(t (or (gloccurs x (car str))
	       (gloccurs x (cdr str))))))

; 4-May-89; 07 Feb 91; 18 Nov 91; 25 Feb 92; 21 Apr 92
; Store rewriting patterns for use in optimization etc.
; Note that order of pattern definitions must be maintained;
; otherwise, some patterns could cause an infinite loop.
(defun gldefpatterns (patwd l)
  (dolist (pat l)
    (if (or (not (eq patwd 'glpatterns)) (glpattest pat))
	(unless (member pat (get (caar pat) patwd) :test #'equal)
	  (setf (get (caar pat) patwd)
		(nconc (get (caar pat) patwd) (list pat))))) ) )

; 01 Nov 89; 28 Jan 92; 26 Apr 94; 28 Apr 94; 29 Apr 94
(defun glnumberp (x)
  (let (v)
    (or (numberp x)
        (and (symbolp x) (constantp x) (numberp (eval x)))
	(and (consp x)
	     (member (car x) '(+ - * / 1+ 1- sin cos tan sqrt cbrt expt))
	     (every #'glnumberp (rest x)))
        (and (glconstantp x)
	     (setq v (glconstval x))
	     (or (numberp v)
		 (and (consp v) (eq (car v) 'q)
		      (consp (cdr v))
		      (numberp (cadr v))
		      (consp (cddr v)))))) ))

; test all physics problems
(defun testall ()
  (dolist (prob *probsents*)
    (dolist (sent (cdr prob))
      (print sent)
      (print (phys sent)) ) ) )

; modified version for stand-alone use with units
; 27 Mar 89; 06 Jun 90; 20 May 93; 03 Jan 95; 18 Apr 03
(defun glerror (fn msgstr &rest args)
  (format t "error detected by ~A~%" fn)
  (apply #'format (cons t (cons msgstr args)))
  (terpri) )

; modified version for stand-alone use with units
; 15-Feb-89; 05 Apr 90; 12 Sep 91; 18 Sep 91; 19 Sep 91; 17 Jan 92; 03 Nov 92
; 10 Nov 95; 26 Jul 96; 18 Apr 03
; Get the value of a compile-time constant 
(defun glconstval (x);Keep
  (cond ((or (null x)
             (eq x t)
             (numberp x)
             (characterp x)
             (stringp x))
          x)
        ((and (symbolp x) (constantp x)) (eval x))
        ((quotep x) (cadr x))
        ((and (symbolp x)
              (glispconstantflg x))
          (glispconstantval x))
        (t (error "NOMSG"))))

; 18 Apr 03
(defun glunitexpansion (u);Keep
  (let ((flat (glunitexpand u)))
    (list '/ (cons '* (car flat)) (cons '* (cadr flat))) ))
; patmatch.lsp           Gordon S. Novak Jr.          ; 09 Aug 06

; Copyright (c) 2007  Gordon S. Novak Jr. and The University of Texas at Austin

; 03 Feb 03; 07 Jan 05; 31 Jan 05

; Pattern Matching and Transformation

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; These programs transform an input according to a pair of patterns:
; If the input matches the first (input) pattern,
; it is transformed according to the second (output) pattern.
; Variables in patterns are prefixed with a '?' character.
; Example:  ( (+ ?x (- ?x))  0 )
; This pair of patterns specifies that the sum of something (?x)
; and its negative should be transformed to 0:
; (transf '( (+ ?x (- ?x))  0 ) '(+ (sin x) (- (sin x))))  =>  0
; Note that "dot notation" can match a trailing list of items:
; (progn . ?s) will bind ?s to the list of statements in the progn.
; Transforms can have two additional components:
;   1. a test.  This test must be satisfied for the transformation
;      to be performed:
;      ( (* ?n (* ?m ?x)) (* (* ?n ?m) ?x) (and (numberp ?n) (numberp ?m)) )
;   2. additional variables.  Each such variable is a list,
;      (var value) where the value may be computed from the other
;      variable values:
;      ( (* ?n (* ?m ?x)) (* ?z ?x) (and (numberp ?n) (numberp ?m))
;                                   ( (?z (* ?n ?m)) ) )
;      will transform (* 3 (* 2 w)) into (* 6 W).

; Matching and substitution are inverses [assuming appropriate arguments],
;    as shown by the identities:
;    (sublis (match pattern instance) pattern)  =  instance
;    (match pattern (sublis bindings pattern))  =  bindings
; (matchid1 '(+ ?x ?y) '(+ a b))
(defun matchid1 (pattern instance)
  (equal (sublis (match pattern instance) pattern) instance))
; (matchid2 '(+ ?x ?y) '((?x . a) (?y . b)))
(defun matchid2 (pattern bindings)
  (set-equal (match pattern (sublis bindings pattern))
	     (cons '(t . t) bindings)))

; Match a pattern against an input.  Returns bindings, or nil if failure.
; This version suggested by Michael Bogomolny.
; (match '(- (+ ?x ?y) (+ ?z ?y))
;        '(- (+ (age tom) (age mary)) (+ (age bill) (age mary))))
;   = ((?Z AGE BILL) (?Y AGE MARY) (?X AGE TOM))
; '((t . t)) is a no-op binding to initialize bindings to non-nil.
(defun match (pat inp)  (matchb pat inp '((t . t))))
(defun matchb (pat inp bindings)
  (and bindings
       (if (consp pat)
	   (and (consp inp)
		(matchb (cdr pat) 
			(cdr inp)
			(matchb (car pat)
				(car inp) bindings)))
	   (if (varp pat)
	       (let ((binding (assoc pat bindings)))
		 (if binding
		     (and (equal inp (cdr binding))
			  bindings)
		     (cons (cons pat inp) bindings)))
	       (and (eql pat inp) bindings)) ) ) )

; Test for variables, indicated by ? prefix, e.g. ?x
(defun varp (v) (and (symbolp v) (char= #\? (char (symbol-name v) 0))) )

; Transform an input given a rewrite rule (pattern output test vars)
;   where:  pattern = input pattern, e.g.  (+ ?x ?n)
;           output  = output pattern, e.g. (+ ?n ?x)
;           test    = predicate, e.g.      (numberp ?n)
;           vars    = new variables, e.g.  ((?z (append ?x ?y))
;                      ((var value) ...)    (?v (gentemp "V")))
; test and vars sections are optional.
; returns transformed input or 'match-failure
; (transf '((- (+ ?x ?y) (+ ?z ?y))
;              (- ?x ?z))
;         '(- (+ (age tom) (age mary)) (+ (age bill) (age mary))))
;        = (- (AGE TOM) (AGE BILL))
(defun transf (rule input)
  (let ((bindings (match (first rule) input))
	(test (third rule)))
    (if (and bindings
	     (or (null test)
		 (eval (sublisq	bindings test))))
	(progn
	  (dolist (var (fourth rule))
	    (push (cons (car var)
			(eval (sublisq bindings (cadr var))))
		  bindings) )	  
	  (sublis bindings (second rule)))
        'match-failure) ))

; sublis, quoting the second part of bindings so they can be evaluated
(defun sublisq (bindings form)
  (sublis (mapcar #'(lambda (x) (cons (car x) (kwote (cdr x))))
		  bindings)
	  form) )

; quote something if it isn't constant.
(defun kwote (x)
  (if (constantp x)
      x
      (if (and (consp x) (eq (car x) 'unquote))
	  (cadr x)
	  (list 'quote x) ) ) )

(defun set-equal (x y) (and (subsetp x y :test #'equal)
			    (subsetp y x :test #'equal)))
; mathfns.lsp             Gordon S. Novak Jr.         ; 13 Feb 07

; Copyright (c) 2007 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; 29 Aug 93; 14 Dec 05; 28 Dec 05; 29 Dec 05; 29 Jan 07

; 06 May 93
; Try to solve function fn for value that produces desired
; value val, starting from initial value init.
; (regula-falsi #'square 2.0 1.0 1.0e-14) ; = 1.4142135623730954
; (regula-falsi #'(lambda (x) (- x (sin x))) 0.5)
(defun regula-falsi (fn val &optional (init 1.0) (epsilon 1.0e-8))
  (let ((delta (/ (abs init) 100.0)) (x init) fx slope)
    (while (> (abs (setq fx (- (funcall fn x) val)))
	      epsilon)
      (setq slope (/ (- (- (funcall fn (+ x delta)) val) fx) delta))
      (setq x (- x (/ fx slope))) )
    x))

; 28 Dec 05; 29 Dec 05
; Try to solve formula for value of var that produces value 0,
; starting from initial value init.
; (regula-falsib '(- (expt x 2) 2) 'x) ; = 1.414213562
(defun regula-falsib (formula var
		      &optional (epsilon 1.0e-8) (init 1.0) (maxn 100))
  (let (delta (x init) xd fx slope (alist (list (cons var init))) (last 3))
    (while (and (> last 0) (> maxn 0))
      (setq fx (safe-eval formula alist))
      (if (<= (abs fx) epsilon) (setq last (1- last)))
      (setq maxn (1- maxn))
      (setq delta (/ (abs x) 100.0))
      (setq xd (+ x delta))
      (setf (cdar alist) xd)
      (setq slope (/ (- (safe-eval formula alist) fx) delta))
      (setq x (- x (/ fx slope)))
      (setf (cdar alist) x) )
    (if (> maxn 0) x) ))

; 29 Aug 93
; Trapezoidal integration
(defun trapint (fn from to step)
  (let (sum (x from) last f)
    (while (<= x to)
      (setq f (funcall fn x))
      (if sum
	  (incf sum (* f step))
	  (setq sum (* 0.5 (* f step))) )
      (incf x step) )
    (decf sum (* 0.5 (* f step)))
    sum ))

; 13 Feb 07
; correlation coefficient: from Wikipedia
(gldefun correl ((xl (listof number)) (yl (listof number)))
  (let ((n (length xl)) (sumsqx 0) (sumsqy 0) (sumcoproduct 0)
        (meanx (pop xl)) (meany (pop yl)) sweep deltax deltay i
        popsdx popsdy covxy correlation)
    (for j in (1- n)
      (i = j + 2)
      (sweep = (i - 1.0) / i)
      (deltax = (pop xl) - meanx)
      (deltay = (pop yl) - meany)
      (sumsqx += deltax * deltax * sweep)
      (sumsqy += deltay * deltay * sweep)
      (sumcoproduct += deltax * deltay * sweep)
      (meanx += deltax / i)
      (meany += deltay / i ) )
    (popsdx = (sqrt sumsqx / N ))
    (popsdy = (sqrt sumsqy / N ))
    (covxy = sumcoproduct / N)
    (correlation = covxy / (popsdx * popsdy))
    correlation))


; mathsimp.lsp                  Gordon S. Novak Jr.         ; 30 Sep 06

; Copyright (c) 2007 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; 23 Dec 94; 05 Jan 95; 02 Jan 97; 28 Feb 02; 27 Apr 04; 07 Jun 04; 08 Jun 04
; 30 Jun 06

(defvar *gleqnsmatch-trace* nil)

; 26 Apr 94; 07 Oct 94; 27 Apr 04; 07 Jun 04
; eval a form that may include constants with units.
; cf. trans in ~/autop/patm.lsp
(defun gleqns-trans (x patwd)
  (let (xp tail)
    (if (and (consp x)            ; if certain fns are applied to constants
	     (not (member (car x) '(quote function))))
	(progn
	  (setq tail (gleqns-transl (rest x) patwd))  ; translate args first
	  (unless (eq tail (rest x)) (setq x (cons (first x) tail)))
	  (if (and (member (first x) '(+ - * / expt sqrt sin cos tan
					 asin acos atan 1+ 1-
					 = /= > >= < <=))
		   (every #'glnumberp (rest x)))
	      (if (and (eq (first x) '/)
		       (cddr x)
		       (zerop (glqn (caddr x))))
		  x
		  (glevalunits x))
	      (progn (setq xp (gleqnsmatch x patwd))   ; was glptmatch
		     (if (eq x xp)               ; if it changed, hit it again
			 x
		         (gleqns-trans xp patwd))) ) )
        x) ))

; 26 Jan 93; 26 Apr 94
; Translate a list of arguments.  Avoids conses if no changes are made.
; cf. pttransl in patm.lsp
(defun gleqns-transl (l patwd)
  (let (new tail)
    (when l
      (setq new (gleqns-trans (first l) patwd))
      (setq tail (gleqns-transl (rest l) patwd))
      (if (or (not (eq tail (rest l)))
	      (not (eq new (first l))))
	  (cons new tail)
	  l)) ))

; 26 Apr 94; 27 Apr 94; 28 Apr 94; 29 Apr 94; 07 Oct 94; 28 Feb 02; 30 Jun 06
; Perform an operation on constants, coercing units as necessary
; e.g. convert feet to meters.  Args may be numbers or '(Q n unit) constants.
; cf. glcoerceunits
(defun glevalunits (form)
  (let (op lhs lhsu rhs rhsu exponent newunit factor expunit)
    (setq op (car form))
    (setq lhs (glqn (cadr form)))
    (setq lhsu (glqunit (cadr form)))
    (when (cddr form)
      (setq rhs (glqn (caddr form)))
      (setq rhsu (glqunit (caddr form))) )

    (case op
      ((* /) (glqres (funcall op lhs rhs)
		     (if (eq op '*)
			 (glsimplifyunit (glmultunits lhsu rhsu))
		         (glsimplifyunit (gldivunits lhsu rhsu)))))
      ((\:= + - < <= = == <> != >= >)
       (if (and (eq op '-) (null (cddr form)))
	   (glqres (funcall op lhs) lhsu)
	   (progn (if (or (equal lhsu rhsu)
			  (zerop lhs)           ; got to be good lookin'
			  (zerop rhs))          ; 'cause it's so hard to see
		      (setq factor 1)
		      (or (setq factor (glconvertunit rhsu lhsu))
			  (progn (glerror 'glevalunits
					  "Cannot apply op ~A to ~A and ~A"
					  op lhsu rhsu)
				 (setq factor 1))))
		  (glqres (funcall op lhs (* factor rhs)) lhsu) )))
      ((^ expt)
       (if (and (integerp rhs)
		(< (setq exponent (abs rhs)) 6)
		(eq rhsu 'unity))
	   (progn
	     (setq expunit 'unity)
	     (while (> exponent 0)
	       (decf exponent)
	       (setq expunit
		     (if (minusp rhs)
			 (glsimplifyunit (gldivunits expunit lhsu))
		         (glsimplifyunit (glmultunits expunit lhsu)))))
	     (glqres (funcall op lhs rhs) expunit))
	 form))
      (sqrt (if (setq newunit (glsqrtunit lhsu nil t))
		(glqres (funcall op lhs) newunit)
	        form))
      (cbrt (if (setq newunit (glcbrtunit lhsu nil t))
		(glqres (funcall op lhs) newunit)
	        form))
      ((sin cos tan) (if (setq factor (glconvertunit lhsu 'radian))
			 (funcall op (* factor lhs))
		         form))
      (atan (if (and rhsu lhsu (setq factor (glconvertunit rhsu lhsu)))
		(glqres (atan lhs (* factor rhs)) 'radian)
	        form))		
      (t (if (every #'glevalnumberp (cdr form))
	     (eval form)
	     form) ) ) ))

; 27 Apr 94
(defun glevalnumberp (x)
  (or (numberp x)
      (and (constantp x) (numberp (eval x)))))
 
; 26 Apr 94; 24 May 94
; Get the unit from a Q constant form
(defun glqunit (x)
  (if (numberp x)
      'unity
      (if (consp x)
	  (if (eq (car x) 'q)
	      (caddr x)
	      (if (and (eq (car x) 'quote)
		       (consp (cadr x))
		       (eq (caadr x) 'q))
		  (caddr (cadr x))))
	  (if (symbolp x)
	      (if (constantp x)
		  'unity
		  (if (glispconstantflg x)
		      (glispconstanttype x)))) ) ) )

; 26 Apr 94; 24 May 94
; Get the number from a Q constant form
(defun glqn (x)
  (if (numberp x)
      x
      (if (consp x)
	  (if (eq (car x) 'q)
	      (cadr x)
	      (if (and (eq (car x) 'quote)
		       (consp (cadr x))
		       (eq (caadr x) 'q))
		  (cadadr x)))
	  (if (symbolp x)
	      (if (constantp x)
		  (eval x)
		  (if (glispconstantflg x)
		      (glispconstantval x)))) ) ) )

; 26 Apr 94; 30 Sep 06
; Put a result back into Q form
(defun glqres (x unit)
  (if (numberp unit)
      (* x unit)
      (if (and (consp unit)
	       (eq (car unit) '*)
	       (numberp (cadr unit)))
	  (glqres (* x (cadr unit))
                  (if (cddr unit)
                      (cons '* (cddr unit))
                      (caddr unit)))
	  (if (eq unit 'unity)
	      x
	      (list 'quote (list 'q x unit))))))

; 28 Apr 94
; Relational operators that work for numbers or q constants
(defun glq>0  (x) (>  (glqn x) 0))
(defun glq>=0 (x) (>= (glqn x) 0))
(defun glq/=0 (x) (/= (glqn x) 0))

; 28 Apr 94
; Test for a Q constant form, (quote (q <n> <unit>))
(defmacro glqconstp (x)
  `(and (consp ,x) (eq (car ,x) 'quote) (consp (cdr ,x))
	(consp (cadr ,x)) (eq (caadr ,x) 'q)
	(consp (cdadr ,x))
	(numberp (cadadr ,x))))

; 28 Apr 94
; Test if it is legal to take sqrt of a constant
(defun glsqrtable (x)
  (or (and (numberp x) (>= x 0))
      (and (glqconstp x)
	   (glsqrtunit (glqunit x) nil t)
	   (>= (glqn x) 0))) )

; 29 Apr 94
; Test if it is legal to take cbrt of a constant
(defun glcbrtable (x)
  (or (numberp x)
      (and (glqconstp x)
	   (glcbrtunit (glqunit x) nil t) ) ) )

; 23 Dec 94
; Test whether a code form x can absorb a multiplicative constant
(defun glabsorbconstant (x)
  (if (atom x)
      (numberp x)
      (case (first x)
	(* (or (glabsorbconstant (cadr x))
	       (glabsorbconstant (caddr x))) )
	(/ (glabsorbconstant (cadr x)) )
	((+ - > >= = /= <= <)
	   (and (glabsorbconstant (cadr x))
		(glabsorbconstant (caddr x))) )
	(if (and (glabsorbconstant (caddr x))
		 (glabsorbconstant (cadddr x))) )
	(sqrt (glabsorbconstant (cadr x)))
	(t nil) ) ) )

; 23 Dec 94
; Push a multiplicative constant value C into an expression X
; Returns a new expression if success, NIL if failure.
(defun glpushconstant (c x)
  (let (tmp tmpb)
    (if (atom x)
	(if (numberp x)
	    (* c x)
	    nil)
        (case (first x)
	  (* (if (setq tmp (glpushconstant c (second x)))
		 (list (first x) tmp (third x))
	         (if (setq tmp (glpushconstant c (third x)))
		     (list (first x) (second x) tmp)
		     nil)))
	  (/ (if (setq tmp (glpushconstant c (second x)))
		 (list (first x) tmp (third x))
	         nil))
	  ((+ - > >= = /= <= <)
	     (if (and (setq tmp (glpushconstant c (second x)))
		      (setq tmpb (glpushconstant c (third x))))
		 (list (first x) tmp tmpb)))
	  (if (if (and (setq tmp (glpushconstant c (third x)))
		      (setq tmpb (glpushconstant c (fourth x))))
		  (list (first x) (second x) tmp tmpb)))
	  (sqrt (if (and (> c 0)
			 (setq tmp (glpushconstant (* c c) (second x))))
		    (list (first x) tmp)))
	  (t nil) ) ) ))

; 27 Apr 04; 08 Jun 04
; try to transform input using patterns
(defun gleqnsmatch (inp patwd)
  (let (patterns done res pat)
    (if (and (consp inp)
	     (symbolp (car inp)))
	(setq patterns (get (car inp) patwd)) )
    (while (and patterns (not done))
      (setq pat (car patterns))
      (setq res (transf pat inp))
      (if (not (eq res 'match-failure))
	  (setq done t))
      (setq patterns (cdr patterns)) )
    (if (and done *gleqnsmatch-trace*)
	(format t "~A ^ ~A --> ~A~%" inp pat res))
    (if done res inp) ))

; equations.lsp            Gordon S. Novak Jr.           ; 12 Jan 09

; Copyright (c) 2009 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; Process equations

; 26 Sep 95; 01 Mar 96; 02 Jan 97; 18 Mar 98; 29 May 98; 22 Dec 98; 30 Dec 98
; 13 Jan 99; 14 Jan 99; 02 Feb 99; 04 Feb 99; 09 Feb 99; 11 Feb 99; 18 Feb 99
; 11 Mar 99; 16 Mar 99; 19 Mar 99; 25 Mar 99; 02 Apr 99; 16 May 01; 28 Feb 02
; 03 Jan 03; 08 Jan 03; 09 Jan 03; 22 Oct 03; 15 Jan 04; 11 Feb 04; 12 Feb 04
; 16 Feb 04; 18 Feb 04; 19 Feb 04; 02 Mar 04; 11 Mar 04; 19 Mar 04; 26 Mar 04
; 29 Mar 04; 01 Apr 04; 08 Apr 04; 12 Apr 04; 14 Apr 04; 16 Apr 04; 20 Apr 04
; 23 Apr 04; 28 Apr 04; 29 May 04; 07 Jun 04; 08 Jun 04; 07 Apr 05; 19 Apr 05
; 11 May 05; 06 Dec 05; 08 Dec 05; 14 Dec 05; 28 Dec 05; 23 Jan 06; 26 Jan 06
; 27 Jan 06; 16 May 06; 29 Jun 06; 06 Jul 06; 02 Aug 06; 09 Aug 06; 12 Sep 06
; 09 Oct 06; 30 Oct 06; 13 Feb 07; 15 Feb 07; 19 Feb 07; 21 Feb 07; 01 Mar 07
; 02 Apr 07; 11 Apr 07; 12 Apr 07; 16 Apr 07; 22 May 07; 01 Jun 07; 11 Oct 07
; 16 Oct 07; 23 Jan 08; 30 Jan 08; 26 Jul 08; 22 Sep 08; 07 Nov 08; 10 Nov 08
; 21 Nov 08; 24 Nov 08

(defvar *eqns-trace* nil)  ; set to t to trace equation processing.
(defvar *equations-constants* nil)
(defvar *equations-solved* nil)
(defvar *equations-units* nil)
(defvar *equations-history* nil)

(glispobjects

(equation-set
  (list (equations        (listof anything))  ; remaining unsolved equations 
        (solved-vars      (listof symbol))    ; vars that are solved or defined
        (solved-equations (listof anything))  ; equations that have been solved
        (defined-vars     (listof symbol))    ; vars that are defined
        (deleted-tuples   (listof symbol))    ; deleted tuple vars
        (all-equations    (listof anything))) ; all equations
  prop  ((delete gleqns-delete-eqn open t) )
  adj   ((empty  ((null all-equations)))) )

) ; glispobjects

(pushnew '(mercator mercator)  *conn-user-laws*)

; 12 Apr 07
; Test for constants, omitting T (often used for time in equations).
(defun gleqnconstantp (x) (and (not (eq x t)) (glconstantp x)))

; Process equations to create views for variants of a type

; 21 Apr 92; 22 Apr 92; 05 Dec 92; 27 Feb 94
; Example: (glispobjects (mycir (listobject (diameter real))))
;          (gleqnstoprops 'mycir (get 'circle 'equations))
; Produce a list of PROP's for a GLISP type based on a set of equations
(defun gleqnstoprops (type equations)
  (let (solved newprops (progress t) vars unsolved var neweqn)
    (setq solved
          (nconc (mapcar #'car (gevdatanames type t))
                 (mapcar #'car (gevpropnames type 'prop t))))
    (while (and progress equations)
      (if (consp progress)
          (setq equations (set-difference equations progress)))
      (setq progress nil)
      (dolist (eqn equations)
        (setq vars (remove-if #'gleqnconstantp (glvarsin eqn)))
        (setq unsolved (set-difference vars solved))
        (when (and (consp unsolved) (null (cdr unsolved)))  ; 1 var unsolved
          (setq var (first unsolved))
          (when (setq neweqn (glsolvefor eqn var))
            (push eqn progress)
            (push var solved)
            (push (list var (glptmatch (last neweqn) 'glpatterns))
                  newprops))) ) )
    (nreverse newprops)))

; 29 Jun 06
; Combine equation sets to derive new equations for an object
; vars is a list of variables that are defined
; supers is a list of principles that apply to this object
;    each super is a symbol, or a list (symbol bindings)
;    where each binding is (var-in-target var)
; Example: (glcombineeqns '(circumference weight) '(physob sphere))
(defun glcombineeqns (vars supers)
  (let ((progress t) alleqns eqns bindings)
    (while progress
      (setq progress nil)
      (dolist (super supers)
        (if (consp super)
            (progn (setq bindings (cadr super))
                   (setq super (car super)))
            (setq bindings nil))
        (setq eqns (gleqns-solveeqns
                     (gleqns-renamevars vars bindings)
                     (gleqns-equations super)))
        (when eqns
          (setq progress t)
          (setq eqns (sublis (gleqns-tosublis bindings) eqns))
          (setq alleqns (append alleqns eqns))
          (setq vars (union vars (mapcar #'cadr eqns))) ) ) )
    alleqns ))

; 29 Jun 06
; Get equations defined for a concept
(defun gleqns-equations (x) (equations x))

; 29 Jun 06
; Rename variables given a bindinng list
(defun gleqns-renamevars (vars bindings)
  (let ()
    (mapcar #'(lambda (var)
                (or (car (find var bindings :key #'cadr))
                    var))
            vars)))

; 29 Jun 06
; Convert a list of lists to list of conses for sublis
(defun gleqns-tosublis (lst)
  (mapcar #'(lambda (x) (cons (car x) (cadr x))) lst))

; 29 Jun 06
; Convert a list of equations to list of GLisp PROP's
(defun gleqns-toprops (eqns)
  (mapcar #'(lambda (eqn) (list (cadr eqn) (list (caddr eqn))))
          eqns))

; 29 Jun 06; 12 Apr 07
; Solve a set of equations given a set of known variables
; Result is a list of equations
(defun gleqns-solveeqns (vars eqns)
  (let ((progress t) result dvars eqn eqnb)
    (setq eqns (subset #'(lambda (x) (not (gleqns-tuplep x))) eqns))
    (while (and progress eqns)
      (setq progress nil)
      (when (setq eqn
                  (some #'(lambda (x)
                            (and (setq dvars
                                       (subset #'(lambda (x)
                                                   (not (gleqnconstantp x)))
                                               (set-difference (varsin x)
                                                               vars)))
                                 (= (length dvars) 1)
                                 x))
                            eqns))
        (setq eqns (remove eqn eqns))
        (setq eqnb (glsolvefor eqn (car dvars)))
        (push eqnb result)
        (push (car dvars) vars)
        (setq progress t) ))
    (nreverse result) ))

; 29 Jun 06
; Test for a tuple equation, (= var (tuple ...))
(defun gleqns-tuplep (eqn)
  (and (consp eqn) (eq (car eqn) '=)
       (cdr eqn) (consp (cddr eqn))
       (eq (caaddr eqn) 'tuple)))

; 06 Jul 06
; vars = variables that are to be externally visible
; eqnsets = equation sets
; conns = connections between variables of equation sets
(defun glcombineeqnsets (vars eqnsets conns)
  (let ()
  ))

; 21 Apr 92; 27 Apr 94; 07 Oct 94; 11 Apr 07; 16 Apr 07
; Solve for a given variable in a lisp formula.  cf. solvefor in ISAAC
(defun glsolvefor (form var)
  (let ()
    (setq form (glfixequation form))
    (or (glsimplesolvefor form var)
        (glsimplesolvefor (gleqns-simplify form) var)
    ; or try to regroup terms, simplify, and try again
 ) ))

; 27 Apr 94
(defun glsimplesolvefor (form var)
  (let (res)
    (if (eq (car form) '=)
        (progn
          (setq res (if (eq (cadr form) var)
                        form
                        (if (gloccurs var (caddr form))
                            (glinvert (cadr form) (caddr form) var))))
          (if (not (gloccurs var (caddr res)))
              res)))))

; 22 Apr 92; 11 Nov 92; 28 Feb 94; 29 Apr 94; 02 Mar 04; 02 Apr 04; 19 Apr 05
; 06 Dec 05; 12 Sep 06
; Invert an equation, lhs = rhs, to find a specified var
(defun glinvert (lhs rhs var)
  (if (atom rhs)
      (if (eq rhs var) (list '= rhs lhs))
      (case (first rhs)
        (+ (or (glinvert (list '- lhs (second rhs)) (third rhs)  var)
               (glinvert (list '- lhs (third rhs))  (second rhs) var)))
        (* (if (equal (second rhs) (third rhs))
               (glinvert lhs (list 'expt (second rhs) 2) var)
               (or (glinvert (list '/ lhs (second rhs)) (third rhs) var)
                   (glinvert (list '/ lhs (third rhs))  (second rhs) var))))
        (- (if (cddr rhs)
               (or (glinvert (list '- (second rhs) lhs) (third rhs)  var)
                   (glinvert (list '+ lhs (third rhs))  (second rhs) var))
               (glinvert (list '- lhs) (second rhs) var)))
        (/ (or (glinvert (list '/ (second rhs) lhs) (third rhs)  var)
               (glinvert (list '* lhs (third rhs))  (second rhs) var)))
        ((sqrt |sqrt|)  (glinvert (list 'expt lhs 2) (second rhs) var))
        ((cbrt |cbrt|)  (glinvert (list 'expt lhs 3) (second rhs) var))
        ((log2 |log2|)  (glinvert (list 'expt 2 lhs) (second rhs) var))
        ((log10 |log10|)  (glinvert (list 'expt 10 lhs) (second rhs) var))
        ((log |log|)  (if (and (cddr rhs) (numberp (caddr rhs)))
                          (glinvert (list 'expt (caddr rhs) lhs)
                                    (second rhs) var)
                          (glinvert (list 'exp lhs) (second rhs) var)))
        ((float |float|) (glinvert lhs (second rhs) var))
        ((atan |atan|)
          (if (cddr rhs)
              (or (glinvert (list '/ (second rhs) (list 'tan lhs))
                            (third rhs) var)
                  (glinvert (list '* (third rhs) (list 'tan lhs))
                            (second rhs) var))
              (glinvert (list 'tan lhs) (second rhs) var)))
        ((sin |sin| cos |cos| tan |tan| asin |asin| acos |acos|
              exp |exp|)
          (glinvert (list (cadr (assoc (first rhs)
                                       '((sin asin) (asin sin)
                                         (|sin| asin) (|asin| sin)
                                         (cos acos) (acos cos)
                                         (|cos| acos) (|acos| cos)
                                         (tan atan) (|tan| atan)
                                         (|exp| log) (exp log) ) ))
                          lhs)
                    (second rhs) var))
        ((expt |expt|) 
               (if (eql (third rhs) 2)
                   (glinvert (list 'sqrt lhs) (second rhs) var)
                   (if (eql (third rhs) 3)
                       (glinvert (list 'cbrt lhs) (second rhs) var)
                       (if (eql (second rhs) 2)
                           (glinvert (list 'log2 lhs) (third rhs) var)
                           (if (numberp (second rhs))
                               (if (eql (second rhs) 10)
                                   (glinvert (list 'log10 lhs)
                                             (third rhs) var)
                                   (glinvert (list 'log lhs (second rhs))
                                             (third rhs) var)))))) )) ))

; 02 Oct 92; 20 Oct 92; 03 Nov 92; 03 Dec 92; 19 Mar 93; 22 Feb 94; 12 Apr 07
; Make a list of the unsolved variables in a formula.
(defun glunsolvedvars (form solved &optional unsolved)
  (if (atom form)
      (if (and form (symbolp form))
          (if (or (member form solved)
                  (member form unsolved)
                  (gleqnconstantp form))
              unsolved
              (cons form unsolved))
          unsolved)
      (if (eq (car form) 'quote)
          unsolved
          (dolist (subexp (cdr form) unsolved)
            (setq unsolved (glunsolvedvars subexp solved unsolved))) ) ) )

;------------------------------------------------------------------------------
; Code to solve sets of simultaneous equations

; Example: cannonball problem
(setq *gleqns-cannon*
      '((= fulltime (* time 2))
        (= time (/ yvel 9.81))         ; time to peak altitude
        (= xvel (* vel (cos elev)))
        (= yvel (* vel (sin elev)))    ; initial y velocity
        (= dist (* xvel fulltime))
        (= vel 300)
        (= dist 8000)))
; (gleqns-solve *gleqns-cannon*)
; (gleqns-solve '((= s (+ x y)) (= d (- x y))) '(s d))

; 22 Feb 94; 27 Feb 94
; Attempt to solve a set of simultaneous equations
; eqns    = list of equations
; solved  = variables considered to be given or already solved
; desired = variables whose solutions are desired
; Result is a list of equations for computing desired variables
(defun gleqns-solve  (eqns &optional solved)
  (let (eqn res)
    (if (and eqns (null (rest eqns))
             (setq eqn (gleqns-solve-eqn (first eqns) solved)))
        (if (null (set-difference (glvarsin (caddr eqn)) solved))
            (list eqn))
        (dolist (eqn eqns)
          (if (setq res (gleqns-solve-reduce eqn eqns solved))
              (return-from gleqns-solve res) ) ) ) ))

; 27 Feb 94
; Try to solve an equation set by solving eqn, a member of eqns, for a
; single variable, substituting in the remaining equations, and
; solving the reduced equation set.
(defun gleqns-solve-reduce (eqn eqns solved)
  (let (neweq neweqns newset)
    (when (setq neweq (gleqns-solve-eqn eqn solved))
      (dolist (eq eqns)
        (unless (eq eq eqn) (push (subst (caddr neweq) (cadr neweq) eq)
                                  neweqns)))
      (if (setq newset (gleqns-solve neweqns solved))
          (cons (gleqns-simplify
                  (sublis (mapcar #'(lambda (eq) (cons (cadr eq) (caddr eq)))
                                 newset)
                          neweq))
                newset)) ) ))

; 27 Feb 94; 01 Mar 94; 07 Oct 94
; Try to solve a single equation for a desired variable
(defun gleqns-solve-eqn (eqn solved)
  (let (seqn)
    (or (and (symbolp (cadr eqn))
             (not (member (cadr eqn) solved))
             (not (gloccurs (cadr eqn) (caddr eqn)))
             eqn)
        (and (symbolp (caddr eqn))
             (not (member (caddr eqn) solved))
             (not (gloccurs (caddr eqn) (cadr eqn)))
             (list '= (caddr eqn) (cadr eqn)))
        (progn (setq seqn (gleqns-simplify eqn))
               (some #'(lambda (var) (gleqns-solvefor seqn var))
                     (set-difference (glvarsin seqn) solved))) ) ))

; 27 Feb 94; 23 Apr 04
; Try to solve an equation for a variable
(defun gleqns-solvefor (eqn var)
  (let (neweqn)
    (if (or (setq neweqn (glsolvefor eqn var))
            (setq neweqn (glsolvefor
                          (list '= 0 (gleqns-simplify (list '- (cadr eqn)
                                                            (caddr eqn))))
                          var)))
        (list '= (cadr neweqn)
                 (gleqns-simplify (eqn-optprod (caddr neweqn)))) ) ))
        

; 22 Feb 94; 24 Feb 94; 26 Apr 94
; Attempt to simplify a formula
(defun gleqns-simplify (form) (gleqns-trans form 'math))

;------------------------------------------------------------------------------

; 28 Dec 05
; attempt a numerical solution for a variable in a formula ; see mathfns.lsp
; (glsolveform '(= (expt x 2) 2) 'x)
; (glsolveform '(= (sin x) (/ x 2)) 'x)
(defun glsolveform (form var)
  (regula-falsib (list '- (cadr form) (caddr form)) var))

; 14 Dec 05; 28 Dec 05; Won 19 Jan 06
; Attempt to solve a quadratic equation for variable
; (glsolvequad '(= (* 1/2 (* a (expt t 2))) s) 't)
(defun glsolvequad (form var &optional minus)
  (let (factors newform)
    (setq newform
          (if (and (eq (car form) '=)
                   (eql (cadr form) 0))
              (caddr form)
              (if (and (eq (car form) '=)
                       (eql (caddr form) 0))
                  (cadr form)
                  (list '- (cadr form) (caddr form)))))
    (setq factors (mapcar #'gleqns-simplify (polyfact newform var)))
    (and factors (cdr factors) (cddr factors)
         (not (and (numberp (caddr factors)) (zerop (caddr factors))))
         (list '= var (glquadsol factors minus))) ))

; 14 Dec 05; 28 Dec 05; 16 Apr 07
; construct a solution to quadratic given factors
; (glquadsol '(c b a)) ; for a*x^2 + b*x + c = 0
(defun glquadsol (factors &optional minus)
  (let ((a (caddr factors)) (b (cadr factors)) (c (car factors))
        (op (if minus '- '+)))
    (if (or (and (numberp a) (minusp a))
            (and (consp a) (eq (car a) '-) (null (cddr a)))
            (and (consp a) (member (car a) '(* /))
                 (numberp (cadr a)) (minusp (cadr a))))
        (progn (setq a (gleqns-simplify (list '- a)))
               (setq b (gleqns-simplify (list '- b)))
               (setq c (gleqns-simplify (list '- c)))) )
    (gleqns-simplify
      `(/ (,op (- ,b) (sqrt (- (expt ,b 2) (* 4 (* ,a ,c))))) (* 2 ,a)) ) ))

; 15 Oct 07
; Try to solve a quadratic, preferring a solution that is
; likely to be positive.
(defun glsolvequadb (form var alist &optional minus)
  (let (sola solb rhsa rhsb preferred other denomvar denomval)
    (setq sola (glsolvequad form var))
    (setq solb (glsolvequad form var t))
    (if (and sola solb)
        (progn
          (setq preferred sola)
          (setq rhsa (third sola))
          (setq rhsb (third solb))
          (if (and (numberp rhsa) (numberp rhsb))
              (if (> rhsb rhsa) (setq preferred solb))
              (if (and (consp rhsa)
                       (eq (car rhsa) '/)
                       (setq denomvar (glsolvequadbxtr (caddr rhsa)))
                       (setq denomval (assoc denomvar alist))
                       (numberp (cdr denomval)))
                   (if (< (cdr denomval) 0) (setq preferred solb))))
          (setq other (if (eq preferred sola) solb sola))
          (if minus other preferred))
        (if minus solb sola) ) ))

; 15 Oct 07
; extract a symbol multiplied by constant
(defun glsolvequadbxtr (expr)
  (if (symbolp expr)
      expr
      (if (consp expr)
          (if (eq (car expr) '*)
              (if (numberp (cadr expr))
                  (glsolvequadbxtr (caddr expr))
                  (if (numberp (caddr expr))
                      (glsolvequadbxtr (cadr expr))))
              (if (eq (car expr) '/)
                  (if (numberp (caddr expr))
                      (glsolvequadbxtr (cadr expr))))))))

; 14 Dec 05; Won 19 Jan 06; 23 Jan 06; 26 Jan 06
; find polynomial factors (c x x^2) of a quadratic expression in var
; (polyfact '(+ (* a (expt x 2)) (+ (* b x) c)) 'x)  ; = (C B A)
(defun polyfact (exp var)
  (let (tmp)
    (if (eq exp var)
        (list 0 1 0)
        (if (or (atom exp) (not (gloccurs var exp)))
            (list exp 0 0)
          (case (car exp)
            (+ (polyfact+ (polyfact (cadr exp) var)
                          (polyfact (caddr exp) var)))
            (- (if (cddr exp)
                   (polyfact- (polyfact (cadr exp) var)
                              (polyfact (caddr exp) var))
                   (polyfactminus (polyfact (cadr exp) var))))
            (* (polyfact* (polyfact (cadr exp) var)
                          (polyfact (caddr exp) var)))
            (/ (if (gloccurs var (caddr exp))
                   (error "Bad expr in polyfact ~A~%" exp)
                   (polyfact* (polyfact (cadr exp) var)
                              (list (list '/ 1 (caddr exp)) 0 0))))
            ((expt |expt|)
              (if (eql (caddr exp) 2)
                  (progn (setq tmp (polyfact (cadr exp) var))
                         (polyfact* tmp tmp))
                (error "Bad expr in polyfact ~A~%" exp)))
            (t (error "Unrecognized operator expr in polyfact ~A~%" exp)))))))

; 14 Dec 05
(defun polyfact+ (facts factsb)
  (mapcar #'(lambda (x y)
              (if (eql x 0)
                  y
                  (if (eql y 0)
                      x
                      (list '+ x y)))) facts factsb))

; 14 Dec 05
(defun polyfact- (facts factsb)
  (mapcar #'(lambda (x y)
              (if (eql x 0)
                  (list '- y)
                  (if (eql y 0)
                      x
                      (list '- x y)))) facts factsb))

; 14 Dec 05
(defun polyfactminus (facts)
  (mapcar #'(lambda (x) (if (eql x 0) 0 (list '- x))) facts))

; 14 Dec 05
(defun polyfact* (facts factsb)
  (let (pow1 pow2)
    (setq pow1 (polyfact*1 (cadr facts) factsb))
    (setq pow2 (polyfact*1 (caddr facts) factsb))
    (polyfact+ (polyfact*1 (car facts) factsb)
               (polyfact+ (list 0 (car pow1) (cadr pow1))
                          (list 0 0 (car pow2)))) ))

; 14 Dec 05
(defun polyfact*1 (fact facts)
  (mapcar #'(lambda (x)
              (if (or (eql fact 0)
                      (eql x 0))
                  0
                  (if (eql fact 1)
                      x
                      (if (eql x 1)
                          fact
                          (list '* fact x)))))
          facts))

;------------------------------------------------------------------------------

; Code for manipulating sets of equations for use by makev / vip

; Define parts of an equation set:
(defmacro eqns-equations        (eqns) `(first ,eqns))
(defmacro eqns-solved-vars      (eqns) `(second ,eqns))
(defmacro eqns-solved-equations (eqns) `(third ,eqns))
(defmacro eqns-defined-vars     (eqns) `(fourth ,eqns))
(defmacro eqns-deleted-tuples   (eqns) `(fifth ,eqns))
(defmacro eqns-all-equations    (eqns) `(sixth ,eqns))

; Example:      (setq *eqns-trace* t)
; (setq eqset (gleqns-init-equations (get 'line-segment 'equations)))
; (gleqns-var-defined eqset 'p1x)
; (gleqns-var-defined eqset 'length)
; (gleqns-var-defined eqset 'theta)
; (gleqns-var-defined eqset 'p2y)
; eqset

; 03 Oct 92; 12 Oct 92; 30 Oct 92; 18 Nov 92; 19 Nov 92
; Initialize equation set for use with gleqns-var-defined
(defun gleqns-init-equations (eqns)
  (list (copy-list eqns) nil nil nil nil eqns) )

; 02 Nov 92; 19 Nov 92
(defun gleqns-delete-eqn (eqset eqn)
  (setf (eqns-equations eqset) (delete eqn (eqns-equations eqset))) )

; 19 Mar 93
; Find equation whose left-hand side defines var
(defmacro gleqns-findeq (var eqnlist)
  `(find-if #'(lambda (x) (eq ,var (cadr x))) ,eqnlist))

; 19 Mar 93
; Find the definition of var among solved equations
(defun gleqns-def (var eqns)
  (caddr (gleqns-findeq var (eqns-solved-equations eqns))))

; 19 Mar 93
; Find the definition of var among all equations
(defun gleqns-alldef (var eqns)
  (caddr (gleqns-findeq var (eqns-all-equations eqns))))

; 03 Oct 92; 07 Oct 92; 12 Oct 92; 20 Oct 92; 30 Oct 92; 02 Nov 92; 16 Nov 92
; 05 Dec 92; 05 Mar 93; 14 Mar 93; 15 Mar 93; 16 Mar 93; 28 Mar 93; 01 Apr 93
; 22 Feb 94; 27 Feb 94; 11 Mar 99
; When a var becomes defined, examine the equation set to see if any
; properties defined by the equations have become defined.
; Returns list of vars that have become defined, solved, or invalid,
; and therefore can no longer be independently specified.
; (A tuple becomes invalid if one of its components is defined.)
(defun gleqns-var-defined (eqns var)
  (let ((vars (list var)) (progress t) uns neweqn newvars)
    (if *eqns-trace*
        (format t "1. Enter var-defined, var = ~A ~%" var))
    (pushnew var (eqns-solved-vars eqns))
    (pushnew var (eqns-defined-vars eqns))
    (while progress
      (setq progress nil)
      (setq newvars nil)
      (dolist (eqn (eqns-equations eqns))
        (setq uns (glunsolvedvars eqn (eqns-solved-vars eqns)))
        (if (null uns)
            (progn (if *eqns-trace*
                       (format t "  2b. deleting eqn   ~A ~%" eqn))
                   (gleqns-delete-eqn eqns eqn)))
        (if (intersection uns (eqns-deleted-tuples eqns))
            (progn (if *eqns-trace*
                       (format t "  2d. deleting eqn   ~A ~%" eqn))
                   (gleqns-delete-eqn eqns eqn)))
        (if (and uns
                 (null (cdr uns))                   ; exactly one unsolved
                 (not (member (first uns) newvars))
                 (not (and (consp (caddr eqn))
                           (eq (caaddr eqn) 'tuple)))
                 (setq neweqn (glsolvefor eqn (first uns))))
            (progn (setq neweqn (list (first neweqn) (second neweqn)
                                      (glptmatch (third neweqn) 'glpatterns)))
                   (if *eqns-trace*
                       (if (equal eqn neweqn)
                         (format t "  2a. solved eqn     ~A~%" eqn)
                         (format t
                           "  2a. solved eqn     ~A~%          giving     ~A~%"
                           eqn neweqn)))
                   (push (second neweqn) newvars)
                   (push (second neweqn) vars)
                   (gleqns-delete-eqn eqns eqn)
                   (push neweqn (eqns-solved-equations eqns))
                   (setq progress t)))
        (if (and uns
                 (consp (caddr eqn))
                 (eq (caaddr eqn) 'tuple)
                 (intersection (glvarsin (caddr eqn))
                               (eqns-solved-vars eqns)))
            (progn (if *eqns-trace* (format t "  2c. deleting tuple ~A~%" eqn))
                   (push (cadr eqn) vars)
                   (push (cadr eqn) (eqns-deleted-tuples eqns))
                   (gleqns-delete-eqn eqns eqn))) )
      (when progress
        (if *eqns-trace* (format t "  3. repeating step 2.~%" ))
        (setf (eqns-solved-vars eqns)
              (nconc newvars (eqns-solved-vars eqns))) ) )
    (if *eqns-trace* (format t "4. exit, vars ~A ~%" vars))
    vars))


; 05 Mar 93; 11 Mar 99; 16 Mar 99
; Make an equation set assuming basis variables are the ones that are stored.
(defun gleqns-basis-eqns (goal)
  (let (eqns)
    (when (get goal 'equations)
      (setq eqns (gleqns-init-equations (get goal 'equations)))
      (dolist (var (gleqns-basis goal)) (gleqns-var-defined eqns var))
      eqns) ))

; 09 Feb 94; 10 Feb 94; 22 Dec 98; 30 Dec 98
; Make equations for a box consisting of an operator
(defun gleqns-op-equations (op)
  (let (nargs)
    (setq nargs (if (gleqns-unaryp op) 1 (or (conn-nargs op) 2)))
    (list (list '= 'out
                (cons op
                      (if (= nargs 1)
                          (list 'in)
                          (butlast '(in in2 in3 in4 in5 in6 in7)
                                   (- 7 nargs)))))) ))

; 10 Feb 94
(defun gleqns-prop-equations (prop) (list (list '= 'out (list prop 'in))))

; 09 Feb 94; 29 Apr 94; 26 Sep 95
(defun gleqns-unaryp (fn) (member fn '(sin cos tan sqrt cbrt exp log not)))

; 23 Oct 92; 06 Nov 92; 10 Nov 92; 17 Nov 92; 18 Nov 92; 05 Mar 93; 16 Mar 93
; 19 Mar 93; 25 Mar 93; 16 Mar 99; 28 Feb 02; 03 Jan 03; 08 Jan 03; 09 Jan 03
; 30 Jan 08
; Produce inverse code to store var, a basis variable of
; the goal type, into the approprate forms in a view type.
(defun gleqns-store-var (goal viewtype var eqns undef)
  (let (basis nocc xfer done (progress t) code letvars pair res sourcetype
        lhs sourcename newv newva tmp dep xfers prop)
    (if *eqns-trace* (format t "0. Entering gleqns-store-var, var ~A~%" var))
    (setq sourcename (caar (glstr viewtype)))
    (setq sourcetype (cadar (glstr viewtype)))
    (setq basis (gleqns-basis goal))
    (setq xfer (cadr (caadr (glgetprop viewtype 'prop 'gltransfernames))))
  ; determine which basis vars, other than the var to be stored, are used
  ; in computing the transfer vars
    (dolist (xvar xfer)
      (setq tmp (gleqns-depends-on eqns xvar basis nil))
      (if *eqns-trace* (format t "  1. var ~A depends on ~A~%" xvar tmp))
      (when (member var tmp)
        (push xvar xfers)
        (setq dep (union tmp dep)) ) )
    (when (and (null (intersection dep undef))
               (not (member var undef))
               (or (setq dep (delete var dep))
                   (cdr xfers)
                        ; if a plain store will work, return nil.
                   (not (and (setq prop (glgetprop viewtype 'prop (car xfers)))
                             (setq tmp (cadr prop))
                             (consp tmp)
                             (consp (car tmp))
                             (assoc (caar tmp) (gldatanames sourcetype))))))
      (setq newv (intern (concatenate 'string "VAR-"
                                      (symbol-name sourcename))))
      (setq newva (list sourcename newv))      ; code to access the var
  ; make let vars for other basis vars used in computing transfer vars
      (dolist (v dep) (push (list v (list v newva)) letvars))
      (setq done xfers)
      (dolist (var done)
        (unless (member var basis)
          (setq nocc (gleqns-countocc (gleqns-def var eqns)
                                      basis (gleqns-incocc var nocc)))))
      (while progress
        (setq progress nil)
        (dolist (pair nocc)
          (unless (member (car pair) done)
            (push (car pair) done)
            (setq progress t)
            (setq nocc (gleqns-countocc (gleqns-def (car pair) eqns)
                                        basis nocc))) ) )
      (dolist (prop (eqns-solved-equations eqns))
        (when (and (setq pair (assoc (cadr prop) nocc))
                   (> (cdr pair) 1))
          (push (cadr prop) letvars)
          (push (list (cadr prop) '=
                      (gleqns-fixcode eqns (caddr prop) nocc))
                code) ) )
      (dolist (xvar xfers)
        (setq tmp (assoc xvar (glget viewtype 'prop)))
        (setq lhs (if tmp
                      (subst newva sourcename (caadr tmp))
                      (list xvar newva)))
        (if (and (member xvar (eqns-deleted-tuples eqns))
                 (setq prop (gleqns-alldef xvar eqns))
                 (consp prop)
                 (eq (car prop) 'tuple))
            (dolist (pair (cdr prop))
              (if (member var (gleqns-depends-on eqns (cadr pair) basis nil))
                  (push (list (list (car pair) lhs) '=
                              (gleqns-fixcode eqns (cadr pair) nocc))
                        code)))
            (push (list lhs '= (gleqns-fixcode eqns xvar nocc))
                  code) ) )
      (when code
        (push var code)
        (setq res (list 'glambda (list newv var)
                        (cons 'let
                              (cons (nreverse letvars)
                                    (nreverse code)))))))  ))

; 16 Mar 99
; Get the nmaes of basis variables of a goal type
(defun gleqns-basis (goal)
  (or (get goal 'basis-vars)
      (mapcar #'car (gldatanames goal)) ) )

; 22 Oct 92; 16 Nov 92; 17 Nov 92; 21 Jan 93; 12 Apr 07
; Count occurrences of variables in an equation.  basis vars are not counted.
; nocc is an alist of (var . count).
(defun gleqns-countocc (form basis nocc)
  (let ()
    (if (atom form)
        (if (and form (symbolp form))
            (if (or (member form basis)
                    (gleqnconstantp form))
                nocc
                (gleqns-incocc form nocc))
            nocc)
        (dolist (subexp (cdr form) nocc)
          (setq nocc (gleqns-countocc subexp basis nocc)) ) ) ))

; 17 Nov 92
; Increment occurrence count
(defun gleqns-incocc (var nocc)
  (let (pair)
    (if (setq pair (assoc var nocc))
        (progn (incf (cdr pair))
               nocc)
        (cons (cons var 1) nocc)) ))

; 22 Oct 92; 30 Oct 92; 16 Mar 93
; Fix code to incorporate other equations that are used only once
(defun gleqns-fixcode (eqns form nocc)
  (let (pair)
    (if (atom form)
        (if (and (symbolp form)
                 (setq pair (assoc form nocc)))
            (if (> (cdr pair) 1)
                form
                (gleqns-fixcode eqns (gleqns-def form eqns) nocc))
            form)
        (cons (car form)
              (mapcar #'(lambda (x) (gleqns-fixcode eqns x nocc))
                      (cdr form))) ) ))

; 22 Oct 92; 30 Oct 92; 18 Nov 92; 19 Mar 93
; Find what basis vars a given property depends on
(defun gleqns-depends-on (eqns form basis dep)
  (let (prop)
    (if (atom form)
        (if (and form (symbolp form))
            (if (member form basis)
                (if (member form dep) dep (cons form dep))
                (if (setq prop (gleqns-def form eqns))
                    (gleqns-depends-on eqns prop basis dep)
                    (if (and (setq prop (gleqns-alldef form eqns))
                             (consp prop)
                             (eq (car prop) 'tuple))
                        (gleqns-depends-on eqns prop basis dep))))
            dep)
        (dolist (subexp (cdr form) dep)
          (setq dep (gleqns-depends-on eqns subexp basis dep)) ) ) ))


; 08 Nov 92; 09 Nov 92; 11 Nov 92; 17 Nov 92; 05 Mar 93; 16 Mar 93; 19 Mar 93
; 20 Dec 93; 16 Mar 99; 18 Mar 99; 19 Mar 99; 30 Jan 08
; Produce code to make an instance of the source type of a view
; from a set of values for the basis variables of the goal type.
; Result is (basis auxcode code)
;   where  basis   = basis vars of goal type
;          auxcode = (var code) to compute each auxiliary var
;          code    = (var code) for each var of source type to be stored
(defun gleqns-bfv (goal viewtype eqns)
  (let (basis sourcename sourcetype xfers done nocc progress auxcode tmp
        datanames rhscode proprhs field parts tupleqn code pair (okay t))
    (setq sourcename (caar (glstr viewtype)))
    (setq sourcetype (cadar (glstr viewtype)))
    (setq basis (gleqns-basis goal))
  ;   get transfer names = names of abstract type to be transferred
    (setq xfers (cadr (caadr (glgetprop viewtype 'prop 'gltransfernames))))
    (setq done xfers)
  ;   count occurrences of vars used in computing transfer set
    (dolist (var done)
      (unless (member var basis)
        (setq nocc (gleqns-countocc (gleqns-def var eqns)
                                    basis (gleqns-incocc var nocc)))))
  ;   recursively count occurrences of all vars used
    (setq progress t)
    (while progress
      (setq progress nil)
      (dolist (pair nocc)
        (unless (member (car pair) done)
          (push (car pair) done)
          (setq progress t)
          (setq nocc (gleqns-countocc (gleqns-def (car pair) eqns)
                                      basis nocc))) ) )
  ;   make let vars and assignment statements for intermediate vars that
  ;   are used more than once
    (dolist (prop (eqns-solved-equations eqns))
      (when (and (setq pair (assoc (cadr prop) nocc))
                 (> (cdr pair) 1))
        (push (list (cadr prop)
                    (gleqns-fixcode eqns (caddr prop) nocc))
              auxcode) ) )
  ;   get the names of data fields in the structure to be built
    (setq datanames (gldatanames sourcetype))
  ;   for each transfer var, if it corresponds to a stored field of the
  ;   goal type, make a pair for use in constructing the 'A' function.
    (dolist (var xfers)
      (setq rhscode nil)
   ;    find the PROP entry that computes this transfer var
      (setq tmp (assoc var (glget viewtype 'prop)))
   ;    if it is legit, proprhs is how to compute it, e.g. (RIGHT LS124)
      (if (and (cdr tmp)
               (consp (cadr tmp))
               (consp (setq proprhs (caadr tmp))))
     ;    if it is a stored field, set field to name and type
        (progn
          (setq field (and (eq (cadr proprhs) sourcename)
                           (assoc (car proprhs) datanames)))
     ;    make code to compute this transfer var
     ;    if it is a tuple, build the substructure
          (if (and (setq tupleqn (gleqns-alldef var eqns))
                   (eq (first tupleqn) 'tuple))
              (progn (setq parts (mapcan #'(lambda (x) (list (car x) (cadr x)))
                                       (cdr tupleqn)))
                     (if field
                   ;    if it is a stored field, make 'A' code
                         (setq rhscode (cons 'a (cons (cadr field) parts)))
                   ;    if it is a view, make special 'A' code
                       (if (and (consp (cadr proprhs))
                                (setq field
                                      (and (eq (cadadr proprhs) sourcename)
                                           (assoc (caadr proprhs) datanames))))
                           (setq rhscode
                             (cons 'a (cons (list 'typeof
                                                  (list (car proprhs)
                                                        (list 'a
                                                              (cadr field))))
                                            parts))))))
              (setq rhscode (or (gleqns-fixcode eqns var nocc)
                                var))) )
          (progn (setq field (list var))
                 (setq rhscode (gleqns-fixcode eqns var nocc)) ) )
      (if (gleqns-bfv-ok rhscode basis auxcode)
          (if field
              ;   if it is stored, make it part of the 'A' function
              (push (list (car field) rhscode) code)
              ;   else do an assignment afterwards.         ; 12/20/93
              (push (list (subst (glmkatom 'glbuildresult) sourcename proprhs)
                          rhscode) auxcode))
          (setq okay nil) ) )
    (and okay (list basis (nreverse auxcode) (nreverse code))) ))

; 30 Jan 08
; Make sure that the code to compute something is legitimate,
; i.e. that every var used is a basis var or defined as a let var
(defun gleqns-bfv-ok (rhscode basis auxcode)
  (and rhscode
       (every #'(lambda (var) (or (member var basis) (assoc var auxcode)))
              (glvarsin rhscode)) ) )

; 19 Mar 99; 02 Apr 99; 09 Jan 03; 30 Jan 08
; Generate code to build an instance of the source type of a view
; from a set of values for the basis variables of the goal type.
(defun gleqns-build-from-view (goal viewtype eqns)
  (let (bfv sourcetype auxcode code letvars acode after aftervar datanames
            basis)
    (when (setq bfv (gleqns-bfv goal viewtype eqns))
      (setq sourcetype (cadar (glstr viewtype)))
      (setq basis (car bfv))
      (setq auxcode (cadr bfv))
      (setq code (caddr bfv))
      (setq datanames (gldatanames sourcetype))
      (setq letvars (mapcar #'car auxcode))
      (dolist (pair code)
        (if (assoc (car pair) datanames)
            (progn (push (car pair) acode)
                   (push (cadr pair) acode))
            (progn (or aftervar (setq aftervar (glmkatom 'glbuildresult)))
                   (push (list (list (car pair) aftervar) '= (cadr pair))
                         after)) ) )
      (setq code (nconc (mapcar #'(lambda (x) (list (car x) '= (cadr x)))
                                auxcode)
                        (cons (if aftervar
                                  (list aftervar '=
                                        (cons 'a (cons sourcetype
                                                       (nreverse acode))))
                                  (cons 'a (cons sourcetype (nreverse acode))))
                              (nreverse after))))
      (cons 'glambda
            (cons (cons 'self basis)
                  (if letvars
                      (list (cons 'let (cons (nreverse letvars) code)))
                      code))))  ))

; 19 Mar 99; 02 Apr 99; 28 Feb 02; 03 Jan 03; 09 Jan 03; 30 Jan 08
; Generate code to store into an instance of the source type of a view
; from a variable of the goal type.
(defun gleqns-store-from-view (goal viewtype eqns undef)
  (let (bfv auxcode code letvars acode goalvar sourcename basis
            (sourcevar 'self))
    (when (setq bfv (gleqns-bfv goal viewtype eqns))
      (setq basis (car bfv))
      (setq auxcode (cadr bfv))
      (setq code (caddr bfv))
      (setq sourcename (caar (glstr viewtype)))
      (setq goalvar (glmkatom goal))
      (dolist (v basis)
        (unless (member v undef)
          (push (list v (list v goalvar)) letvars)))
      (setq letvars (nconc (nreverse letvars) (mapcar #'car auxcode)))
      (setq acode
            (nconc (mapcar #'(lambda (x)
                               (list (list (car x) (list sourcename sourcevar))
                                     '= (cadr x)))
                           auxcode)
                   (mapcar #'(lambda (x)
                               (list (list (car x) (list sourcename sourcevar))
                                     '= (cadr x)))
                           code)
                   (list sourcevar)))
      (cons 'glambda
            (cons (list sourcevar goalvar)
                  (if letvars
                      (list (cons 'let (cons (nreverse letvars) acode)))
                      code))))  ))


; 21 Dec 93; 11 Feb 94; 03 Jan 03
; Materialize a data structure of the goal type from an instance of the
; viewed type.
(defun gleqns-materialize-view (goal viewtype undef)
  (let (datanames props items eqns tmp)
    (setq datanames (gldatanames goal))
    (setq props (glget viewtype 'prop))
    (setq eqns (get goal 'equations))
    (setq items
          (mapcan #'(lambda (pair)
                      (if (and (assoc (car pair) props)
                               (not (member (car pair) undef)))
                          (list (car pair) (list (car pair) 'self))
                          (if (setq tmp
                                    (gleqns-materialize-tuple pair eqns props))
                              (list (car pair) tmp))))
                        datanames))
    (if items (list 'glambda (list 'self) (cons 'a (cons goal items)))) ))

; 11 Feb 94; 07 Oct 94
; Materialize a substructure based on TUPLE specification
; e.g. nametype = (P1 VECTOR)
;      eqns     = ((= P1 (TUPLE (X P1X) (Y P1Y))) ...)
;         returns (A VECTOR X (P1X SELF) Y (P1Y SELF))
(defun gleqns-materialize-tuple (nametype eqns props)
  (let (tupleqn tuple items tmp)
    (setq tupleqn (find-if #'(lambda (eqn) (and (eq (cadr eqn) (car nametype))
                                                (consp (caddr eqn))
                                                (eq (caaddr eqn) 'tuple)))
                           eqns))
    (when tupleqn
      (setq tuple (cdaddr tupleqn))
      (setq items
            (mapcan #'(lambda (pair)
                        (if (assoc (cadr pair) props)
                            (list (car pair) (list (cadr pair) 'self))
                            (if (setq tmp
                                    (gleqns-materialize-tuple pair eqns props))
                                (list (car pair) tmp))))
                    tuple))
      (if items (cons 'a (cons (cadr nametype) items))) ) ))

; 13 Nov 92; 16 Nov 92; 17 Nov 92; 16 Mar 93
; Create a function to transfer from one data representation to another
; according to a view shared by both representations.
(defun gleqns-transfer-by-view (goal source
                                     &optional goalviewname sourceviewname)
  (let (sourcevs goalvs sourcev goalv viewtype done datanames vartype parts
        goalvar newv newvt newvv code xfers goalview tmp)
; find views that the source and goal types have in common
    (setq sourcevs (glget source 'view-choices))
    (setq goalvs (glget goal 'view-choices))
    (if (and goalviewname (null sourceviewname))
        (setq sourceviewname goalviewname))
    (if (and goalviewname
             (setq sourcev (assoc sourceviewname sourcevs))
             (setq goalv (assoc goalviewname goalvs)))
        (if (not (eq (second sourcev) (second goalv)))
            (error "Incompatible views  between types ~A and ~A~%"
                   goal source))
        (dolist (gv goalvs)
          (if (and (not done)
                   (setq sourcev (find-if #'(lambda (x) (eq (second x)
                                                            (second gv)))
                                          sourcevs)))
              (progn (setq goalv gv) (setq done t)))) )
    (or goalv (error "No view in common between types ~A and ~A~%"
                     goal source))
    (setq datanames (gldatanames goal))
    (setq viewtype (second goalv))
    (setq goalview (or (caddr (assoc (first goalv) (glget goal 'views)))
                       (mkv goal source nil goalviewname)))
    (setq xfers (cadr (caadr (glgetprop goalview 'prop 'gltransfernames))))
    (setq newv  (intern (concatenate 'string "VAR-" (symbol-name source))))
    (setq newvt (intern (concatenate 'string (symbol-name newv)
                                             ":" (symbol-name source))))
    (setq newvv (intern (concatenate 'string "VAR-" (symbol-name source)
                                             "-VIEW")))
    (dolist (var xfers)
      (setq tmp (find-if #'(lambda (x) (eq (caar x) var))
                         (fifth goalv)))
      (setq goalvar (caadr tmp))
      (setq vartype (cadr (assoc goalvar datanames)))
      (if (glbasictypep vartype)
          (push (list var newvv) code)
          (progn (setq parts (caddr (find-if #'(lambda (x) (eq var (cadr x)))
                                             (get viewtype 'equations))))
                 (push (if (eq (car parts) 'tuple)
                           (cons 'a
                             (cons vartype
                                   (mapcan #'(lambda (x)
                                               (list (car x)
                                                     (list (cadr x) newvv)))
                                           (cdr parts)))))
                       code)))
      (push goalvar code))
    (list 'glambda (list newvt)
          (list 'let (list (list newvv (list viewtype newv)))
                (cons 'a (cons goal code)))) ))

; 19 Nov 94
; make equations from a view-choices form.
; vc = (goals sources correspondences)
(defun gleqns-from-view-choices (vc)
  (let (subs)
    (setq subs (gleqns-vc-vars vc))
    (mapcar #'(lambda (corr)
              (list '= (sublis subs (first corr) :test #'equal)
                       (sublis subs (second corr) :test #'equal)))
          (third vc) ) ))

; 19 Nov 94
; Make a substitution list for variables.
; a variable form is (var source).  If var is unique it is used,
; else it is changed to type-var.
(defun gleqns-vc-vars (vc)
  (let (sources subs)
    (setq sources (append (car vc) (cadr vc)))  ; combine sources and goals
    (dolist (corr (third vc))
      (setq subs (gleqns-vc-vars-subs-l sources corr subs)))
    subs))

(defun gleqns-vc-vars-subs-l (sources items subs)
  (if items
      (gleqns-vc-vars-subs-l sources (cdr items)
                             (gleqns-vc-vars-subs sources (car items) subs))
      subs))

(defun gleqns-vc-vars-subs (sources item subs)
  (let (pair)
    (if (consp item)
        (if (and (consp (cdr item))
                 (symbolp (cadr item))
                 (null (cddr item))
                 (setq pair (assoc (cadr item) sources))
                 (not (assoc item subs :test #'equal)))
            (cons (cons item
                        (if (some #'(lambda (x) (eq (cdr x) (car item)))
                                  subs)
                            (intern (concatenate 'string (cadr pair) "-"
                                                 (car item)))
                            (car item)))
                  subs)
            (gleqns-vc-vars-subs-l sources (rest item) subs))
        subs) ))

; 13 Jan 99; 14 Jan 99; 09 Jan 03
; simple primality test for examples
(gldefun primep ((n integer)) (result boolean)
  (and (> n 1) (integerp n)
       (or (= n 2)
           (and (oddp n)
                (let ((ndiv (floor (sqrt n))) (i 3) (prime t))
                  (while (and prime (<= i ndiv))
                    (if (= 0 (mod n i))
                        (setq prime nil)
                        (i = (+ i 2))))
                  prime)))))

(defun log10 (x) (/ (log x) (log 10.0)))
(defun log2 (x) (/ (log x) (log 2.0)))
(defun atand (y x) (* (/ 180.0 pi) (atan y x)))  ; atan in degrees
(defun flip (&optional (n 1000000)) (> (random n) (/ n 2)))

; 28 Apr 94
; Cube root -- defined here so patterns using it can be tested.
; returns a negative real root for a negative argument.
(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))

; 13 Feb 07
; simulate a random coin flip
(defun flipcoin (&rest stuff)
  (declare (ignore stuff))
  (< (random 1.0) 0.5))

(dolist (pair '((sin |sin|) (asin |asin|) (cos |cos|) (acos |acos|)
                (tan |tan|) (log |log|) (exp |exp|) (sqrt |sqrt|)
                (cbrt |cbrt|) (log2 |log2|) (log10 |log10|)
                (float |float|) (atan |atan|) (expt |expt|) ) )
  (setf (symbol-function (cadr pair)) (symbol-function (car pair))) )

; 22 Oct 03; 12 Apr 07
; based on equation eqn, how does the value of variable dvar
; change based on new values newvals (alist).  oldvals default to 1.
; The answer is adjusted for the oldval of dvar.
; "doubling the thickness of a given wire and making it 10 times longer
; will cause its resistance to be... "
; (eqnchanged '(= r (* k (/ l (* pi (expt rad 2))))) 'r '((l . 10) (rad . 2)))
; "what change to the radius will make the resistance double?"
; (eqnchanged '(= r (* k (/ l (* pi (expt rad 2))))) 'rad '((r . 2)))
(defun eqnchanged (eqn dvar newvals &optional oldvals)
  (let ((neweqn (glsolvefor eqn dvar)))
    (when neweqn
      (dolist (var (glvarsin neweqn))
        (or (assoc var oldvals)
            (gleqnconstantp var)
            (push (cons var 1) oldvals)))
      (* (cdr (assoc dvar oldvals))
         (/ (eval (sublis (append newvals oldvals) (caddr neweqn)))
            (eval (sublis oldvals (caddr neweqn))))) ) ))

; 07 Jun 04; 12 Apr 07
; determine how expr changes if dvar is doubled
; only powers of 2 are meaningful results; ignore others
(defun exprchanged (expr dvar)
  (let (vals)
    (dolist (var (glvarsin expr))
      (or (and (gleqnconstantp var) (numberp (eval var)))
          (push (cons var (random 1.0)) vals)))
    (safe-eval
      (list '/ (safe-eval (sublis (cons (cons dvar
                                              (* 2 (cdr (assoc dvar vals))))
                                        vals)
                                  expr))
                     (safe-eval (sublis vals expr)))) ))
 
; 07 Jun 04; 14 Dec 05
; Eval that avoids an error for divide by zero
(defun safe-eval (x &optional alist)
  (if (consp x)
      (let ((lhs (safe-eval (cadr x) alist))
            (rhs (safe-eval (caddr x) alist)))
        (case (car x)
          (/ (if (zerop rhs) 9999999 (/ lhs rhs)))
          (t (if (cddr x)
                 (funcall (car x) lhs rhs)
                 (funcall (car x) lhs)))))
      (if (symbolp x)
          (or (cdr (assoc x alist)) (eval x))
          (eval x) ) ) )

; 15 Jan 04
; make sure that an equation uses only binary operators,
; e.g. (+ a b c) -> (+ (+ a b) c)
(defun glbinarize (eqn)
  (if (consp eqn)
      (if (cdddr eqn)
          (glbinarize (cons (car eqn)
                            (cons (list (car eqn)
                                        (glbinarize (cadr eqn))
                                        (glbinarize (caddr eqn)))
                                  (cdddr eqn))))
          (list (car eqn)
                (glbinarize (cadr eqn))
                (glbinarize (caddr eqn))) )
      eqn))

; 13 Jan 04
; evaluate an expression given an alist of variable values
(defun evalexp (form alist)  (eval (sublis alist form)))

; 06 Feb 04; 12 Feb 04; 13 Feb 04; 18 Feb 04; 18 Mar 04; 26 Mar 04; 01 Apr 04
; 22 Apr 04; 16 May 06; 06 Jul 06; 12 Sep 06; 20 Feb 07; 21 Feb 07; 16 Apr 07
; 22 May 07; 16 Oct 07; 24 Nov 08
; Solve for desired var of a kind of object given defined
; and desired vars.
; defined is a list of items, (var (value units)) or (var value)
; e.g. (solvobjvar '((= DIAMETER (* 2 RADIUS)) (= AREA (* PI (EXPT RADIUS 2))))
;                  'radius '((area a)) )
;      (solvobjvar (get 'fall 'equations) 'time '((height (125 meter))))
;      (solvobjvar (get 'fall 'equations) 'time '((height (h meter))) nil t)
; codeflag is set to include the '(Q number unit) form in output
; quadflag can be set to use the negative solution of a quadratic
(defun solvobjvar (eqns goalvar defined &optional objtype codeflag quadflag)
  (let (eqn eqnb vars solved alist ulist sol (progress t) done tmp quad)
    (setq *equations-history* nil)
    (dolist (var defined)
      (if (consp (cadr var))              ; (<number> <units>)
          (progn (push (cons (car var) (caadr var)) alist)
                 (if (cadadr var)
                     (push (cons (car var) (cadadr var)) ulist)
                     (if (setq tmp (assumedvartype (car var) objtype))
                         (push (cons (car var) tmp) ulist))))
          (progn (push (cons (car var) (cadr var)) alist)
                 (if (setq tmp (assumedvartype (car var) objtype))
                     (push (cons (car var) tmp) ulist)) ) ) )
    (setq done (assoc goalvar alist))
    (while (and progress (not done))
      (setq progress nil)
      (setq solved (mapcar #'car alist))
      (when (setq eqn
                  (some #'(lambda (x)
                            (and (setq vars (set-difference
                                              (varsin x) solved))
                                 (= (length vars) 1)
                                 (not (and (consp (caddr x))
                                           (eq (caaddr x) 'tuple)))
                                 (not (solvboguseqn x (car vars) alist ulist))
                                 x))
                            eqns))
        (setq eqns (remove eqn eqns))
    ; try to change expr to isolate var on one side of the eqn
        (setq eqnb (glsolvefor eqn (car vars)))
        (if (null eqnb)        ; could not isolate the var
            (setq eqnb (glsolvequadb (glfixequation eqn) (car vars)
                                     alist  quadflag)))
        (if (and (consp (rhs eqnb))
                 (eq (car (rhs eqnb)) 'quote)
                 (consp (cadr (rhs eqnb)))
                 (eq (caadr (rhs eqnb)) 'q))
            (if (not (eq (lhs eqnb) goalvar))
                (progn (push (cons (car vars)
                                   (if codeflag
                                       (rhs eqnb)
                                     (cadadr (rhs eqnb))))
                             alist)
                       (push (cons (car vars) (caddr (cadr (rhs eqnb))))
                             ulist)
                       (solveeqhist (car vars) (cadadr (rhs eqnb))
                                    (caddr (cadr (rhs eqnb))) eqn eqnb) ))
            (progn (setq sol (eqn-eval (caddr eqnb) alist ulist))
                   (push (cons (car vars) (car sol)) alist)
                   (push (cons (car vars) (cadr sol)) ulist)
                   (solveeqhist (car vars) (car sol) (cadr sol) eqn eqnb) ))
        (setq progress t)
        (setq done (assoc goalvar alist)) ))
    (setq *equations-solved* alist)
    (setq *equations-units* ulist)
    (list (cdr done) (or (cdr (assoc goalvar ulist)) 1)) ))


; 07 Feb 04; 20 Apr 04; 11 Apr 07; 12 Apr 07
; vars in a formula, omitting constants (except t) and things multiplied by 0
(defun varsin (x) (varsinb x nil))
(defun varsinb (x vars)
  (if (null x)
      vars
      (if (symbolp x)
          (if (or (member x vars)
                  (gleqnconstantp x)
                  (member x *equations-constants*))
              vars
              (cons x vars))
          (if (and (consp x)
                   (not (member (car x) '(quote function))))
              (if (or (and (eq (car x) '*)
                           (or (and (numberp (cadr x)) (zerop (cadr x)))
                               (and (numberp (caddr x)) (zerop (caddr x)))))
                      (and (eq (car x) '/)
                           (numberp (cadr x)) (zerop (cadr x))))
                  vars
                  (dolist (z (cdr x) vars) (setq vars (varsinb z vars))))
              vars) )))

; 18 Feb 04; 05 Apr 04; 16 May 06
; calculate the change in goalvar for specified changes in other vars
; as specified by the alist defined.
; defined = vars that have defined values
; changed = vars that are changed: value is the proportional change.
; Returns a list (number constvars) or nil, where constvars are held constant
; e.g. (changeobjvar 'circle nil '((radius 2)) 'area)
(defun changeobjvar (objtype defined changed goalvar)
  (let (origval newval val lst solved constvars sol origvars newvars tmp)
  ; construct a set of vars with changes made
    (dolist (item defined)
      (push (if (assoc (car item) changed)
                (list (car item) (cons (* (caadr item) (cadr changed))
                                       (cdadr item)))
                item)
            newvars) )
    (dolist (item changed)
      (unless (assoc (car item) defined)
        (if (setq tmp (caddr (assoc (car item) (vars-units objtype))))
            (progn (push (list (car item) (* tmp (cadr item))) newvars)
                   (push (list (car item) tmp) origvars))
            (progn (push item newvars)
                   (push (list (car item) 1) origvars)))))
    (setq newval (solvobjvar (get objtype 'equations) goalvar newvars objtype))
  ; see if the given info is enough to solve it
    (if (and (consp newval)
             (numberp (car newval)))
        (progn (setq origval
                     (solvobjvar (get objtype 'equations) goalvar
                                 (append defined origvars) objtype))
               (setq val (/ (car newval) (car origval)) ))
  ; see if defining one still-undefined basis var will allow solving it
        (progn (setq lst (basis-vars objtype))
               (setq solved *equations-solved*)
               (while (and (not val) lst)
                 (if (not (or (eq (car lst) goalvar)
                              (assoc (car lst) solved)))
                     (progn (setq sol (changeobjvar objtype defined
                                                    (cons (list (car lst) 1)
                                                          changed)
                                                    goalvar))
                            (setq val (car sol))
                            (if val
                                (setq constvars (cons (car lst) (cadr sol))))))
                 (setq lst (cdr lst)) ) ) )
    (list val constvars) ))

; 14 Apr 04; 22 Apr 04
; Identify the form of a change in a variable
; e.g. (meta-change 'circle nil 'radius 'area)
(defun meta-change (objtype defined changevar goalvar)
  (let (chg)
    (setq chg (changeobjvar objtype defined (list (list changevar 2)) goalvar))
    (and (consp chg)
         (numberp (car chg))
         (some #'(lambda (x)
                   (if (< (abs (- (car x) (car chg))) 0.001) (cadr x)))
               '((1/8 inverse-cube) (1/4 inverse-square) (1/2 inverse)
                 (0.70710678118654746 inverse-square-root)
                 (0.79370052598409968 inverse-cube-root)(1 invariant)
                 (1.2599210498948732 cube-root)
                 (1.4142135623730951 square-root) (2 linear) (4 square)
                 (8 cube)) ) )))

; 11 Mar 04
; calculate transitions between energy band levels
; given a list of energy band levels, returns a list of possible transitions
; e.g. 1993 #34, Princeton #12
(defun energybands (lst)
  (let (res)
    (while lst
      (dolist (x (cdr lst))
        (pushnew (abs (- (car lst) x)) res))
      (setq lst (cdr lst)) )
    res))

; 11 Mar 04; 12 Apr 04
; calculate relative differences between two quantities, each of which is
; of the form (number units)
; Returns NIL if failure, else numeric ratio
(defun reldiff (q1 q2)
  (let ((unitconv 1) n1 maxq diff tempconv)
    (when (and (consp q1) (consp q2)
               (numberp (car q1))
               (numberp (car q2)))
        (if (and (cadr q1) (cadr q2))
            (if (and (basictempunit (cadr q1))
                     (basictempunit (cadr q2))
                     (setq tempconv (tempconvert (cons '* q2) (cadr q1))))
                (setq q2 (list tempconv (cadr q1)))
                (if (and (glunitp (cadr q1)) (glunitp (cadr q2)))
                    (setq unitconv (glconvertunit (cadr q1) (cadr q2))))))
      (when unitconv
        (setq n1 (* (car q1) unitconv))
        (setq diff (- n1 (car q2)))
        (setq maxq (if (or (zerop n1) (zerop (car q2)))
                       1
                       (max (abs n1) (abs (car q2)))))
        (abs (/ diff maxq)))) ))


; 24 Mar 04; 29 Mar 04; 20 Apr 04; 19 Feb 07; 20 Feb 07; 12 Apr 07
; evaluate an expression, given alists of numeric values and units of variables
; returns a list, (<value> <units>)
; old:   returns a (Q <number> <units>) form, or a symbolic expression
(defun eqn-eval (expr vals units)
  (let (res un)
    (if (consp expr)
        (if (eq (op expr) 'q)
            (list expr (caddr expr))
            (if (and (eq (op expr) 'quote)
                     (consp (cadr expr))
                     (eq (caadr expr) 'q))
                (list (cadr expr) (caddr (cadr expr)))
                (eqn-evalq expr (eqn-eval (lhs expr) vals units)
                                (and (cddr expr)
                                     (eqn-eval (rhs expr) vals units)))))
        (if (symbolp expr)
            (if (setq res (or (assoc expr vals)
                              (and (gleqnconstantp expr)
                                   (cons expr (eval expr)))))
                (if (numberp (cdr res))
                    (list (cdr res)
                          (if (setq un (assoc expr units))
                              (cdr un)
                              'unity))
                    (list (cdr res) (cdr (assoc expr units))))
                (list expr 'unity))
            (and (numberp expr) (list expr 'unity)))) ))

; 25 Mar 04; 26 Mar 04; 02 Apr 04; 05 Apr 04; 16 Apr 04; 19 Apr 05; 16 May 06
; 30 Oct 06; 19 Feb 07; 20 Feb 07; 02 Apr 07; 01 Jun 07; 26 Jul 08
; Evaluate/create an expression
; lhsv, rhsv are (<value> <units>); returns the same form.
(defun eqn-evalq (expr lhsv rhsv)
  (let (uconv un (lhsn (car lhsv)) (lhsu (cadr lhsv))
                 (rhsn (car rhsv)) (rhsu (cadr rhsv)))
    (case (op expr)
          (* (eqn-makeq (eqn-make '* lhsn rhsn)
                        (eqn-simu (list (op expr) lhsu rhsu))))
          (/ (eqn-makeq (if (and (numberp rhsn)
                                 (= 0 rhsn))
                            1.0e33               ; in case of divide by 0
                            (eqn-make '/ lhsn rhsn))
                        (eqn-simu (list (op expr) lhsu rhsu))))
          ((+ -) (if (cddr expr)   ; test for binary op
                     (progn
                       (setq uconv
                             (if (or (equal lhsu rhsu)
                                     (equal 0 lhsn)
                                     (equal 0 rhsn))
                                 1
                                 (or (glconvertunit rhsu lhsu)
                                     (error "incompatible units ~A ~A~%"
                                            rhsu lhsu))))
                       (eqn-makeq (list (op expr) lhsn
                                        (if (= uconv 1)
                                            rhsn
                                            (eqn-make '* uconv rhsn)))
                                  (if (equal 0 lhsn) rhsu lhsu)))
                     (eqn-makeq (list (op expr) lhsn) lhsu)))
          ((expt |expt|)
             (setq un 1)
             (dotimes (i (abs rhsn))
               (setq un (glsimplifyunit (list '* un lhsu))))
             (if (< rhsn 0)
                 (setq un (list '/ 1 un)))
             (eqn-makeq (eqn-make 'expt lhsn rhsn) un))
          ((sin cos asin acos tan exp log log2 log10
                |sin| |cos| |asin| |acos| |tan| |exp| |log| |log2| |log10|)
            (if (setq un (glconvertunit lhsu 'unity))
                (eqn-makeq (list (op expr) (if (= un 1)
                                               lhsn (eqn-make '* un lhsn))) ; Won
                           (if (member (op expr) '(asin acos |asin| |acos|))
                               'radian 'unity))                ; Won/GSN
                (error "incompatible units ~A ~A~%" (op expr) lhsu)))
          ((atan |atan|)
            (if rhsv
                (if (glconvertunit rhsu lhsu)
                    (eqn-makeq (eqn-make (op expr) lhsn rhsn)
                               'radian)                            ; Won
                    (error "incompatible units ~A ~A~%"
                           rhsu lhsu))
                (eqn-makeq (list (op expr) lhsn) 'radian)) )       ; Won
          ((sqrt |sqrt|) (eqn-makeq (list 'sqrt lhsn)
                                    (glsqrtunit lhsu)))
          ((cbrt |cbrt|) (eqn-makeq (list 'cbrt lhsn)
                                    (glcbrtunit lhsu)))
          (t (error "eqn-evalq: unknown op ~A ~A ~A~%" expr lhsv rhsv)) ) ))

; 12 Apr 07
; Make an expression (value unit)
; possibly removing multiplicative constant from unit
(defun eqn-makeq (value unit)
  (let (num mul)
    (if (or (numberp value)
            (gleqnconstantp value)
            (and (consp value)
                 (every #'numberp (cdr value))))
        (progn
          (setq num (eval value))
          (if (and (consp unit)
                   (eq (car unit) '*)
                   (setq mul (eqn-numval (cadr unit))))
              (eqn-makeq (* mul (eqn-numval num))
                         (if (cdddr unit)
                             (cons '* (cddr unit))
                             (caddr unit)))
              (if (and (consp unit)
                       (eq (car unit) '/)
                       (setq mul (eqn-numval (cadr unit)))
                       (not (equal mul 1)))
                  (eqn-makeq (* mul (eqn-numval num))
                             (glsimplifyunit (list '/ 1 (caddr unit))))
                  (list num unit))))
        (list value unit) ) ))

; 25 Mar 04; 26 Mar 04; 12 Apr 07
; get the numeric value of a unit multiplier such as kilo, else nil
(defun eqn-numval (unit)
  (if (numberp unit)
      unit
      (if (symbolp unit)
          (if (gleqnconstantp unit)
              (eval unit)
              (and (eq (get unit 'glunittype) 'dimensionless)
                   (get unit 'glsiconversion))))))

; 20 Apr 04; 19 Apr 04
; make a binary op expression from components, doing partial evaluation
(defun eqn-make (op lhs rhs)
  (if (and (member op '(* / expt |expt|)) (numberp lhs) (zerop lhs))
      0
    (if (and (eq op '*) (numberp rhs) (zerop rhs))
        0
      (if (and (member op '(+ -)) (numberp rhs) (zerop rhs))
          lhs
        (if (and (eq op '+) (numberp lhs) (zerop lhs))
            rhs
          (if (and (eq op '-) (numberp lhs) (zerop lhs))
              (list '- rhs)
            (if (and (numberp lhs) (numberp rhs))
                (funcall op lhs rhs)
              (list op lhs rhs) )))))) )

; 26 Jul 08
; simplify a unit expression
(defun eqn-simu (unit)
  (if (consp unit)
      (case (car unit)
        (* (if (eq (lhs unit) 'unity)
               (if (cdddr unit)
                   (eqn-simu (cons (first unit) (cddr unit)))
                   (rhs unit))
               (if (eq (rhs unit) 'unity)
                   (if (cdddr unit)
                       (eqn-simu (cons (first unit) (cons (lhs unit)
                                                          (cddr unit))))
                       (lhs unit))
                   (glsimplifyunit unit))))
        (/ (if (eq (rhs unit) 'unity)
               (lhs unit)
               (glsimplifyunit unit)))
        (t (glsimplifyunit unit))) ) )

; 12 Apr 04; 07 Jun 04; 08 Jun 04
; choose the best answer from a multiple-choice answer set
; choices is coded
(defun bestans (answer choices)
  (let ((bestval 99999) (answers '(a b c d e)) diff ans exp bestans)
    (case (car choices)
      (num (dolist (c (cdr choices))
             (if (and (consp c) (numberp (car c))
                      (consp answer) (numberp (car answer)))
                 (progn (setq diff (reldiff answer c))
                        (when (and (numberp diff)
                                   (< diff bestval))
                          (setq bestval diff)
                          (setq ans (car answers))))
               )
             (setq answers (cdr answers)) )
           (and (< bestval 0.1) ans) )
      (expr (setq exp (unvar-copy answer))
            (dolist (c (cdr choices))
              (if (random-equal exp c)
                  (progn (setq bestans c)
                         (setq ans (car answers))))
              (setq answers (cdr answers)))
            (if (and ans (not (expr-equal exp bestans)))
                (format t "Failed to verify ~A = ~A~%" exp bestans))
            ans)
      (t nil)) ))

(defvar *eqn-powers* (make-array 20 :initial-element 0))
(defvar *eqn-vars* (make-array 20))
(defvar *eqn-nvars* 0)
(defvar *eqn-multiplier* 1)

; 22 Apr 04; 23 Apr 04
; optimize an expression composed of product, quotient, sqrt
(defun eqn-optprod (expr)
  (let (newex tmp)
    (setq *eqn-nvars* 0)
    (setq *eqn-multiplier* 1)
    (eqn-optpr expr 1)
    (dotimes (i (truncate *eqn-nvars* 2))
      (setq tmp (aref *eqn-vars* i))
      (setf (aref *eqn-vars* i) (aref *eqn-vars* (- (1- *eqn-nvars*) i)))
      (setf (aref *eqn-vars* (- (1- *eqn-nvars*) i)) tmp)
      (setq tmp (aref *eqn-powers* i))
      (setf (aref *eqn-powers* i) (aref *eqn-powers* (- (1- *eqn-nvars*) i)))
      (setf (aref *eqn-powers* (- (1- *eqn-nvars*) i)) tmp) )
    (setq newex (eqn-formexpr))
    (if (not (equal *eqn-multiplier* 1))
        (if (numberp newex)
            (* newex *eqn-multiplier*)
            (list '* newex *eqn-multiplier*))
        newex)))

; 19 Apr 05
(defun eqn-optpr (expr power)
  (if (numberp expr)
      (setq *eqn-multiplier* (* *eqn-multiplier* (expt expr power)))
    (if (symbolp expr)
        (eqn-accumpower expr power)
      (if (consp expr)
          (case (car expr)
            (* (dolist (x (cdr expr)) (eqn-optpr x power)))
            (/ (eqn-optpr (cadr expr) power)
               (eqn-optpr (caddr expr) (- power)))
            ((expt |expt|)
              (if (numberp (caddr expr))
                  (eqn-optpr (cadr expr) (* power (caddr expr)))
                  (error "expt expression ~A~%" expr)))
            ((sqrt |sqrt|) (eqn-optpr (cadr expr) (/ power 2)))
            (- (if (null (cddr expr))
                   (setq *eqn-multiplier* (* *eqn-multiplier* (expt -1 power)))
                   (eqn-accumpower expr power)))
            (t (eqn-accumpower expr power)))))))

(defun eqn-accumpower (expr power)
  (let (n)
    (dotimes (i *eqn-nvars*) (if (equal expr (aref *eqn-vars* i)) (setq n i)))
    (if (null n)
        (progn (setq n *eqn-nvars*)
               (incf *eqn-nvars*)
               (setf (aref *eqn-powers* n) 0)
               (setf (aref *eqn-vars* n) expr)))
    (incf (aref *eqn-powers* n) power) ))

; form a new expression from powers of terms
(defun eqn-formexpr ()
  (let ((num 1) (den 1) (newn 0) pow var)
    (dotimes (i *eqn-nvars*)
      (setq pow (aref *eqn-powers* i))
      (setq var (aref *eqn-vars* i))
      (if (not (zerop pow))
          (if (integerp pow)
              (if (minusp pow)
                  (setq den (eqn-formmul (if (> (abs pow) 1)
                                             (list 'expt var (abs pow))
                                             var)
                                         den))
                  (setq num (eqn-formmul (if (> pow 1)
                                             (list 'expt var pow)
                                             var)
                                         num)))
              (progn (setf (aref *eqn-powers* newn) (* pow 2))
                     (setf (aref *eqn-vars* newn) var)
                     (incf newn)) ) ) )
    (if (> newn 0)
        (progn (setq *eqn-nvars* newn)
               (setq num (eqn-formmul num (list 'sqrt (eqn-formexpr))))))
    (if (equal den 1)
        num
        (list '/ num den)) ))

(defun eqn-formmul (x y)
  (if (equal x 1)
      y
      (if (equal y 1)
          x
          (list '* x y))))

; 07 Jun 04; 08 Jun 04; 12 Apr 07
; Compare two expressions for algebraic equality
; search is used to try different methods for harder cases.
(defun expr-equal (x y)
  (let (simx simy res)
    (or (equal x y)
        (if (or (consp x) (consp y))
            (or (equal (setq simx (gleqns-simplify x))
                       (setq simy (gleqns-simplify y)))
                (and (consp x) (consp y) (eq (car x) (car y))
                     (every #'expr-equal (cdr x) (cdr y)))
                (and (not (or (safe-zerop simy)
                              (and (gleqnconstantp simy)
                                   (safe-zerop (eval simy)))))
                     (setq res (gleqns-simplify (list '/ x y)))
                     (or (equal res 1)
                         (and (numberp res) (nearly-equal res 1))))
                (and (setq res (gleqns-simplify (list '- simx simy)))
                     (or (equal res 0)
                         (and (numberp res) (nearly-equal res 0))))
                (expr-equal-factor simx simy)
                (expr-equal-perm simx simy) ) ) ) ))

; 07 Jun 04; 08 Jun 04
; Try to remove factors from expressions, test for equality
(defun expr-equal-factor (x y)
  (let ((vars (glvarsin x)) fact new)
    (some #'(lambda (var)
              (setq fact (exprchanged x var))
              (or (and (or (nearly-equal fact 2) (nearly-equal fact 4)
                           (nearly-equal fact 8))
                       (setq new (expr-pushfact '/ x var))
                       (< (expr-size new) (expr-size x))
                       (expr-equal new (expr-pushfact '/ y var)))
                  (and (or (nearly-equal fact 1/2) (nearly-equal fact 1/4)
                           (nearly-equal fact 1/8))
                       (setq new (expr-pushfact '* x var))
                       (< (expr-size new) (expr-size x))
                       (expr-equal new (expr-pushfact '* y var))) ))
          vars)))

; 07 Jun 04; 08 Jun 04
; Try to test permutations of expressions, test for equality
(defun expr-equal-perm (x y)
  (and (consp x) (consp y)
       (if (and (eq (op x) '+) (eq (op y) '+))
           (or (and (expr-equal (lhs x) (lhs y))
                    (expr-equal (rhs x) (rhs y)))
               (and (expr-equal (lhs x) (rhs y))
                    (expr-equal (rhs x) (lhs y))))
         (if (and (eq (op x) '-) (eq (op y) '-))
             (and (expr-equal (lhs x) (lhs y))
                  (expr-equal (rhs x) (rhs y)))
           (if (and (member (op x) '(+ -)) (member (op y) '(* /)))
               (expr-equal-perm x (pushinfact y))
               (if (and (member (op y) '(+ -)) (member (op x) '(* /)))
                   (expr-equal-perm (pushinfact x) y)))))))

(defun nearly-equal (x y) (< (abs (- x y)) 0.000001))

(defun set-equal (x y) (and (subsetp x y :test #'equal)
                            (subsetp y x :test #'equal)))

(defun safe-zerop (x) (and (numberp x) (zerop x)) )

; 07 Jun 04; 08 Jun 04; 12 Apr 07
; Compare two expressions for numeric equality using random variable values
(defun random-equal (x y)
  (let (vars vals)
    (setq vars (varsin x))
    (if (set-equal vars (varsin y))
        (progn
          (dolist (var vars)
            (or (gleqnconstantp var)
                (push (cons var (random 1.0)) vals)))
          (< (reldiff (list (safe-eval (sublis vals x)) 'unity)
                      (list (safe-eval (sublis vals y)) 'unity))
             0.0001)) ) ))

; 07 Jun 04; 12 Apr 07
; size of a binary expression: operators + variables
(defun expr-size (x)
  (if (consp x)
      (+ 1 (expr-size (cadr x)) (expr-size (caddr x)))
      (if (gleqnconstantp x) 0 1)))

; 07 Jun 04
; push a factor into an expression
; we expect this factor to cancel out
(defun expr-pushfact (op x var)
  (let (newx newarg)
    (setq newx (gleqns-simplify (list op x var)))
    (if (< (expr-size newx) (expr-size x))
        newx
      (if (consp x)
          (case (op x)
            ((+ -) (gleqns-simplify
                     (cons (op x)
                           (mapcar #'(lambda (arg) (expr-pushfact op arg var))
                                   (cdr x)))))
            ((* /)
              (setq newarg (expr-pushfact op (lhs x) var))
              (if (< (expr-size newarg) (expr-size (lhs x)))
                  (gleqns-simplify (list (op x) newarg (rhs x)))
                (if (eq (op x) '*)
                    (progn (setq newarg (expr-pushfact op (rhs x) var))
                           (if (< (expr-size newarg) (expr-size (rhs x)))
                               (gleqns-simplify (list (op x) (lhs x) newarg))
                               newx))
                    (progn (setq newarg
                                 (expr-pushfact (if (eq op '*) '/ '*)
                                                (rhs x) var))
                           (if (< (expr-size newarg) (expr-size (rhs x)))
                               (gleqns-simplify (list (op x) (lhs x) newarg))
                               newx)))))
            (t newx))
          newx)) ))

; 07 Jun 04
; Copy an expression, removing VAR- prefix of variables
(defun unvar-copy (x)
  (if (consp x)
      (cons (unvar-copy (car x)) (unvar-copy (cdr x)))
      (if (and x (symbolp x))
          (let ((str (symbol-name x)))
            (if (and (> (length str) 4)
                     (string= str "VAR-" :end1 4))
                (intern (subseq str 4))
                x))
          x)))

; 08 Jun 04
; try to push in multiplicative factors until a + or - is reached.
(defun pushinfact (x) (pushinfactb x 1))
(defun pushinfactb (x factor)
  (let ()
    (and (consp x)
         (if (eq (car x) '*)
             (if (factorp (cadr x))
                 (pushinfactb (caddr x) (makefactor factor (cadr x)))
                 (if (factorp (caddr x))
                     (pushinfactb (cadr x) (makefactor factor (caddr x)))))
             (if (eq (car x) '/)
                 (if (factorp (caddr x))
                     (pushinfactb (cadr x) (makequot factor (caddr x))))
                 (if (and (member (car x) '(+ -)) (cddr x))
                     (list (car x)
                           (gleqns-simplify (makefactor factor (cadr x)))
                           (gleqns-simplify (makefactor factor (caddr x)))
                           )))))))

; 08 Jun 04; 19 Apr 05
; Test whether x is a factor
(defun factorp (x)
  (or (atom x)
      (and (consp x)
           (member (car x) '(* / sqrt expt sin cos tan
                               |sqrt| |expt| |sin| |cos| |tan|))
           (factorp (cadr x))
           (factorp (caddr x)))))

; 08 Jun 04
; Make a product
(defun makefactor (x y)
  (if (equal x 1)
      y
      (if (and (consp x)
               (eq (car x) '/)
               (equal (cadr x) 1))
          (list '/ y (caddr x))
          (list '* x y))))

; 08 Jun 04
; Make a quotient
(defun makequot (x y) (list '/ x y))

; 07 Apr 04
; Find all variables of a kind of object
(defun eqn-allvars (objtype)
  (let ((vars (basis-vars objtype)))
    (dolist (eqn (equations objtype))
      (setq vars (union vars (varsin eqn))) )
    vars))

; 08 Dec 05; 11 Apr 07; 24 Sep 08
; Fix an equation so that the lhs is a single variable
; (glfixequation '(= (* f time) (* m v)))
(defun glfixequation (eqn)
  (let ((op (car eqn)) (lhs (cadr eqn)) (rhs (caddr eqn)) new lhsvars rhsvars)
    (if (symbolp lhs)
        eqn
        (if (symbolp rhs)
            (list op rhs lhs)
            (progn (setq lhsvars (varsin lhs))
                   (setq rhsvars (varsin rhs))
                   (if (and lhsvars
                            (or (null rhsvars)
                                (<= (length lhsvars) (length rhsvars))))
                       (glsimplesolvefor (list '= rhs lhs)
                                         (first (last lhsvars)))
                       (if rhsvars
                           (glsimplesolvefor eqn (first (last rhsvars)))))))) ))

; 08 Dec 05
; Fix an equation so that the lhs is a single variable
; (glfixequation '(= (* f t) (* m v)))
(defun glfixequation-old (eqn)
  (let ((op (car eqn)) (lhs (cadr eqn)) (rhs (caddr eqn)) new)
    (if (symbolp lhs)
        eqn
        (if (symbolp rhs)
            (list op rhs lhs)
          (if (and (consp lhs) (consp rhs)
                   (or (eq (car lhs) '+)
                       (and (eq (car lhs) '-) (cddr lhs)))
                   (or (eq (car rhs) '+)
                       (and (eq (car rhs) '-) (cddr rhs))))
              (progn
                (setq new (glsolvefor (list '= 0 (list '- lhs rhs))
                                      (first (last (glvarsin eqn)))))
                (list '= (cadr new) (gleqns-simplify (caddr new))))
              (progn
                (setq new (glsolvefor (list '= 1 (list '/ lhs rhs))
                                      (first (last (glvarsin eqn)))))
                (list '= (cadr new) (gleqns-simplify (caddr new)))) ) ) ) ))

; 16 May 06; 02 Aug 06
; Record equation solving history (backwards)
(defun solveeqhist (var value units orig-equation new-equation)
  (push (list var value units orig-equation new-equation)
        *equations-history*) )

; 16 May 06; 02 Aug 06
; Filter equation solving history, leaving only relevant steps.
(defun filtereqhist (history goalvar)
  (let (newhist needed)
    (setq needed (list goalvar))
    (dolist (histitem history)
      (if (member (car histitem) needed)     ; if this var was needed
          (progn (push histitem newhist)
                 (setq needed (union needed
                                     (varsin (third (fifth histitem))))))))
    newhist))

; 02 Aug 06; 09 Oct 06; 27 Dec 06; 21 Nov 08
; Explain solution process for var given history
; (phys '(what is the volume of a cone with circumference 3 m
;          and height = 4 m))
; (explaineqhist)
(defun explaineqhist (&optional infix html history goalvar)
  (let (newhist)
    (or history (setq history *equations-history*))
    (or goalvar (setq goalvar (caar history)))
    (setq newhist (filtereqhist history goalvar))
    (dolist (item newhist)
      (if (fourth item)
          (progn
            ; if already solved for desired var, don't explain
            (if (not (equal (fourth item) (fifth item)))
                (progn
                  (format t "Solved equation ~A~%   for ~A~%   giving ~A~%"
                        (if infix (infixstr (fourth item) html) (fourth item))
                        (first item)
                        (if infix (infixstr (fifth item) html) (fifth item)))
                  (if html (format t "<BR>"))))
            ; if a constant such as gravity, don't explain
            (if (not (and (consp (third (fifth item)))
                          (eq (car (third (fifth item))) 'quote)))
                (progn
                  (format t "Evaluated ~A~%   giving ~A = ~A ~A~%"
                        (if infix (infixstr (fifth item) html) (fifth item))
                        (first item)
                        (if infix (infixstr (second item) html) (second item))
                        (or (third item) " "))
                  (if html (format t "<BR>")) ) ) ) ) ) ))

; 22 May 07; 24 Nov 08
; Test for a bogus equation, e.g. one that will divide by zero
(defun solvboguseqn (eqn var alist ulist)
  (let (eqnb)
    (and (setq eqnb (glsolvefor eqn var))
         (eq (lhs eqnb) var)
         (solvbogusexpr (rhs eqnb) alist ulist)) ) )

; 22 May 07; 01 Jun 07; 10 Nov 08; 24 Nov 08
; Test for a bogus expression, e.g. one that includes divide by zero
(defun solvbogusexpr (exp alist ulist)
  (and (consp exp)
       (case (op exp)
         ((+ - * expt |expt| atan |atan|)
           (or (solvbogusexpr (lhs exp) alist ulist)
               (solvbogusexpr (rhs exp) alist ulist)))
         (/ (or (solvbogusexpr (lhs exp) alist ulist)
                (solvbogusexpr (rhs exp) alist ulist)
                (solvzeroexpr (rhs exp) alist ulist)))
         ((sin |sin| cos |cos| exp |exp| log |log|
               log2 |log2| log10 |log10| sqrt |sqrt| cbrt |cbrt|)
           (solvbogusexpr (lhs exp) alist ulist))
         ((tan |tan|)
           (or (solvbogusexpr (lhs exp) alist ulist)
               (solvzeroexpr (list 'cos (lhs exp)) alist ulist)))
         ((acos |acos| asin |asin|)
           (or (solvbogusexpr (lhs exp) alist ulist)
               (solvzeroexpr (lhs exp) alist ulist)))
         (t nil))))

; 22 May 07; 01 Jun 07; 20 Nov 08; 21 Nov 08; 24 Nov 08
; Test for a zero expression
(defun solvzeroexpr (exp alist ulist)
  (let (val)
    (or (and (numberp exp) (zerop exp))
        (and (symbolp exp)
             (setq val (assoc exp alist))
             (numberp (cdr val))
             (zerop (cdr val)))
        (and (consp exp)
             (case (op exp)
               (* (or (solvzeroexpr (lhs exp) alist ulist)
                      (solvzeroexpr (rhs exp) alist ulist)))
               ((/ sqrt |sqrt| cbrt |cbrt|)
                 (solvzeroexpr (lhs exp) alist ulist))
               ((sin |sin| tan |tan|)
                 (setq val (eqn-eval (lhs exp) alist ulist))
                 (if (and (numberp (car val))
                          (member (cadr val) '(degree |*degree|)))
                     (setq val (list (* (car val) (/ pi 180))
                                     'radian)))
                 (or (solvzeroexpr (lhs exp) alist ulist)
                     (and (numberp (car val))
                          (or (approx= (car val) 0)
                              (approx= (car val) pi)
                              (approx= (car val) (- pi))))))
               ((cos |cos|)
                 (setq val (eqn-eval (lhs exp) alist ulist))
                 (if (and (numberp (car val))
                          (member (cadr val) '(degree |*degree|)))
                     (setq val (list (* (car val) (/ pi 180))
                                     'radian)))
                 (and (numberp (car val))
                      (or (approx= (car val) (/ pi 2))
                          (approx= (car val) (- (/ pi 2)))
                          (approx= (car val) (/ (* 3 pi) 2)))))
               ((log |log| log2 |log2| log10 |log10|)
                 (and (setq val (eqn-eval (lhs exp) alist ulist))
                      (numberp (car val))
                      (approx= (car val) 1.0)))
               (t nil))) ) ))

; 23 Jan 08
; Find vars needed given equation, known, and desired vars
(defun eqn-needed (equation known desired)
  (let ((vars (glvarsin equation)))
    (set-difference (set-difference vars known) desired) ))

; 07 Nov 08
; ****** uses list alist rather than cons alist
; ****** does not handle units
; used by conprop.lsp
; Solve a set of equations given an alist (var value)* of values.
; returns alist of solved variables
(defun solveqnset (equations values)
  (let ((progress t) known vars diffvars newvals neweqn pair)
    (while progress
      (setq progress nil)
      (dolist (equation equations)
        (setq vars (varsin equation))
        (setq diffvars (set-difference vars (mapcar #'car values)))
        (when (and diffvars (null (cdr diffvars)))  ; exactly 1
          (setq neweqn (glsolvefor equation (first diffvars)))
          (setq newval (eval (sublis (sublisify values) (rhs neweqn))))
          (setq pair (list (first diffvars) newval))
          (push pair newvals)
          (push pair values)
          (setq progress t) ) ) )
    newvals))

; 21 Nov 08
; test approximate equality
(defun approx= (x y &optional (tolerance 0.00001))
  (or (equal x y)
      (and (numberp x) (numberp y)
           (< (abs (- x y)) (* tolerance (max (abs x) (abs y)))))))
;   mathpat.lsp              Gordon S. Novak Jr.           ; 07 Feb 07

; Copyright (c) 2007 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; Math simplification patterns

; 19 Oct 94; 05 Jan 95; 02 Jan 97; 03 Jan 00; 03 Mar 04; 11 Mar 04; 20 Apr 04
; 23 Apr 04; 27 Apr 04; 07 Jun 04; 08 Jun 04; 14 Dec 05; 28 Dec 05; 28 Dec 05

(gldefpatterns 'math
'(
((* (sin ?x) (cos ?x))     (* 1/2 (sin (* 2 ?x))))
((* (cos ?x) (sin ?x))     (* 1/2 (sin (* 2 ?x))))

((- ?x ?x)                 0)
((+ ?x ?n)                 (+ ?n ?x)     (numberp ?n))
((+ 0 ?x)                 ?x)
((- 0 ?x)                  (- ?x))
((+ 0.0 ?x)               ?x)
((1+ (+ ?n ?x))            (+ (1+ ?n) ?x)     (numberp ?n))
((1+ (- ?n ?x))            (- (1+ ?n) ?x)     (numberp ?n))
((1+ (- ?x ?n))            (- ?x (1- ?n))     (numberp ?n))
((1- (+ ?n ?x))            (+ (1- ?n) ?x)     (numberp ?n))
((1- (- ?n ?x))            (- (1- ?n) ?x)     (numberp ?n))
((1- (- ?x ?n))            (- ?x (1+ ?n))     (numberp ?n))
((+ ?n (1+ ?x))            (+ (1+ ?n) ?x)     (numberp ?n))
((- ?n (1+ ?x))            (- (1- ?n) ?x)     (numberp ?n))
((- (1+ ?x) ?n)            (- ?x (1- ?n))     (numberp ?n))
((+ ?n (1- ?x))            (+ (1- ?n) ?x)     (numberp ?n))
((- ?n (1- ?x))            (- (1+ ?n) ?x)     (numberp ?n))
((- (1- ?x) ?n)            (- ?x (1+ ?n))     (numberp ?n))
((+ (- ?n ?x) (- ?m ?y))     (- (+ ?n ?m) (+ ?x ?y)) (and (numberp ?n) (numberp ?m)))
((- ?n (- ?m ?x))           (+ (- ?n ?m) ?x) (and (numberp ?n) (numberp ?m)) )
((+ ?n (- ?m ?x))           (- (+ ?n ?m) ?x) (and (numberp ?n) (numberp ?m)))
((+ ?n (+ ?m ?x))           (+ (+ ?n ?m) ?x) (and (numberp ?n) (numberp ?m)))
((- ?x ?n)                 (+ (- ?n) ?x)     (numberp ?n))
((+ (+ ?n ?x) (+ ?m ?y))     (+ (+ ?n ?m) (+ ?x ?y)) (and (numberp ?n) (numberp ?m)))
((- ?n (+ ?m ?x))           (- (- ?n ?m) ?x) (and (numberp ?n) (numberp ?m)))
((- (- ?x ?y))             (- ?y ?x))
((- (- ?x ?y) ?y)           (- ?x (* 2 ?y)))
((- (- ?x ?y) ?z)           (- ?x (+ ?y ?z)))              ; 19 Oct 94 ?
((- (* ?n ?x))             (* (- ?n) ?x)     (numberp ?n))
((- (/ ?n ?x))             (/ (- ?n) ?x)     (numberp ?n))
((+ (- ?n ?x) ?y)           (+ ?n (- ?y ?x))     (numberp ?n))
((- ?x (- ?n ?y))           (+ (- ?n) (+ ?x ?y))     (numberp ?n))
((- (- ?n ?x) ?y)           (- ?n (+ ?x ?y))     (numberp ?n))
((- (* ?n ?x) (* ?n ?y))     (* ?n (- ?x ?y))     (numberp ?n))
((+ (* ?n ?x) (* ?n ?y))     (* ?n (+ ?x ?y))     (numberp ?n))
((- (+ ?x ?y) ?x)           ?y)
((- (+ ?x ?y) ?y)           ?x)
((- (+ ?x ?y) (+ ?y ?x))    0)
((- ?x (- ?x ?y))           ?y)
((- ?x (+ ?x ?y))           (- ?y))
((- (* ?n ?x) (+ ?x ?y))     (- (* (- ?n 1) ?x) ?y)     (numberp ?n))
((- (* ?n ?x) (- ?x ?y))     (+ (* (- ?n 1) ?x) ?y)     (numberp ?n))
((- ?x (* ?n (- ?x ?y)))     (+ (* (- 1 ?n) ?x) (* ?n ?y))     (numberp ?n))
((min ?x (+ ?x ?y))         (+ ?x (min 0 ?y)))     
((min ?x (+ ?y ?x))         (+ ?x (min 0 ?y)))
((max ?x (+ ?x ?y))         (+ ?x (max 0 ?y)))
((max ?x (+ ?y ?x))         (+ ?x (max 0 ?y)))
((- ?x ?n)                 (+ (- ?n) ?x)     (numberp ?n))
((- (+ ?x ?y) ?x)           ?y)
((- (+ ?y ?x) ?x)           ?y)
((+ ?x (- ?y ?x))           ?y)
((- ?x (+ ?n ?y))           (+ (- ?n) (- ?x ?y))     (numberp ?n))
((- ?x (+ ?y ?n))           (+ (- ?n) (- ?x ?y))     (numberp ?n))
((* ?x ?n)                 (* ?n ?x)     (numberp ?n))
((* ?n (+ ?m ?x))           (+ (* ?m ?n) (* ?n ?x))(and (numberp ?n) (numberp ?m)) )
((* ?n (- ?m ?x))           (- (* ?m ?n) (* ?n ?x))(and (numberp ?n) (numberp ?m)) )
((* ?n (/ ?x ?m))           (* (/ ?n ?m) ?x)(and (numberp ?n) (numberp ?m)) )
((* ?n (/ ?m ?x))           (/ (* ?n ?m) ?x)(and (numberp ?n) (numberp ?m)) )
((* ?n (* ?m ?y))           (* (* ?n ?m) ?y)(and (numberp ?n) (numberp ?m)) )
((* ?n (+ (* ?m ?x) (* ?k ?y))) (+ (* (* ?n ?m) ?x) (* (* ?n ?k) ?y))(and (numberp ?n) (numberp ?m)) )
((* ?n (- (* ?m ?x) (* ?k ?y))) (- (* (* ?n ?m) ?x) (* (* ?n ?k) ?y))(and (numberp ?n) (numberp ?m)) )
((* 1 ?x)                 ?x)
((* 1.0 ?x)               ?x)
((* -1 ?x)                (- ?x))
((* -1.0 ?x)              (- ?x))
((* 0 ?x)                 0)
((* 0.0 ?x)               0)
((* (- ?x) ?y)             (- (* ?x ?y)))
((* ?x (- ?y))             (- (* ?x ?y)))
((- (- ?x))               ?x)
((* ?x (* ?n ?y))           (* ?n (* ?x ?y))     (numberp ?n))
((* (* ?n ?x) ?y)           (* ?n (* ?x ?y))     (numberp ?n))
((* ?x (/ ?n ?y))           (* ?n (/ ?x ?y))     (numberp ?n))
((* (/ 1 ?y) ?x)           (/ ?x ?y))
((* ?x (/ 1 ?y))           (/ ?x ?y))
((* (/ 1.0 ?y) ?x)         (/ ?x ?y))
((* ?x (/ 1.0 ?y))         (/ ?x ?y))
((* ?x ?x)                 (expt ?x 2))
((* (/ ?n ?y) ?x)           (* ?n (/ ?x ?y))
                              (and (numberp ?n) (not (numberp ?y))) )
((* ?x (/ ?n ?y))           (* ?n (/ ?x ?y))     (numberp ?n))
((* ?x (/ ?y ?x))           ?y)
((* (/ ?y ?x) ?x)           ?y)
((* ?x (+ (/ ?y ?x) (/ ?z ?x))) (+ ?y ?z))
((* ?x (- (/ ?y ?x) (/ ?z ?x))) (- ?y ?z))
((* (+ (/ ?y ?x) (/ ?z ?x)) ?x) (+ ?y ?z))
((* (- (/ ?y ?x) (/ ?z ?x)) ?x) (- ?y ?z))
((/ 0 ?x)                   0)
((/ ?x ?x)                  1)
((/ ?n (* ?m ?x))           (/ (/ ?n ?m) ?x)
                              (and (numberp ?n) (numberp ?m) (not (zerop ?m))))
((/ (* ?n ?x) ?m)           (* (/ ?n ?m) ?x)
                              (and (numberp ?n) (numberp ?m) (not (zerop ?m))))
((/ (* ?n ?x) ?y)           (* ?n (/ ?x ?y))     (numberp ?n))
((/ (/ ?n ?x) ?m)           (/ (/ ?n ?m) ?x)
                              (and (numberp ?n) (numberp ?m) (not (zerop ?m))))
((/ (+ ?n ?x) ?m)           (+ (/ ?n ?m) (/ ?x ?m))
                              (and (numberp ?n) (numberp ?m) (not (zerop ?m))))
((/ (- ?n ?x) ?m)           (- (/ ?n ?m) (/ ?x ?m))
                              (and (numberp ?n) (numberp ?m) (not (zerop ?m))))
((/ ?x ?n)                  (* (/ 1 ?n) ?x)
                              (and (numberp ?n) (not (zerop ?n))))
((/ ?x (expt ?x 2))         (/ 1 ?x))
((/ (expt ?x 2) ?x)         ?x)
((/ (* ?x ?y) ?x)           ?y)
((/ (* ?x ?y) ?y)           ?x)
((/ ?x (* ?x ?y))           (/ 1 ?y))
((/ ?x (* ?y ?x))           (/ 1 ?y))
((/ (* ?x ?y) (* ?z ?x))     (/ ?y ?z))
((/ (* ?x ?y) (* ?x ?z))     (/ ?y ?z))
((/ (* ?y ?x) (* ?z ?x))     (/ ?y ?z))
((/ (* ?y ?x) (* ?x ?z))     (/ ?y ?z))
((/ (- ?x (* ?y ?z)) ?y)     (- (/ ?x ?y) ?z))
((/ (- ?x (* ?y ?z)) ?z)     (- (/ ?x ?z) ?y))
((/ (- (* ?y ?z) ?x) ?y)     (- ?z (/ ?x ?y)))
((/ (- (* ?y ?z) ?x) ?z)     (- ?y (/ ?x ?z)))
((/ (+ ?x (* ?y ?z)) ?y)     (+ (/ ?x ?y) ?z))
((/ (+ ?x (* ?y ?z)) ?z)     (+ (/ ?x ?z) ?y))
((/ (+ (* ?y ?z) ?x) ?y)     (+ ?z (/ ?x ?y)))
((/ (+ (* ?y ?z) ?x) ?z)     (+ ?y (/ ?x ?z)))
((/ (+ (/ ?x ?y) (/ ?x ?z)) ?x) (+ (/ 1 ?y) (/ 1 ?z)))
((/ (- (/ ?x ?y) (/ ?x ?z)) ?x) (- (/ 1 ?y) (/ 1 ?z)))
((/ 1 (/ ?x ?y))             (/ ?y ?x))
((/ ?x (/ ?y ?z))            (/ (* ?x ?z) ?y))
((+ ?x (* ?n ?x))            (* (1+ ?n) ?x)      (numberp ?n))
((+ (* ?n ?x) ?x)            (* (1+ ?n) ?x)      (numberp ?n))
((- ?x (* ?n ?x))            (* (- 1 ?n) ?x)     (numberp ?n))
((- (* ?n ?x) ?x)            (* (1- ?n) ?x)      (numberp ?n))
((+ (* ?x ?y) (* ?z ?y))     (* (+ ?x ?z) ?y))
((- (* ?x ?y) (* ?z ?y))     (* (- ?x ?z) ?y))
((+ (* ?y ?x) (* ?z ?y))     (* (+ ?x ?z) ?y))
((- (* ?y ?x) (* ?z ?y))     (* (- ?x ?z) ?y))
((+ (* ?x ?y) (* ?y ?z))     (* (+ ?x ?z) ?y))
((- (* ?x ?y) (* ?y ?z))     (* (- ?x ?z) ?y))
((+ (* ?y ?x) (* ?y ?z))     (* (+ ?x ?z) ?y))
((- (* ?y ?x) (* ?y ?z))     (* (- ?x ?z) ?y))
((+ ?x (- ?y))               (- ?x ?y))
((* (expt ?x 2) (/ ?y ?x))   (* ?x ?y))
((* (/ ?y ?x) (expt ?x 2))   (* ?x ?y))
((truncate (/ ?x ?y))        (truncate ?x ?y))
((sqrt (* ?n ?x))            (* (sqrt ?n) (sqrt ?x))
                             (and (numberp ?n) (glsqrtable ?n)) )
((sqrt (/ ?x ?n))            (* (/ 1.0 (sqrt ?n)) (sqrt ?x))
                             (and (numberp ?n) (glsqrtable ?n)) )
((sqrt (/ ?n ?x))            (/ (sqrt ?n) (sqrt ?x))
                             (and (numberp ?n) (glsqrtable ?n)) )
((sqrt (expt ?x 2))          ?x)   ; (abs ?x) ? but causes trouble for physics
((/ (sqrt ?x) (sqrt ?y))     (sqrt (/ ?x ?y)))
((/ (sqrt (* ?x ?y)) ?x)     (sqrt (/ ?y ?x)))
((/ (sqrt (* ?x ?y)) ?y)     (sqrt (/ ?x ?y)))
((/ ?x (sqrt (* ?x ?y)))     (sqrt (/ ?x ?y)))
((/ ?y (sqrt (* ?x ?y)))     (sqrt (/ ?y ?x)))
((expt (sqrt ?x) 2)          ?x)   ; (abs ?x) ?
((cbrt (* ?n ?x))            (* (cbrt ?n) (cbrt ?x))
                             (and (numberp ?n) (glcbrtable ?n)) )
((cbrt (/ ?x ?n))            (* (/ 1.0 (cbrt ?n)) (cbrt ?x))
                             (and (numberp ?n) (glcbrtable ?n)) )
((cbrt (/ ?n ?x))            (/ (cbrt ?n) (cbrt ?x))
                             (and (numberp ?n) (glcbrtable ?n)) )
((cbrt (expt ?x 3))          ?x)
((expt (cbrt ?x) 3)          ?x)
((expt ?x 1)                 ?x)
((expt ?x 0)                 1)
((expt ?x -1)                (/ 1 ?x))
((expt (- ?x) 2)             (expt ?x 2))
((expt (* ?n ?x) ?m)         (* (expt ?n ?m) (expt ?x ?m))
                             (and (numberp ?n) (numberp ?m) (glq>=0 ?n)) )
((expt (* ?x ?n) ?m)         (* (expt ?n ?m) (expt ?x ?m))
                             (and (numberp ?n) (numberp ?m) (glq>=0 ?n)) )
((expt (/ ?n ?x) ?m)         (/ (expt ?n ?m) (expt ?x ?m))
                             (and (numberp ?n) (numberp ?m) (glq>=0 ?n)) )
((expt (/ ?x ?n) ?m)         (/ (expt ?x ?m) (expt ?n ?m))
                             (and (numberp ?n) (numberp ?m) (glq>0 ?n)) )
((expt (/ ?x ?y) ?m)         (/ (expt ?x ?m) (expt ?y ?m))    (numberp ?m))
((* ?x (expt ?x ?n))         (expt ?x (1+ ?n))     (numberp ?n))
((* (expt ?x ?n) ?x)         (expt ?x (1+ ?n))     (numberp ?n))
((/ (expt ?x ?n) ?x)         (expt ?x (1- ?n))     (numberp ?n))
((* (expt ?x ?n) (/ 1 ?x))   (expt ?x (1- ?n))     (numberp ?n))
((* (/ 1 ?x) (expt ?x ?n))   (expt ?x (1- ?n))     (numberp ?n))
((* (expt ?x ?m) (expt ?x ?n))       (expt ?x (+ ?m ?n))
                             (and (numberp ?n) (numberp ?m)) )
((expt (* ?x ?y) 2)          (* (expt ?x 2) (expt ?y 2)))
((expt (/ ?x ?y) 2)          (/ (expt ?x 2) (expt ?y 2)))
((atan (* ?n ?y) (* ?n ?z))  (atan ?y ?z) (and (numberp ?n) (glq/=0 ?n)) ) 
((atan (sin ?x) (cos ?x))    ?x)
((atan (cos ?x) (sin ?x))    (- (/ pi 2.0) ?x))
((cos (- (/ pi 2.0) ?x))         (sin ?x))
((sin (- (/ pi 2.0) ?x))         (cos ?x))
((cos (- 1.5707963267948966 ?x)) (sin ?x))
((sin (- 1.5707963267948966 ?x)) (cos ?x))
((/ (sin ?x) (cos ?x))           (tan ?x))
((/ (cos ?x) (sin ?x))           (/ 1 (tan ?x)))
((* (/ ?x (cos ?y)) (sin ?y))    (* ?x (tan ?y)))
((* (sin ?y) (/ ?x (cos ?y)))    (* ?x (tan ?y)))
((/ (* (sin ?y) ?x) (cos ?y))    (* ?x (tan ?y)))
((/ (* ?x (sin ?y)) (cos ?y))    (* ?x (tan ?y)))
; Commented out: these can lead to loop; do not decrease size  08 Jun 04
;((- (* ?n ?x) (* ?m ?y))         (* ?n (- ?x (* (/ ?m ?n) ?y)))
;                                 (and (numberp ?n) (numberp ?m)
;				      (or (floatp ?n) (floatp ?m)
;					  (= (mod ?m ?n) 0)))  )
;((- (* ?n ?x) (* ?m ?y))         (* ?m (- (* (/ ?n ?m) ?x) ?y))
;			         (and (numberp ?n) (numberp ?m)
;				      (= (mod ?n ?m) 0)) )
;((+ (* ?n ?x) (* ?m ?y))         (* ?n (+ ?x (* (/ ?m ?n) ?y)))
;			         (and (numberp ?n) (numberp ?m)
;				      (or (floatp ?n) (floatp ?m)
;					  (= (mod ?m ?n) 0))) )
;((+ (* ?n ?x) (* ?m ?y))         (* ?m (+ (* (/ ?n ?m) ?x) ?y))
;			         (and (numberp ?n) (numberp ?m)
;				      (= (mod ?n ?m) 0)) )
((- (* ?n ?x) (* ?m ?x))         (* (- ?n ?m) ?x)
                                   (and (numberp ?n) (numberp ?m)) )
((+ (* ?n ?x) (* ?m ?x))         (* (+ ?n ?m) ?x)
                                   (and (numberp ?n) (numberp ?m)) )


((= (+ ?x ?y) ?x)          (= ?y 0))     ; 14 Oct 92
((= (+ ?x ?y) ?y)          (= ?x 0))
((= (- ?x ?y) ?x)          (= ?y 0))
((= ?x (+ ?x ?y))          (= ?y 0))
((= ?y (+ ?x ?y))          (= ?x 0))
((= ?x (- ?x ?y))          (= ?y 0))

((= (* ?n ?x) (* ?m ?y))   (= ?x (* (/ ?m ?n) ?y))   ; 28 Apr 94
                           (and (numberp ?n) (numberp ?m)
				(glq/=0 ?n)) )
((= (/ ?n ?x) ?y)          (= ?n (* ?x ?y))     (numberp ?n))
((= ?y (/ ?n ?x))          (= ?n (* ?x ?y))     (numberp ?n))
((= (/ ?n ?x) (/ ?m ?y))   (= (* ?m ?x) (* ?n ?y))
                           (and (numberp ?n) (numberp ?m)) )
((= ?n (* ?m ?x))          (= ?x (/ ?n ?m))
                           (and (numberp ?n) (numberp ?m) (glq/=0 ?m)) )

))
; infix.lsp                  Gordon S. Novak Jr.            ; 02 Apr 08

; Copyright (c) 2008 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; 09 Oct 06; 27 Dec 06; 02 Apr 08 jchaw/kbarker

(defvar *infixstr* nil)

; 09 Oct 06
; Convert a formula in Lisp form to a string in infix form.
(defun infixstr (form &optional html)
  (setq *infixstr* nil)
  (infixstrb form html 0)
  (apply #'concatenate (cons 'string (nreverse *infixstr*))) )

(defun infixstrb (form html lastprec)
  (let (str op prec)
    (if (symbolp form)
        (progn (setq str (symbol-name form))
               (if (and (> (length str) 4)
                        (string= str "VAR-" :end1 4))
                   (push (subseq str 4) *infixstr*)
                   (push (string-downcase str) *infixstr*) ) )
        (if (numberp form)
            (push (princ-to-string form) *infixstr*)
            (if (consp form)
                (progn
                  (setq op (car form))
                  (if (and (eq op '-)
                           (null (cddr form)))
                      (progn (push "(- " *infixstr*)
                             (infixstrb (lhs form) html 8)
                             (push ")" *infixstr*))
                      (if (and html (or (eq op 'sqrt)  ;; jchaw/kbarker
                                        (eq op '|sqrt|)))
                          (progn (push "&radic; " *infixstr*)   ;; jchaw/kbarker
                                 (infixstrb (lhs form) html 20) )
                          (if (or (eq op 'expt)     ;; jchaw/kbarker
                                  (eq op '|expt|))
                              (progn (infixstrb (lhs form) html 20)
                                     (if html (push "<SUP>" *infixstr*)
                                              (push "^" *infixstr*))
                                     (infixstrb (rhs form) html 20)
                                     (if html (push "</SUP>" *infixstr*)))
                              (if (setq prec (cdr (assoc op
                                         '((+ . 5) (- . 6) (* . 7)
                                           (/ . 7) (= . 1)))))
                                  (progn (if (<= prec lastprec)
                                             (push "(" *infixstr*))
                                         (infixstrb (lhs form) html prec)
                                         (push " " *infixstr*)
                                         (push (stringify op) *infixstr*)
                                         (push " " *infixstr*)
                                         (infixstrb (rhs form) html prec)
                                         (if (<= prec lastprec)
                                             (push ")" *infixstr*)))
                                  (progn (push (string-downcase
                                                (symbol-name op)) *infixstr*)
                                         (push "(" *infixstr*)
                                         (pop form)
                                         (while form
                                           (infixstrb (pop form) html 0)
                                           (if form (push ", " *infixstr*)))
                                         (push ")" *infixstr*) ))))))))) ))
; units.lsp              Gordon S. Novak Jr.           ; 26 Jul 08

; Software for conversion and simplification of Units of Measurement

; Copyright (c) 2007 Gordon S. Novak Jr. and The University of Texas at Austin.

; See the file gnu.license for the GNU General Public License.

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; gnu@gnu.org, http://www.gnu.org/, 617 542-5942, fax 617 542-2652.

; Floating-point numbers that are returned as conversion factors
; by this software may appear to have more digits of accuracy than are
; justified by the accuracy of the underlying conversion factors.

; Written by: Gordon S. Novak Jr., Department of Computer Sciences,
; University of Texas at Austin  78712.    novak@cs.utexas.edu
; http://www.cs.utexas.edu/users/novak/

; Thanks to Erik Enge (erik@nittin.net) for thorough testing and suggestions
; and to him and Kevin Rosenberg for making this a Debian package.

; History:
; converted from Interlisp " 1-MAR-82 17:36:34" {DSK}MEASUREMENT.LSP;8 15862
; adapted from ./physics/measu.lsp of 29 Aug 90 for use with GLISP compiler
; modified 03 June 93 for paper on units: replaced MKS by SI.
; 05 Jun 95; 15 Dec 95; 09 Mar 99; 17 Mar 99; 20 May 99; 03 May 01; 02 Apr 02
; 17 Apr 02; 08 Apr 03; 18 Apr 03; 22 May 03; 26 Jan 04; 04 Feb 04; 13 Feb 04
; 16 Mar 04; 19 Mar 04; 26 Mar 04; 08 Apr 04; 09 Apr 04; 12 Apr 04; 13 Apr 04
; 15 Apr 04; 10 May 04; 21 Jun 04; 26 May 05; 07 Jun 05; 08 Jun 05; 10 Aug 05
; 22 Aug 05; 24 Aug 05; 25 Aug 05; 08 Sep 05; 06 Dec 05; 08 Dec 05; 19 Jan 06
; 07 Feb 06; 30 Mar 06; 04 May 06; 28 Jun 06; 12 Sep 06; 03 Jan 07; 01 Jun 07
; 02 Aug 07; 07 Jan 08

; Use the following to import the basic functions to the package where used:
; (import '(units:glconvertunit units:glsimplifyunit units:glunitp
;           units:*glunitenable* units:glmultunits units:glsimplunit
;           units:gldivunits units:gldominantunit units:glsqrtunit))

; To make the basic conversion functions have easier names, do:
; (setf (symbol-function 'convertu)  (symbol-function 'units:glconvertunit))
; (setf (symbol-function 'simplifyu) (symbol-function 'units:glsimplifyunit))
; (setf (symbol-function 'unitp)     (symbol-function 'units:glunitp))

; A unit expression is:
;   a number
;   a unit name
;   (* unit-exp1 unit-exp2 ...)
;   (/ unit-exp1 unit-exp2)
; A simplified unit expression will have at most one (/ ...) at the top.

; (setq *glunitenable* t) to enable mass -> weight, mass -> energy conversion
; *glunitmethod* is set to w2m m2w m2e e2m if this is done.

; Examples of unit conversion:  (glconvertunit <from> <to>)
; (glconvertunit 'mile 'foot)
; (glconvertunit 'kilogram 'lb)
; (glconvertunit '(/ (* atto parsec) (* micro fortnight)) '(/ inch second))
; (glconvertunit '(* acre foot) 'teaspoon)
; (glconvertunit '(/ (* 2000 kilo calorie) day) 'watt)    ; average human power
; (glconvertunit '(* 100 kgf 4 m) '(* kilo calorie)) ; calories to climb stairs
; the following require (setq *glunitenable* t)
; (glconvertunit 'kilogram 'lbf)          ; mass to weight
; (glconvertunit 'gram 'kilowatt-hour)    ; mass to energy

; Examples of unit simplification:
; (glsimplifyunit '(/ meter foot))
; (glsimplifyunit '(/ joule watt))
; (glsimplifyunit '(/ joule horsepower))
; (glsimplifyunit '(/ (* kilogram meter) (* second second)))
; (glsimplifyunit 'atm)
; (glsimplifyunit 'atm 'english)
; (glsimplifyunit '(/ (* amp second) volt))
; (glsimplifyunit '(/ (* newton meter) (* ampere second)))
; (glsimplifyunit '(/ (* volt volt) (* lbf (/ (* atto parsec) hour))))

; Example of unit conversion by GLISP compiler:
; (gldefun test (speed\:(units real (/ (* atto parsec) (* micro fortnight))))
;   (if (speed > '(q 55 mph)) (print "speeding")))

(defmacro gldimension     (x) `(get ,x 'gldimension))
(defmacro glunittype      (x) `(get ,x 'glunittype))
(defmacro glsiconversion  (x) `(get ,x 'glsiconversion))
(defmacro glabbreviations (x) `(get ,x 'glabbreviations))
(defmacro glexpansion     (x) `(get ,x 'glexpansion))
(defmacro glactualunit    (x) `(get ,x 'glactualunit))
(defmacro glstdunits      (x) `(get ,x 'glstdunits))
(defmacro glsystemunits   (x) `(get ,x 'glsystemunits))
(defmacro glactualu       (x) `(or (glactualunit ,x) ,x))
(defmacro glunitsys       (x) `(get ,x 'glunitsys))

(defvar *gldimsizes* (make-array 8 :initial-contents
				   '(20 20 20 10 10 10 10 10)))
(defvar *gldimvals*  (make-array 8))
(defvar *gldimbias*  0)
(defvar *gldimtounittype*   nil)

; 28 Apr 94
; Cube root
; returns a negative real root for a negative argument.
(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))

; Test X to see if it represents a compile-time constant value.
; If undefined, define a simplified version.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'glconstantp)
    (defun glconstantp (x) (constantp x)))
)

; Get the value of a compile-time constant 
; If undefined, define a simplified version.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'glconstantval)
    (defun glconstantval (x) (if (constantp x) (eval x))))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'while)
    (defmacro while (test &rest forms)
      `(loop (unless ,test (return)) ,@forms)))
)

; 03 Jun 93; 16 Jul 93; 11 Nov 94
; Initialize factors for use in computing dimensions
(defun gldiminit ()
  (let ((f 1) (del 1) (bias 0))
    (dotimes (i 8)
      (setq f (* f del))
      (setf (aref *gldimvals* i) f)
      (setq del (aref *gldimsizes* i))
      (incf bias (* f (truncate del 2))) )
    (setq *gldimbias* bias) ))

(gldiminit)
  
; 04 Nov 92; 03 Jun 93; 16 Jul 93
; Convert a dimension list, which is a list of integers representing
; the powers of (mass length time charge), to an integer.
(defun gldimtoint (l)
  (let ((n 0))
    (dotimes (i 8) (incf n (* (pop l) (aref *gldimvals* i))))
    n))

; 03 Jun 93; 16 Jul 93; 26 Jul 93
; Convert a dimension integer to a dimension list
(defun glinttodim (n)
  (let ((m (+ n *gldimbias*)) l sz mm)
    (dotimes (i 8)
      (setq sz (aref *gldimsizes* i))
      (setq mm (truncate m sz))
      (push (- (- m (* mm sz)) (truncate sz 2))
	    l)
      (setq m mm))
    (nreverse l) ))

(dolist (pair '((length          1 0 0 0 0 0 0 0)
		(time            0 1 0 0 0 0 0 0)
		(temperature     0 0 1 0 0 0 0 0)
                (mass            0 0 0 1 0 0 0 0)
		(current         0 0 0 0 1 0 0 0)
		(substance       0 0 0 0 0 1 0 0)
		(luminosity      0 0 0 0 0 0 1 0)
		(money           0 0 0 0 0 0 0 1)
		(dimensionless   0 0 0 0 0 0 0 0)) )
        (setf (gldimension (car pair)) (gldimtoint (cdr pair)))
	(pushnew (list (gldimension (car pair)) (car pair))
		 *gldimtounittype*) ) 

(defvar *glunitenable* nil)  ; t to enable mass/weight, mass/energy conversions
(defvar *glunitmethod* nil)  ; which of above was used
(defvar *gldimweighttomass* (gldimtoint '( 1 -2 0 0 0 0 0 0)))
(defvar *gldimmasstoweight* (gldimtoint '(-1  2 0 0 0 0 0 0)))
(defvar *gldimmasstoenergy* (gldimtoint '(-2  2 0 0 0 0 0 0)))
(defvar *gldimenergytomass* (gldimtoint '( 2 -2 0 0 0 0 0 0)))
(defvar *gldimunitytomolar* -799997)     ; for pH = -log10(M)
(defvar *gldimmolartounity*  799997)

(defvar *glunkunits*   nil)  ; unknown units
(defvar *glunitdimerror* nil) ; error in dimensionality

; 04 Nov 92
; Define simple measurement units.  Args are unit type and a list of units.
; Each unit is specified by a list: (unit-name glsiconversion synonyms)
(defun gldefsimpleunits (unit-type units)
  (dolist (unitlist units)
    (gldefunit (first unitlist) unit-type (second unitlist) (third unitlist))))

; 04 Nov 92; 07 Jun 05
(defun gldefunit (unit unit-type factor abbrevs)
  (if (and unit-type (symbolp unit-type))
      (setf (glunittype unit) unit-type))
  (setf (gldimension unit)
	(if (symbolp unit-type)
	    (gldimension unit-type)
	    unit-type))
  (setf (glsiconversion unit) factor)
  (if abbrevs (setf (glabbreviations unit) abbrevs))
  (dolist (abbrev abbrevs) (setf (glactualunit abbrev) unit))
  unit)

; 04 Nov 92
; Define derived measurement units.  Args are unit type and a list of units.
; Unit type may be a named constant, such as mass, or nil.
; Each unit is specified by a list of: (unit-name unit-spec synonyms)
(defun gldefderivedunits (unit-type units)
  (dolist (unitlist units)
    (let ((dim (glunitdim (second unitlist))))
      (if unit-type (unless (= (gldimension unit-type) dim)
			    (error "Bad unit spec ~S" unitlist)))
      (setf (glexpansion (first unitlist)) (second unitlist))
      (gldefunit (first unitlist)
		 (or unit-type dim)
		 (glunitfactor (second unitlist))
		 (third unitlist)) )))

; 04 Nov 92; 04 May 93; 03 Jun 93; 28 Apr 95; 03 Apr 02; 18 Apr 03; 08 Dec 05
; Convert from one unit to another.
; Returns a numeric factor or nil if conversion is improper.
; sets *glunitmethod* to w2m m2w m2e e2m if special conversion done.
(defun glconvertunit (from to)
  (let (f dim)
    (setq *glunkunits* nil)
    (setq *glunitdimerror* nil)
    (if (and (glunitp from) (glunitp to)) 
	(progn
	  (setq f (/ (glunitfactor from) (glunitfactor to)))
	  (setq dim (- (glunitdim from) (glunitdim to)))
	  (setq *glunitmethod* nil)
	  (if (= dim 0)
	      f
	    (if *glunitenable*
		(cond ((= dim *gldimweighttomass*)
		        (setq *glunitmethod* 'w2m) (/ f 9.80665))
		      ((= dim *gldimmasstoweight*)
		        (setq *glunitmethod* 'm2w) (* f 9.80665))
		      ((= dim *gldimmasstoenergy*)
		        (setq *glunitmethod* 'm2e) (* f 8.987554305625E16))
		      ((= dim *gldimenergytomass*)
		        (setq *glunitmethod* 'e2m) (/ f 8.987554305625E16))
		      ((= dim *gldimunitytomolar*)
		        (setq *glunitmethod* 'u2m) (* f 1000))
		      ((= dim *gldimmolartounity*)
		        (setq *glunitmethod* 'm2u) (/ f 1000))
		      (t (setq *glunitdimerror* t) nil))
	        (progn (setq *glunitdimerror* t) nil))))) ))

; 04 Nov 92; 30 Nov 92; 03 Apr 02; 08 Apr 04
; Test whether unit is a legitimate unit specification
(defun glunitp (unit)
  (or (numberp unit)
      (and (glconstantp unit)
	   (numberp (glconstval unit)))
      (and unit
	   (symbolp unit)
	   (or (glsiconversion unit)
	       (glactualunit unit)))
      (and (consp unit)
	   (eq (car unit) '*)
	   (every #'glunitp (cdr unit)))
      (and (consp unit)
	   (eq (car unit) '/)
	   (= (length unit) 3)
	   (every #'glunitp (cdr unit)))
      (and (consp unit)
	   (eq (car unit) 'expt)
	   (= (length unit) 3)
	   (glunitp (cadr unit))
	   (integerp (caddr unit)))
      (progn (if (atom unit) (push unit *glunkunits*))
	     nil) ) )

; 07 Jun 05
; Get the base unit for a specified unit
(defun glbaseunit (unit)
  (and (symbolp unit)
       (if (glsiconversion unit)
	   unit
	   (glactualunit unit))) )

; 04 Nov 92; 03 Jun 93; 08 Apr 04; 09 Apr 04; 25 Aug 05; 08 Sep 05
; Find dimension from a unit expression.
(defun glunitdim (unit)
  (if (atom unit)
      (if (or (numberp unit) (null unit))
	  0
	  (if (symbolp unit)
	      (or (gldimension unit)
		  (gldimension (glactunit unit))
		  (and (glexpansion (glactunit unit))
		       (glunitdim (glexpansion (glactunit unit)))))
	      (error "~A is not a unit")))
      (if (eq (car unit) '*)
	  (let ((dim 0))
	    (dolist (u (cdr unit) dim)
	      (setq dim (+ dim (glunitdim u)))))
	  (if (eq (car unit) '/)
	      (- (glunitdim (cadr unit))
		 (glunitdim (caddr unit)))
	      (if (eq (car unit) 'expt)
		  (let ((dim 0))
		    (dotimes (i (abs (caddr unit)) dim)
		      (setq dim (if (minusp (caddr unit))
				    (- dim (glunitdim (cadr unit)))
				    (+ dim (glunitdim (cadr unit)))))))
		  (error "~A has bad unit operator.~%" unit))))) )

; 04 Nov 92; 08 Apr 04; 09 Apr 04; 25 Aug 05; 08 Sep 05
; Find  conversion to SI units of a unit expression.
(defun glunitfactor (unit)
  (if (atom unit)
      (if (numberp unit)
	  unit
	  (if (symbolp unit)
	      (or (glsiconversion unit)
		  (glsiconversion (glactunit unit))
		  (and (glexpansion (glactunit unit))
		       (glunitfactor (glexpansion (glactunit unit)))))))
      (if (eq (car unit) '*)
	  (let ((f 1.0))
	    (dolist (u (cdr unit) f)
	      (setq f (* f (glunitfactor u)))))
	  (if (eq (car unit) '/)
	      (/ (glunitfactor (second unit))
		 (glunitfactor (third unit)))
	      (if (eq (car unit) 'expt)
		  (let ((f 1.0))
		    (dotimes (i (abs (caddr unit)) f)
		      (setq f (if (minusp (caddr unit))
				  (/ f (glunitfactor (cadr unit)))
				  (* f (glunitfactor (cadr unit)))))))
		  (error "~A has bad unit operator.~%" unit))))) )

; 03 Apr 02; 25 Aug 05; 08 Sep 05
; get actual unit name
(defun glactunit (u)
  (and u (symbolp u)
       (if (or (glsiconversion u) (glexpansion u))
	   u
	   (and (glactualunit u)
		(glactunit (glactualunit u))))))

; 16 July 93; 19 Jul 93; 05 Mar 99; 18 Apr 03; 19 Mar 04; 26 Mar 04; 01 Apr 04
; 09 Apr 04
; Expand a unit into a flat quotient of simple factors
(defun glunitexpand (unit) (glunitexpandb unit (list nil nil) nil))
(defun glunitexpandb (unit flat flg)
  (if (atom unit)
      (if (numberp unit)
	  (if (= unit 1)
	      flat
	      (glunitpush unit flat flg))
	  (if (symbolp unit)
	      (progn
		(if (glactualunit unit) (setq unit (glactualunit unit)))
		(if (eq unit 'unity)
		    flat
		    (if (glexpansion unit)
			(glunitexpandb (glexpansion unit) flat flg)
			(if (and (gldimension unit) (= (gldimension unit) 0))
			    (glunitpush (glsiconversion unit) flat flg)
			    (glunitpush unit flat flg)))))
	      (error "~A has bad unit contents.~%" unit)))
      (case (car unit)
	(* (dolist (x (cdr unit) flat)
	     (setq flat (glunitexpandb x flat flg))) )
	(/ (glunitexpandb (second unit)
			  (glunitexpandb (third unit) flat (not flg))
			  flg) )
	(sqrt (glunitexpand (glsqrtunit (cadr unit))) )
	(cbrt (glunitexpand (glcbrtunit (cadr unit))) )
	((+ -) (if (or (null (cddr unit))
		       (glconvertunit (cadr unit) (caddr unit)))
		   (glunitexpandb (cadr unit) flat flg)
		   (error "~A has bad unit operator.~%" unit)) )
	(expt (if (numberp (caddr unit))
		  (let (lst)
		    (dotimes (i (abs (caddr unit))) (push (cadr unit) lst))
		    (glunitexpandb (if (minusp (caddr unit))
				       (list '/ 1 (cons '* lst))
				       (cons '* lst))
				   flat flg))
		  (error "~A has bad unit operator.~%" unit) ) )
	(t (error "~A has bad unit operator.~%" unit) ) ) ))

; 19 Jul 93
; Expand a unit into a flat quotient of simple factors, removing duplicates
(defun glunitexpandc (unit)
  (let (flat num den)
    (setq flat (glunitexpand unit))
    (setq num (glunitsort (first flat)))
    (setq den (glunitsort (second flat)))
    (list (glmultiset-diff num den)
	  (glmultiset-diff den num)) ))

; 16 July 93
; Push a unit onto numerator, or denominator if flg = t.
(defun glunitpush (unit flat flg)
  (if flg (push unit (second flat))
          (push unit (first flat)))
  flat)

; 30 Nov 92
; Invert a unit speciication
(defun glinvertunit (u)
  (if (and (consp u)
	   (eq (car u) '/))
      (list '/ (third u) (second u))
      (list '/ 1 u)))

; 04 Aug 93
(defun glmultunits (ua ub) (list '* ua ub))
(defun gldivunits  (ua ub) (list '/ ua ub))

; 05 Mar 99; 09 Mar 99; 03 May 01; 18 Apr 03
; Simplify a unit expression, leaving it as is when possible
(defun glsimplunit (unit)
  (let (flat)
    (if (atom unit)
	unit
        (if (and (consp unit)
		 (eq (car unit) '*)
		 (or (eq (cadr unit) 'unity)
		     (eq (caddr unit) 'unity)))
	    (if (eq (cadr unit) 'unity)
		(caddr unit)
	        (cadr unit))
	    (if (and (consp unit)
		     (eq (car unit) '/)
		     (eq (caddr unit) 'unity))
		(cadr unit)
	        (progn (setq flat (glunitexpandc unit))
		       (glflattounit (car flat) (cadr flat) 1.0)) ) ) ) ))

; 16 Jul 93; 19 Jul 93; 03 Mar 95; 04 May 06
; Simplify a unit expression.
(defun glsimplifyunit (unit &optional system)
  (let (flat (factor 1.0) num den tmp tmpb (progress t) max lng best inv)
    (or system (setq system (or (gldominantunit unit) 'si)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (while progress
      (setq progress nil)
      (setq max 0)
      ; (format t "num = ~A~%den = ~A~%" num den)
      (dolist (lst (glgetsystemunits system))
	(setq flat (fourth lst))
	(setq lng (+ (length (first flat)) (length (second flat))))
	(when (and (> lng 1)
		   (or (> lng max)
		       (and (= lng max) inv)))
	  (if (and (glsubmultiset (first flat) num)
		   (glsubmultiset (second flat) den))
	      (progn (setq max lng)
		     (setq inv nil)
		     (setq best lst))
	      (if (and (glsubmultiset (first flat) den)
		       (glsubmultiset (second flat) num))
		  (progn (setq max lng)
			 (setq inv t)
			 (setq best lst))))))
      (when (> max 0)
	(setq progress t)
	; (format t "best = ~A~%" best)
	(setq flat (fourth best))
	(if inv
	    (progn (setq num (glmultiset-diff num (second flat)))
		   (setq den (glunitsort
			       (cons (second best)
				     (glmultiset-diff den (first flat)))))
		   (setq factor (* factor (third best))))
	    (progn (setq num (glunitsort
			       (cons (second best)
				     (glmultiset-diff num (first flat)))))
		   (setq den (glmultiset-diff den (second flat)))
		   (setq factor (/ factor (third best)))))))
    ; (format t "num = ~A~%den = ~A~%" num den)
    (glflattounit num den factor) ))

; 19 Jul 93; 12 May 95; 30 Mar 06
; Make a unit from two flat lists
(defun glflattounit (num den factor)
    (setq num (if (glunitapprox= factor 1.0)
		  (if (cdr num) (cons '* num) (car num))
		  (if num (cons '* (cons (or (glpowerten factor) factor) num))
		      (or (glpowerten factor) factor))))	
    (setq den (if (cdr den) (cons '* den) (car den)))
    (if (and num den)
	(if (and (consp num) (eq (car num) '*) (numberp (cadr num)))
	    (list '* (cadr num)
		     (list '/ (if (cdddr num)
				  (cons '* (cddr num))
				  (caddr num))
			      den))
	    (list '/ num den))
	(or num
	    (if den (list '/ 1 den)
		'unity))) )

; 12 May 95
(defun glunitapprox= (x y) (< (abs (- x y)) 1.0e-6))

; Test if a factor is equivalent to a standard power of ten.
(defun glpowerten (x)
  (let (logx rlogx)
    (and (numberp x)
	 (plusp x)
	 (setq logx (log x 10))
	 (setq rlogx (round logx))
	 (glunitapprox= logx rlogx)
	 (cadr (assoc rlogx
		      '((24 yotta) (21 zetta) (18 exa) (15 peta) (12 tera)
			(9 giga) (6 mega) (3 kilo) (-3 milli) (-6 micro)
			(-9 nano) (-12 pico) (-15 femto) (-18 atto)
			(-21 zepto) (-24 yocto))))) ))

; 19 Jul 93
; Convert units list to a specified system
(defun glsimpsystem (units system)
  (let ((factor 1.0) lst f new)
    (dolist (x units)
      (if (numberp x)
	  (setq factor (* factor x))
	  (progn 
	    (if (and (setq new (glsystemunit x system))
		     (not (eq new x)))
		(if (setq f (glconvertunit x new))
		    (progn (setq factor (* factor f))
			   (push new lst))
		    (error "Failed to convert ~A to ~A~%" x new))
		(push x lst)))))
    (list factor (glunitsort lst))))

; 19 Jul 93
; Sort a list of symbols alphabetically
(defun glunitsort (lst) (sort lst #'glunitsortp))
		
; 19 Jul 93
(defun glunitsortp (x y)
  (or (numberp x)
      (and (symbolp x) (symbolp y)
	   (string< (symbol-name x) (symbol-name y)))))

; 19 Jul 93
; Test if first list is a sub-multiset of the second (both sorted)
(defun glsubmultiset (seta setb)
  (or (null seta)
      (and setb
	   (if (eq (car seta) (car setb))
	       (glsubmultiset (cdr seta) (cdr setb))
	       (if (glunitsortp (car setb) (car seta))
		   (glsubmultiset seta (cdr setb)))))))

; 19 Jul 93
; Multiset difference, seta - setb (both sorted)
(defun glmultiset-diff (seta setb)
  (if seta
      (if (null setb)
	  seta
	  (if (eq (car seta) (car setb))
	      (glmultiset-diff (cdr seta) (cdr setb))
	      (if (glunitsortp (car seta) (car setb))
		  (cons (car seta) (glmultiset-diff (cdr seta) setb))
		  (glmultiset-diff seta (cdr setb)))))))

; 07 Dec 92; 17 Dec 92; 23 Jul 93; 28 Apr 94
; Divide a units list "in half" for sqrt
(defun glsqrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glsqrtunit "bad unit ~A" unit))
        (glflattounit num den (sqrt factor))) ))

; 29 Apr 94
; Divide a units list "in thirds" for cbrt
(defun glcbrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glcbrtunit "bad unit ~A" unit))
        (glflattounit num den (cbrt factor))) ))

; 04 Nov 92; 03 Dec 92; 07 Dec 92; 17 Dec 92
; Simplify a unit expression.
;   (glsimplifyunit '(/ (* pound-force second second) (* slug foot))) ; = unity
; Still need to do: e.g.
;   (glsimplifyunit '(/ (* kilogram meter) (* second second)))  ; = newton
; 07 Dec 92
; Get numeric factor for a unit if it is a pure number
(defun glnumfactor (unit)
  (if (numberp unit)
      unit
      (if (glconstantp unit)
	  (glconstval unit)
	  (and (symbolp unit)
	       (= (gldimension unit)
		  (gldimension 'unity))
	       (glsiconversion unit)))) )

; 04 Nov 92
; Remove the first occurrence of an item from a list.
(defun remove-first (item lst)
  (if (consp lst)
      (if (eq item (first lst))
	  (rest lst)
	  (cons (first lst) (remove-first item (rest lst))))))

; 16 July 93; 29 Sep 94
; Find the dominant unit system used in a given unit
(defun gldominantunit (unit)
  (let (pairs system (max 0) val)
    (setq pairs (gldominantunitb unit pairs))
    (dolist (pair pairs)
      (setq val (+ (cdr pair)
		   (or (cdr (assoc (car pair)
				   '((si . 0.8) (cgs . 0.5) (english . 0.2))))
		       0)))
      (when (> val max)
	(setq max val)
	(setq system (car pair))))
    system ))

; 16 July 93; 29 Sep 94; 05 Mar 99; 17 Mar 99
(defun gldominantunitb (unit pairs)
  (let (unittype tmp aunit sys)
    (if (atom unit)
	(if (symbolp unit)
	    (if (setq sys
		      (or (and (setq unittype
				 (glunittype (setq aunit (glactualu unit))))
			       (caar (member aunit (glstdunits unittype)
					     :key #'cadr)))
			  (glunitsys unit)))
		(if (setq tmp (assoc sys pairs))
		    (progn (incf (cdr tmp)) pairs)
		    (push (cons sys 1) pairs))
	        pairs)
	    pairs)
	(progn (dolist (subunit (cdr unit))
		 (setq pairs (gldominantunitb subunit pairs)))
	       pairs)) ))

; 16 July 93
; find the system of units in which unit is used, if known.
(defun glunitsystem (unit)
  (let (unittype)
    (and (symbolp unit)
	 (setq unittype (glunittype unit))
	 (some #'(lambda (x) (if (eq unit (cadr x)) (car x)))
			 (glstdunits unittype))) ))

; 16 July 93; 15 Apr 04; 04 May 06; 12 Sep 06
; find the unit to be substituted for unit in the specified system
(defun glsystemunit (unit &optional (system 'si))
   (and (symbolp unit)
	(glstandardunit (glunittype unit) system)) )

; 15 Apr 04; 04 May 06
; get the standard unit to be used for a type of unit, e.g. power
(defun glstandardunit (unittype &optional (system 'si))
  (let (tmp)
    (and (symbolp unittype)
	 (if (consp system)
	     (cadr (assoc unittype system))
	     (and (setq tmp (assoc system (glstdunits unittype)))
		  (cadr tmp)) ) ) ))

; 04 May 06
(defun glgetsystemunits (system)
  (if (symbolp system)
      (glsystemunits system)
      (mapcar #'(lambda (pair)
		  (list (car pair) (cadr pair) 1.0
			(list (list (cadr pair)) '())))
	      system)))

; 07 June 05
; find the type of an arbitrary unit
(defun glfindunittype (unit)
  (cadr (assoc (glunitdim unit) *gldimtounittype*)))

(defun glfindunittype-old (unit)
  (let (newunit)
    (if (symbolp unit)
	(glunittype (or (glactualunit unit) unit))
        (progn (setq newunit (glsimplifyunit unit))
	       (if (symbolp newunit)
		   (glfindunittype newunit)
		   (if (numberp newunit)
		       'dimensionless
		       (if (and (consp newunit)
				(eq (car newunit) '*)
				(numberp (cadr newunit))
				(symbolp (caddr newunit)))
			   (glfindunittype (caddr newunit))))))) ))

; 12 Apr 04; 02 Aug 07
(defun basictempunit (u)
  (member u '(k kelvin degK degreeK degree-kelvin
	      c centigrade celsius degC degreeC degree-celsius degree-centigrade
              f fahrenheit degF degreeF degree-fahrenheit
              r rankine degR degreeR degree-rankine)))

; 12 Apr 04
; convert temperature units.  source is (* n unit), goal is unit
(defun tempconvert (source goal)
  (let (sourcekelvin)
    (setq sourcekelvin
	  (case (caddr source)
	    ((k kelvin degree-kelvin degK) (cadr source))
	    ((c centigrade celsius degree-celsius degree-centigrade degC)
	       (+ 273.15 (cadr source)))
	    ((f fahrenheit degree-fahrenheit degF)
	       (+ 273.15 (* (/ 5 9) (- (cadr source) 32))))
	    ((rankine degR degree-rankine) (* (/ 5 9) (cadr source)))))
    (case goal
      ((k kelvin degree-kelvin degK) sourcekelvin)
      ((c centigrade celsius degree-celsius degree-centigrade degC)
        (- sourcekelvin 273.15))
      ((f fahrenheit degree-fahrenheit degF)
        (+ (* (- sourcekelvin 273.15) (/ 9 5)) 32))
      ((rankine degR degree-rankine) (* (/ 9 5) sourcekelvin)) ) ))

; 07 Jun 05
; change a KM symbol into a plain Lisp symbol: remove initial *, upper-case
(defun glunkmify (sym)
  (let ((str (symbol-name sym)))
    (intern (string-upcase (if (char= (char str 0) #\*)
			       (subseq str 1)
			       str))) ))

(defvar *kmunittypes* nil)

; 08 Sep 05
; add a km unit to known unit types
(defun defkmunittype (kmunit unit)
  (let (unittype lst)
    (setq unittype (or (glfindunittype unit) 'misc))
    (setq lst (assoc unittype *kmunittypes*))
    (if lst (pushnew kmunit (cdr lst))
        (push (list unittype kmunit) *kmunittypes*)) ))

; 07 Jun 05; 10 Aug 05; 25 Aug 05; 08 Sep 05
; define a KM unit.
; Note: basic units such as |*meter| must be defined first
; e.g. (defkmunit '|*meter| 'meter)
; e.g. (defkmunit '|*square-meter| '(* meter meter))
(defun defkmunit (kmunit unit)
  (let (system unit-type sunit newsystem tmp un f)
    (setq system (or (gldominantunit unit) 'si))
    (setq newsystem (if (eq system 'english) 'km-english 'km-si))
    (setq unit-type (glfindunittype unit))
    (setq sunit (glsimplifyunit unit))
    (if unit-type
	(progn
	  (if (not (symbolp sunit))
	      (progn (setq sunit (glunkmify kmunit))
		     (if (not (or (glexpansion sunit)
				  (eq unit sunit)))
			 (setf (glexpansion sunit) unit)) ))
	  (setf (glactualunit kmunit) sunit)
	  (pushnew kmunit (glabbreviations sunit))
	  (if (and (setq un (cadr (assoc system (glstdunits unit-type))))
		   (numberp (setq f (glconvertunit unit un)))
		   (< (abs (- f 1.0)) 0.0001))
	      (pushnew (list newsystem kmunit) (glstdunits unit-type)
		       :test #'equal))
	  (setq tmp (glunitexpandc unit))
	  (pushnew (list unit-type kmunit 1.0
			 (list
			   (mapcar #'(lambda (x) (glsimplifyunit x newsystem))
				   (car tmp))
			   (mapcar #'(lambda (x) (glsimplifyunit x newsystem))
				   (cadr tmp))))
		   (glsystemunits newsystem) :test #'equal)
	  (defkmunittype kmunit unit))
        (format t "~A does not have known unit type~%" kmunit) ) ))

; 08 Jun 05; 08 Sep 05; 01 Jun 07
(defun kmifyunit (unit)
  (let (system unittype lst f)
    (setq system (or (gldominantunit unit) 'si))
    (setq unittype (or (glfindunittype unit) 'misc))
    (setq lst (cdr (assoc unittype *kmunittypes*)))
    (or (and (member unit '(radian radians)) '|*radian|)
        (some #'(lambda (un)
		  (and (numberp (setq f (glconvertunit unit un)))
		       (< (abs (- f 1.0)) 0.0001)
		       un))
	      lst)
	(glsimplifyunit unit (if (eq system 'english) 'km-english 'km-si)) ) ))

; 28 Jun 06
; Find assumed unit, given a unit or property name
(defun glassumedunit (x)
  (if (glunitp x)
      x
      (or (get x 'assumed-unit)
	  (cadr (assoc 'si (glstdunits x))))) )

; 28 Jun 06
; Find abstract unit for a given unit
(defun glabstractunit (unit)
  (let (sim)
    (or (and (symbolp unit) (get unit 'glunittype))
	(progn (setq sim (glsimplifyunit unit 'si))
	       (if (and (consp sim)
			(eq (car sim) '*)
			(numberp (cadr sim)))
		   (setq sim (caddr sim)))
	       (or (and (symbolp sim) (get sim 'glunittype))
		   (glcopyabstractunit sim)))) ))

; 28 Jun 06
(defun glcopyabstractunit (unit)
  (if (symbolp unit)
      (glabstractunit unit)
      (if (consp unit)
	  (cons (car unit)
		(mapcar #'glcopyabstractunit (cdr unit)))
	  unit)))

; 07 Jan 08
; Notice a given unit by recording unit and unit type of the given
; unit and its expansion.  Unit should be a symbol.
; Result is a list ((unittype unit) ...)
; (glnoticeunit 'kilometer-per-hour)
(defun glnoticeunit (unit)
  (let ((actunit (glactualu unit)))
    (glnoticeunitb (glexpansion actunit)
                   (list (list (glfindunittype actunit) actunit))) ))

(defun glnoticeunitb (unit set)
  (if (symbolp unit)
      (if (not (or (eq (gldimension unit) 0)
                   (member unit '(bit))))
          (union set (list (list (glfindunittype unit) unit))
                 :test #'equal)
          set)
      (if (and (consp unit) (member (car unit) '(* /)))
          (dolist (subunit (cdr unit) set)
            (setq set (glnoticeunitb subunit set)) )
          set) ) )


(dolist (pair '((force              (/ (* mass length) (* time time)))
		(area               (* length length))
		(volume             (* length length length))
		(power              (/ (* mass length length)
				       (* time time time)))
		(energy             (/ (* mass length length) (* time time)))
		(speed              (/ length time))
                (acceleration       (/ length (* time time)))
                (pressure           (/ force area))
                (density            (/ mass volume))
                (charge             (* current time))
		(electric-potential (/ power current))
		(electric-field     (/ force charge))
		(capacitance        (/ charge electric-potential))
		(resistance         (/ electric-potential current))
		(conductance        (/ current electric-potential))
		(magnetic-field     (/ mass (* current time time)))
		(magnetic-flux      (* magnetic-field area))
		(inductance         (/ magnetic-flux current))
		(frequency          (/ 1 time))
		(time-squared       (* time time))
		(dose               (/ (* length length) (* time time)))
		(concentration      (/ substance volume))
		(flow               (/ volume time))
		) )
  (let ((dim (glunitdim (second pair))))
    (setf (gldimension (first pair)) dim)
    (pushnew (list dim (first pair)) *gldimtounittype*) ))

(gldefsimpleunits 'dimensionless
               '((radian    1.0                (radians))
		 (steradian 1.0                (sr steradians))
		 (degree    0.017453292519943295   (deg degrees))
		 (arcminute 0.0002908882086657 (arcmin arcminutes arc-minute
						       arc-minutes))
		 (arcsecond 4.848136811095e-6  (arcsec arcseconds arc-second
						       arc-seconds))
		 (grad      0.015707963267948967 (grads)) ; 1/100 of right angle
		 (pi             3.1415926535897931 ())
		 (unity          1.0       ())
		 (zero           0         ())
		 (one            1         ())
		 (two            2         ())
		 (pair           2         ())
		 (three          3         ())
		 (four           4         ())
		 (five           5         ())
		 (six            6         ())
		 (seven          7         ())
		 (eight          8         ())
		 (nine           9         ())
		 (dozen          12.0      ())
		 (gross          144.0     ())
		 (ten            10.0      ())
		 (twenty         20.0      ())
		 (score          20.0      ())
		 (thirty         30.0      ())
		 (forty          40.0      ())
		 (fifty          50.0      ())
		 (sixty          60.0      ())
		 (seventy        70.0      ())
		 (eighty         80.0      ())
		 (ninety         90.0      ())
		 (hundred        100.0     ())
		 (gross          144       ())
		 (thousand       1000.0    ())
		 (million        1.0e6     ())
		 (billion        1.0e9     ())
		 (trillion       1.0e12    ())
		 (quadrillion    1.0e15    ())
		 (quintillion    1.0e18    ())
		 (percent        0.01      (\% percent))
		 (tenth          0.1       ())
		 (hundredth      0.01      ())
		 (thousandth     0.001     ())
		 (millionth      1.0e-6    ())
		 (billionth      1.0e-9    ())
		 (trillionth     1.0e-12   ())
		 (quadrillionth  1.0e-15   ())
		 (quintillionth  1.0e-18   ())
		 (yotta          1.0e24    (yotta-))
		 (zetta          1.0e21    (zetta-))
		 (exa            1.0e18    (exa-))
		 (peta           1.0e15    (peta-))
		 (tera           1.0e12    (tera-))
		 (giga           1.0e9     (giga-))
		 (mega           1.0e6     (mega-))
		 (kilo           1000.0    (kilo-))
		 (hecto          100.0     (hecto-))
		 (deka           10.0      (deca deka- deca-))
		 (deci           0.1       (deci-))
		 (centi          0.01      (centi-))
		 (milli          0.001     (milli-))
		 (micro          1.0e-6    (micro-))
		 (nano           1.0e-9    (nano-))
		 (pico           1.0e-12   (pico-))
		 (femto          1.0e-15   (femto-))
		 (atto           1.0e-18   (atto-))
		 (zepto          1.0e-21   (zepto-))
		 (yocto          1.0e-24   (yocto-))
		 (bit            1         (bits))
		 (byte           8         (bytes))
		 (kibi           1024      ())
		 (mebi           1048576   ())
		 (gibi           1073741824       ())
		 (tebi           1099511627776       ())
		 (pebi           1125899906842624       ())
		 (exbi           1152921504606846976       ())
		 (zebi           1180591620717411303424       ())
		 (yobi           1208925819614629174706176       ())
		 ))

(gldefsimpleunits 'length
               '((meter         1.0       (m meters metre))
		 (foot          0.3048    (ft feet))
		 (decimeter     0.1       (dm decimeters decimetre))
		 (centimeter    0.01      (cm centimeters centimetre))
		 (millimeter    0.001     (mm millimeters millimetre))
		 (dekameter     10.0      (dam dekameters decameter
					       decameters decametre))
		 (hectometer    100.0     (hm hectometers hectometre))
		 (kilometer     1000.0    (km kilometers kilometre))
		 (micron        1.0e-6    (um micro-meter micrometer
					      micrometers micro-meters
					      microns micrometre))
		 (nanometer     1.0e-9    (nm nanometers nanometre))
		 (angstrom      1.0e-10   (ang angstroms))
		 (inch          0.0254    (in inches))
		 (mile          1609.344  (mi miles))
		 (nautical-mile 1852.0    (nm nauticalmiles
					      nauticalmile nautical-miles))
                 (astronomical-unit 
                                1.49598e11 (au))
		 (light-year    9.46e15    (ly light-years
					       lightyear lightyears))
		 (parsec        3.083e16   (parsecs))
		 (fathom        1.8054     (fathoms))
		 (yard          0.9144     (yd yards))
		 (rod           5.0292     (rods))
		 (mil           0.0000254  (mils))
		 (furlong       201.168    (furlongs)) ) )

(dolist (x '(foot inch mile nautical-mile fathom yard rod furlong
	     pound slug pound-force ounce-force pound ounce ton long-ton
	     hundredweight dram grain pennyweight scruple acre square-mile
	     cubic-inch cubic-foot cubic-yard cubic-mile acre-foot gallon
	     quart peck bushel fifth pint cup fluid-ounce gill fluidram
	     minim tablespoon teaspoon foot-pound horsepower-hour grain
	     horsepower british-thermal-unit  btu pounds-per-square-inch psi
	     miles-per-hour miles-per-second feet-per-second knot
	     square-foot square-yard square-inch))
  (setf (glunitsys x) 'english))

(gldefsimpleunits 'mass
               '((kilogram         1.0           (kg kilograms))
		 (hectogram        0.1           (hg hectograms))
		 (dekagram         0.01     (dag dekagrams decagram decagrams))
		 (gram             0.001         (gm grams))
		 (decigram         0.0001        (dg decigrams))
		 (centigram        0.00001       (cg centigrams))
		 (milligram        1.0e-6        (mg milligrams))
		 (microgram        1.0e-9        (ug micrograms))
		 (metric-ton       1000.0        (metric-tons tonne tonnes))
		 (pound            0.45359237    (lb lbs pounds))    ; exactly
		 (slug             14.593902937  (slugs))
                    ; derived 02 Jun 95 based on pound, foot, and earth-gravity
		 (atomic-mass-unit 1.66053886e-27 (amu atomic-mass-units dalton))
		 (kilodalton       1.66053886e-24 (kilodaltons))
		 (earth-mass       5.98e24       ()) ) )

(gldefsimpleunits 'time
               '((second      1.0        (s sec secs seconds)) ))

(gldefderivedunits 'dimensionless
		   '((kilobyte   (* kilo byte)  (kB))
		     (megabyte   (* mega byte)  (MB))
		     (gigabyte   (* giga byte)  (GB))
		     (terabyte   (* tera byte)  (TB))
		     (petabyte   (* peta byte)  (PB))
		     (exabyte    (* exa byte)   (EB))
		     (zettabyte  (* zetta byte) (ZB))
		     (yottabyte  (* yotta byte) (YB))
		     (kibibyte   (* kibi byte)  (KiB))
		     (mebibyte   (* mebi byte)  (MiB))
		     (gibibyte   (* gibi byte)  (GiB))
		     (tebibyte   (* tebi byte)  (TiB))
		     (pebibyte   (* pebi byte)  (PiB))
		     (exbibyte   (* exbi byte)  (EiB))
		     (zebibyte   (* zebi byte)  (ZiB))
		     (yobibyte   (* yobi byte)  (YiB))
		     (kilobit    (* kilo bit)   (kbit))
		     (megabit    (* mega bit)   (Mbit))
		     (gigabit    (* giga bit)   (Gbit))
		     (terabit    (* tera bit)   (Tbit))
		     (petabit    (* peta bit)   (Pbit))
		     (exabit     (* exa bit)    (Ebit))
		     (zettabit   (* zetta bit)  (Zbit))
		     (yottabit   (* yotta bit)  (Ybit))
		     (kibibit    (* kibi bit)   (Kibit))
		     (mebibit    (* mebi bit)   (Mibit))
		     (gibibit    (* gibi bit)   (Gibit))
		     (tebibit    (* tebi bit)   (Tibit))
		     (pebibit    (* pebi bit)   (Pibit))
		     (exbibit    (* exbi bit)   (Eibit))
		     (zebibit    (* zebi bit)   (Zibit))
		     (yobibit    (* yobi bit)   (Yibit))
		     ))

(gldefderivedunits 'time
		   '((millisecond (* milli second)     (ms msec millisec
							   milliseconds))
		     (microsecond (* micro second)     (us usec microsec
							   microseconds))
		     (nanosecond  (* nano  second)     (ns nsec nanosec
							   nanoseconds))
		     (picosecond  (* pico  second)     (ps psec picosec
							   picoseconds))
		     (femtosecond (* femto second)     (femtoseconds femtosec))
		     (attosecond  (* atto  second)     (attoseconds attosec))
		     (minute      (* 60    second)     (min minutes))
		     (hour        (* 3600  second)     (hr hours))
		     (day         (* 86400 second)     (days))
		     (week        (* 604800 second)    (wk weeks))
		     (fortnight   (* 1209600 second)   (fortnights))
		     (month       (* 2629728 second)   (mon months))
		     (year        (* 31556736 second)  (yr years))
		     (century     (* 3155673600 second) (centuries)) ) )

(gldefderivedunits 'time-squared
		   '((second-squared (* second second) (s2 s^2)) ) )

(gldefderivedunits 'frequency '((hertz    (/ 1 second) (hz))
			        (becquerel (/ 1 second) (bq)) ) )

(gldefderivedunits 'frequency
		   '((kilohertz   (* kilo hertz)       (khz))
		     (megahertz   (* mega hertz)       (mhz))
		     (gigahertz   (* giga hertz)       (ghz))
		     (terahertz   (* tera hertz)       (thz))
		     (curie       (* 3.7e10 becquerel) (curies))
		     (kilobit/second      (/ (* kilo bit) second)  (kbit/s))
		     (megabit/second      (/ (* mega bit) second)  (Mbit/s))
		     (gigabit/second      (/ (* giga bit) second)  (Gbit/s))
		     (terabit/second      (/ (* tera bit) second)  (Tbit/s))
		     (petabit/second      (/ (* peta bit) second)  (Pbit/s))
		     (exabit/second       (/ (* exa bit) second)   ())
		     (kibibit/second      (/ (* kibi bit) second)  (Kibit/s))
		     (mebibit/second      (/ (* mebi bit) second)  (Mibit/s))
		     (gibibit/second      (/ (* gibi bit) second)  (Gibit/s))
		     (tebibit/second      (/ (* tebi bit) second)  (Tibit/s))
		     (pebibit/second      (/ (* pebi bit) second)  (Pibit/s))
		     (exbibit/second      (/ (* exbi bit) second)  (Exbit/s))

 ) )

(gldefsimpleunits 'current
               '((ampere      1.0       (A amp amps amperes)) ))

(gldefderivedunits 'acceleration
                 '((earth-gravity (* 9.80665 (/ meter (* second second))))
		   (gravity (* 9.80665 (/ meter (* second second))))
		   (feet-per-second-squared (/ foot (* second second))
		     (foot-per-second-squared ft/s/s ft/sec/sec foot/sec/sec
		      ft/s2 ft/sec2 foot/second/second))
		   (meters-per-second-squared (/ meter (* second second))
		      (meter-per-second-squared m/s/s m/sec/sec m/second/second
			m/s2 m/sec2 meter/sec/sec meter/second/second))
		   (centimeters-per-second-squared
		     (/ centimeter (* second second))
		     (centimeter-per-second-squared cm/s/s cm/sec/sec cm/s2
						      cm/sec2))
		   ))

(gldefderivedunits 'current
		   '((milliampere (* milli ampere)
				  (milliamp milliamps ma milliampere))
		     (microampere (* micro ampere)
				  (microamp microamps ua microamperes))
		     (abampere    (* 10 ampere) (abamp abamperes))
		     (statampere  (* 3.336e-10 ampere) (statamp statamperes))
		     ))

(gldefderivedunits 'electric-potential
		   '((volt      (/ (* kilogram meter meter)
				   (* ampere second second second))
				                (v volts))
		     (millivolt (* milli volt)  (mv millivolts))
		     (microvolt (* micro volt)  (uv microvolts))
		     (abvolt    (* 1.0e-8 volt) (abvolts))
		     (statvolt  (* 299.8 volt)  (statvolts)) ))

(gldefderivedunits 'resistance
		   '((ohm      (/ (* kilogram meter meter)
				  (* ampere ampere second second second))
		                 (ohms))
		     (kilohm   (* kilo ohm)     (kilohms))
		     (megohm   (* mega ohm)     (megohms))
		     (abohm    (* nano ohm)     (abohms))
		     (statohm  (* 8.987e11 ohm) (statohms)) ))

(gldefderivedunits 'conductance
		   '((siemens      (/ (* ampere ampere second second second)
				      (* kilogram meter meter)) (mho) ) ))

(gldefderivedunits 'capacitance
		   '((farad   (/ (* ampere ampere second second second second)
				 (* kilogram meter meter))
			                             (farads))
		     (microfarad (* micro farad)     (uf microfarads))
		     (picofarad  (* pico farad)      (pf picofarads))
		     (abfarad    (* giga farad)      (abfarads))
		     (statfarad  (* 1.113e-12 farad) (statfarads)) ))

(gldefderivedunits 'inductance
		   '((henry      (/ (* kilogram meter meter)
				    (* ampere ampere second second))
				                    (henrys))
		     (millihenry (* milli henry)    (mh millihenrys))
		     (microhenry (* micro henry)    (uh microhenrys))
		     (abhenry    (* nano henry)     (abhenrys))
		     (stathenry  (* 8.987e11 henry) (stathenrys)) ))

(gldefderivedunits 'magnetic-flux
		   '((weber      (/ (* kilogram meter meter)
				    (* ampere second second))
				                   (wb webers))
		     (maxwell    (* 1.0e-8 weber)  (maxwells)) ))

(gldefderivedunits 'magnetic-field
		   '((tesla      (/ kilogram (* ampere second second))
				                  (teslas T))
		     (gauss      (* 1.0e-4 tesla) ())
		     (milligauss (* milli gauss)  ()) ))

; 02 Aug 07
(gldefsimpleunits 'temperature
		 '((degree-kelvin      1.0       (k kelvin kelvins degK degreeK
                                                  degC degreeC degree-celsius
                                                  degree-centigrade))
		   (degree-rankine     5/9       (rankine degR degreeR
                                                  degree-rankine degF degreeF
                                                  degree-fahrenheit)) ))

; candela = 1/683 watt/steradian at 540E12 Hz (wavelength 550 nm)
; 100 watt incandescent bulb = 150 candela
; lumen = 1 candela * 1 steradian = 1/683 watt at wavelength 550 nm
; lux = 1 lumen / square-meter                      illuminance
; footcandle = 1 lumen / square-foot = 10.76 lux
; nit = candela / square-meter

(gldefsimpleunits 'luminosity
		 '((candela            1.0       (cd candelas)) ))

(gldefsimpleunits 'substance
		 '((mole               1.0       (mol moles)) ))

(gldefsimpleunits 'money
		 '((dollar             1.0       (dollars $))
                   (euro               1.33      (eu euros))
		   (cent               0.01      (cents penny pennies))
		   ))

(gldefderivedunits 'force
                 '((pound-force  (/ (* slug foot) (* second second)) (lbf))
		   (ounce-force  (/ pound-force 16)        ())
		   (newton (/ (* kilogram meter) (* second second))
			    (N nt newtons))
		   (dyne   (/ (* gram centimeter) (* second second))
			    (dynes))
		   (kilogram-force  (* kilogram earth-gravity)
				    (kgf kilogram-weight))
		   (gram-weight     (* gram earth-gravity) (gram-force)) ))

(gldefderivedunits 'mass
		 '((ounce  (/ pound 16)
			    (oz ounces))
		   (ton    (* 2000 pound)
			    (tons short-ton short-tons))
		   (long-ton (* 2240 pound)
			    (tons long-ton long-tons))
		   (hundredweight (* 100 pound) (hundredweights))
		   (dram   (/ ounce 16) (drams))
		   (grain  (/ dram 27.344) (grains))
		   (troy-pound (* 0.373 kilogram) (troy-pounds))
		   (troy-ounce (* 31.103 gram)
			       (troy-ounces ounce-troy ounces-troy))
		   (pennyweight (* 1.555 gram) (pennyweights))
		   (scruple (* 1.296 gram) (scruples))
		   ))

(gldefderivedunits 'area
                 '((square-meter (* meter meter)
				 (m^2 m2 meter-squared meters-squared 
				      metersquared square-meters))
		   (square-centimeter (* centimeter centimeter)
				      (cm^2 centimetersquared
					    centimeters-squared
					    centimeter-squared
					    square-centimeters))
		   (square-foot (* foot foot)
				(ft^2 foot-squared feet-squared footsquared
				      feetsquared square-feet))
		   (square-yard (* yard yard)
				(yard^2 yard-squared yardsquared yards-squared
					square-yards))
		   (square-inch (* inch inch)
				(in^2 inch-squared inchsquared inches-squared
				      square-inches))
		   (hectare (* 10000 metersquared)
			    (hectares))
		   (are     (* 100 metersquared) (ares))
		   (acre (* 43560 footsquared)
			 (acres))
		   (square-mile (* mile mile)
				(mile^2 mile-squared miles-squared milesquared
					square-miles))
		   (square-kilometer (* kilometer kilometer)
				     (km^2 kilometer-squared
					   kilometers-squared
					   kilometersquared
					   square-kilometers))
		   (square-millimeter (* millimeter millimeter)
				      (mm^2 millimeter-squared
					    millimeters-squared
					    millimetersquared
					    square-millimeters))
		   (square-micron (* micrometer micrometer)
				      (um^2 micrometer-squared
					    micrometers-squared
					    micron-squared microns-squared
					    micrometersquared
					    micronsquared square-microns))
		   (barn (* 1.0e-28 metersquared) (barns))
		   ))

(gldefderivedunits 'volume
		 '((cubic-meter     (* meter meter meter)
			            (m^3 meter-cubed metercubed meters-cubed
					 cubic-meters kiloliter kiloliters
					 kilolitre))
		   (cubic-centimeter (* centimeter centimeter centimeter)
				    (cm^3 centimeter-cubed centimeters-cubed
					  centimetercubed centimeterscubed
					  cubic-centimeters milliliter
					  milliliters ml cc
					  cubic-centimetre millilitre))
		   (cubic-millimeter (* millimeter millimeter millimeter)
				    (mm^3 millimeter-cubed millimeters-cubed
					  millimetercubed millimeterscubed
					  cubic-millimeters cubic-millimetre))
		   (cubic-micron     (* micron micron micron)
				     (micron-cubed microns-cubed
						   cubic-microns))
		   (cubic-kilometer  (* kilometer kilometer kilometer)
				    (km^3 kilometer-cubed kilometers-cubed
					  kilometercubed kilometerscubed
					  cubic-kilometers cubic-kilometre))
		   (cubic-inch       (* inch inch inch)
			            (in^3 inch-cubed inchcubed inchescubed
					  cubic-inches))
		   (cubic-foot       (* foot foot foot)
			            (ft^3 foot-cubed footcubed feetcubed
					  cubic-feet))
		   (cubic-yard       (* yard yard yard)
			            (yd^3 yard-cubed yardcubed yardscubed
					  yards-cubed cubic-yards))
		   (cubic-mile      (* mile mile mile)
			            (mile^3 mile-cubed miles-cubed
					     cubic-miles))
		   (acre-foot        (* acre foot)
			            (acrefoot acre-feet acrefeet))
		   (liter           (* 0.001 metercubed)
			            (l liters litre cubic-decimeter
				       cubic-decimeters))
		   (deciliter       (/ liter 10)
			            (dl deciliters decilitre))
		   (centiliter      (/ liter 100) (cl centiliters centilitre))
		   (dekaliter       (* liter 10)
				    (dekaliters decaliter decaliters decalitre
						dekalitre))
		   (hectoliter      (* 100 liter) (hectoliters hectolitre))
		   (gallon          (* 3.785411784 liter) (gal gallons))
		   (quart           (/ gallon 4) (qt quarts))
		   (peck            (* 8 quart) (pecks))
		   (bushel          (* 4 peck) (bushels))
		   (fifth           (/ gallon 5) (fifths))
		   (pint            (* 0.473 liter) (pt pints))
		   (cup             (/ pint 2) (cups))
		   (fluid-ounce     (* 0.029573 liter)
			            (floz fluidounce fluidounces fluid-ounces))
		   (gill            (* 4 fluid-ounce) (gills))
		   (fluidram        (* 3.5516 cubic-centimeter) (fluidrams))
		   (minim           (* 0.059194 cubic-centimeter) (minims))
		   (tablespoon      (/ fluidounce 2) (tbsp tablespoons))
		   (teaspoon        (/ tablespoon 3) (tsp teaspoons))
		   (barrel          (* 159 liter) (bbl))  ; as in oil
		   ) )

(gldefderivedunits 'power
                 '((watt       (/ (* kilogram meter meter)
				  (* second second second))
			       (w watts))
		   (milliwatt  (* milli watt)
			       (mw milliwatts milli-watt milli-watts))
		   (microwatt  (* micro watt)
			       (uw microwatts micro-watt micro-watts))
		   (kilowatt   (* kilo watt)
			       (kw kilowatts kilo-watt kilo-watts))
		   (megawatt   (* mega watt)
			       (mw megawatts mega-watt mega-watts))
		   (gigawatt   (* giga watt)
			       (gw gigawatts giga-watt giga-watts))
		   (terawatt   (* tera watt)
			       (tw terawatts tera-watt tera-watts))
		   (petawatt   (* peta watt)
			       (pw petawatts peta-watt peta-watts))
		   (horsepower (* 550 (/ (* foot pound-force) second))
			       (hp)) ) )

(gldefderivedunits 'energy
		 '((joule (/ (* kilogram meter meter) (* second second))
			  (j joules))
		   (kilojoule  (* kilo joule)
			       (kilojoules kilo-joule kilo-joules kj))
		   (megajoule  (* mega joule)
			       (megajoules mega-joule mega-joules mj))
		   (foot-pound (* foot pound-force)
			      (ftlb ft-lb footpound footpounds foot-pounds))
		   (kilowatt-hour (* kilo watt hour)
				 (kwh kilowatthour kilowatthours
				      kilowatt-hours))
		   (megawatt-hour (* mega watt hour)
				 (mwh megawatthour megawatthours
				      megawatt-hours mega-watt-hour
				      mega-watt-hours))
		   (watt-hour (* watt hour)
			     (watthour watthours watt-hours))
		   (horsepower-hour (* horsepower hour)
				    (hp-hour pferdstaerkenstunde
                                             cheval-vapeur-heure))
		   (electron-volt (* 1.60217733e-19 joule)
				  (ev electronvolt electronvolts
				      electron-volts))
		   (mev (* 1.60217733e-13 joule)
			(mega-electron-volts))
		   (gev (* 1.60217733e-10 joule)
			(giga-electron-volts))
		   (tev (* 1.60217733e-7 joule)
			(tera-electron-volts))
		   (hartree-energy (* 4.35974417e-18 joule) (E_h))
		   (calorie (* 4.184 joule)
			    (cal calorie calories))
		   (kilocalorie (* 4184.0 joule)
				(kcal kilo-calorie kilo-calories))
		   (british-thermal-unit (* 1055.056 joule)
				       (btu btus britishthermalunit
					    britishthermalunits
					    british-thermal-units))
		   (erg (* 1.0e-7 joule)
			(ergs))
		   (gallon-gasoline (* 114100 BTU) (gallon-gas gal-gas))
                   (mbtu (* mega btu))
                   (quad (* quadrillion btu) (quads))
                   (therm (* 100000 btu) (therms))
                   (mcf-ch4 (* 1027000 btu) (mcf))   ; 1000 cubic feet of ch4
                   (bcf-ch4 (* mega mcf-ch4) (bcf))  ; billion cubic feet ch4
                   (ton-coal (* 29.29e9 joule) (tce tonne-coal))
                         ; metric ton of coal equivalent
                         ; http://www.maproyalty.com/conversions.html
                   (barrel-oil (* 6119e6 joule) (barrels-oil boe))
                         ; barrel of oil equivalent
                   (ton-oil (* 44.76e9 joule) (tons-oil toe))
                         ; metric ton of oil equivalent
                   (cubic-meter-ch4 (* 37.26e6 joule))
                   (bcm-ch4 (* billion cubic-meter-ch4) (bcm))
                   (kcm-ch4 (* kilo cubic-meter-ch4) (kcm))
                   (terawatt-year (* tera watt year) (terawatt-years twyr))
                   (ton-tnt (* 4.185e9 joule) (tons-tnt))
                   (kiloton-tnt (* kilo ton-tnt) (kilotons-tnt))
                   (megaton-tnt (* mega ton-tnt) (megatons-tnt))
  ) )

(gldefderivedunits 'charge
  '((coulomb     (* ampere second)     (coul coulombs C))
    (microcoulomb (* micro coulomb)    (micro-coulomb uC))
    (nanocoulomb (* nano coulomb)      (nano-coulomb nC))
    (abcoulomb   (* 10.0 coulomb)      (abcoul abcoulombs))
    (statcoulomb (* 3.336e-10 coulomb) (statcoul statcoulombs))
    (amperehour  (* 3600.0  coulomb)   (amp-hour ampere-hour
				        amperehours ampere-hours)) ))

(gldefderivedunits 'electric-field
  '((newton-per-coulomb (/ newton coulomb) (N/C newton/coulomb))
    (volts-per-meter    (/ volt meter)     (v/m volt/meter)) ) )

(gldefderivedunits 'pressure
  '((pounds-per-square-inch (/ (* 144 pound-force) (* foot foot)) (psi))
    (pascal     (/ newton (* meter meter)) (pa))
    (kilopascal (* 1000.0 pascal) (kilo-pascal kpa kilopascals))
    (bar        (* 1.0e5 pascal)  (bars))
    (millibar   (* milli bar)     (millibars))
    (torr       (* (/ 101325 760) pascal) ())
    (dynes-per-square-centimeter (/ dyne (* centimeter centimeter)))
    (atmosphere (* 101325 pascal) (atm)) ))

(gldefderivedunits 'speed
                 '((miles-per-hour (/ mile hour) (mph mile-per-hour))
		   (miles-per-second (/ mile second) (mile-per-second))
		   (kilometers-per-hour (/ kilometer hour)
					(kph kilometer-per-hour))
		   (kilometers-per-second (/ kilometer second)
					  (kps kilometer-per-second))
		   (feet-per-second (/ foot second)
				    (foot-per-second fps ft/s ft/sec foot/sec
						     ft/second foot/second))
		   (meters-per-second (/ meter second)
				      (meter-per-second m/s m/sec m/second
				       meter/sec meter/second))
		   (centimeters-per-second (/ centimeter second)
					   (centimeter-per-second cm/s cm/sec))
		   (knot              (/ nautical-mile hour) (knots))
		   (speed-of-light    (* 299792458 (/ meter second)))
		   ))

(gldefderivedunits 'dose      ; of radiation
		   '((gray    (/ joule kilogram)   (gy))
		     (sievert (/ joule kilogram)   (sv))
		     (rad     (/ gray 100)         ())
		     (rem     (/ sievert 100)      ()) ))

(gldefderivedunits 'concentration
		   '((molar (/ mole liter) ()) ) )

(gldefderivedunits 'flow
		   '((cubic-meter-per-second (/ (* meter meter meter) second)
					     (cubic-meters-per-second))
		     (gallon-per-second      (/ gallon second)
					     (gallons-per-second))
		     (sverdrup               (/ (* mega meter meter meter)
						second)
					     (sverdrups))
		     (acre-foot-per-day      (/ acre-foot day)
					     (acre-feet-per-day)) ))

(dolist (x '(si cgs english)) (setf (glsystemunits x) nil))

(dolist (pair '((length   (si meter)        (cgs centimeter) (english foot))
		(mass     (si kilogram)     (cgs gram)       (english slug))
		(time     (si second)       (cgs second)     (english second))
		(force    (si newton)       (cgs dyne)    (english pound-force))
		(area     (si square-meter) (cgs square-centimeter)
		          (english square-foot))
		(volume   (si cubic-meter)  (cgs cubic-centimeter)
		          (english cubic-foot))
		(power    (si watt)         (cgs watt)  (english horsepower))
		(energy   (si joule)        (cgs erg)   (english foot-pound))
		(pressure (si pascal)       (cgs dynes-per-square-centimeter)
			  (english pounds-per-square-inch))
		(speed               (si meters-per-second)
				     (cgs centimeters-per-second)
				     (english feet-per-second))
                (acceleration        (si meters-per-second-squared)
				     (cgs centimeters-per-second-squared)
				     (english feet-per-second-squared))
                (density             (si ) (cgs ) (english ))
                (charge              (si coulomb) (cgs ) (english ))
		(electric-potential  (si volt) (cgs ) (english ))
		(current             (si ampere) (cgs ) (english ))
		(capacitance         (si farad) (cgs ) (english ))
		(resistance          (si ohm) (cgs ) (english ))
		(conductance         (si siemens) (cgs ) (english ))
		(magnetic-field      (si tesla) (cgs ) (english ))
		(magnetic-flux       (si weber) (cgs ) (english ))
		(inductance          (si henry) (cgs ) (english ))
		(concentration       (si molar))
		(substance           (si mole))
		))
  (setf (glstdunits (car pair)) (cdr pair))
  (dolist (pr (cdr pair))
    (let (tmp)
      (when (cdr pr)
	(setq tmp (glunitexpandc (cadr pr)))
	(push (list (car pair) (cadr pr)
		    (if (numberp (caar tmp)) (caar tmp) 1.0)
		    (if (numberp (caar tmp)) (cons (cdar tmp) (cdr tmp)) tmp))
	      (glsystemunits (car pr)))) ) ))

(defconstant *speed-of-light*    '(q 2.99792458e8 (/ meter second)))
(defconstant *gravitational-constant*
             '(q 6.6720e-11  (/ (* meter meter meter)
			     (* kilogram second second))))
(defconstant *elementary-charge* '(q 1.6021892e-19 coulomb))
(defconstant *electron-mass*     '(q 9.109534e-31 kilogram))
(defconstant *earth-gravity*     '(q 9.80665 (/ meter (* second second))))
; physlaws.lsp              Gordon S. Novak Jr.            ; 31 Jan 08

; Copyright (c) 2008 Gordon S. Novak Jr. and The University of Texas at Austin.

; Definitions for physical laws.  Derived from equations.lsp

; 19 Feb 04; 23 Feb 04; 27 Feb 04; 01 Mar 04; 02 Mar 04; 15 Mar 04; 17 Mar 04
; 18 Mar 04; 19 Mar 04; 22 Mar 04; 23 Mar 04; 30 Mar 04; 31 Mar 04; 01 Apr 04
; 02 Apr 04; 05 Apr 04; 06 Apr 04; 08 Apr 04; 09 Apr 04; 12 Apr 04; 13 Apr 04
; 15 Apr 04; 16 Apr 04; 20 Apr 04; 22 Apr 04; 23 Apr 04; 27 Apr 04; 03 Jun 04
; 07 Apr 05; 13 Sep 05; 06 Dec 05; 28 Jun 06; 29 Jun 06; 30 Jun 06; 25 Aug 06
; 28 Feb 07

; Things that should be added:
;   symbol used externally for each variable (e..g. rho for resistivity)

(defmacro basis-vars     (x) `(get ,x 'basis-vars))
(defmacro vars-units     (x) `(get ,x 'vars-units))
(defmacro vars-unittypes (x) `(get ,x 'vars-unittypes))
(defmacro equations      (x) `(get ,x 'equations))
(defmacro assumed-unit   (x) `(get ,x 'assumed-unit))

(defvar *assumed-units*)
(setq *assumed-units* '(
  ((time period) second)
  (frequency hertz)
  ((length radius height distance wavelength) meter)
  (mass kilogram)
  ((velocity speed) (/ meter second))
  ((acceleration gravity) (/ meter (* second second)))
  (area (* meter meter))
  (volume (* meter meter meter))
  ((weight force) newton)
  (pressure pascal)
  ((impulse momentum) (* kilogram (/ meter second)))
  ((energy kinetic-energy potential-energy work) joule)
  (power watt)
  (charge coulomb)
  (capacitance farad)
  ((voltage emf) volt)
  (magnetic-field tesla)
  (resistance ohm)
  (current ampere)
  (cost dollar)
  (temperature degree-kelvin)
  ))

(dolist (pair *assumed-units*)
  (if (consp (car pair))
      (dolist (var (car pair)) (setf (assumed-unit var) (cadr pair)))
      (setf (assumed-unit (car pair)) (cadr pair)) ) )

; format is (name basis-vars constant-vars units equations)
; units is (var unit assumed-value) where assumed-value is used in changeobjvar
(defvar *physlaws*)
(setq *physlaws* '(
; Geometry

(circle (radius centerx centery)
  ((diameter meter)
   (circumference meter))
  ((= diameter (* 2 radius))
   (= circumference (* pi diameter))
   (= area (* pi (expt radius 2))) ) )

(sphere (radius)
  ((diameter meter)
   (circumference meter))
  ((= diameter (* 2 radius))
   (= circumference (* pi diameter))
   (= area (* 4 (* pi (expt radius 2))))
   (= volume (* (/ (* 4 pi) 3) (expt radius 3)))
   (= center (tuple (x centerx) (y centery)))
   (= centerx (x center))
   (= centery (y center)) ) )

(cylinder (radius length)
  ((diameter meter)
   (circumference meter))
  ((= diameter (* 2 radius))
   (= circumference (* pi diameter))
   (= area (* circumference length))
   (= volume (* (* pi (expt radius 2)) length)) ) )

(cone (radius height)
  ((side meter)
   (side-area (* meter meter))
   (bottom-area (* meter meter))
   (total-area (* meter meter)) )
  ((= diameter (* 2 radius))
   (= circumference (* pi diameter))
   (= side (sqrt (+ (expt radius 2) (expt height 2))))
   (= side-area (* pi (* radius side)))
   (= bottom-area (* pi (expt radius 2)))
   (= volume (* (/ pi 3) (* (expt radius 2) height)))
   (= total-area (+ side-area bottom-area)) ))

(square (side)
  ((side meter)
   (circumference meter)
   (diagonal meter))
  ((= diagonal (* (sqrt 2) side))
   (= circumference (* 4 side))
   (= area (expt side 2)) ) )

(rectangle (width height)
  ((width meter)
   (height meter)
   (circumference meter)
   (diagonal meter))
  ((= diagonal (sqrt (+ (expt width 2) (expt height 2))))
   (= circumference (* 2 (+ width height)))
   (= area (* width height)) ) )

; density and number of items in area/volume assuming perfect packing
(conglomerate (number item-volume)
  ((number unity))
  ((= number (/ container-volume item-volume))))


; 01 Mar 96
(line-segment (p1x p1y p2x p2y)
  ()
  ((= p1     (tuple (x p1x) (y p1y)))
   (= p1x    (x p1))
   (= p1y    (y p1))
   (= p2     (tuple (x p2x) (y p2y)))
   (= p2x    (x p2))
   (= p2y    (y p2))
   (= deltax (- p2x p1x))
   (= deltay (- p2y p1y))
   (= size   (tuple (x deltax) (y deltay)))
   (= deltax (x size))
   (= deltay (y size))
   (= slope  (/ deltay (float deltax)))
   (= slope  (tan theta))
   (= slope  (/ 1.0 (tan phi)))
   (= length (sqrt (+ (expt deltax 2) (expt deltay 2))))
   (= theta  (atan deltay deltax))
   (= phi    (- (/ pi 2.0) theta))
   (= phi    (atan deltax deltay))
   (= deltay (* length (sin theta)))
   (= deltax (* length (cos theta)))
   (= deltay (* length (cos phi)))
   (= deltax (* length (sin phi))) ) )

(vector (x y)
  ()
  ((= slope  (/ y (float x)))
   (= slope  (tan angle))
   (= slope  (/ 1.0 (tan phi)))
   (= magnitude (sqrt (+ (expt x 2) (expt y 2))))
   (= angle  (atan y x))
   (= phi    (- (/ pi 2.0) angle))
   (= phi    (atan x y))
   (= y      (* magnitude (sin angle)))
   (= x      (* magnitude (cos angle)))
   (= y      (* magnitude (cos phi)))
   (= x      (* magnitude (sin phi))) ) )

(region (left bottom width height)
  ((left meter) (bottom meter) (width meter) (height meter))
  ((= left   (x start))
   (= Bottom (y start))
   (= start  (tuple (x left) (y bottom)))
   (= width  (x size))
   (= height (y size))
   (= area   (* width height))
   (= perimeter (+ (* 2 width) (* 2 height)))
   (= size   (tuple (x width) (y height)))
   (= right  (+ left width))
   (= top    (+ bottom height))
   (= c2     (tuple (x right) (y bottom)))
   (= right  (x c2))
   (= bottom (y c2))
   (= c3     (tuple (x right) (y top)))
   (= right  (x c3))
   (= top    (y c3))
   (= c4     (tuple (x left) (y top)))
   (= left   (x c4))
   (= top    (y c4)) ) )

(right-triangle (x y)
  ((x meter) (y meter))
  ((= theta  (atan y x))
   (= phi    (atan x y))
   (= r      (sqrt (+ (expt x 2) (expt y 2))))
   (= x      (* r (cos theta)))
   (= y      (* r (sin theta)))
   (= x      (* r (sin phi)))
   (= y      (* r (cos phi)))
   (= slope  (tan theta))
   (= slope  (/ y x)) ) )

(triangle (side-a side-b side-c)
  ((side-a meter) (side-b meter) (side-c meter))
  ((= angle-a (- '(q 180 degrees) (+ angle-b angle-c)))
   (= side-a (* side-b (/ (sin angle-a) (sin angle-b))))
   (= side-b (* side-a (/ (sin angle-b) (sin angle-a))))
   (= side-c (* side-b (/ (sin angle-c) (sin angle-b))))
   (= angle-a (acos (/ (- (+ (expt side-b 2) (expt side-c 2))
			  (expt side-a 2))
		       (* 2 (* side-b side-c))))) ) )

; for block on inclined plane problems
(inclined-plane (x y weight)
  ((r meter) (x meter) (y meter) (theta radian) (phi radian)
   (slope unity) (normal-force newton) (tangential-force newton)
   (friction-force newton) (net-force newton) (mu unity) )
  ((= gravity     '(q 9.80665 (/ m (* s s))))
   (= mu 0)   ; default
   (= weight      (* gravity mass))
   (= theta  (atan y x))
   (= phi    (atan x y))
   (= r      (sqrt (+ (expt x 2) (expt y 2))))
   (= x      (* r (cos theta)))
   (= y      (* r (sin theta)))
   (= x      (* r (sin phi)))
   (= y      (* r (cos phi)))
   (= slope  (tan theta))
   (= slope  (/ y x))
   (= normal-force (* weight (cos theta)))
   (= tangential-force (* weight (sin theta)))
   (= friction-force (* mu normal-force))
   (= work    (* friction-force r))
   (= net-force (- tangential-force friction-force))
   (= net-force (* mass acceleration))
   (= r (* 1/2 (* acceleration (expt time 2))))
   (= velocity (* acceleration time))
 ))

; Mechanics

; perhaps should have a 'mechanics' task for e.g. Princeton 71.

(uniform-motion (x0 v dt)
  ((x0 meter) (distance meter) (velocity (/ meter second))
   (dt second) (t0 second))
  ((= x (+ x0 distance))
   (= t0 '(q 0 second))
   (= x0 '(q 0 meter))
   (= dt (- time t0))
   (= distance (* velocity dt)) ))

(acceleration (force mass)
  ()
  ((= force  (* mass acceleration)) ) )   ; Newton's 2nd law

(uniform-acceleration (acceleration time)
  ((distance meter) (acceleration (/ meter (* second second)))
   (velocity (/ meter second)))
  ((= distance      (* (/ acceleration 2) (expt time 2)))
   (= velocity      (* acceleration time)) ) )

(falling (time)
  ((g (/ meter (* second second))) (h meter) (v (/ meter second)))
  ((= g      '(q 9.80665 (/ m (* s s))))
   (= h      (* (/ g 2) (expt time 2)))
   (= v      (* g time)) ) )

(fall (time)
  ((horizontal-distance meter)
   (horizontal-velocity (/ meter second))
   (total-velocity (/ meter second)))
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= horizontal-velocity '(q 0 (/ m s)))           ; default
   (= height   (* 1/2 (* gravity (expt time 2))))
   (= velocity (* gravity time))                   ; vertical velocity
   (= kinetic-energy (* 1/2 (* mass (expt total-velocity 2))))
   (= horizontal-distance (* horizontal-velocity time))
   (= total-velocity (sqrt (+ (expt velocity 2) (expt horizontal-velocity 2))))
  ))

; (phys '(what is the distance of a projectile with height 7.5 m and
;         x-velocity 4.5 m/s))    ; Giancoli 3.19 p. 72
; (phys '(what is the x-velocity of a projectile with height 56 m and
;         distance 45 m))         ; Giancoli 3.24 p. 72
; angle is w.r.t. horizontal
; average acceleration, impulse, time of max height   AP #21
(projectile (time)
  ((angle unity) (x-velocity (/ meter second)) (y-velocity (/ meter second))
   (v0 (/ meter second)))
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= v1 v0)
   (= x-velocity (* v0 (cos angle)))
   (= y-velocity (* v0 (sin angle)))
   (= time (/ (* 2 y-velocity) gravity))     ; time to impact at y = 0
   (= distance (* time x-velocity))          ; x distance to impact at y = 0
   (= height   (/ (expt y-velocity 2) (* 2 gravity)))
 ))

(centrifugal-force (m v r)
  ((acceleration (/ meter (* second second))) (f newton) (v (/ meter second))
   (r meter))
  ((= acceleration (/ (expt v 2) r))
   (= f            (* m acceleration))))

(circular-motion (mass velocity radius)

  ((omega (/ radian second))
   (moment-of-inertia (* kilogram (* meter meter)))
   (angular-momentum (/ (* kilogram (* meter meter)) second)) )
  ((= acceleration (/ (expt velocity 2) radius))
   (= force        (* mass acceleration))
   (= kinetic-energy   (* (/ mass 2) (expt velocity 2)))
   (= moment-of-inertia (* mass (expt radius 2)))
   (= omega (/ velocity radius))
   (= angular-momentum (* omega moment-of-inertia)) ))

(gravity-kinetic (mass height velocity)
  ()
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= kinetic-energy   (* (/ mass 2) (expt velocity 2)))
   (= potential-energy (* (* mass gravity) height))
   (= energy (+ kinetic-energy potential-energy))) )

(kinetic-energy (mass velocity)
  ()
  ((= energy (* (/ mass 2) (expt velocity 2))) ))

(work-energy (mass v1 v2)
  ((v1 (/ meter second)) (v2 (/ meter second))
   (energy1 joule) (energy2 joule))
  ((= energy-change (- energy2 energy1))
   (= work energy-change)
   (= power (/ work time))
   (= energy1 (* 1/2 (* mass (expt v1 2))))
   (= energy2 (* 1/2 (* mass (expt v2 2))))
   ))

; ***** add to this.  AP #22
; add frequency of oscillation of spring Princeton #71
(harmonic-motion (mass height velocity)
  ((angular-speed (/ radian second)))
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= kinetic-energy   (* (/ mass 2) (expt velocity 2)))
   (= potential-energy (* (* mass gravity) height))
   (= energy (+ kinetic-energy potential-energy))
   (= speed (* frequency wavelength))
   (= angular-speed (* (* 2 pi) frequency))
   (= period (/ 1 frequency)) ))

; These equations assume that m1 is the large body (e.g. planet)
; and m2 is the small body (e.g. satellite).
(gravitation (m1 m2 radius)
  ((m1 kilogram) (m2 kilogram) (escape-velocity (/ meter second))
   (gravitation-constant (/ (* nt m m) (* kg kg)))
   (gravity (/ meter (* second second)))
   (satellite-velocity (/ meter second))
   (omega (/ 1 second)) (moment-of-inertia (* kg (* m m))))
  ((= gravitation-constant '(q 6.6742e-11 (/ (* nt m m) (* kg kg))))
   (= gravity (/ (* gravitation-constant m1) (expt radius 2)))
   (= satellite-velocity (sqrt (/ (* m1 gravitation-constant) radius)))
   (= kinetic-energy   (* (/ m2 2) (expt satellite-velocity 2))) ; satellite
   (= potential-energy (- (/ (* gravitation-constant (* m1 m2)) radius)))
   (= escape-velocity (sqrt (/ (* 2 (* m1 gravitation-constant)) radius)))
   (= force   (/ (* gravitation-constant (* m1 m2)) (expt radius 2)))
   (= moment-of-inertia (* m2 (expt radius 2)))                  ; satellite
   (= omega (/ satellite-velocity radius))                       ; satellite
   (= angular-momentum (* omega moment-of-inertia))              ; satellite
 ))

(weight (mass volume)
  ()
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= weight  (* gravity mass))
   (= density (/ mass volume))  ))

(physob (mass volume)
  ()
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= weight  (* gravity mass))
   (= density (/ mass volume))  ))

(friction (normal-force mu)
  ((friction-force newton) (mu unity) (normal-force newton))
  ((= friction-force (* mu normal-force))
   (= work    (* friction-force distance))
   (= power   (* friction-force speed)) ))

(friction-weight (weight mu)
  ((friction-force newton) (mu unity))
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= friction-force (* mu weight))
   (= work    (* friction-force distance))
   (= power   (* friction-force speed))
   (= weight  (* gravity mass)) ))

; two masses, one on table (m1), one connected via string and pulley (m2),
; possibly with friction
(weight-pulley (m1 m2)
  ((m1 kilogram) (m2 kilogram) (friction-force newton) (net-force newton)
   (m1-weight newton) (m2-weight newton) (total-mass kilogram) (mu unity))
  ((= mu 0)                     ; default
   (= gravity '(q 9.80665 (/ m (* s s))))
   (= friction-force (* mu m1-weight))
   (= work    (* friction-force distance))
   (= power   (* friction-force speed))
   (= m1-weight  (* gravity m1))
   (= net-force (- m2-weight friction-force))
   (= m2-weight (* gravity m2))
   (= net-force (* total-mass acceleration))
   (= total-mass (+ m1 m2))
 ))

(spring (spring-constant stretch)
  ((spring-constant unity) (stretch meter))
  ((= gravity '(q 9.80665 (/ m (* s s))))
   (= weight  (* gravity mass))
   (= force weight)
   (= force (* spring-constant stretch)) ))   ; must be careful about direction

(lift (mass height time)
  ()
  ((= gravity     '(q 9.80665 (/ m (* s s))))
   (= weight      (* gravity mass))
   (= acceleration '(q 0 (/ m (* s s))))  ; default.  acceleration upward
   (= force (+ weight (* mass acceleration)))
   (= work        (* force height))
   (= speed       (/ height time))
   (= power       (* force speed))
   (= power       (/ work time)) ))

(collide-and-stick (m1 v1 m2 v2)
  ((total-mass kilogram) (total-momentum (* kilogram (/ meter second)))
   (m1 kilogram) (m2 kilogram) (v1 (/ meter second)) (v2 (/ meter second))
   (final-velocity (/ meter second)))
  ((= total-mass (+ m1 m2))
   (= total-momentum (+ (* m1 v1) (* m2 v2)))
   (= total-momentum (* total-mass final-velocity))))

(momentum (mass velocity)
  ()
  ((= momentum (* mass velocity))
   (= impulse momentum)))

; two bodies exert a force on each other.
(symmetric-forces (m1 m2 force)
  ((m1 kilogram) (m2 kilogram) (f1 newton) (f2 newton)
   (a1 (/ meter (* second second))) (a2 (/ meter (* second second))) )
  ((= f1 force)
   (= f2 (- f1))
   (= f1 (* m1 a1))
   (= f2 (* m2 a2))
   ))

(rope (tension)
  ((tension newton))
  ((= force (- tension))))    ; tension >= 0

; object floating in a liquid
(float (mass volume)
  ((displaced-volume (* meter meter meter))
   (floating-volume (* meter meter meter))
   (percentage-floating unity)
   (percentage-below unity)
   (density (/ kilogram (* meter meter meter))))
  ((= gravity     '(q 9.80665 (/ m (* s s))))
   (= weight      (* gravity mass))
   (= density  (/ weight volume))
   (= liquid-density '(q 1000 (/ kilogram (* meter meter meter))))   ; default: water
   (= displaced-volume (/ mass liquid-density))
   (= floating-volume (- volume displaced-volume))
   (= proportion-floating (/ floating-volume volume))
   (= proportion-below (/ displaced-volume volume))
 ))

; determine density of an object by weighing it in fluid and out
; used as an example for entering new knowledge
(object-density (weight mass)
  ((displaced-volume (* meter meter meter))
   (weight-in-liquid newton) (displaced-weight newton)
   (displaced-mass kg) )
  ((= gravity '(q 9.80665 (/ m (* s s))))           ; default
   (= weight  (* gravity mass))
   (= mass    (* density volume))
   (= weight-in-liquid (- weight displaced-weight))
   (= displaced-weight (* gravity displaced-mass))
   (= fluid-density '(q 1 (/ kg liter)))            ; water, default
   (= displaced-mass (* fluid-density volume))
 ))


; Electronics

(electric-dipole (q1 q2 radius)
  ((electric-field (/ volt meter)) (q1 coulomb) (q2 coulomb))
  ((= ke '(q 8.9875517879979115E9
	     (/ (* Newton (* meter meter)) (* coulomb coulomb))))
   (= electric-field (* ke (/ q1 (expt radius 2))))
   (= force (* q2 electric-field))
   (= potential-energy (* ke (* q2 (/ q1 radius))))
   (= work potential-energy)
 ))

(charged-sphere (q radius)
  ((electric-field (/ volt meter)) (q coulomb) (potential volt))
  ((= ke '(q 8.9875517879979115E9
	     (/ (* Newton (* meter meter)) (* coulomb coulomb))))
   (= electric-field (* ke (* q (expt radius 2))))
   (= potential (/ (* ke q) radius))
  ))

(capacitor (capacitance voltage)
  ((electric-field (/ volt meter)))
  ((= epsilon0 '(q 8.854187817E-12 (/ farad meter)))
   (= electric-field (/ voltage distance))
   (= force (* charge electric-field))
   (= dielectric-constant 1)  ; default
   (= capacitance (* (* epsilon0 dielectric-constant)
		     (/ area distance)))
   (= charge (* capacitance voltage))
   (= work energy)
   (= energy (* 1/2 (* capacitance (expt voltage 2))))
  ))

; *****
; add equation for induced current 1998 #45
(magnetic-field (resistance dflux/dt)
  ((delta-time second) (field tesla))
  ((= flux (* field area))
   (= dflux/dt (/ delta-flux delta-time))
   (= emf dflux/dt)
   (= emf (* current resistance))     ; induced current in a wire loop
))


; *****
; This combines (a) emf due to wire moving in magnetic field
;               (b) magnetic field due to current in a wire
;               (c) force on a wire in a magnetic field
; Maybe they should be separate
(wire-magnetic (length magnetic-field)
  ((magnetic-field tesla) (force/length (/ newton meter))
   (emf (/ (* m m tesla) second)))
  ((= mu0 '(q 12.566370614359172E-7 (/ newton (* ampere ampere))))
              ; exactly 4 * PI * 10^-7
   (= emf (* length (* magnetic-field velocity)))
   ; force/length in a current-carrying wire, assumed perpendicular to field
   (= force/length (* magnetic-field current))
   (= magnetic-field (/ (* mu0 current) (* (* 2 pi) radius)))
	))

(parallel-capacitors (c1 c2)
  ((c1 farad) (c2 farad))
  ((= capacitance (+ c1 c2)) ))

(series-capacitors (c1 c2)
  ((c1 farad) (c2 farad))
  ((= capacitance  (/ 1 (+ (/ 1 c1) (/ 1 c2)))) ))

(resistor (resistance current)
  ((unit-cost (/ dollar joule)))
  ((= voltage (* resistance current))
   (= power   (* resistance (expt current 2)))
   (= power   (/ (expt voltage 2) resistance))
   (= work    (* power time))
   (= cost    (* work unit-cost)) ))

; ohmic conductor
(wire (resistivity radius length)
  ((resistivity (* ohm meter)) (diameter meter) (circumference meter)
   (resistance/length (/ ohm meter)))
  ((= diameter (* 2 radius))
   (= circumference (* pi diameter))
   (= area (* pi (expt radius 2)))
   (= resistance/length (/ resistivity area))
   (= resistance (* resistance/length length)) ))

(series-resistors (r1 r2)
  ((r1 ohm) (r2 ohm))
  ((= resistance (+ r1 r2)) ))

(parallel-resistors (r1 r2)
  ((r1 ohm) (r2 ohm))
  ((= resistance (/ 1 (+ (/ 1 r1) (/ 1 r2)))) ))

(battery (voltage current)
  ((loss-voltage volt) (loss-power watt) (terminal-voltage volt)
   (internal-resistance ohm))
  ((= loss-voltage (* internal-resistance current))
   (= loss-power (* internal-resistance (expt current 2)))
   (= emf voltage)
   (= terminal-voltage (- voltage loss-voltage))
   (= power (* terminal-voltage current))
   (= work (* charge terminal-voltage))
   (= internal-resistance '(q 0 ohm))
 ))

(fluid-flow (area velocity)
  ((diameter meter) (circumference meter)
   (c2 meter) (d2 meter) (area2 (* meter meter))
   (flow-rate (/ (* meter meter meter) second)))
  ((= diameter (* 2 radius))
   (= d2 (* 2 r2))
   (= circumference (* pi diameter))
   (= c2 (* pi d2))
   (= area (* pi (expt radius 2)))
   (= area2 (* pi (expt r2 2)))
   (= flow-rate (* velocity area))
   (= flow-rate (* v2 area2))
   (= bogo-bernoulli (* velocity pressure))  ; qualitative only
 ))

; waves

; Assumes wavelength = distance between 2 nodes in a standing wave
(wave (frequency speed)
  ((fundamental-frequency (/ 1 second)) (angular-speed (/ radian second))
   (period second))
  ((= speed (* frequency wavelength))
   (= fundamental-frequency (/ frequency 2))
   (= angular-speed (* (* 2 pi) frequency))
   (= period (/ 1 frequency)) ))
 
; ***** relate mass/length, tension, etc.  1998 #FR5
; harmonic-frequency = frequency of the nth harmonic
(string-wave (fundamental-frequency mass/length length)
  ((frequency hertz) (harmonic-frequency hertz) (harmonic-number unity))
  ((= fundamental-frequency (/ (sqrt (/ force mass/length)) (* 2 length)))
             ; Serway p. 443
   (= harmonic-frequency (* harmonic-number fundamental-frequency))
   ))

(diffraction (wavelength offset-first-max screen-distance)
  ((offset-first-max meter) (screen-distance meter)
   (angle radian) (slit-separation meter))
  ((= offset-first-max (* (sin angle) radius))
   (= radius (sqrt (+ (expt offset-first-max 2)
		      (expt screen-distance 2))))
   (= wavelength (* slit-separation (/ offset-first-max radius)))
   (= speed (* frequency wavelength))
   (= period (/ 1 frequency)) ))

(sound (frequency)
  ((doppler-frequency hertz)
   (observer-speed (/ meter second))
   (source-speed (/ meter second))
   (intensity (/ watt (* meter meter)))
   (power watt))
  ((= speed '(q 343 (/ meter second)))   ; air at 20 C
   (= speed (* frequency wavelength))
   (= period (/ 1 frequency))
   (= doppler-frequency (* frequency (/ (+ speed observer-speed)
					(- speed source-speed))))
   (= intensity0 '(q 1.0e-12 (/ watt (* meter meter))))
   (= decibel (* 10 (log10 (/ intensity intensity0))))
   (= intensity (/ power (* (* 4 pi) (expt radius 2))))
 ))

(light (frequency)
  ((index-of-refraction unity))
  ((= speed-of-light '(q 299792458 (/ meter second)))
   (= planck-constant '(q 6.6260693E-34 (* joule second)))
   (= index-of-refraction 1)     ; default
   (= speed (/ speed-of-light index-of-refraction))
   (= speed (* frequency wavelength))
   (= momentum (/ planck-constant wavelength))
   (= period (/ 1 frequency))
   (= energy (* planck-constant frequency))
 ))

(refraction (index-of-refraction1 index-of-refraction2)
  ((index-of-refraction1 unity) (index-of-refraction2 unity)
   (theta1 radian) (theta2 radian))
  ((= speed-of-light '(q 299792458 (/ meter second)))
   (= speed (/ speed-of-light index-of-refraction))
   (= speed (* frequency wavelength))
   (= period (/ 1 frequency))
	(= (* index-of-refraction1 (sin theta1))
	   (* index-of-refraction2 (sin theta2)))
 ))

(concave-mirror (focal-length)
  ((focal-length meter 0.1)   ; to avoid divide by 0 in changeobjvar
   (image-distance meter)
   (subject-distance meter) (image-height meter) (subject-height meter)
   (magnification unity))
  ((= focal-length (/ radius 2))
   (= (/ 1 focal-length) (+ (/ 1 image-distance) (/ 1 subject-distance)))
   (= magnification (- (/ image-distance subject-distance)))
   (= image-height (* magnification subject-height))
 ))

(converging-lens (focal-length)
  ((focal-length meter) (magnification unity)
   (image-distance meter) (subject-distance meter)
   (image-height meter) (subject-height meter))
  ((= focal-length (/ radius 2))
   (= (/ 1 focal-length) (+ (/ 1 image-distance) (/ 1 subject-distance)))
   (= magnification (- (/ image-distance subject-distance)))
   (= image-height (* magnification subject-height))
 ))

(photoelectric (frequency)
  ((work-function electron-volt))
  ((= speed-of-light '(q 299792458 (/ meter second)))
   (= planck-constant '(q 6.6260693E-34 (* joule second)))
   (= speed-of-light (* frequency wavelength))
   (= work-function '(q 4.08 electron-volt))     ; default: Al
   (= energy (* planck-constant frequency))
   (= kinetic-energy (- energy work-function))
   (= fudge-factor '(q 1 (/ ampere candela)))   ; ***
   (= current (* fudge-factor intensity))       ; *** approx
   (= electron-mass '(q 9.1093826E-31 kg))
   (= kinetic-energy (* 1/2 (* electron-mass (expt velocity 2))))
 ))

; particle physics

(elementary-particle ()
  ()
  ((= planck-constant '(q 6.6260693E-34 (* joule second)))
   (= momentum (* mass velocity))
   (= wavelength (/ planck-constant momentum))
   (= kinetic-energy (* 1/2 (* mass (expt velocity 2))))
 ))

(radioactive-decay (initial-rate half-life)
  ((initial-rate hertz) (final-rate hertz) (half-life second)
   (initial-amount kilogram) (final-amount kilogram))
  ((= final-rate (/ initial-rate (expt 2 (/ time half-life))))
   (= final-amount (/ initial-amount (expt 2 (/ time half-life))))
 ))

(nuclear-reaction (final-mass initial-mass)
  ((final-mass kilogram) (initial-mass kilogram)
   (mass-difference kilogram))
  ((= speed-of-light '(q 299792458 (/ meter second)))
   (= mass-difference (- initial-mass final-mass))
   (= energy (* mass-difference (expt speed-of-light 2)))  ; released
 ))

; ***** light emissions vs. energy band transitions  1998 #FR7
(bohr-model ()
  ()
  ())

; *****
(compton-scatter (frequency)
  ()
  () )

; charged particle moving in a magnetic field
(charge-magnetic (charge field velocity)
  ((field tesla))
  ((= force (* charge (* field velocity)))
   (= radius (/ (* mass velocity) (* charge field)))
 ))

; Thermodynamics

; m = mass, dt = delta temperature, c = specific heat
(heat-transfer (m1 dt1 c1)
  ( (t1f kelvin) (t2f kelvin) (t1 kelvin) (t2 kelvin)
    (dt1 kelvin) (dt2 kelvin) (m1 kilogram) (m2 kilogram)
    (c1 (/ joule (* kilogram degreeK))) (c2 (/ joule (* kilogram degreeK))) )
  ((= dt1 (- t1f t1))
   (= dt2 (- t2f t2))
   (= hm1 (* m1 c1))
   (= hm2 (* m2 c2))
   (= equilibrium-temperature (/ (+ (* hm1 t1) (* hm2 t2)) (+ hm1 hm2)))
   (= energy (* (* m1 c1) dt1))
   (= energy (- (* (* m2 c2) dt2)))
   (= rate (/ energy time)) ))

(ideal-gas (pressure volume)
  ((density (/ kilogram (* meter meter meter)))
   (delta-volume (* meter meter meter))
   (average-speed (/ meter second)) (molar-mass (/ kilogram mole))
   (n-moles mole))
  ((= temperature (/ (* pressure volume)
		     (* n-moles universal-gas-constant)))
   (= universal-gas-constant '(q 8.314472 (/ joule (* mole degree-kelvin))))
   (= n-moles '(q 1 mole))
   (= mass (* n-moles molar-mass))
   (= density (/ mass volume))
   (= work (- (* pressure delta-volume)))  ; assumes pressure const ***
   (= internal-energy (* 3/2 (* n-moles
				(* universal-gas-constant temperature))))
   (= average-speed (sqrt (/ (* 3 (* universal-gas-constant temperature))
			     molar-mass)))
 ))

; heat = heat added to system
; work = work done by system
(thermo-system (initial-energy heat work)
  ((work-absorbed joule) (total-work joule) (energy-change joule)
   (final-energy joule) (initial-energy joule) (heat joule))
  ((= work-absorbed '(q 0 joule))  ; default
   (= total-work (- work work-absorbed))
   (= energy-change (- heat total-work))
   (= final-energy  (+ initial-energy energy-change))
))

(heat-engine (heat work)
  ((efficiency unity) (heat joule) (heat-exhausted joule))
  ((= work (- heat heat-exhausted))
   (= efficiency (/ work heat))
   (= max-efficiency (- 1 (/ temperature-cold temperature-hot)))
  ))

(relativity (c-factor)
  ((relativity-factor unity) (observed-time second)
   (observed-length meter))
  ((= speed-of-light '(q 299792458 (/ meter second)))
   (= velocity (* c-factor speed-of-light))
   (= relativity-factor (sqrt (- 1 (/ (expt velocity 2)
				      (expt speed-of-light 2)))))
   (= observed-time (/ time relativity-factor))
   (= observed-length (* length relativity-factor))
))

; Map coordinates

; 29 Mar 98; 14 Jan 99; 09 Feb 99
(linear-scale (ratio base)
  ()
  ((= width (- xmax xmin))
   (= goalwidth (- goalmax goalmin))
   (= ratio (/ goalwidth width))
   (= base (- goalmin (* ratio xmin)))
   (= y (+ base (* x ratio))) ) )

; 25 Nov 94
(mercator (latitude longitude radius)
  ()
  ((= maplong (* radius longitude))
   (= maplat  (* radius (log (/ (+ 1.0 (sin latitude))
				(cos latitude))))) ) )

(map-scale (xratio xbase yratio ybase)
  ()
  ((= xscale (tuple (base xbase) (ratio xratio)))
   (= yscale (tuple (base ybase) (ratio yratio)))
   (= width (- xmax xmin))
   (= height (- ymax ymin))
   (= goalwidth (- goalxmax goalxmin))
   (= goalheight (- goalymax goalymin))
   (= xratio (/ goalwidth width))
   (= yratio (/ goalheight height))
   (= xbase (- goalxmin (* xratio xmin)))
   (= ybase (- goalymin (* yratio ymin)))
   (= goalx (+ xbase (* x xratio)))
   (= goaly (+ ybase (* y yratio))) ) )


; Physical constants, from http://www.physics.nist.gov/
; 2002 CODATA recommended values
; note that the number in parens is +/- error in last digits

; unified atomic mass unit 1.660 538 86(28) x 10-27 kg
; atomic mass unit-electron volt relationship u*c^2  931.494 043(80) x 10+6 eV
; proton mass 1.672 621 71(29) x 10-27 kg
; neutron mass 1.674 927 28(29) x 10-27 kg
; electron mass 9.109 3826(16) x 10-31 kg
; elementary charge  1.602 176 53(14) x 10-19 C
; Avogadro constant 6.022 1415(10) x 10+23 mol-1
; molar gas constant R 8.314 472(15) J mol-1 K-1
; Boltzmann constant k 1.380 6505(24) x 10-23 J K-1
; speed of light in vacuum 299 792 458 m s-1
; Planck constant h 6.626 0693(11) x 10-34 J s
; Planck constant in eV s 4.135 667 43(35) x 10-15 eV s
; hc = 1.9864456023253393E-25 J m     (* 6.6260693E-34 299792458)  * calculated
;    = 1.2398419057638671E+3  eV * nm
;             (/ (* 6.6260693E-34 299792458) 1.60217653E-19)       * calculated
; electric constant E0 = 8.854 187 817... x 10-12 F m-1
;                E0 = permittivity of free space = 1/(4*pi*ke)
; Coulomb's law const ke = 1/(4*pi*e0) = 8.9875517879979115E9 N m2 C-2  *calc
;             (/ 1 (* 4 pi 8.854187817E-12))
; magnetic constant mu0 4pi x 10-7 = 12.566 370 614... x 10-7
; Newtonian constant of gravitation G 6.6742(10) x 10-11 m3 kg-1 s-2
; standard acceleration of gravity 9.806 65 m s-2
; standard atmosphere 101 325 Pa = 101 325 N m-2
; electron volt 1.602 176 53(14) x 10-19 J
; angstrom  1 x 10-10 m
; {220} lattice spacing of silicon 192.015 5965(70) x 10-12 m

; Hardy-Weinberg describes the population distribution of a genetic trait 
; with a single gene locus (ex: Rh factors in human blood types).
; p is the frequency of allele A while q is the frequency of allele a
(population (p)
  ((homozygous-recessive unity) (heterozygous unity)
   (homozygous-dominant unity) (p unity) (q unity))
  ((= 1 (+ p q))
   (= homozygous-recessive (expt q 2))
   (= heterozygous (* 2 (* p q)))
   (= homozygous-dominant (expt p 2)) ))
))

; 01 Apr 04; 05 Apr 04
; find assumed unit of a variable
(defun assumedvartype (var &optional objtype)
  (or (and objtype
	   (cadr (assoc var (vars-units objtype))))
      (assumed-unit var)))

; 29 Jun 06
; problem: assumed units may be expressions
(defun defphyslaw (item)
  (setf (basis-vars (car item)) (second item))
  (setf (vars-units (car item)) (third item))
  (setf (vars-unittypes (car item))
	(mapcar #'(lambda (pair)
		    (list (car pair) (glabstractunit (cadr pair)) ))
		(third item)))
  (setf (equations (car item)) (fourth item)) )

(dolist (item *physlaws*) (defphyslaw item))

(pushnew '(linear-scale linear-scale) *conn-user-laws*)
(pushnew '(map-scale map-scale)  *conn-user-laws*)
