;;
;; $Id: valid-equation-expression-predicate.lisp,v 1.3 2008/10/13 19:08:34 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Routines to check that the terms of a chemical expression are well-formed.

;;Predicate to check well-formedness of an eq-expression, i.e. LHS --> RHS.
(defun valid-equation-expressionp (input)
  (let ((lhs (second input))
	(rhs (third  input)))
    (and (valid-equation-expression-sidep lhs)
	 (valid-equation-expression-sidep rhs))))
	 
;;Predicate to check well-formedness of LHS or RHS for eq-expression.
(defun valid-equation-expression-sidep(input)
  (cond ((equal (first input) ':|pair|)
	 (valid-equation-termp input))
	((= (length input) 2)  
	 (valid-equation-expression-sidep (second input)))
	(t (and (valid-equation-termp (second input))
		(valid-equation-expression-sidep (cons (first input) 
						     (cddr  input)))))))

;;Predicate to check well-formedness of an equation term
(defun valid-equation-termp(input)
  (let ((count   (second input))
	(symbol  (third  input)))
    (not (or (null count)
	     (null symbol)))))
			   

(defun htmlsubscriptify-chem-name (chemnamestr)
  (let ((namestr (string-right-trim "0123456789" chemnamestr))
        (subscriptstr (string-left-trim "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" chemnamestr))
       )
    (if (and (not (equal namestr chemnamestr))
             (equal (concatenate 'string namestr subscriptstr) chemnamestr)    ;; purely a sanity check
        )
        (format nil "~a<sub>~a</sub>" namestr subscriptstr)
        chemnamestr
    )
  )
)

