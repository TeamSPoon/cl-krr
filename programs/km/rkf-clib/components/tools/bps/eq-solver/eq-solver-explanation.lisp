;;
;; $Id: eq-solver-explanation.lisp,v 1.23 2008/10/17 19:24:04 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun update-eq-history-variable-bindings (VP-INST))

(defun update-eq-history-explanation())

;;Wrapper function to eq-history explanation code.
(defun bps-explaineqhist(vq-slot vq-frame)
  (format nil "~a"
;;	  (pretty-print-eq-variable-bindings *CONTROLLER-BPS-VARIABLE-BINDINGS*)
;;	  (pretty-print-eq-expressions 
;;            (filter-uninteresting-eq-expressions 
;;              (append (convert-var-bindings-to-eq-expressions *CONTROLLER-BPS-VARIABLE-BINDINGS*)
;;                      (get-used-eq-expressions))))
	  (get-bps-explaineqhist)))

(defun filter-eq-expr-by-symbols(sym-lst eq-expr-lst)
  (remove nil
	  (mapcar #'(lambda(eq-expr)
		      (if (not (null (set-difference sym-lst (flatten eq-expr))))
			  eq-expr))
		  eq-expr-lst)))

(defun convert-var-bindings-to-eq-expressions (var-bindings)
  (mapcar #'convert-varb-to-eq var-bindings)
)

(defun convert-varb-to-eq (varb)
  (if varb
      (list '= (first varb) (format nil "~a ~a" (first (second varb))
                                                (km-name-for-unit (second (second varb)))
                            )
      )
  )
)

(defun filter-uninteresting-eq-expressions (eq-expressions)
  (let ((x-eq-y (find-if #'(lambda (eq) (and (= (length (flatten eq)) 3)
                                             (not (numberp (second eq)))
                                             (not (numberp (third eq)))
                                             (not (stringp (third eq)))
                                        )
                           ) eq-expressions)
       ))
    (if (not x-eq-y)
        eq-expressions
        (let* ((xsym (second x-eq-y))
               (ysym (third x-eq-y))
               (prefsym (get-preferred-var-symbol xsym ysym))
               (disprefsym (if (eq prefsym xsym)
                               ysym
                               xsym
                           )
               )
              )
          (filter-uninteresting-eq-expressions
              (replace-element-in-eqn
                 disprefsym
                 prefsym
                 (remove x-eq-y eq-expressions)
              )
          )
        )
    )
  )
)


(defun get-preferred-var-symbol (X Y)
  (let ((Xstr (format nil "~a" X))
        (Ystr (format nil "~a" Y))
       )
    (cond ((and (not (symbol-has-subscript Xstr)) (symbol-has-subscript Ystr))
             X)
          ((and (symbol-has-subscript Xstr) (not (symbol-has-subscript Ystr)))
             Y)
          ((and (not (symbol-has-numeric-subscript Xstr)) (symbol-has-numeric-subscript Ystr))
             X)
          ((and (symbol-has-numeric-subscript Xstr) (not (symbol-has-numeric-subscript Ystr)))
             Y)
          ((and (< (num-subscripts-on-symbol Xstr) (num-subscripts-on-symbol Ystr)))
             X)
          ((and (> (num-subscripts-on-symbol Xstr) (num-subscripts-on-symbol Ystr)))
             Y)
          (t X)
    )
  )
)

(defun symbol-has-subscript (symstr)
  (search "_" symstr)
)

(defun symbol-has-numeric-subscript (symstr)
  (search-any-char "0123456789" (get-symbol-subscript symstr))
)

(defun get-symbol-subscript (symstr)
  (let ((subscript-pos (search "_" symstr)))
    (if subscript-pos
        (if (< (+ subscript-pos 1) (length symstr))
            (subseq symstr (+ subscript-pos 1))
            NIL
        )
        NIL
    )
  )
)

(defun search-any-char (searchcharsstr targetstr)
  (if (and (> (length searchcharsstr) 0)
           (> (length targetstr) 0)
      )
      (or (search (subseq searchcharsstr 0 1) targetstr)
          (search-any-char (subseq searchcharsstr 1) targetstr)
      )
  )
)

(defun num-subscripts-on-symbol (symstr)
  (count-char-in-str "_" symstr)
)

(defun count-char-in-str (charstr targetstr)
  (if (and (> (length charstr) 0)
           (> (length targetstr) 0)
      )
      (let ((charpos (search charstr targetstr)))
        (if charpos
            (+ 1 (count-char-in-str charstr (subseq targetstr (+ charpos 1))))
            0
        )
      )
      0
  )
)

(defun get-used-eq-expressions()
  (intersection
   *CONTROLLER-BPS-EXPRESSIONS*
   (remove-duplicates (mappend 'cdddr *CONTROLLER-EQ-HISTORY*) 
		     :test 'equal)
   :test 'equal))

(defun bps-all-eq-symbols()
  (get-symbols-from-eq-expr-list *CONTROLLER-BPS-EXPRESSIONS*))

(defun bps-unused-eq-symbols()
  (set-difference (bps-all-eq-symbols)
		  (bps-used-eq-symbols)))

(defun bps-used-eq-symbols()
  (get-symbols-from-eq-expr-list
   (remove-duplicates
    (flatten (mappend 'cdddr *CONTROLLER-EQ-HISTORY*)))))

(defun pretty-print-eq-variable-bindings(binding)
  (let ((s (make-string-output-stream)))
    (format s "Given the values<br><ul>")
    (dolist (b binding)
      (let ((sym (htmlify-var-subscript (car b)))
	    (val (caadr b))
	    (uom (km-name-for-unit (cadr (cadr b))))
           )
      (format s "<li> ~a = ~a ~a </li>~%" sym val uom)))
    (format s "</ul> <br>")
    (get-output-stream-string s)))

(defun pretty-print-eq-expressions(all-eq-expr)
  (let ((s (make-string-output-stream)))
    (format s "Given:<br><ul>")
    (dolist (eq-expr all-eq-expr)
      (format s "<li>~a [~a]</li>~%" 
              (infixstr-htmlify eq-expr 'html)
              (get-description-for-bound-instance (second eq-expr))
      )
    )
    (format s "</ul>~%")
    (get-output-stream-string s)
  )
)

(defun get-description-for-bound-instance (varsym)
  (let ((varinst (second (find-if #'(lambda (r) (equal (first r) varsym)) *EQ-SOLVER-SYMBOL-TABLE*))))
    (if varinst
        (let* ((p-slot (or (car (ps-km-query `(|the| |property-slot| |of| ,varinst)))
                           (car (ps-km-query `(|the| |primary-slot| |of| ,varinst)))
                       )
               )
               (p-slot-name (car (ps-km-query `(|the| |name| |of| ,p-slot))))
               (p-of-slot (car (ps-km-query `(|the| |property-of-slot| |of| ,varinst))))
               (p-of-inst (if p-of-slot
                              (car (ps-km-query `(|the| ,p-of-slot |of| ,varinst)))
               ))
               (p-of-text (if p-of-inst
                              (if (isa p-of-inst '|Property-Value|)
                                  (format nil "the ~a" (car (ps-km-query `(|the| |text-slot| |of| ,p-of-inst))))
                                  (make-phrase (car (ps-km-query `(|the| |text-def-head| |of| ,p-of-inst))))
                              )
                          )
               )
              )
          (if p-of-text
              (format nil "the ~a of ~a" p-slot-name p-of-text)
              (format nil "the ~a" p-slot-name)
          )
        )
    )
  )
)

(defun get-bps-equation-bound-variables(vq-slot vq-frame)
  (let* ((target-frame         (car (ps-km-query `(|the| ,vq-slot |of| ,vq-frame))))
	 (used-in-equation     (get-used-in-equation vq-slot vq-frame))
	 (in-system            (get-system-for-equation used-in-equation))
	 (all-equations        (get-all-equation-for-system in-system))
	 (all-eq-variables     (remove-duplicates 
				(set-difference (get-all-equation-variables-for-equation-list all-equations)
						(list target-frame))))
	 (all-eq-variable-info (get-info-for-equation-variable-list all-eq-variables))
	 (result               (format nil "Given the values<br>")))
    (dolist (eq-var-info all-eq-variable-info)
      (setq result (format nil "~a <li> ~a </li>" result eq-var-info)))
    (setq result (format nil "~a <br>" result))))

(defun get-bps-equation-expressions(vq-slot vq-frame)
  (let* ((target-frame         (car (ps-km-query `(|the| ,vq-slot |of| ,vq-frame))))
	 (used-in-equation     (get-used-in-equation vq-slot vq-frame))
	 (in-system            (get-system-for-equation used-in-equation))
	 (all-equations        (get-all-equation-for-system in-system))
	 (all-eq-expressions   (get-all-equation-expressions-for-equation-list all-equations))
	 (result               (format nil "Given the equations<br>")))
  (dolist (eq-expression all-eq-expressions)
    (setq result (format nil "~a <li>~a</li>" result (PRETTY-PRINT-EQ-HTML eq-expression))))
  (setq result (format nil "~a <br>" result))))

(defun get-all-equation-expressions-for-equation(equation)
  (let ((result (ps-km-query `(|the| |equation-expression| |of| ,equation))))
    result))

(defun get-all-equation-expressions-for-equation-list (equation-list)
  (mappend 'get-all-equation-expressions-for-equation
	   equation-list))

(defun get-symbol-for-variable(var)
  (let ((result (get-symbol-for-variable-standard var)))
  (if result 
      result
      (get-symbol-for-variable-non-standard var))))

(defun get-symbol-for-variable-standard(var)
  (car (ps-km-query `(|the| |called| |of| ,var))))

(defun get-symbol-for-variable-non-standard(var)
  (let ((eq-symbol-bindings (ps-km-query `(|the| |equation-symbol| |of| 
					   (|the| |used-in-equation| |of| ,var)))))
    (cadr  (member var (reverse (flatten eq-symbol-bindings))))))

(defun get-info-for-equation-variable(var)
  (let* ((*EQ-FLAG* nil)
	 (symbol (get-symbol-for-variable var))
	 (value-pair  (car (lookup-small-v-value var)))
	 (numeric-value (nth 1 value-pair))
	 (uom-value     (car (ps-km-query `(|the| |name| |of| ,(nth 2 value-pair))))))
    (if numeric-value
	(list (format nil "~a = ~a ~a" symbol numeric-value uom-value))
        (list ""))))

(defun get-info-for-equation-variable-list(var-list)
  (remove-if #'(lambda(x) (string= "" x)) 
	     (mappend #'get-info-for-equation-variable 
		      var-list)))

(defun get-all-equation-variables-for-equation(equation)
  (ps-km-query `(|the| |equation-uses| |of|  ,equation)))

(defun get-all-equation-variables-for-equation-list (all-equations)
  (mappend #'get-all-equation-variables-for-equation
	   all-equations))

(defun get-all-equation-for-system (system)
  (let ((result (ps-km-query `(|the| |equation| |of|  ,system))))
    result))

(defun get-system-for-equation(equation)
  (let ((result (ps-km-query `(|the| |equation-of| |of|  ,equation))))
    (car result)))

(defun get-used-in-equation (vq-slot vq-frame)
  (let ((result (or (ps-km-query `(|the| |used-in-equation| |of|  (|the| ,vq-slot |of| ,vq-frame)))
                    (ps-km-query `(|the| |equation| |of| (|the| |component-of| |of| (|the| ,vq-slot |of| ,vq-frame)))))))
    (car result)))

;;Wrapper function to eq-history explanation code.
(defun get-bps-explaineqhist()
  (let* ((s (make-string-output-stream)))
    (get-bps-explaineqhist0 s *CONTROLLER-EQ-HISTORY*)
    (replace-all (get-output-stream-string s) "unity" "unit")))


;; newer version does better filtering
;; and nests equation solving
(defun get-bps-explaineqhist0 (&optional (outputstream t)
					 (history      *equations-history*)
					 (goalvar      (caar history)))
  (if (and history goalvar)
      (let ((eqhisttree (treeify-eq-dependencies (filtereqhist history goalvar) goalvar)))
        (format outputstream "~a"
          (pretty-print-eq-expressions (filter-eq-expr-by-symbols 
                                         (gather-used-vars-from-tree eqhisttree)
                                         (filter-uninteresting-eq-expressions 
                                           (append 
                                             (convert-var-bindings-to-eq-expressions *CONTROLLER-BPS-VARIABLE-BINDINGS*)
                                             (gather-used-eqexprs-from-tree eqhisttree)
                                           )
                                         )
                                       )
          )
        )
        (format outputstream "Solving for ~a ...~%" (htmlify-var-subscript goalvar))
        (format outputstream "<dl>~%")
        (pretty-print-eq-soln-tree eqhisttree outputstream)
        (format outputstream "</dl>~%")
      )
  )
)

(defun gather-used-vars-from-tree (eqtree)
  (if eqtree
      (let ((goalvar  (first eqtree))
            (depvars  (sixth eqtree))
            (subtrees (seventh eqtree))
           )
        (remove-duplicates (cons goalvar
                                 (append depvars (flatten (mapcar #'gather-used-vars-from-tree subtrees)))
                           )
        )
      )
  )
)

(defun gather-used-eqexprs-from-tree (eqtree)
  (if eqtree
      (let ((origeq   (fourth eqtree))
            (subtrees (seventh eqtree))
           )
        (remove-duplicates (append (list origeq)
                                   (mappend #'gather-used-eqexprs-from-tree subtrees)
                           )
        )
      )
  )
)

(defun treeify-eq-dependencies (eqhist goalvar)
  (let* ((legalvars (mapcar #'car eqhist))
         (goalrec (find-if #'(lambda (e) (eq (car e) goalvar)) eqhist))
         (goaleqn (fifth goalrec))
         (dependentvars (reverse (intersection legalvars (flatten (third goaleqn)))))
        )
    (if dependentvars
        (append (append goalrec (list dependentvars))
                (list (mapcar #'(lambda (dv) (treeify-eq-dependencies eqhist dv)) dependentvars))
        )
        goalrec
    )
  )
)

(defun pretty-print-eq-soln-tree (eqtree outputstream)
  (if eqtree
      (let ((goalvar  (first eqtree))
            (goalval  (second eqtree))
            (goalunit (third eqtree))
            (origeq   (fourth eqtree))
            (rearreq  (fifth eqtree))
            (depvars  (sixth eqtree))
            (subtrees (seventh eqtree))
           )
        (format outputstream "<dl>")
        (format outputstream "~a" (infixstr-htmlify rearreq 'html))
        (if (not (equivalence-equations origeq rearreq))
            (format outputstream " [solving (~a) for ~a]"
                    (infixstr-htmlify origeq 'html)
                    (htmlify-var-subscript goalvar)
            )
        )
        (format outputstream "<br />~%")
        (if depvars
            (dolist (subtree subtrees)
              (pretty-print-eq-soln-tree subtree outputstream)
            )
        )
        (format outputstream ".:. ~a = ~a ~a"
                (htmlify-var-subscript goalvar)

                (erase-minus-zero (erase-trailing-zeros (format nil "~,1f" goalval)))

                (km-name-for-unit (kmifyunit goalunit))
        )
        (format outputstream "~%</dl>~%")
      )
  )
)

;; not actually used, since tree stuff above is better
(defun nest-eqhist-solution (eqhist outputstream level)
  (if eqhist
      (let ((html-tab "")
            (item (car eqhist))
            (eqhistrest (cdr eqhist))
           )
        (format outputstream "<dl>~%")
        (if (not (equivalence-equations (fourth item) (fifth item)))
            (format outputstream "~a~a [solving (~a) for ~a]<br />~%"
                    html-tab
                    (infixstr-htmlify (fifth item) 'html)
                    (infixstr-htmlify (fourth item) 'html)
                    (htmlify-var-subscript (first item))
            )
        )
        ; if a constant such as gravity, don't explain
        (if (not (and (consp (third (fifth item)))
                      (eq (car (third (fifth item))) 'quote)))
            (progn
              (format outputstream "~%~a~a<br />~%"
                      html-tab
                      (infixstr-htmlify (fifth item) 'html)
              )
              (nest-eqhist-solution eqhistrest outputstream (+ level 1))
              (format outputstream "~a~a = ~a ~a<br />~%"
                      html-tab
                      (htmlify-var-subscript (first item))
                      (infixstr-htmlify (second item) 'html)
                      (km-name-for-unit (kmifyunit (third item)))
              )
            )
        )
        (format outputstream "</dl>~%")
      )
  )
)

#|
;;Dual version of Novak's explaineqhist routine.
;;Changes include: 
;;  (a) Formatted output to contain HTML tags.
(defun get-bps-explaineqhist0 (&optional (outputstream t)
					 (history      *equations-history*)
					 (goalvar      (caar history)))
  (let ((newhist (filtereqhist history goalvar))
	(infix   t)
	(html    t)
	(html-tab "&nbsp;&nbsp;&nbsp;"))
    (dolist (item newhist)
      (if (fourth item)
	  (progn
            ; if already solved for desired var, don't explain
	    (if (not (equivalence-equations (fourth item) (fifth item)))
		(format outputstream "<BR>Solved equation ~A<BR> ~A for ~A<BR> ~A giving ~A<BR>"
			(if infix (infixstr-htmlify (fourth item) html) (fourth item))
			html-tab
                        (htmlify-var-subscript (first item))
			html-tab
                        (if infix (infixstr-htmlify (fifth  item) html) (fifth item))))
            ; if a constant such as gravity, don't explain
	    (if (not (and (consp (third (fifth item)))
			  (eq (car (third (fifth item))) 'quote)))
		(format outputstream "<BR>Evaluated ~A<BR> ~A giving ~A = ~A ~A<BR>"
                        (if infix (infixstr-htmlify (fifth item) html) (fifth item))
			html-tab
                        (htmlify-var-subscript (first item))
                        (if infix (infixstr-htmlify (second item) html) (second item))
                        (format nil "~a" (km-name-for-unit (kmifyunit (third item)))))) )))))
|#


(defun equivalence-equations (eq1 eq2)
  (or (equal eq1 eq2)
      (and (= (length (flatten eq1)) 3)
           (= (length (flatten eq2)) 3)
           (eq (first eq1) '=)
           (eq (first eq2) '=)
           (eq (second eq1) (third eq2))
           (eq (second eq2) (third eq1))
      )
  )
)


(defun km-name-for-unit (km-unit)
  (or (car (km0 `(|the| |name| |of| ,km-unit)))
      km-unit
  )
)

(defun htmlify-var-subscript (var)
  (if (null var)
      nil
      (if (listp var)
          (cons (htmlify-var-subscript (car var)) (htmlify-var-subscript (cdr var)))
          (let* ((var-str (format nil "~a" var))
                 (first-underbar-pos (search "_" var-str))
                )
            (if first-underbar-pos
                (intern (format nil "~a<sub>~a</sub>" (subseq var-str 0 first-underbar-pos)
                                                      (subseq var-str (+ first-underbar-pos 1))
                        ) :km
                )
                var
            )
          )
      )
  )
)

(defun str-htmlify-var-subscript (var)
  (format nil "~a" (htmlify-var-subscript var))
)

(defun infixstr-htmlify (eq-in flag)
  (if (listp eq-in)
      (infixstr-respect-case (mapcar #'htmlify-var-subscript eq-in) flag)
      (infixstr-respect-case eq-in flag)
  )
)

(defun is-sym-used (sym)
  (let ((sym-list (bps-used-eq-symbols)))
    (if (member (eval sym) sym-list)
        t
        nil
    )
  )
)

(defun generate-symbolic-equation-list-html ()
  (with-output-to-string (s)
    (multiple-value-bind 
      (eqs syms) 
      (extract-applicable-eq-systems-for-instance) 
      (dolist (eq eqs)
        (format s "<li>~a</li>~%" (infixstr-htmlify eq 'html))
      )
    )
  )
)


(defun infixstr-respect-case (form &optional html)
  (setq *infixstr* nil)
  (infixstrb-respect-case form html 0)
  (apply #'concatenate (cons 'string (nreverse *infixstr*))) )

(defun infixstrb-respect-case (form html lastprec)
  (let (str op prec)
    (if (stringp form)
        (push form *infixstr*)
        (if (symbolp form)
            (progn (setq str (symbol-name form))
                   (if (and (> (length str) 4)
                            (string= str "VAR-" :end1 4))
                       (push (subseq str 4) *infixstr*)
                       (push str *infixstr*) ) )
            (if (numberp form)
                (push (princ-to-string form) *infixstr*)
                (if (consp form)
                    (progn
                      (setq op (car form))
                      (if (and (eq op '-)
                               (null (cddr form)))
                          (progn (push "(- " *infixstr*)
                                 (infixstrb-respect-case (lhs form) html 8)
                                 (push ")" *infixstr*))
                          (if (and html (or (eq op 'sqrt)  ;; jchaw/kbarker
                                            (eq op '|sqrt|)))
                              (progn (push "&radic; " *infixstr*)   ;; jchaw/kbarker
                                     (infixstrb-respect-case (lhs form) html 20) )
                              (if (or (eq op 'expt)     ;; jchaw/kbarker
                                      (eq op '|expt|))
                                  (progn (infixstrb-respect-case (lhs form) html 20)
                                         (if html (push "<SUP>" *infixstr*)
                                                  (push "^" *infixstr*))
                                         (infixstrb-respect-case (rhs form) html 20)
                                         (if html (push "</SUP>" *infixstr*)))
                                  (if (setq prec (cdr (assoc op
                                             '((+ . 5) (- . 6) (* . 7)
                                               (/ . 7) (= . 1)))))
                                      (progn (if (<= prec lastprec)
                                                 (push "(" *infixstr*))
                                             (infixstrb-respect-case (lhs form) html prec)
                                             (push " " *infixstr*)
                                             (push (stringify op) *infixstr*)
                                             (push " " *infixstr*)
                                             (infixstrb-respect-case (rhs form) html prec)
                                             (if (<= prec lastprec)
                                                 (push ")" *infixstr*)))
                                      (progn (push (string-downcase
                                                    (symbol-name op)) *infixstr*)
                                             (push "(" *infixstr*)
                                             (pop form)
                                             (while form
                                               (infixstrb-respect-case (pop form) html 0)
                                               (if form (push ", " *infixstr*)))
                                             (push ")" *infixstr*) ))))))))) )))



