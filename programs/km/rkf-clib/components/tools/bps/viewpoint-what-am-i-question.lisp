;;
;; $Id: viewpoint-what-am-i-question.lisp,v 1.18 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun is-analogy-question-p (cpl-scenario cpl-question &optional (verbose nil))
  (let* ((cpl-scenario (make-triple-proper cpl-scenario))
	 (val (car cpl-question)))
    (cond ((and (consp val)       
		(eql (first val) 'WHAT-IS-THE))
	   (progn (if verbose (format t "BPS: Analogy question~%"))
		  t)))))

(defun output-class-definition-viewpoint-for-what-is-the-question(sl-vp-inst)
  (multiple-value-bind
      (simplified-scenario full-scenario)
      (get-simplified-context-for-viewpoint sl-vp-inst)
  (let* ((debug nil)
	 (explained-instances (extract-all-km-instances 
			       (get-instance-of-slot-value-query sl-vp-inst)))
	 (vp-defn 
	  `(|a| |Class-Definition-Viewpoint| |with|
	    (|viewpoint-scenario|  (,(cons :|set| (affix-triple-prefix full-scenario))))
	    (|viewpoint-parent|	 (,sl-vp-inst))
;;Supposedly viewpoint-target should be (:seq class1 ... classn). But only for Slot-Value-Viewpoint and Multislot-Value-Viewpoint
;;For Class-Definition-Viewpoint, viewpoint-target is a :seq of km instances.
	    (|viewpoint-target|	 (,(cons :|seq| explained-instances)))
	    (|viewpoint-query|	 ((:|seq| (:|pair| |*class-description| nil)))))))
    (let ((result (car (ps-km-query vp-defn debug))))
      (reinstall-viewpoint-answer-provenance 
       result
       sl-vp-inst)
      (install-equation-set-triples result)
      (ps-km-query `(,sl-vp-inst |now-has| (|viewpoint-child| (,result))) debug)
      result))))

;;These triples, e.g., equation-uses, equation-expression, equation-symbol, component, are required for explanation
;;They supplement the triples in the viewpoint-scenario
(defun install-equation-set-triples(vp-inst)
  (let* ((debug nil)
	 (existing-triples                 (strip-triple-prefix (get-viewpoint-scenario vp-inst)))
	 (km-instances-connected-to-system (remove-if-not #'(lambda(instance)
							      (not (null (ps-km-query `(|the| |component-of| |of| ,instance) debug))))
							  (extract-all-km-instances existing-triples)))
	 (additional-triples (mappend 'get-equation-set-triples-for-km-instance km-instances-connected-to-system)))
	(ps-km-query `(,vp-inst |now-has| (|viewpoint-scenario| ,(affix-triple-prefix 
								  (append existing-triples
									  additional-triples)))) debug)
))

(defun get-equation-set-triples-for-km-instance(instance)
  (let* ((debug nil)
	 (system (car (ps-km-query `(|the| |component-of| |of| ,instance) debug)))
	 (eq-set-lst (ps-km-query `(|the| |equation| |of| ,system) debug)))
    (cons
     (list instance '|component-of| system)
     (mappend #'(lambda(eq-set)
		  (cons (list system '|equation| eq-set)
		  (append
		   (mapcar #'(lambda(eq-sym-entry)
			       (list eq-set '|equation-symbol| eq-sym-entry))
			   (ps-km-query `(|the| |equation-symbol| |of| ,eq-set) debug))
		   (mapcar #'(lambda(eq-expr-entry)
			       (list eq-set '|equation-expression| eq-expr-entry))
			   (ps-km-query `(|the| |equation-expression| |of| ,eq-set) debug)))))
	      eq-set-lst))
))

(defun get-instance-of-slot-value-query(vp-inst)
  (remove-if-not #'(lambda(entry)
		     (and (equal (nth 1 entry) '|*slot-value|)
			  (equal (triple-relation (cdr (nth 2 entry)))
				 '|instance-of|)))
		 (get-viewpoint-query vp-inst)))

