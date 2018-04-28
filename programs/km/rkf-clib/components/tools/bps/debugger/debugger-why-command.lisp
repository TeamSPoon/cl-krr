;;
;; $Id: debugger-why-command.lisp,v 1.12 2008/08/12 22:19:59 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun why-command (operands)
  (let* ((vp-inst  (if (null operands)
		       *current-vp-inst*
		       (first  operands)))
	 (stream   (make-string-output-stream)))
    (if (kb-viewpointp vp-inst)
	(let* ((vp-context (get-simplified-context-for-viewpoint vp-inst))
	       (concept-name  (car (get-viewpoint-target vp-inst))))
	  (why-reasons stream vp-inst concept-name)
	  (whynot-reasons stream vp-inst concept-name)
	  ;(format stream "~a~%" (car (ps-km-query `(|the| |viewpoint-eq-solver-info| |of| ,vp-inst))))
	  )
        (format stream "Usage : why~%      : why <vp-inst>~%"))
    (get-output-stream-string stream)))

(defun why-reasons(stream vp-inst concept-name)
  (let* ((parent-vp-context (get-simplified-context-for-viewpoint (first (get-viewpoint-parent vp-inst)))))
    (format stream "The following triples match up between the viewpoint context and the KB concept, e.g. <viewpoint-triple> maps to <kb-concept-triple> with score <fraction>.~%")
    (parse-why-output stream (compute-why-match-reasons parent-vp-context concept-name))
    (format stream "~%~%")))

(defun compute-why-match-reasons (initial-graph concept)
  (let* ((irrelevant-relns 
	  (union '(|component| |equation-uses| |equation-expression| |equation-symbol|) 
		 *triple-relations-to-ignore*))
	 (scenario-graph (remove-irrelevant-triples-for-matching initial-graph irrelevant-relns))
	 (relations-to-extract (append *vp-match-default-lookups* (include-subslots (extract-relations-in-graph-inv initial-graph)))))
    (multiple-value-bind
	(root graph)
	(ps-get-graph relations-to-extract concept)
      (let* ((model-graph (remove-irrelevant-triples-for-matching 
			   graph
			   irrelevant-relns)))
	;;FIXME! Should involve all matches, not just the strongest
	(car (perform-kb-match scenario-graph concept 'weak))))))

;;FIXME! Should involve all matches, not just the strongest
(defun parse-why-output(stream strongest-match-result)
  (multiple-value-bind
      (target target-root scenario-graph scenario-clone-map candidate-graph instance-map triple-map simple-score taxonomic-score)
      (parse-concept-selector-result strongest-match-result)
    (dolist (item triple-map)
      (let* ((score (cdr item))
	     (lhs   (car (car item)))
	     (rhs   (cadr (car item)))
	     (context-triple  lhs) ;(replace-kb-instance-with-kb-concept lhs))
	     (concept-triple  rhs));(replace-kb-instance-with-kb-concept rhs)))
	(format stream "   ~a maps to ~a~%" context-triple concept-triple)))))

(defun get-unique-scenario-triples (whynot-output)
  (get-unique-for-idx whynot-output 1))

(defun get-unique-for-idx(lst idx)
  (remove-duplicates (mapcar #'(lambda (item) 
				(let* ((concept-triple (nth idx item)))
				  concept-triple))
			    lst)))

(defun sieve-list-having-val-for-idx(val lst idx)
    (remove nil
	    (mapcar #'(lambda (item)
		      (let* ((idx_val (nth idx item)))
			(if (equal val idx_val)
			    item)))
		    lst)))

