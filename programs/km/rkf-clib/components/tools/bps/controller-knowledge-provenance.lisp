;;
;; $Id: controller-knowledge-provenance.lisp,v 1.13 2008/08/18 21:01:16 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun gather-knowledge-provenance ()
  (list (get-question-formulation-triples (get-answer-viewpoint))
	(get-question-setup-triples       (get-answer-viewpoint))
	(get-problem-solving-triples      (get-answer-viewpoint))))

(defun get-original-question-formulation-triples()
  *CONTROLLER-NON-CPL-TRIPLES*)

(defun get-original-question-setup-triples()
  *CONTROLLER-RULE-ENGINE-TRIPLES*)

(defun get-question-formulation-triples (vp-inst)
  (let ((result (find-cloned-triple-lst *CONTROLLER-NON-CPL-TRIPLES*
					(get-simplified-context-for-viewpoint vp-inst))))
    (if (not (= (length result) (length *CONTROLLER-NON-CPL-TRIPLES*)))
	(format t "bps: There were originally ~a question formulation triples, but only found ~a in viewpoint context. The others were specialized during problem-solving.~%" 
		(length *CONTROLLER-NON-CPL-TRIPLES*) 
		(length result)))
    result))
      
(defun get-question-setup-triples (vp-inst)
  (let ((result (find-cloned-triple-lst *CONTROLLER-RULE-ENGINE-TRIPLES* (get-simplified-context-for-viewpoint vp-inst))))
    (if (not (= (length result) (length *CONTROLLER-RULE-ENGINE-TRIPLES*)))
	(format t "bps: There were originally ~a question setup triples, but only found ~a in viewpoint context. The others were specialized during problem-solving.~%" 
		   (length *CONTROLLER-RULE-ENGINE-TRIPLES*) 
		   (length result)))
    result))

(defun get-problem-solving-triples (vp-inst)
  (let ((question-formulation-triples (get-question-formulation-triples vp-inst))
	(question-setup-triples       (get-question-setup-triples       vp-inst)))
    (set-difference  (get-simplified-context-for-viewpoint vp-inst) 
		     (union question-formulation-triples
			    question-setup-triples)
		     :test #'equal)))

(defun get-concept-origin(concept-name)
  (remove nil
	  (list (if (member concept-name *phys-halo-pump-prime-list*)        '*phys-halo-pump-prime-list*)
		(if (member concept-name *chem-halo-pump-prime-list*)        '*chem-halo-pump-prime-list*)
		(if (member concept-name *bio-halo-pump-prime-list*)         '*bio-halo-pump-prime-list*)
		(if (member concept-name *bio-lbr-pump-prime-list*)          '*bio-lbr-pump-prime-list*)
		(if (member concept-name *engine-lbr-pump-prime-list*)       '*engine-lbr-pump-prime-list*)
		(if (member concept-name *science-shared-pump-prime-list*)   '*science-shared-pump-prime-list*)
		(if (member concept-name *clib-list*)                        '*clib-list*))))

#|FIXME... Fill in the semantics for the following 2 routines.

(defun CLIB-concept-p(concept)
  )

(defun pump-primed-concept-p(concept)
  )

|#

;;Predicate to test if a concept is user-defined.
(defun user-defined-concept-p(concept)
  (if (member concept *all-user-defined-concepts*) t))

;;Returns all correspondence mappings between scenario and model graph created in scenario elaboration.
(defun get-elaboration-mappings(scenario vp-instance)
  (convert-viewpoint-correspondence-to-assoc-list 
   (get-all-viewpoint-correspondence vp-instance)))

;;Returns clone mapping for instance-list in elaborated scenario
(defun get-clone-mappings-for-elaborated-scenario(original-scenario
						  elaborated-scenario)
  (let ((original-instances   (extract-all-instances-from-triple-list original-scenario))
	(elaborated-instances (extract-all-instances-from-triple-list elaborated-scenario)))
    (remove nil
	    (mapcar #'(lambda(elaborated-instance)
			(let ((target (intersection (ps-get-clone-ancestry elaborated-instance)
						    original-instances)))
			  (if (= (length target) 1)
			      (cons (car target) elaborated-instance))))
		    elaborated-instances))))

;;Returns all viewpoint correspondences for vp-instance along path to vp-root in bps-tree.
(defun get-all-viewpoint-correspondence(vp-instance)
  (cond ((viewpoint-rootp vp-instance) ())
	(t (append (get-viewpoint-correspondence vp-instance)
		   (get-all-viewpoint-correspondence (get-viewpoint-parent vp-instance))))))
