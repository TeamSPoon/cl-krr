;;
;; $Id: viewpoint-accessor-routines.lisp,v 1.27 2009/06/17 18:04:03 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Returns the slotvalue of slot in some Viewpoint instance
(defun get-slotvalue-from-viewpoint-instance(slotname vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (ps-slot-lookup vp-instance slotname)))

;;Returns the filler for viewpoint-scenario slot in some Viewpoint instance
(defun get-viewpoint-scenario(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
   (let ((slotvalue (strip-triple-prefix
		     (get-slotvalue-from-viewpoint-instance '|viewpoint-scenario|
							   vp-instance))))
     (if (root-viewpointp vp-instance)
	 (multiple-value-bind
	     (lookup-query-lst yn-query-lst)
	     (collate-viewpoint-query (get-viewpoint-query vp-instance))
	   (install-ae-triples-for-skolem-query-instances-in-scenario
	    slotvalue
	    (append lookup-query-lst yn-query-lst)))
       slotvalue
       ))))     

;;Returns the filler for viewpoint-score slot in some Viewpoint instance
(defun get-viewpoint-score(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-score|
					 vp-instance)))

;;Returns the filler for viewpoint-model-graph slot in some Viewpoint instance
(defun get-viewpoint-model(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-model-graph|
					 vp-instance)))

(defun get-viewpoint-assumption-triples(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-assumption-triples|
					 vp-instance)))

(defun get-viewpoint-model-root(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (car (get-slotvalue-from-viewpoint-instance '|viewpoint-model-root|
					 vp-instance))))

(defun get-viewpoint-model-root-in-asserted-triples(vp-inst)
  (let ((map (invert-map (strip-pair-prefix (get-viewpoint-correspondence vp-inst)))))
    (intern (cadr (assoc (format nil "~a" (get-viewpoint-model-root vp-inst)) map :test 'string=)) :km)))

(defun get-viewpoint-triple-map(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-triple-map|
					 vp-instance)))

;;Returns the filler for viewpoint-correspondence slot in some Viewpoint instance
(defun get-viewpoint-correspondence(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-correspondence|
					 vp-instance)))

;;Returns the filler for viewpoint-source slot in some Viewpoint instance
(defun get-viewpoint-source(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-source|
					 vp-instance)))

;;Returns the filler for viewpoint-target slot in some Viewpoint instance
(defun get-viewpoint-target(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (cdr (car (get-slotvalue-from-viewpoint-instance '|viewpoint-target|
						   vp-instance)))))

;;Returns the filler for viewpoint-target slot in some Viewpoint instance
(defun get-viewpoint-concept(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
   (car (get-slotvalue-from-viewpoint-instance '|viewpoint-concept|
					       vp-instance))))

;;Returns the filler for viewpoint-target slot for all viewpoints in list.
(defun get-viewpoint-target-for-viewpoint-list(vp-inst-list)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (flatten 
   (mapcar #'(lambda (vp-inst) (get-viewpoint-target vp-inst))
	   vp-inst-list))))

;;Returns the filler for viewpoint-query slot in some Viewpoint instance
(defun get-viewpoint-query(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (cdr (car (get-slotvalue-from-viewpoint-instance '|viewpoint-query|
						   vp-instance)))))

;;Returns the filler for viewpoint-answerable-query slot in some Viewpoint instance
(defun get-viewpoint-answerable-query(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
   (let ((filler (cdr (car (get-slotvalue-from-viewpoint-instance '|viewpoint-answerable-query|
								  vp-instance)))))
     (values filler
	     (remove-if-not #'(lambda(entry)
				(and (listp entry)
				     (equal (nth 1 entry) '|*yes-no-question|)))
			    filler)
	     (remove-if-not #'(lambda(entry)
				(and (listp entry)
				     (equal (nth 1 entry) '|*slot-value|)))
			    filler)))))
	     
;;Returns the filler for viewpoint-query-matched slot in some Viewpoint instance
(defun get-viewpoint-query-matched(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-query-matched|
					 vp-instance)))

;;Returns the filler for viewpoint-answer slot in some Viewpoint instance
(defun get-viewpoint-answer(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-answer|
					 vp-instance)))

;;Returns the filler for viewpoint-parent slot in some Viewpoint instance
(defun get-viewpoint-parent(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-parent|
					 vp-instance)))

;;Returns the filler for viewpoint-parent slot in some Viewpoint instance
(defun get-viewpoint-filter(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-filter|
					 vp-instance)))

;;Returns the depth of viewpoint in problem-solving tree.
(defun get-viewpoint-depth(vp-inst)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (let ((vp-parent (get-viewpoint-parent vp-inst)))
    (cond ((null vp-parent) 0)
	  (t (1+ (get-viewpoint-depth vp-parent)))))))

;;Returns the root viewpoint in problem-solving tree.
(defun get-viewpoint-root(vp-inst)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (let ((vp-parent (get-viewpoint-parent vp-inst)))
    (cond ((null vp-parent) vp-inst)
	  (t (get-viewpoint-root vp-parent))))))

;;Returns the filler for viewpoint-child slot in some Viewpoint instance
(defun get-viewpoint-children(vp-instance)
 (with-restoring-situations-reasoning ()
   (in-situation *CONTROLLER-KM-SITUATION*)
  (get-slotvalue-from-viewpoint-instance '|viewpoint-child|
					 vp-instance)))

(defun unformat-viewpoint-correspondence(input)
  (mapcar #'(lambda(entry)
	      (list (intern (nth 1 entry) :km)
		    (intern (nth 2 entry) :km)))
	  input))

