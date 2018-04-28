;;
;; $Id: controller-pruning-conditions.lisp,v 1.45 2008/10/16 10:41:48 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Determines the skolem text-description of the query instance.
;;The input scenario and query-instance must not have been asserted into KM, i.e. completely foreign to it.
;;Uses KM's logging facility to keep the KM context clean.
;; 15Oct08
;; deprecated (defun determine-pruning-condition (vp-inst
;; 				    &optional(verbose nil))
;;   (multiple-value-bind
;;       (scenario orig->clone clone->orig)
;;       (ps-clone-triple-list 
;;        (strip-triple-prefix 
;; 	(get-viewpoint-scenario vp-inst)))
;;     (let* ((vp-query              (get-viewpoint-query vp-inst))
;; 	   (skolem-textgen-list   (generate-contextual-skolem-text-for-query-list 
;; 				   scenario 
;; 				   vp-query 
;; 				   clone->orig))
;; 	   (skolem-slot-filler-list (generate-contextual-skolem-slot-filler-for-query-list 
;; 				     scenario
;; 				     vp-query 
;; 				     clone->orig))
;; 	   (query-skolem-pair-lst   (pair-up vp-query skolem-textgen-list)))
;;       (if verbose
;; 	  (dolist (query-skolem-pair query-skolem-pair-lst)
;; 	    (let ((vp-query-entry  (first  query-skolem-pair))
;; 		  (skolem-textgen  (second query-skolem-pair)))
;; 	      (format verbose "BPS: Skolem-txt : ~a => ~a~%" (extract-query-path vp-query-entry) skolem-textgen))))
;;       (setq *CONTROLLER-SKOLEM-TEXTGEN-LIST* skolem-textgen-list)
;;       (mutate-pruning-condition (pruning-condition skolem-textgen-list
;; 						   skolem-slot-filler-list)))))

(defun generate-contextual-skolem-text-for-query-list (cloned-scenario vp-query mappings)
  (let ((debug nil))
  (mapcar #'(lambda (vp-query-entry)
	      (let* ((query-path             (extract-query-path vp-query-entry))
		     (query-slot-name        (nth 1 query-path))
		     (cloned-query-frame     (find-clone (nth 3 query-path) mappings))
		     (orig-query-instance    (car (ps-km-query query-path debug)))
		     (cloned-query-instance  (find-clone orig-query-instance mappings)))
		(generate-contextual-skolem-text-for-query cloned-scenario 
							   cloned-query-instance 
							   query-slot-name 
							   cloned-query-frame)))
	  vp-query)))

(defun get-skolem-text-description-for-instance(instance)
  (get-text-description-for-km-frame 
   (ps-instantiate-concept 
    (get-concept-for-kb-instance instance))))

(defun generate-contextual-skolem-text-for-query (scenario
						  query-instance 
						  query-slot-name 
						  query-frame-instance 
						  &optional (verbose nil))
  (cond ((equal query-slot-name '|instance-of|) (format nil "~a" query-instance))            ;;Skolem text for instance-of
	((null query-instance)  nil)                                                         ;;Empty query-instance without slot-filler. No defaults.
	(t (get-skolem-text-description-for-instance query-instance))))

(defun generate-contextual-skolem-slot-filler-for-query-list (cloned-scenario 
							      vp-query 
							      mappings 
							      &optional(verbose nil))
  (let ((verbose nil))
    (mapcar #'(lambda (vp-query-entry)
		(let* ((query-path             (extract-query-path vp-query-entry))
		       (query-slot-name        (nth 1 query-path))
		       (cloned-query-frame     (find-clone (nth 3 query-path) mappings))
		       (orig-query-slot-filler (ps-km-query query-path verbose)))
		  (mapcar #'(lambda(x)
			      (ps-km-query `(|the| |instance-of| |of| ,x) verbose))
			  orig-query-slot-filler)))
	    vp-query)))

#|
;;deprecated (defun generate-query-paths-for-slot-list(slot-list query-frame)
  (mapcar #'(lambda (slot)
	      (list '|the| slot '|of| query-frame))
	  slot-list))

;;deprecated (defun show-skolem-text()
  (let* ((vp-query                (get-viewpoint-query (root-viewpoint)))
	(query-skolem-pair-lst    (pair-up vp-query    *CONTROLLER-SKOLEM-TEXTGEN-LIST*)))
    (format t "BPS: Skolem text descriptions(~a entries)~%" (length query-skolem-pair-lst))
    (dolist (query-skolem-pair query-skolem-pair-lst)
      (let ((vp-query-entry  (first  query-skolem-pair))
	    (skolem-textgen  (second query-skolem-pair)))
	(format t "   ~a => ~a~%" (extract-query-path vp-query-entry) skolem-textgen)))))

|#

