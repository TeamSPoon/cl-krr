;;
;; $Id: controller-viewpoint-querying.lisp,v 1.25 2009/08/31 18:58:49 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-include-related-triples(input)
  (cond ((null input) ())
	((triple-p input)
	 (let ((query-frame (nth 0 input))
	       (slot-name   (nth 1 input))
	       (expected-filler (nth 2 input)))
	   (list input
		 (mapcar #'(lambda(x)
			     (cdr (nth 2 x)))
			 (make-viewpoint-query-slot slot-name query-frame expected-filler)
			 )
		 )))
	(t 
	 (cons 
	  (ps-include-related-triples (car input))
	  (ps-include-related-triples (cdr input))))
	))

(defun make-viewpoint-query-slot (slot-name frame-instance expected-filler)
  (let ((multislot-spatial-query-frame     (and 
					    *controller-expand-query*
					    (is-spatial-dimension-query-p     slot-name 
									      frame-instance)))
	(multislot-participant-query-frame (and
					    *controller-expand-query*
					    (is-participant-dimension-query-p slot-name 
									      frame-instance)))
	(multislot-meronymic-query-frame   (and
					    *controller-expand-query*
					    (is-meronymic-dimension-query-p   slot-name 
									      frame-instance)))
	(multislot-agentive-query-frame    (and 
					    *controller-expand-query*
					    (is-agentive-dimension-query-p    slot-name 
									      frame-instance))))
    (cond 
     ((not (null multislot-spatial-query-frame))
      (make-viewpoint-query-slot-for-dimension-spatial      multislot-spatial-query-frame expected-filler))
     ((not (null multislot-participant-query-frame))
      (make-viewpoint-query-slot-for-dimension-participant  multislot-participant-query-frame expected-filler))
     ((not (null multislot-meronymic-query-frame))
      (make-viewpoint-query-slot-for-dimension-meronymic    multislot-meronymic-query-frame expected-filler))
     ((not (null multislot-agentive-query-frame))
      (make-viewpoint-query-slot-for-dimension-agentive     multislot-agentive-query-frame expected-filler))
     (t (list (make-viewpoint-query-slot-entry slot-name frame-instance expected-filler))))))

(defun make-viewpoint-query-slot-for-question-triple-list (question-triple-list)
  (mappend #'(lambda(query-triple)
	       (let ((slot-name      (triple-relation query-triple))
		     (frame-instance (triple-head     query-triple))
		     (expected-filler  (triple-tail     query-triple)))
		 (list (make-viewpoint-query-slot-entry slot-name
							frame-instance 
							expected-filler))
		 )
	       )
	   question-triple-list))

(defun expand-viewpoint-query-slot-entry(vp-query-entry)
  (multiple-value-bind
      (slotname query-frame expected-filler)
      (extract-query-components-from-vp-query-entry vp-query-entry)
    (make-viewpoint-query-slot slotname query-frame expected-filler)))

(defun question-triple-lst2query-slot-lst(question-triple-lst)
  (mapcar 'question-triple2query-slot question-triple-lst))

(defun question-triple2query-slot(question-triple)
  (let ((slot-name      (triple-relation question-triple))
	(frame-instance (triple-head     question-triple))
	(target-filler  (triple-tail     question-triple)))
    (make-viewpoint-query-slot-entry slot-name
				     frame-instance 
				     target-filler)))

(defun make-viewpoint-query-slot-entry (slot-name frame-instance &optional(target-filler '*))
  (cond ((eq target-filler '*)
	 `(:|pair| |*slot-value| (:|triple| ,frame-instance ,slot-name ,target-filler)))
	(t 
	 `(:|pair| |*yes-no-question| (:|triple| ,frame-instance ,slot-name ,target-filler)))))

(defun construct-viewpoint-query (vp-inst 
				  query-prefixes)
  (attach-query (cons vp-inst query-prefixes)))

(defun construct-viewpoint-result-query (vp-inst 
					 query-prefixes)
  (let* ((query-path (extract-query-path 
		      vp-inst)))
    (attach-query 
     (cons query-path 
	   query-prefixes))))

;;Executes a KM query and returns a viewpoint result triple containing 
;;viewpoint instance, km-query, and km-query result
(defun apply-km-query-on-viewpoint (vp-inst
				    km-query)
  (list vp-inst
	km-query
	(ps-km-query km-query)))

;;Constructs a KM query and applies it to the viewpoint-result slot of
;;a Viewpoint instance. It returns a viewpoint-result-triple.
(defun apply-result-slot-query-on-viewpoint (vp-inst 
					     query-prefixes)
  (apply-km-query-on-viewpoint vp-inst 
			       (construct-viewpoint-result-query vp-inst
								 query-prefixes)))

;;Constructs a KM query and applies it to the viewpoint-result slot of
;;a list of Viewpoint instances. It returns a viewpoint-result-triple list.
(defun apply-result-slot-query-on-viewpoint-list(vp-inst-list 
						 query-prefixes)
  (mapcar #'(lambda (vp-inst) 
	      (apply-result-slot-query-on-viewpoint vp-inst
						    query-prefixes))
	  vp-inst-list))

;;Constructs a KM query and applies it to a Viewpoint instance. 
;;It returns a viewpoint-result-triple.
(defun apply-query-on-viewpoint (vp-inst 
				 query-prefixes)
  (apply-km-query-on-viewpoint vp-inst 
			       (construct-viewpoint-query vp-inst
							  query-prefixes)))

;;Constructs a KM query and applies it to a list of Viewpoint instances. 
;;It returns a viewpoint-result-triple list.
(defun apply-query-on-viewpoint-list(vp-inst-list 
				     query-prefixes)
  (mapcar #'(lambda (vp-inst) 
	      (apply-query-on-viewpoint vp-inst
					query-prefixes))
	  vp-inst-list))

;;Returns a list containing only values from viewpoint result triple list
(defun extract-result-from-viewpoint-result-triple-list (vp-result-triple-list)
  (mapcar #'(lambda (vp-result-triple) 
	      (third vp-result-triple))
	  vp-result-triple-list))

;;Returns a list containing unique values from viewpoint result triple list.
(defun get-unique-values-from-viewpoint-result-triple-list (vp-result-triple-list)
  (remove-duplicates
   (extract-result-from-viewpoint-result-triple-list vp-result-triple-list)
   :test #'equal))

(defun attach-query (query-prefixes)
  (reduce #'(lambda (tail head)
	      (insert-at-end tail head))
	  query-prefixes))

(defun pop-viewpoint-query-prefixes ()
  (progn 
    (mutate-viewpoint-query-prefixes
     (reverse (cdr (reverse *viewpoint-query-prefixes*))))
    t))

(defun pop-viewpoint-result-query-prefixes ()
  (progn 
    (mutate-viewpoint-result-query-prefixes
     (reverse (cdr (reverse *viewpoint-result-query-prefixes*))))
    t))

(defun push-viewpoint-query-prefixes (path)
  (progn 
    (mutate-viewpoint-query-prefixes
     (insert-at-end path *viewpoint-query-prefixes*))
    t))

(defun push-viewpoint-result-query-prefixes (path)
  (progn 
    (mutate-viewpoint-result-query-prefixes
     (insert-at-end path *viewpoint-result-query-prefixes*))
    t))



