;;
;; $Id: controller-viewpoint-evolution.lisp,v 1.18 2007/10/10 19:48:25 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun determine-multislot-query-type(query-path-list)
  (remove nil
	  (mapcar #'(lambda(query-path)
		      (let* ((query-path (car query-path-list))
			     (vp-slot-value-pair       (list   (second query-path)
							       (fourth query-path)))
			     (query-slot-name          (first  vp-slot-value-pair))
			     (query-frame-instance     (second vp-slot-value-pair)))
			(determine-multislot-query-type0 query-slot-name query-frame-instance)))
		  query-path-list)))

(defun determine-multislot-query-type0(slot-name frame-instance)
  (let ((multislot-spatial-query-frame     (is-spatial-dimension-query-p     slot-name 
									     frame-instance))
	(multislot-participant-query-frame (is-participant-dimension-query-p slot-name 
									     frame-instance))
	(multislot-meronymic-query-frame   (is-meronymic-dimension-query-p   slot-name 
									     frame-instance))
	(multislot-agentive-query-frame    (is-agentive-dimension-query-p    slot-name 
									     frame-instance)))
    (cond ((not (null multislot-spatial-query-frame))
	   '|*spatial-multislot-query*|)
	  ((not (null multislot-participant-query-frame))
	   '|*participant-multislot-query*|)
	  ((not (null multislot-meronymic-query-frame))
	   '|*meronymic-multislot-query*|)
	  ((not (null multislot-agentive-query-frame))
	   '|*agentive-multislot-query*|)
	  (t nil))))

