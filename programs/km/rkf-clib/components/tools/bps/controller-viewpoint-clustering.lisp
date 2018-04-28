(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Given a viewpoint instance list, it will return a list of clusters, where
;;all Viewpoints in the same cluster has the same value.
(defun cluster-viewpoint-list-by-query (vp-inst-list
					query-prefixes)
  (let* ((vp-result-triple-list 
	  (apply-result-slot-query-on-viewpoint-list vp-inst-list
						     query-prefixes)))
    (cluster-viewpoint-set-by-value vp-result-triple-list)))

;;Given a viewpoint-result-triple list, it will return a list of clusters, where
;;all Viewpoints in the same cluster has the same value.
(defun cluster-viewpoint-set-by-value (vp-result-triple-list)
  (mapcar 
   #'(lambda(value)
       (list value (get-viewpoints-having-value vp-result-triple-list
						value)))
   (get-unique-values-from-viewpoint-result-triple-list vp-result-triple-list)))

;;Given a viewpoint-result-triple list and a value as input, it will returns 
;;the sub-list of Viewpoints having the same input value.
(defun get-viewpoints-having-value(vp-result-triple-list value)
  (remove 'nil (mapcar #'(lambda (vp-result-triple) 
			   (if (equal (third vp-result-triple)
				      value)
			       (first vp-result-triple)
			     nil))
		       vp-result-triple-list)))