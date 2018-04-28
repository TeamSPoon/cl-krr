;;
;; $Id: assumption-extractor.lisp,v 1.1 2008/04/21 20:30:58 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-specialized-property-value-instances(triple-lst)
  (let ((target-lst (remove-if-not #'(lambda(x) 
				       (quasi-property-value-instance-p x triple-lst))
				   (extract-all-instances-from-triple-list triple-lst)))
	(result nil))
    (dolist (x target-lst)
      (if (not (null (extract-triples-for-root-slot x '|value| triple-lst)))
	  (push x result)))
    (dolist (x target-lst)
      (dolist (y (copy-list result))
	(dolist (triple triple-lst)
	  (if (and (not (equal x y))
		   (member x triple) 
		   (member y triple))
	      (progn 
		(push (triple-head triple) result)
		(push (triple-tail triple) result))))))
    (remove-duplicates result)))

(defun get-interesting-assumption-triples(concept-root model-graph assumption-triple-lst)
  (let ((target-lst (get-specialized-property-value-instances assumption-triple-lst)))
    (append
     (mappend #'(lambda(x)
		  (extract-triples-for-root-slot x '|value| assumption-triple-lst))
	      target-lst)
     (quasi-properly-terminate-triple-list
      (remove-duplicates
       (mappend #'(lambda(x)
		    (ps-get-path concept-root x assumption-triple-lst))
		target-lst)
       :test 'ps-triple-equal)
      (append assumption-triple-lst model-graph)))))

(defun get-interesting-assumption-triples-for-viewpoint(vp-inst)
  (let ((concept-root     (get-viewpoint-model-root vp-inst))
	(model-graph      (strip-triple-prefix (get-viewpoint-model vp-inst)))
	(assumption-triples     (strip-triple-prefix (get-viewpoint-assumption-triples vp-inst))))
    (get-interesting-assumption-triples concept-root model-graph assumption-triples)
))