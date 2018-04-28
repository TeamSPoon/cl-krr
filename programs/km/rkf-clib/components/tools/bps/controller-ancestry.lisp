;;
;; $Id: controller-ancestry.lisp,v 1.6 2008/08/13 19:32:23 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-children(vp-inst)
  (let* ((target-slot-value (get-slotvalue-from-viewpoint-instance '|viewpoint-child|  
								   vp-inst)))
    (sort-viewpoint-list target-slot-value)))

(defun get-viewpoint-tree(&optional (vp-inst (get-root-viewpoint)))
  (cond ((null vp-inst) ())
	((get-children vp-inst) (cons vp-inst (list (mapcar #'(lambda(x) (get-viewpoint-tree x)) (get-children vp-inst)))))
	(t (list vp-inst))))

(defun sort-viewpoint-list(input)
  (sort (copy-list input) 
	#'(lambda(x y)(and (numberp (get-viewpoint-ordering x))
			   (numberp (get-viewpoint-ordering y))
			   (< (get-viewpoint-ordering x)
			      (get-viewpoint-ordering y))))))

(defun get-viewpoint-ordering(vp-inst)
  (if (member vp-inst (get-ordered-viewpoint-list))
      (- (length (get-ordered-viewpoint-list))
	 (length (member vp-inst (get-ordered-viewpoint-list))))))

(defun get-ordered-viewpoint-list()
  (append *closedlist* (mapcar 'car *openlist*)))

(defun get-root-viewpoint()
  (car (get-ordered-viewpoint-list)))

(defun get-parent(vp-inst)
  (let* ((target-slot-value (get-slotvalue-from-viewpoint-instance '|viewpoint-parent|  
								   vp-inst)))
    target-slot-value))

(defun get-descendants(vp-inst)
  (get-viewpoint-tree vp-inst))

(defun get-ancestors-sub(vp-inst)
  (let* ((target-slot-value (get-slotvalue-from-viewpoint-instance '|viewpoint-parent|  
								   vp-inst)))
    (cons vp-inst 
	  (mapcar #'(lambda (x) (get-ancestors-sub x)) target-slot-value))))

(defun get-ancestors(vp-inst)
  (flip-linear-tree (get-ancestors-sub vp-inst)))

(defun descendant-viewpoint-p(ancestor descendant)
  (not (null (member descendant (flatten (get-descendants ancestor))))))
