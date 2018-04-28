;;
;; $Id: debugger-tree-printer.lisp,v 1.9 2006/08/14 20:59:34 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun tree-printer(stream vp-tree resultset)
  (tree-printer-sub stream vp-tree "" resultset))

(defun tree-printer-sub(stream vp-tree prefix resultset)
  (progn
    (mapcar #'(lambda (x) (if(listp x) 
			      (tree-printer-sub stream x 
						(concatenate 'STRING "    " prefix)
						resultset)
		              (progn (format stream "~a~a" prefix x)
				     (if (not (member x *CLOSEDLIST*))
					 (format stream " ...")
				         (let ((result (cadr (car (member x resultset :test #'(lambda (x y)
											   (progn
											     (equal x (car y)))))))))
					   (if result
					       (format stream " ~a" result))))
				     (format stream "~%"))))
	  vp-tree)
    t))

(defun satisfyp-tree-printer(stream vp-tree pruning-condition)
  (if *controller-inspection-viewpoint*
      (tree-printer stream vp-tree nil)
      (tree-printer stream vp-tree 
		    (mapcar #'(lambda (vp-inst)
				(list vp-inst 
				      (if (not (null (funcall pruning-condition vp-inst)))
					  "*")))
			    *CLOSEDLIST*))))


(defun mapped-concept-tree-printer(vp-tree)
  (tree-printer t 
		vp-tree 
		(mapcar #'(lambda (vp-inst)
			    (list vp-inst 
				  (let* ((concept (car (get-concept-for-kb-instance (car (get-viewpoint-target vp-inst))))))
				    (if (null concept)
					"(None)"
				        (format nil "~a" concept)))))
			*CLOSEDLIST*)))

