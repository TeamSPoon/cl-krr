;
;; $Id: ps-get-path.lisp,v 1.5 2008/05/19 02:03:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defvar *ps-get-path-computations* nil)

(defun ps-get-path (head tail triple-list  &optional(previous-head-list  nil))
  (let ((*ps-get-path-computations* nil))
    (remove-duplicates (ps-get-path0 head tail 
				     (remove-reflexive-triples-from-triple-list
				     triple-list)
				     previous-head-list)
		       :test 'ps-triple-equal)))

(defun ps-get-path0 (head tail triple-list &optional(previous-head-list  nil))
  (let ((prev-compute (assoc (list head tail) *ps-get-path-computations* :test 'equal)))
    (cond ((not (null prev-compute))
	   (cadr prev-compute))
	  ((equal head tail) ())
	  (t 
	   (let ((result (or (ps-get-direct-path head tail triple-list)
			     (mappend #'(lambda(triple)
					  (let* ((intermediate-head (triple-tail triple)))
					    (if (not (member intermediate-head previous-head-list))
						(let ((tmp-result 
						       (ps-get-path0 intermediate-head 
								     tail 
								     triple-list 
								     (cons head 
									   previous-head-list))))
						  (cond ((not (null tmp-result))
							 (cons triple tmp-result)))))))
				      (extract-non-instance-triples (sieve-triple-list-having-head 
								     head triple-list))))))
	     (push (list (list head tail) result)
		   *ps-get-path-computations*)
	     result)))))

(defun ps-get-direct-path (head tail triple-list)
  (remove nil
	  (mapcar #'(lambda(triple)
		      (if (or (and (equal head (triple-head triple))
				   (equal tail (triple-tail triple)))
			      (and (equal head (triple-tail triple))
				   (equal tail (triple-head triple))))
			  triple))
		  triple-list)))

;;assumes that (ps-get-path) returns ordered lst of triples from head to tail instance.
(defun ps-get-query-path (head tail triple-lst)
  (let* ((path-triple-lst           (ps-get-path head tail triple-lst))
	 (intermediate-relation-lst (mapcar 'cadr path-triple-lst))
	 result)
    (if (not (null path-triple-lst))
	(progn
	  (setf result (list '|the| (car intermediate-relation-lst) '|of| head))
	  (dolist (x (cdr intermediate-relation-lst))
	    (setf result (cons '|the| (cons x (cons '|of| (list result))))))))
    result))