;;
;; $Id: km-assertion-ordering-routines.lisp,v 1.2 2008/06/24 21:22:26 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun order-km-assertions(asserted-instances km-assertion-lst &optional(results nil))
  (let ((next-contained-km-assertion (find-contained-km-assertion asserted-instances km-assertion-lst)))
    (cond ((null km-assertion-lst) results)
	  ((null next-contained-km-assertion) (append results km-assertion-lst))
	  (t (order-km-assertions (remove-duplicates 
				   (append asserted-instances 
					   (extract-all-km-instances
					    next-contained-km-assertion)))
				  (set-difference km-assertion-lst (list next-contained-km-assertion) :test 'equal)
				  (insert-at-end next-contained-km-assertion results))))))

(defun find-contained-km-assertion(asserted-instances km-assertion-lst)
  (let ((target-km-assertion (car km-assertion-lst)))
    (cond ((null target-km-assertion) nil)
	  ((contained-km-assertion-p asserted-instances target-km-assertion)
	   target-km-assertion)
	  (t (find-contained-km-assertion asserted-instances (cdr km-assertion-lst))))))

(defun contained-km-assertion-p (asserted-instances km-assertion)
  (let ((mentioned-km-instances (extract-all-km-instances (cdr km-assertion))))
    (eval 
     (cons 'and
	   (mapcar #'(lambda(instance)
		       (not (null (member instance asserted-instances))))
		   mentioned-km-instances)))))

#|
;;Deprecated.
(defun extract-all-km-instances-in-list(input)
  (remove nil 
	  (remove-duplicates
	   (mapcar #'(lambda(symbol)
		       (if (km-instancep symbol) symbol))
		   (flatten input)))))
|#

(defun frames-to-triple-lst(km-frame-lst)
  (mappend #'(lambda(km-frame)
	       (let ((root (car km-frame)))
		 (mappend #'(lambda(slot)
			      (let ((slotname (car slot))
				    (filler-lst (cadr slot)))
				(mapcar #'(lambda(filler)
					    (list root slotname filler))
					filler-lst)))
			  (cddr km-frame))))
	   km-frame-lst))

(defun order-km-triple-lst(triple-list)
  (multiple-value-bind 
      (skolem-assertions non-skolem-assertions)
      (ps-get-frames-for-triples triple-list)
    (let ((debug nil)
	  (result (frames-to-triple-lst (append skolem-assertions 
						non-skolem-assertions))))
      (if (and debug (not (null (set-difference triple-list result :test 'ps-triple-equal))))
	  (progn 
	    (format t "Warning: (order-km-triple-lst ...) returning less triples? ~A~%Appending to end!~%~%"
		    (set-difference triple-list result :test 'ps-triple-equal))
	    (append result (set-difference triple-list result :test 'ps-triple-equal)))
	result))))
