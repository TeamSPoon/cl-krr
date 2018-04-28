;;
;; $Id: spreading-activation-code.lisp,v 1.3 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-relate (concept1 concept2)
  (ps-relate-instances (ps-instantiate-concept concept1)
		       (ps-instantiate-concept concept2)))

(defun ps-relate-instances (instance1 instance2)
  (let ((left-to-right-resultset (do-spreading-activation instance1
							  instance2))
	(right-to-left-resultset (do-spreading-activation instance2
							  instance1)))
    (cond ((and (= (length left-to-right-resultset) 1)
		(= (length right-to-left-resultset) 1)
		(equal '|related-to| (triple-relation (car left-to-right-resultset)))
		(equal '|related-to| (triple-relation (car right-to-left-resultset))))
	   (let ((result nil))
	     (format t "Regular spreading activation did not find anything between ~a and ~a.~%Will try another way by looking at concept prototypes directly.~%"
		     instance1 instance2)
	     (dolist (prototype (quasi-find-prototype-relating 
				 (car (get-concept-for-kb-instance instance1))
				 (car (get-concept-for-kb-instance instance2))))
	       (multiple-value-bind
		   (root graph)
		   (ps-get-new-graph prototype)
		 (setq result (append graph result))))
	     result))
	  (t 
	   (properly-terminate-triple-list
	    (append left-to-right-resultset
		    right-to-left-resultset))))))

(defun ps-relate-all-instances (lst)
  (let ((debug nil)
	(pairs (remove-duplicates (remove-if #'(lambda(x) (equal (car x)(cadr x))) (all-pairs lst)) :test 'set-equal)))
    (if debug (progn (format t "Building a graph for ~%")
		     (dolist (x pairs)
		       (format t "   ~a~%" x))))
    (remove-duplicates
     (mappend #'(lambda(pair)
		  (ps-relate-instances (car pair) (cadr pair)))
	      pairs)
     :test 'ps-triple-equal)))

