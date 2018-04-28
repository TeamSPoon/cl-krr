;;
;; $Id: semantic-matcher-auxiliary-routines.lisp,v 1.4 2008/06/21 02:30:35 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-convert-to-semantic-matcher-form (petes-triples &optional (node-to-km-mappings *node-to-km-mappings*)
					                            (km-to-node-mappings *km-to-node-mappings*))
  (progn
    (ps-get-instance-of-triples petes-triples 
				node-to-km-mappings 
				km-to-node-mappings)
    (remove nil
	    (mappend #'(lambda(triple)
			 (ps-convert-non-instance-triple-to-peters-form triple node-to-km-mappings km-to-node-mappings))
		     (extract-non-instance-triples petes-triples)))))
  
(defun ps-convert-non-instance-triple-to-peters-form (triple node-to-km-mappings km-to-node-mappings &optional(verbose nil) )
  (if (not (instance-triple-p triple))
      (let ((instance (triple-head     triple))
	    (relation (triple-relation triple))
	    (filler   (triple-tail     triple)))
	(let ((head-candidate-list (ps-get-km-to-node-mapping instance km-to-node-mappings))
	      (tail-candidate-list (if (atom filler)
				       (ps-get-km-to-node-mapping filler km-to-node-mappings)
				     (list (triple-tail triple)))))
	  (if verbose
	      (progn 
		(format t "head-candidate-list: ~a~%" head-candidate-list)
		(format t "relation           : ~a~%" relation)
		(format t "tail-candidate-list: ~a~%" tail-candidate-list)))
	  (mappend #'(lambda(head-candidate) 
		       (mapcar #'(lambda(tail-candidate)
				   (list head-candidate relation tail-candidate))
			       tail-candidate-list))
		   head-candidate-list)))))

(defun ps-get-km-to-node-mapping (instance km-to-node-mappings)
  (let ((hash-value (gethash instance km-to-node-mappings)))
    (cond ((null hash-value) nil)
	  (t hash-value))))

(defun ps-get-node-to-km-mapping (node node-to-km-mappings)
  (let ((hash-value (gethash node node-to-km-mappings)))
    (cond ((null hash-value) nil)
	  (t hash-value))))

(defun ps-get-instance-of-triples (triples 
				   node-to-km-mappings 
				   km-to-node-mappings 
				   &optional(verbose nil))
  (let ((instance-list (append (extract-all-km-instances triples)
			       (extract-all-km-constants triples))))
    (dolist (instance instance-list)
      (let* ((concept-list  
	      (cond ((km-constantp instance)
		     (ps-slot-lookup instance '|instance-of|))
		    (t (quasi-get-concept-for-kb-instance instance triples)))))
	(dolist (c concept-list)
	  (let* ((unique-id     (string (gensym "Node")))
		 (new-node      (cons c unique-id)))
	    (if verbose (format t "new-node: ~a~%" new-node))
	    (setf (gethash new-node node-to-km-mappings) instance)
	    (mutate-hash-table instance 
			       new-node 
			       km-to-node-mappings)))))))
