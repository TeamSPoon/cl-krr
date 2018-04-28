;;
;; $Id: write-viewpoint-context.lisp,v 1.13 2008/08/12 22:19:59 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun write-context-to-file (file vp-inst)
  (let* ((stream (tell file))
	 (context  (get-simplified-context-for-viewpoint vp-inst)))
      (format stream "~%;;; The Basic Problem Solver: ~a~%" vp-inst)
      (format stream ";;; Declared instances      : ~a~%" 
	      (extract-triple-head-from-triple-list (sieve-triple-list-having-relation '|instance-of| context)))
      (format stream ";;; All instances           : ~a~%" 
	      (extract-all-instances-from-triple-list context))
      (dolist (triple-cluster (triples-to-km-assertions context))
	(format stream "~%~a~%" triple-cluster))
      (cond ((streamp stream) (close stream)))))

(defun triples-to-km-assertions (triple-list)
  (convert-triple-cluster-to-km 
   (sort 
    (cluster-triple-list-by-relevant-instance-of-relation-head 
     (aggregate-similar-relations 
      (make-triple-proper triple-list))) 
    #'(lambda(x y) (< (length x) (length y))))))

;;Returns a cluster list where each cluster contain triples associated with some KM instance
;;Also, Aura specific frames,e.g. Property-Node and Equation-Big-Node are removed.
(defun cluster-triple-list-by-relevant-instance-of-relation-head(triple-list)
  (cluster-triple-list-having-head  
   (extract-triple-head-from-triple-list 
    (remove nil 
	    (mapcar #'(lambda (triple)
			(if (not (concept-ignorep (triple-tail triple)))
			    triple))
		    (sieve-triple-list-having-relation '|instance-of| triple-list))))
   triple-list))
  
;;Converts a list of triple-clusters into KM representation.
;;Basically each cluster is the triples associated with some KM instance.
(defun convert-triple-cluster-to-km (triple-cluster)
  (mapcar #'(lambda (triple-list)
	      (my-triples2km triple-list))
	  triple-cluster))

(defun my-triples2km (triple-list)
  (let* ((instance-of-triple   (first (sieve-triple-list-having-relation '|instance-of| triple-list)))
	 (content-triple-lst   (sieve-triple-list-not-having-relation '|instance-of| triple-list))
	 (instance-name        (triple-head instance-of-triple))
	 (instance-of-content  (triple-tail instance-of-triple))
	 (result               `(,instance-name |has|)))
    (if (listp instance-of-content)
	(setf result (append result `((|instance-of| ,(remove-subsumers instance-of-content)))))
	(setf result (append result `((|instance-of| (,instance-of-content))))))
    (dolist (triple content-triple-lst)
      (let ((reln     (triple-relation triple))
	    (content  (triple-tail     triple)))
	(if (or (ps-aggregatep content)
		(quotep content)
		(not (listp content)))
	    (setf result (append result `((,reln (,content)))))
	    (setf result (append result `((,reln ,(reverse content)))))))) ;;IMPT! a) asserted ordering must mirror reverse of instance ordering in prototype def. If not, bad unification will occur.
    result))

(defun ps-aggregatep(content)
  (if (listp content)
      (let ((1st-elem (first content)))
	(cond ((or (eql 1st-elem ':|pair|)
		   (eql 1st-elem ':|seq|)
		   (eql 1st-elem ':|set|)
		   (eql 1st-elem ':|bag|))
	       t)))))
