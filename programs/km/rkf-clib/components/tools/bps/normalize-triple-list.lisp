;;
;; $Id: normalize-triple-list.lisp,v 1.5 2009/06/14 18:33:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Normalizes the KM instances in the triple list. This makes the triple list easier to read.
(defun normalize-triple-list(triple-lst 
			     &optional
			     (compute-question nil)
			     (yn-query nil)
			    )
  (let ((map    (generate-km-instance-normalized-assoc-list triple-lst)))
    (values (replace-elements-in-list 
	     triple-lst map)
	    (replace-elements-in-list compute-question map)
	    (replace-elements-in-list yn-query map)
	    map
	    )
))


(defun generate-km-instance-normalized-assoc-list(triple-lst)
  (generate-km-instance-normalized-assoc-list0
   (aggregate-assoc-list 
    (mapcar #'(lambda(instance)
		(cons (remove-numbering-for-km-instance instance)
		      instance))
	    (extract-all-instances-from-triple-list triple-lst)))))

(defun generate-km-instance-normalized-assoc-list0(aggregated-assoc-list)
  (mappend 'generate-km-instance-normalized-assoc-list-entry
	   aggregated-assoc-list))

(defun generate-km-instance-normalized-assoc-list-entry(aggregated-assoc-list-entry)
  (let* ((head     (car  aggregated-assoc-list-entry))
	 (tail     (cadr aggregated-assoc-list-entry))
	 (tail-len (length tail))
	 (idx      1))
    (cond ((= tail-len 1)
	   (list (cons (car tail) head)))
	  (t (mapcar #'(lambda(tail-entry)
			 (let ((new-head (intern (format nil "~a~a" head idx) :km)))
			   (setq idx (1+ idx))
			   (cons tail-entry new-head)))
		     tail)))))

(defun aggregate-assoc-list(assoc-list)
  (remove-duplicates (mapcar #'(lambda(head)
	      (aggregate-head-for-assoc-list head assoc-list))
	  (mapcar #'(lambda(assoc-list-entry)
		      (car assoc-list-entry))
		  assoc-list)) :test 'equal))
	
(defun aggregate-head-for-assoc-list (head assoc-list)
  (cons head
	(list 
	 (mapcar #'(lambda(x)
		     (cdr x))
		 (remove-if #'(lambda(x) (not (equal head (car x))))
			    assoc-list)))))

(defun remove-numbering-for-km-instance (km-instance)
  (intern (remove-trailing-number-from-string (string km-instance)) :km))

(defun remove-trailing-number-from-string(input)
  (let ((len (length input)))
  (cond ((numberp (string-to-number (subseq input 
                                            (1- len)
                                            len)))
         (remove-trailing-number-from-string (subseq input 0
                                                     (1- len))))
        (t input))))

(defun get-instance-of-assoc-lst(triple-lst)
  (let (result)
  (mapcar #'(lambda(triple)
	      (let ((inst    (triple-head triple))
		    (concept (triple-tail triple)))
		(cond ((not (null (assoc inst result)))
		       (push (list inst (cons concept (cadr (assoc inst result))))
			     result))
		      (t (push (list inst (list concept)) result)))))
	  (extract-instance-triples triple-lst))
  result))

;;replaces skolems with concept names in triple-lst
(defun triple-lst-structure(triple-lst)
  (let ((inst-concept-alist (get-instance-of-assoc-lst triple-lst)))
    (mappend #'(lambda(triple)
		 (triple-structure triple inst-concept-alist))
	     triple-lst)))

;;replaces skolems with concept names in triple
(defun triple-structure (triple inst-concept-alist)
  (let* ((head   (triple-head     triple))
	 (rel    (triple-relation triple))
	 (tail   (triple-tail     triple))
	 (head-repl (cadr (assoc head inst-concept-alist)))
	 (tail-repl (cadr (assoc tail inst-concept-alist)))
	 (result nil))
    (mappend #'(lambda(head-candidate)
		 (mapcar #'(lambda (tail-candidate)
			     (list head-candidate rel tail-candidate))
			 tail-repl))
	     head-repl)))
