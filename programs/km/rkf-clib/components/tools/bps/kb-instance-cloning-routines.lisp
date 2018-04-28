;;
;; $Id: kb-instance-cloning-routines.lisp,v 1.25 2009/06/14 18:33:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Returns the clone of a instance given some mapping.
;;If no clone exist, return orig instance
(defun find-clone(orig clone->orig)
  (let* ((result orig))
    (dolist (mapping clone->orig)      
      (if (equal (cdr mapping) orig) 
	  (progn (setf result (car mapping)) (return))))
    result))

(defun find-clone-for-list(expr clone->orig)
  (cond ((atom expr)  (find-clone expr clone->orig))
	((consp expr) (cons (find-clone          (car expr) 
						 clone->orig)
			    (find-clone-for-list (cdr expr) 
						 clone->orig)))))

(defun ps-get-clone-ancestry(instance)
  (let ((immediate-ancestor (ps-unclone instance)))
    (if (not (equal instance immediate-ancestor))
	(cons immediate-ancestor
	      (ps-get-clone-ancestry immediate-ancestor)))))

(defun clone-kb-instancep(km-instance)
  (and (atom km-instance)
       (not (null (get-suffix (string km-instance) "_c")))
       (numberp (string-to-number (get-suffix (string km-instance) "_c")))))

(defun get-suffix(input-str pattern-str)
  (let* ((idx           (search pattern-str input-str)))
    (if (numberp idx)
	(subseq input-str (+ idx (length pattern-str)) (length input-str)))))

(defun get-prefix(input-str pattern-str)
  (let* ((idx           (search pattern-str input-str)))
    (if (numberp idx)
	(subseq input-str 0 idx))))
  
(defun extract-kb-instance-for-rootname(rootname-str vp-inst)
  (remove nil
	  (mapcar #'(lambda (km-instance)
		      (if (string-equal rootname-str (ps-unclone-naively km-instance))
			  km-instance))
		  (extract-all-instances-from-triple-list
		   (get-simplified-context-for-viewpoint vp-inst)))))

(defun ps-unclone-naively (km-instance)
  (if (and (atom km-instance)
	   (not (numberp km-instance)))
      (let* ((km-inst-str        (string km-instance)))
	(if (clone-kb-instancep km-instance)    
	    (subseq km-inst-str 0 (search "_c" km-inst-str))
	  km-inst-str))))

(defun ps-unclone-triple(triple)
  (if (triplep triple)
      (let ((head (triple-head     triple))
	    (reln (triple-relation triple))
	    (tail (triple-tail     triple)))
	(list (ps-unclone head)
	      reln
	      (ps-unclone tail)))))

(defun find-cloned-triple (orig-triple triple-lst)
  (car (remove nil 
	  (mapcar #'(lambda (triple)
		      (if (equal (ps-unclone-triple orig-triple) (ps-unclone-triple triple))
			  triple))
		  triple-lst))))

(defun find-cloned-triple-lst (orig-triple-lst triple-lst)
  (remove nil (mapcar #'(lambda (triple)
	      (find-cloned-triple triple triple-lst))
	  orig-triple-lst)))

(defun ps-unclone(input)
  (if (atom input)
      (let ((map-entry (assoc input *CONTROLLER-UNCLONE-MAP*)))
	(if (not (null map-entry))
	    (cdr map-entry)
	    input))
      (cons   (ps-unclone (car input))
	      (ps-unclone (cdr input)))))

(defun extract-rootname-for-clone-kb-instance-list (km-instance-list)
  (remove nil
	  (mapcar #'(lambda (km-instance)
		      (ps-unclone-naively km-instance))
		  km-instance-list)))

;;FIXME. Should we move it elsewhere?
;;FIXME. What does this function do??
(defun get-clone-km-instance-symbol(km-instance)
  (let* ((km-inst-str        (string km-instance))
	 (is-clone           (search "_c" km-inst-str))
	 (km-inst-root-str   (if is-clone
			         (subseq km-inst-str 0 is-clone)
			         km-inst-str))
	 (cloned-km-inst-str (format nil "~a~a~a" km-inst-root-str
				                  "_c"
						  (find-clone-counter km-inst-root-str))))
    (intern cloned-km-inst-str :km)))

(defun find-clone-counter(root-str)
  (let* ((result (assoc root-str *clone-counter-list* :test #'string-equal))
	 (exist-counter (cdr result))
	 (new-counter   (if (numberp exist-counter)
			    (1+ exist-counter) 
			    0)))
    (mutate-clone-counter-list (cons (cons root-str new-counter)
				     *clone-counter-list*))
    new-counter))

;;Generates the clone-pair mappings.
(defun generate-clone-pair-list(all-instances)
  (let ((orig->clone (mapcar #'(lambda(km-instance)
				 (cons km-instance
				       (get-clone-km-instance-symbol km-instance)))
			     all-instances)))
    (values orig->clone (invert-map orig->clone))))

(defun rootnamep(rootname)
  (let* ((rootname-str (string rootname)))
    (not (null
	  (remove nil
	  (mapcar #'(lambda (vp-inst)
		      (member rootname-str
			      (extract-rootname-for-clone-kb-instance-list
			       (extract-all-instances-from-triple-list
				(get-simplified-context-for-viewpoint vp-inst)))
			      :test 'string-equal))
		  *closedlist*))))))

(defun viewpoint-contains-rootname(rootname vp-inst)
  (let* ((rootname-str (string rootname)))
    (if (member rootname-str
		(extract-rootname-for-clone-kb-instance-list
		 (extract-all-instances-from-triple-list
		  (get-simplified-context-for-viewpoint vp-inst)))
		:test 'string-equal)
	(extract-kb-instance-for-rootname rootname-str vp-inst))))

;;Routine to unclone a triple-list given clone-map
(defun unclone-triple-list-with-clone-map (triple-list clone-map)
  (let ((reverse-clone-map (invert-map clone-map)))
    (replace-elements-in-list triple-list reverse-clone-map)))

;;Routine to unclone a triple-list given clone-map
(defun unclone-triple-list(triple-list 
			   &optional
			   (compute-question nil)
			   (yn-query nil)
			   )
  (let ((map
	 (mapcar #'(lambda(x)
		     (cons (ps-unclone x) x))
		 (extract-all-instances-from-triple-list triple-list))))
    (values
     (unclone-triple-list-with-clone-map triple-list map)
     (replace-elements-in-list compute-question map)
     (replace-elements-in-list yn-query map)
)
))

(defun unclone-triple-naively(triple)
  (list (intern (ps-unclone-naively (triple-head triple)) :km)
	(triple-relation triple)
	(intern (ps-unclone-naively (triple-tail triple)) :km)))

;;Routine to unclone a triple-list given clone-map
(defun unclone-triple-list-naively(triple-list
				   &optional
				   (compute-question nil)
				   (yn-query nil)
				   )
  (let ((map
	 (mapcar #'(lambda(x)
		     (cons (intern (ps-unclone-naively x) :km) x))
		 (extract-all-instances-from-triple-list triple-list))))
    (values 
     (unclone-triple-list-with-clone-map triple-list map)
     (replace-elements-in-list compute-question map)
     (replace-elements-in-list yn-query map)    
     map
     )))

(defun uncloned-triple-diff(triple-lst1 triple-lst2)
  (set-difference triple-lst1 triple-lst2 :test 'ps-triple-equal-uncloned))

(defun ps-triple-equal-uncloned(input-triple1 input-triple2)
  (let ((triple1 (unclone-triple-naively input-triple1))
	(triple2 (unclone-triple-naively input-triple2)))
    (ps-triple-equal triple1 triple2)))

