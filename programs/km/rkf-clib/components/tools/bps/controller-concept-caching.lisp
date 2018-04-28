;;
;; $Id: controller-concept-caching.lisp,v 1.94 2008/10/22 20:55:44 jchaw Exp $
;;

(unless (find-package :km)   (make-package :km))
(unless (find-package :aura) (make-package :aura))
(in-package :km)
(setq *using-km-package* t)

;; public routines.
;; (ps-get-graph     <concept> &optional(relations-to-ignore nil))
;; (ps-get-new-graph <concept> &optional(relations-to-ignore nil))

;;Returns set of uncached user defined concepts
(defun get-uncached-user-defined-concepts()
  (get-uncached-concepts *all-user-defined-concepts*))

;;Get uncached concepts for given concept list
(defun get-uncached-concepts (&optional (candidate-concepts (ps-get-all-concepts)))
  (remove-if #'cached-p candidate-concepts))

(defun get-cached-concepts-non-fixpoint()
  (set-difference
   (set-difference *all-user-defined-concepts* 
		   (get-uncached-concepts))
   (get-cached-concepts-fixpoint)))

(defun get-cached-concepts-fixpoint()
  (remove nil (mapcar #'(lambda(x)
			  (if (cached-p x)
			      (let ((key-value (gethash x *concept-cache*)))
				(multiple-value-bind 
				    (root graph fixpoint? md5hash)
				    (parse-concept-cache-entry key-value)
				  (if fixpoint? x)))))
   (set-difference *all-user-defined-concepts* 
		   (get-uncached-concepts)))))

;;Predicate to test if a input, e.g. concept or concept-list, is cached.
(defun cached-p(input)
  (cond ((null input) t)
	((atom input) 
	 (let ((key-value (gethash input *concept-cache*)))
	   (multiple-value-bind 
	       (root graph fixpoint? md5hash)
	       (parse-concept-cache-entry key-value)
	     (and (not (null root))
		  (not (null graph))))))
	(t (and (cached-p (car input))
		(cached-p (cdr input))))))

;;Get cached concepts for given concept list
(defun get-cached-concepts (&optional (candidate-concepts (ps-get-all-concepts)))
  (set-difference candidate-concepts 
		  (get-uncached-concepts candidate-concepts)))

;;Returns list of all concepts
(defun ps-get-all-concepts()
  (set-difference (ps-km-query '(|the| |all-subclasses| |of| |Thing|))
		  *CONCEPTS-TO-IGNORE*))

;;Caches list of concepts
(defun update-gather-graph-hash(candidate-concepts)
  (let ((uncached-concepts (get-uncached-concepts candidate-concepts)))
    (if uncached-concepts
	(mapcar #'(lambda(concept)
		    (cache-concept concept)
		    (save-user-defined-concept-cache))
		(get-uncached-concepts)))))

;;Cache all uncached concepts in candidate concpet list
(defun cache-all-uncached-concepts(candidate-concept-list)
  (let ((uncached-concept-list (get-uncached-concepts candidate-concept-list)))
    (if (not (null uncached-concept-list))
	(progn
	  (format t "BPS: Begin propagating cache for ~a uncached concept(s)~%" (length uncached-concept-list))
	  (update-gather-graph-hash uncached-concept-list)
	  (format t "BPS: Finish propagating cache for ~a uncached concept(s)~%" (length uncached-concept-list))))))

;;Invalidates entry for concept
(defun invalidate-cache-entry(candidate-concept &optional(verbose nil))
  (if *CONTROLLER-PERSISTENT-HASH-MUTABLE*
      (progn
	(format t "BPS: Invalidating cache for ~a.~%" candidate-concept)
	(setf (gethash candidate-concept 
		       *concept-cache*)
	      nil))))

(defun parse-concept-cache-entry(key-value)
  (let* ((root                    (first  key-value))
	 (value                   (second key-value)))
    (cond ((and (atom value)
		(not (null value)))
	   (values root graph))
	  ((and (listp value)
		(= (length value) 2))
	   (let ((graph     (first  value))
		 (fixpoint? (second value)))
	     (values root graph fixpoint?)))
	  ((and (listp value)
		(= (length value) 3))
	   (let ((graph     (first  value))
		 (fixpoint? (second value))
		 (md5hash   (third  value)))
	     (values root graph fixpoint? md5hash))))))

;;Gets graph for concept.
;;Creates cache entry if concept is not yet cached.
(defun procure-graph(candidate-concept)
  (progn 
    (if (not (cached-p candidate-concept))
	(cache-concept candidate-concept))
    (let* ((key-value (gethash candidate-concept *concept-cache*)))
      (multiple-value-bind 
	  (root graph fixpoint? md5hash)
	  (parse-concept-cache-entry key-value)
	(values root
		(ps-remove-constraint-triples
		 (remove-reflexive-triples-from-triple-list
		  graph))
		fixpoint?)))))

;;Returns the cached graph
(defun ps-get-graph (candidate-concept
		     &optional(relations-to-ignore nil))
  (multiple-value-bind 
      (root hashed-graph fixpoint? md5hash)
      (procure-graph candidate-concept)
    (values root
	     (ps-prune-graph relations-to-ignore
			     hashed-graph)
	    fixpoint?)))

;;Returns a cloned copy of cached graph
(defun ps-get-new-graph(candidate-concept
			&optional(relations-to-ignore nil))
  (multiple-value-bind
    (root hashed-graph fixpoint?)
    (ps-get-graph candidate-concept
		  relations-to-ignore)
    (multiple-value-bind
	(cloned-triple-list orig->clone clone->orig)
	(ps-clone-triple-list hashed-graph)
      (values (cdr (assoc root orig->clone))
	      (remove-duplicates cloned-triple-list
				 :test 'ps-triple-equal)
	      fixpoint?))))

(defun ps-prune-graph (relations-to-ignore
		       graph)
  (quasi-properly-terminate-triple-list 
   (set-difference 
    graph
    (SIEVE-TRIPLE-LIST-HAVING-RELATION-LIST relations-to-ignore
					    graph)
    :test 'ps-triple-equal)
   graph))

(defun quasi-properly-terminate-triple-list (triple-list original-triple-list)
   (append (extract-instance-triple-for-instance 
	    (extract-all-instances-from-triple-list triple-list)
	    original-triple-list)
	   (extract-non-instance-triples triple-list)))

(defun properly-terminate-triple-list(triple-list)
  (let ((all-instances (extract-all-instances-from-triple-list triple-list)))
	(append (extract-non-instance-triples triple-list)
		(generate-instance-triples all-instances))))

;;Creates a cache entry for concept
(defun cache-concept(candidate-concept 
		     &optional(verbose t)
		              (timeout 1200))
  (let ((*on-error* 'abort))
    (ps-non-global-km-situation)
    (sys:with-timeout
     (timeout
      (progn
        (format t "timed out[> ~a s]. BPS will now ignore ~a.~%" timeout candidate-concept)
        (setq *ALL-USER-DEFINED-CONCEPTS*
              (set-difference *ALL-USER-DEFINED-CONCEPTS* (list candidate-concept)))))
     (handler-case (cache-concept0 candidate-concept verbose)
		   (error (condition)
			  (values nil nil nil))))))

;;Creates a cache entry for concept
(defun cache-concept0(candidate-concept verbose)
  (let* ((start-time           (get-internal-real-time)))
    (if verbose (format t "BPS: Begin caching ~a~%" candidate-concept))
    (multiple-value-bind
	(root graph fixpoint? md5hash)
	(ps-gather-graph candidate-concept)
      (setf (gethash candidate-concept
		     *concept-cache*)
	    (list root (list graph fixpoint? md5hash)))
      (if verbose (format t "BPS: Finish caching ~a (~a ms). There are [~a/~a] uncached concepts ~%" 
	      candidate-concept
	      (- (get-internal-real-time)
		 start-time)
	      (length (get-uncached-concepts))
	      (length (ps-get-all-concepts))))
      (values root graph fixpoint?))))

;;Updates contents of cache entry.
(defun update-cached-concept (candidate-concept)
  (with-restoring-situations-reasoning ()
  (progn
    (format t "BPS: Updating ~a~%" candidate-concept)
    (invalidate-cache-entry candidate-concept)
    (cache-concept candidate-concept))))

(defun get-uncached-relations (cached-graph relations-to-extract)
  (set-difference relations-to-extract 
		  (include-subslots (extract-relations-in-graph-inv cached-graph))
		  :test 'equal))

(defun prefetch-concept-cache()
  (let ((*logging* t))
    (cache-all-uncached-concepts *all-user-defined-concepts*)))

;;Clears concept cache
(defun clr-cache()
  (progn
    (format t "BPS: Clearing concept cache~%")
    (clrhash *concept-cache*)))

;;Generates a cache containing only user defined concepts
;;DONE: 1) Walk concept-cache
;;      2) create temp hash containing only user-defined concepts
(defun generate-user-defined-concept-cache()
  (let ((*temp-cache*                       (make-hash-table :test #'equal)))
    (dolist (concept *all-user-defined-concepts*)
      (setf (gethash concept *temp-cache*)
	    (gethash concept *concept-cache*)))
    *temp-cache*))

;;Saves user defined-concept cache
;;DONE: 1) Walk concept-cache
;;       2) create temp hash containing only user-defined concepts
;;       3) Save temp hash into user-concept-cache.hash
(defun save-user-defined-concept-cache(&optional (verbose t))
  (let ((filename (get-location-of-user-defined-concept-cache verbose)))
    (cond (*CONTROLLER-PERSISTENT-HASH-MUTABLE*
	   (progn
	     (if verbose (format t "BPS: Saving concept hash as ~a~%" filename))
	     (write-hash (generate-user-defined-concept-cache) filename)
	     t))
	  (t 
	   (progn
	     (if verbose (format t "BPS: Concept hash is not mutable."))
	     nil)))))

(defun install-user-defined-concept-cache()
  (let (result)
    (format t "Caching user defined concepts(~a) " (length *all-user-defined-concepts*))
    (dolist (concept *all-user-defined-concepts*)
      (multiple-value-bind 
	  (concept-root graph fixpoint?)
	  (cache-concept concept nil)
	(if (not (null graph))
	    (progn
	      (format t ".")
	      (push concept result))
	    (format t "X"))))
    (format t "~%")
    (setq *all-user-defined-concepts* result)
))

;;Installs CLIB concept cache
(defun install-CLIB-concept-cache()
  (let* ((domain (if (boundp 'aura::*aura-domain*) 
		     (string-downcase (format nil "~a" aura::*aura-domain*))
		   "unknown"))
	 (target-sym (intern (string-upcase (format nil "*~a-clib-persistent-hash*" domain)) :km)))
    (if (boundp target-sym)
      (progn 
	(format t "BPS: Installing CLib concept cache for ~a domain~%" domain)
	(install-assoc-lst-into-concept-cache (eval target-sym)))
      (format t "BPS: CLIB concept cache not found for ~a domain!~%" domain))))

(defun install-assoc-lst-into-concept-cache(alist)
  (dolist (key+val alist)
    (setf (gethash (first key+val) *concept-cache*) (second key+val))))

;;Loads user defined-concept cache
;; 1) Load user-concept-cache.hash
;; 2) Install defns into concept-hash, check for updates.
(defun load-user-defined-concept-cache(&optional (verbose t))
  (let ((filename (get-location-of-user-defined-concept-cache verbose)))
    (dolist (sme-concept *all-user-defined-concepts*)
	(invalidate-cache-entry sme-concept verbose))
    (if (probe-file (pathname filename))
	(let ((*temp-hash* (make-hash-table :test #'equal)))
	  (if verbose (format t "BPS: Loading concept hash from ~a~%" filename))
	  (load-hash *temp-hash* filename)
	  (dolist (key+val *persistent-hash*)
	    (progn
	      (if verbose (format t "BPS: Installing user defined concept ~a.~%" (first key+val)))
	      (setf (gethash (first key+val) *concept-cache*) 
		    (second key+val)))))
      (if verbose (format t "BPS: No persistent cache found in ~a.~%" filename)))))

;;Returns location of user-defined concept cache file.
(defun get-location-of-user-defined-concept-cache(&optional (verbose nil))
 (let* ((dir (if (fboundp 'aura::get-data-directory-path)
                 (aura::get-data-directory-path)
                 (pathname *controller-kb-location*)))
        (result (make-pathname :name "persistent-cache" :type "hash" :defaults dir)))
   (if verbose (format t "BPS: User defined concept cache located at ~a~%" result))
   result))

(defun get-location-of-user-defined-kb(&optional (verbose nil))
  (if (fboundp 'aura::get-data-directory-path)
      (format nil "~akb/" (aura::get-data-directory-path))
      (format nil "~akb/" *CONTROLLER-KB-LOCATION*)))

(defun compute-md5-for-concept-file(candidate-concept)
  (let ((filename (format nil "~a~a.km" (get-location-of-user-defined-kb) candidate-concept)))
    (if (probe-file filename)
	(cl-user::md5-file filename :return :hex))))

;;Generates instance-of triples for concepts
(defun generate-instance-triples(input)
  (cond ((null input) ())
	((atom input)
	 (let ((classes (get-concept-for-kb-instance input)))
	   (mapcar #'(lambda (class)
		       (list input '|instance-of| class))
		   classes)))
	(t (append (generate-instance-triples (car input))
		   (generate-instance-triples (cdr input))))))

(defun check-concept-cache()
  (maphash #'(lambda(concept root+graph)
	       (multiple-value-bind
		   (root graph)
		   (parse-concept-cache-entry root+graph)
		 (if (not (properly-terminated? graph))
		     (invalidate-cache-entry concept))))
	     *concept-cache*))

