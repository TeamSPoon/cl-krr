;;
;; $Id: controller-km-bridge.lisp,v 1.64 2009/06/28 16:36:43 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;checks if KM classification is enabled. This is a little different to 
;;KM's (classification-enabled) routine as it also reports if prototype 
;;classification is turned on.
(defun ps-km-classification-enabled-p()
  (or *classification-enabled* 
      *prototype-classification-enabled*))

;;Crude predicate to test if something is a KM instance.
;;Returns true if the 1st character is an underscore, e.g. _Viewpoint.
(defun km-instancep(input)
  (and (not (stringp input))
       (not (numberp input))
       (atom input)
       (equal (char (string input) 0) '#\_)))

;;Crude predicate to test if something is a KM constant.
;;Returns true if the 1st character is an asterisk, e.g. _Viewpoint.
(defun km-constantp(input)
  (and (not (stringp input))
       (not (numberp input))
       (atom input)
       (equal (char (string input) 0) '#\*)))

(defun km-uom-constantp(input)
  (and (km-constantp input)
       (not (member input (all-instances '|Unit-of-Measurement|)))))

;;Issues a KM slot lookup
(defun ps-slot-lookup(query-frame query-slot &optional(debug nil))
  (ps-km-query `(|the| ,QUERY-SLOT |of| ,QUERY-FRAME) debug))

(defun get-bps-km-situation()
  (let ((other-situations (all-instances '|Situation|)))
  (cond ((and (not (null *CONTROLLER-KM-SITUATION*))
	      (member *CONTROLLER-KM-SITUATION* other-situations))
	      *CONTROLLER-KM-SITUATION*)
	(t (progn
	     (cond ((null other-situations) 
		    (setq *CONTROLLER-KM-SITUATION* (car (new-situation))))
		 (t (setq *CONTROLLER-KM-SITUATION* (car other-situations))))
	     *CONTROLLER-KM-SITUATION*)))))

;;Issues KM query
(defun ps-km-query(km-query &optional (verbose nil)
                                      (timeout 900))
  (let* ((start (if verbose (get-internal-run-time)))
	 (*am-in-situations-mode* t)
	 (*curr-situation*  (get-bps-km-situation)))
    (if verbose (format t "BPS: KM[~a]> ~s => "
                        (ps-km-classification-enabled-p)
                        km-query))
    (multiple-value-bind
      (result error-str error-struc warn-str)
      (sys:with-timeout 
       (timeout
        (progn
          (error (format nil "(ps-km-query ~a) timeout'ed! Fatal error!~%" km-query))))
       (km km-query))
      (cond ((not (null error-str))
             (progn
               (format t "Reasoning error!~%")
               (throw 'BPS-REASONING-ERROR
                      (list 'BPS-REASONING-ERROR
                            error-str))
            ))
	    (t
	     (if verbose (format t "~s, [~a ms]~%" result (- (get-internal-run-time) start)))
	     result)))))

;; same as ps-km-query, but no throw
;; but uses with-standard-km-params
(defun sri-km-query (km-query &optional (verbose nil) (timeout 900)) 
  (with-standard-bps-parameters ()
     (if verbose (format t "~% curr-situation = ~a" (curr-situation)))
     (let* ((start (if verbose (get-internal-run-time))))
       (if verbose (format t "BPS: KM[~a]> ~s => "
			   (ps-km-classification-enabled-p)
			   km-query))
       (multiple-value-bind
	   (result error-str error-struc warn-str)
	   (sys:with-timeout 
	    (timeout
	     (progn
	       (error (format nil "(ps-km-query ~a) timeout'ed! Fatal error!~%" km-query))))
	    (km km-query))
	 
	 (cond ((not (null error-str))
		(progn
		  (format t "Reasoning error!~%")
		  ))
	       (t
		(if verbose (format t "~s, [~a ms]~%" result (- (get-internal-run-time) start)))
		result))))))
  
;;Includes guard for checking null arguments to KM's (isa) routine.
(defun ps-isa(x y)
  (cond ((and (not (null x))
	      (not (null y)))
	 (isa x y))))

;;Issues KM query without modifying KM's memory.
(defun ps-peek-km-query(km-query &optional (verbose nil)
			              (timeout 900))
  (let* ((*logging* t))
    (bps-set-checkpoint 'ps-peek-km-query)
    (let ((result (ps-km-query km-query verbose timeout)))
      (bps-undo 'ps-peek-km-query)
      result)))

;;Gets the concept type given a KB instance.
(defun get-concept-for-kb-instance (kb-instance)
  (cond ((consp kb-instance)
	 (mapcar #'(lambda (in)
		     (car (get-concept-for-kb-instance in)))
		 kb-instance))
	(t (remove-subsumers (ps-km-query `(|the| |instance-of| |of| ,kb-instance))))))

;;Makes sure we are in non-global situation.
(defun ps-non-global-km-situation()
  (if (equal (curr-situation) '|*Global|)
      (if (and (not (null *CONTROLLER-KM-SITUATION*))
	       (not (equal *CONTROLLER-KM-SITUATION* '|*Global|))
	       (member *CONTROLLER-KM-SITUATION* (all-situations)))
	  (in-situation *CONTROLLER-KM-SITUATION*)
	(setq *CONTROLLER-KM-SITUATION* (car (new-situation))))))
  
;;Instantiates a concept
(defun ps-instantiate-concept (km-class-name &optional (debug nil))
  (let ((debug nil)
	(*classification-enabled*           nil)
	(*prototype-classification-enabled* nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|)))
    (ps-non-global-km-situation)
    (if (and (not (null km-class-name))
	     (listp km-class-name))
	(let* ((km-class-name 
		(remove-subsumers 
		 (remove-if-not #'(lambda(x) (and (kb-objectp x) (known-frame x))) 
				km-class-name)))
	       (result (ps-instantiate-concept (car km-class-name) debug)))
	  (if (> (length km-class-name) 1)
	      (dolist (x (cdr km-class-name))
		(ps-km-query `(,result & ,(ps-instantiate-concept x)) debug)))
	  result)
      (first (ps-km-query `(|a| ,km-class-name) debug)))))

;;creates instance-of triples given concept-lst
;;The resulting instances are not asserted in KM's working memory
(defun ps-instantiate-concept-as-triple-list(concept-lst)
  (if (null concept-lst)
      (ps-instantiate-concept-as-triple-list '(|Thing|))
    (let* ((rootname  (intern (format nil "_~A" (car concept-lst)) :km))
	   (clonename (get-clone-km-instance-symbol rootname)))
      (values clonename (mapcar #'(lambda(x)`(,clonename |instance-of| ,x)) (remove-subsumers concept-lst))))))

;;removes subsuming instance-of filler entries.
(defun simplify-instance-triple(triple)
  (let ((head (triple-head triple))
	(relation (triple-relation triple))
	(tail (triple-tail triple)))
    (cond ((not (equal relation '|instance-of|)) triple)
	  ((atom tail) triple)
	  (t (list head relation (remove-subsumers tail))))))

;;Given triple, generalize it into a triple with class names.
(defun generalize-triple(triple)
  (let ((head (triple-head triple))
	(relation (triple-relation triple))
	(tail (triple-tail triple)))
    (list (get-concept-for-kb-instance head)
	  relation
	  (get-concept-for-kb-instance tail))))

(defun bps-set-checkpoint(id)
  (let ((result (set-checkpoint id)))
    (if (not result) (format t "BPS ERROR: Cannot set KM checkpoint ~a.~%" id))
    result))

(defun bps-undo(id)
  (let ((result (undo id)))
    (if (not result) (format t "BPS ERROR: Cannot undo KM checkpoint ~a.~%" id))
    result))

(defun ps-improved-isa (instance concept)
  (let* ((debug nil)
	 (*prototype-classification-enabled* t)  ;temporarily turn on KM prototype classification
	 (*classification-enabled*           t)
	 (*built-in-remove-subsumers-slots*  '(|instance-of|)))
    (not (null 
	  (or
	   (ps-km-query `(,instance |isa| ,concept) debug)
	   (progn
	     (classify instance)
	     (ps-km-query `(,instance |isa| ,concept) debug)))))))

(defun clib-uom-p(uom)
  (and (atom uom)
       (not (null (isa uom '|Unit-of-Measurement|)))))

(defun abs-tax-dist-to-thing(concept)
  (abs-tax-dist concept '|Thing|))

(defun abs-tax-dist (target concept)
  (let ((result (tax-dist target concept)))
    (if (null result)
	(tax-dist concept target)
        result)))

(defun abs-concept-dist(src dest)
  (if (abs-tax-dist src dest)
      (abs-tax-dist src dest)
    (let ((common-concept (car 
			   (remove-subsumers
			    (intersection (all-superclasses src)
					  (all-superclasses dest))))))
      (if (not (null common-concept))
	  (+ (abs-tax-dist common-concept src)
	     (abs-tax-dist common-concept dest))))))

(defun sort-concept-list(input &optional(operator '>))
  (sort (copy-list input) 
	#'(lambda(x y)
	    (eval (list operator 
			(abs-tax-dist '|Thing| x) 
			(abs-tax-dist '|Thing| y))))))

(defun applicable-slots-for(instance)
  (let ((debug nil)
	(start(get-universal-time))
	(concept-lst
	 (remove-duplicates
	  (mappend #'(lambda(concept)
		       (cons concept 
			     (all-superclasses concept)))
		   (ps-slot-lookup instance '|instance-of|)))))
    (let ((result (remove-if-not #'(lambda(x) 
				     (intersection 
				      concept-lst
				      (domains-of x)))
				 (ps-gather-graph-target-relations))))
      (if debug
      (format t "~A applicable-slots-for ~A [~A ms]~%" (length result) instance
	      (- (get-universal-time) start)))
      result)))

;; sets km-parameters to UT's settings for reasoning
;; NOTE: use only inside with-standard-aura-parameters
(defun ps-set-km-params-for-UT ()
  (SETQ *BUILT-IN-REMOVE-SUBSUMERS-SLOTS* '(|instance-of| |classes| |superclasses| |member-type|))
  (SETQ *BUILT-IN-REMOVE-SUBSUMEES-SLOTS* '(|subclasses| |prototype-of| |domain| |range|))
  (SETQ *RECURSIVE-CLASSIFICATION* T)
  (SETQ *CLASSIFICATION-ENABLED*   T)
  (SETQ *PROTOTYPE-CLASSIFICATION-ENABLED* T)
  (SETQ *AM-IN-SITUATIONS-MODE* T))

(defun concept-p(input)
  (not (null (member input (cons '|Thing| (all-subclasses '|Thing|))))))

(defun ps-frame-now-has(frame slot-axiom)
  (let ((debug nil))
    (if (and (not (null frame))
	     (not (null slot-axiom)))
	(ps-km-query `(,frame |now-has| ,slot-axiom) debug))))

(defun sort-taxonomy()
  (sort (copy-list (all-subclasses '|Thing|))
	#'(lambda(x y) (> (abs-tax-dist x '|Thing|)
			  (abs-tax-dist y '|Thing|)))))

(defun taxonomy-depth()
  (abs-tax-dist (car (sort-taxonomy)) '|Thing|))

(defun generalize-concept-to-tax-dist(concept depth &optional(concepts-to-ignore nil))
  (let ((concept-hierarchy (cons concept (all-superclasses concept))))
    (remove-subsumers
     (append (intersection concept-hierarchy
			   concepts-to-ignore)
	     (remove-if #'(lambda(x) (> (abs-tax-dist x '|Thing|) depth))
			concept-hierarchy)))))

(defun get-filler-for-slot-frame(slot frame)
  (let* ((debug nil)
	 (filler-concept-lst (ranges-of slot))
	 (filler (car (ps-km-query `(|the| ,slot |of| ,frame) debug))))
    (if (and (not (null filler-concept-lst))
	     (null filler))
	(let ((result (ps-instantiate-concept filler-concept-lst)))
	  (ps-km-query `(,frame |now-has| (,slot (,result))) debug)
	  (values result (get-concept-for-kb-instance result)))
      (values filler (get-concept-for-kb-instance filler)))))

(defun ps-find-instances-of(triple-list concept)
  (remove-if-not #'(lambda(instance)
		     (let ((concept-lst (quasi-get-concept-for-kb-instance instance triple-list)))
		       (not (null (intersection concept-lst 
						(cons concept (all-subclasses concept)))))))
		     (extract-all-km-instances triple-list)))

(defun ps-find-slot-value (instance slot triple-list)
  (let* ((inverse (km0 `(|the| |inverse| |of| ,slot)))
	 (result))
    (dolist (triple triple-list)
      (if (and (eq (first triple) instance)
	       (eq (second triple) slot))
	  (push (third triple) result)
	(if (and (eq (third triple) instance)
		 (eq (second triple) inverse))
	    (push (first triple) result))))
    (remove-duplicates result)))

	 
(defun ps-showme(concept)
  (multiple-value-bind
      (root graph)
      (ps-get-graph concept)
    (format t "~a~%" 
	    (htmlify-km-assertion-list
	     (ps-get-all-frames-for-triples 
	      (normalize-triple-list graph))))))

;;deprecated (defun bps-restore-kb()
;;   (cond ((or (not (boundp '*CONTROLLER-BACKUP-KB*))
;; 	     (null *CONTROLLER-BACKUP-KB*))
;; 	 (format t "BPS: *CONTROLLER-BACKUP-KB* is null~%"))
;; 	(t (progn
;; 	     (put-kb *CONTROLLER-BACKUP-KB*)
;; 	     (reset-problem-solver)))))


