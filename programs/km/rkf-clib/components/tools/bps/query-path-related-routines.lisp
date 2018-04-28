;;
;; $Id: query-path-related-routines.lisp,v 1.10 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-complete-query-paths-for-instance(instance &optional(ignored-instances nil))
  (if (not (member instance ignored-instances))
      (let ((query-path (get-immediate-query-paths instance)))
	(cond ((null query-path) nil)
	      (t (remove-duplicates (append (get-complete-query-paths-for-instance-list
					     (extract-all-km-instances query-path)
					     (cons instance ignored-instances))
					    query-path)
				    :test 'equal))))))

(defun get-complete-query-paths-for-instance-list(instance-list &optional(ignored-instances nil))
  (cond ((null instance-list) ())
	(t (let ((target-instance (car instance-list)))
	     (cond ((member target-instance ignored-instances)
		    (get-complete-query-paths-for-instance-list (cdr instance-list) 
							       ignored-instances))
		   (t (append (get-complete-query-paths-for-instance target-instance 
								     ignored-instances)
			      (get-complete-query-paths-for-instance-list (cdr  instance-list)
									  (cons target-instance
										ignored-instances)))))))))

;;Returns the query-paths associated with instance. Found via applicable domain relations.
(defun get-immediate-query-paths-for-instance(instance)
  (let ((target-slots (get-applicable-inverse-slots-for-instance instance)))
    (mappend #'(lambda(slot)
		 (let ((frame-list (non-mutable-km-slotvals instance slot)))
		   (if frame-list
		       (mappend #'(lambda(frame)
				   (list `(|the| ,(invert-slot slot) |of| ,frame)))
			       frame-list))))
	     target-slots)))

(defun non-mutable-km-slotvals (instance slot)
  (progn
    (bps-set-checkpoint 'NON-MUTABLE-KM-SLOTVALS)   
    (let ((result (km-slotvals instance slot)))
      (bps-undo 'NON-MUTABLE-KM-SLOTVALS)           
      result)))

(defun get-applicable-inverse-slots-for-instance(instance)
  (mappend #'(lambda(instance)
	       (km-slotvals instance '|domain-of|))
	   (km-slotvals instance '|instance-of|)))

(defun get-valid-inverse-slots-for-instance(instance)
  (remove nil
	  (mapcar #'(lambda(slot)
		      (if (not (null (km-slotvals instance slot)))
			  slot))
		  (get-applicable-inverse-slots-for-instance instance))))

(defun get-applicable-slots-for-instance(instance)
  (mappend #'(lambda(instance)
	       (km-slotvals instance '|domain-of|))
	   (km-slotvals instance '|instance-of|)))

;;Returns the query-paths associated with instance list. Found via applicable domain relations.
(defun get-immediate-query-paths-for-instance-list (instance-list)
  (mappend #'(lambda(instance)
	       (get-immediate-query-paths-for-instance instance))
	   instance-list))

;;Returns the query-paths associated with instance list. Found via applicable domain relations.
(defun get-immediate-query-paths(input)
  (cond ((null input) ())
	((atom input) (get-immediate-query-paths-for-instance input))
	(t            (get-immediate-query-paths-for-instance-list input))))

;;Eagerly evaluate slots associated with km-instance
(defun touch-query-paths(query-paths &optional(verbose nil))
  (let ((*EQ-FLAG* nil))
    (mappend #'(lambda(query-path)
		(ps-km-query query-path verbose))
	     query-paths)))

;;Eagerly evaluate slots associated with km-instance
(defun touch-km-instance (input &optional (verbose nil))
  (let ((*EQ-FLAG* nil))
    (if (null (member input *EQ-SOLVER-TOUCHED-INSTANCES*))
      (let ((query-paths (get-complete-query-paths-for-instance input)))
	(touch-query-paths query-paths verbose)
	(setq *EQ-SOLVER-TOUCHED-INSTANCES* (remove-duplicates (cons input *EQ-SOLVER-TOUCHED-INSTANCES*))))))
)

;;Eagerly evaluate slots associated with km-instance
(defun touch-km-instance-list (instance-list &optional (verbose nil))
  (let ((*EQ-FLAG* nil))
  (dolist (instance instance-list)
    (touch-km-instance instance verbose))))

