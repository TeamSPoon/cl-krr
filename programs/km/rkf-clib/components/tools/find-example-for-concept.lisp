;;
;; $Id: find-example-for-concept.lisp,v 1.7 2010/05/19 20:44:51 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Routines to help find an example for KB concept.

(defparameter *clib-example-hash* (make-hash-table :test #'equal))
(clrhash *clib-example-hash*)

;;Only cache the example concept-name rather than instance. Because the instances do not carry over to other KM situations.
(defun find-example-for-concept(concept)
  (let ((result (gethash concept *clib-example-hash*)))
    (if (null result)
	 (let ((computed-example (compute-example-for-concept concept)))
	   (setf    (gethash concept *clib-example-hash*) (km0 `(|the| |instance-of| |of| ,computed-example)))
	   computed-example)
      (let ((cached-example (first (km0 `(|a| |Thing|))))) ;;to handle examples having multiple superclasses, e.g. Base examples.
	(km0 `(,cached-example |has| (|instance-of| ,result)))))))

(defun compute-example-for-concept(concept)
  (let* ((skolem-text             (get-skolem-text-description-for-concept concept))
	 (available-subclass-inst (find-available-example-subclass-instance-for-concept concept)))
    (if (null available-subclass-inst)
        (find-available-example-instance-for-concept concept)
        available-subclass-inst)))

(defun find-available-example-instance-for-concept (concept)
  (let* ((skolem-text            (get-skolem-text-description-for-concept concept))
	 (instance-text-pair-set (mapcar #'(lambda(instance)
					     (cons instance (car (km0 `(|the| |text-description| |of| ,instance)))))
					 (km0 `(|the| |all-instances| |of| ,CONCEPT)))))
    (find-inst-with-different-text-description instance-text-pair-set skolem-text)))

(defun find-available-example-subclass-instance-for-concept (concept)
  (let* ((skolem-text     (get-skolem-text-description-for-concept concept))
	 (subclass-skolem-text-pair-set (mapcar #'(lambda (subclass-concept)
						    (cons subclass-concept (get-skolem-text-description-for-concept subclass-concept)))
						(km0 `(|the| |all-subclasses| |of| ,CONCEPT))))
	 (result  (find-inst-with-different-text-description subclass-skolem-text-pair-set skolem-text)))
    (if (not (null result))
	(km0 `(|an| |instance| |of| ,result)))))
    
(defun find-inst-with-different-text-description (subclass-skolem-text-pair-set skolem-text)
  (let* ((target (car subclass-skolem-text-pair-set))
	 (target-subclass  (car target))
	 (target-text-desc (cdr target)))
    (cond ((null target) nil)
	  ((and (not (null target-text-desc))
		(not (string= target-text-desc ""))
		(not (string= target-text-desc skolem-text)))
	   target-subclass)
	  (t (find-inst-with-different-text-description (cdr subclass-skolem-text-pair-set)
							 skolem-text)))))

