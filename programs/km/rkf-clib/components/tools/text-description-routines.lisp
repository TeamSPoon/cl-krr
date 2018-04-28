;;
;; $Id: text-description-routines.lisp,v 1.15 2008/05/07 16:37:49 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defmacro with-restoring-situations-reasoning (() &body body)
 (let ((g-prev-situation (gensym "prev-situation"))
       (g-prev-situation-mode (gensym "prev-situation-mode")))
   `(let ((,g-prev-situation (curr-situation))
          (,g-prev-situation-mode km::*am-in-situations-mode*))
      (unwind-protect (progn ,@body)
        (in-situation ,g-prev-situation)
        (setq km::*am-in-situations-mode* ,g-prev-situation-mode)))))

(defparameter *concept-text-description-hash* (make-hash-table :test #'equal))
(clrhash *concept-text-description-hash*)

;;-----------------------------------------------
;;Following routines are text-description related
;;-----------------------------------------------

;;Returns the text-description string for a CLIB concept
(defun get-skolem-text-description-for-concept(concept)
  (let ((result  (gethash concept *concept-text-description-hash*)))
    (if (null result)
	(progn 
	  (setf (gethash concept *concept-text-description-hash*) (first (km0 `(|the| |text-description| |of| (|a| ,concept)))))
	  (gethash concept *concept-text-description-hash*))
      result)))

;;Returns the skolem(generic) text-description string for a CLIB instance
(defun get-skolem-text-description-for-kb-instance(inst)
  (let* ((superclasses (km0 `(|the| |instance-of| |of| ,inst))))
    (if (> (length superclasses) 1)
	(format t "Warning: ~a has more than 1 superclass, i.e. ~a. Picking the 1st one, i.e. ~a.~%" inst superclasses (first superclasses))) ;;FIXME. Should instantiate each of these and unify them together to get text-description of the unified instance.
    (get-skolem-text-description-for-concept (first superclasses))))

;;Returns the text-description string for a CLIB instance
(defun get-text-description-for-km-frame(frame &optional(verbose nil))
  (let ((*prototype-classification-enabled* nil)  
	(*classification-enabled*           nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|))
	;(*error-report-silent* t)
	)
  (if (km-instancep frame)
      (car (ps-km-query `(|the| |text-description| |of| ,frame) verbose))
      (format nil "~a" frame))))

(defun ps-get-text-description-for-frame-slot(frame slot)
  (ps-get-text-description-for-query-path `(|the|,slot |of| ,frame)))

;;Returns the text-description for a query-path.
;;If the query-path is (the instance-of of ?). Then it returns the concept name.
(defun ps-get-text-description-for-query-path(query-path &optional(verbose nil))
  (let ((verbose nil)
	(query-slot-name (nth 1 query-path))
	(*prototype-classification-enabled* nil)  
	(*classification-enabled*           nil)
	(*built-in-remove-subsumers-slots*  '(|instance-of|))
       )
    (cond ((equal query-slot-name '|instance-of|) 
	   (format nil "~a" (car (ps-km-query query-path verbose))))
	  (t 
	   (let ((km-expr `(|the| |text-description| |of| ,query-path)))
    (if verbose (format t "KM: ~a~%" km-expr))
    (multiple-value-bind 
	(km-result km-error km-error-structure)
	(km km-expr)
      (if (and verbose
	       (not (null km-error)))
	  (format t "KM error: ~a~%" km-error))
      (if (not (null km-error))
	  (throw 'BPS-REASONING-ERROR
		 (list 'BPS-REASONING-ERROR km-error)))
      (if (not (null km-result))
	  (concat-lst-into-string km-result))))))))

(defun concat-lst-into-string(input)
  (let ((s (make-string-output-stream)))
    (dolist (x input)
      (format s "~a" x))
    (get-output-stream-string s)))

;;---------------------------------------
;;Following routines are text-gen related
;;---------------------------------------

;;Returns the text-gen string for a CLIB concept
(defun get-skolem-text-gen-for-concept(concept)
  (first (km0 `(|the| |text-gen| |of| (|a| ,concept)))))

;;Returns the skolem(generic) text-gen string for a CLIB instance
(defun get-skolem-textgen-for-kb-instance(inst)
  (let* ((superclasses (km0 `(|the| |instance-of| |of| ,inst))))
    (if (> (length superclasses) 1)
	(format t "Warning: ~a has more than 1 superclass, i.e. ~a. Picking the 1st one, i.e. ~a.~%" inst superclasses (first superclasses)))
    (get-skolem-textgen-for-concept (first superclasses))))

;;Returns the text-gen string for a CLIB instance
(defun get-textgen-for-kb-instance(inst)
  (car (km0 `(|the| |text-gen| |of| ,inst))))

(defun ps-get-textgen-for-frame-slot(frame slot)
  (car (ps-km-query `(|the| |text-gen| |of| (|the|,slot |of| ,frame)))))

