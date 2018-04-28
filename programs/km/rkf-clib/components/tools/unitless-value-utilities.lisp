;;
;; $Id: unitless-value-utilities.lisp,v 1.4 2010/05/19 20:44:51 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *order-overlapping-intervals?* nil
  "should overlapping intervals be ordered using heuristics?")

(defun min-value-unitless (unitless-values)
  (let ((result (first unitless-values)))
    (dolist (candidate (rest unitless-values))
      (setq result (unitless-lesser-of-two-wee-vals result candidate)))
    result))

(defun unitless-lesser-of-two-wee-vals (unitless-value-1 unitless-value-2 
					&optional (handle-overlaps? *order-overlapping-intervals?*))
  ;; defaults to unitless-value-1 unless unitless-value-2 is demonstrated to be less
  (cond ((km0 `((|the| |less-than| |of| ,unitless-value-2) |includes| ,unitless-value-1))
	 unitless-value-1)
	((km0 `((|the| |less-than| |of| ,unitless-value-1) |includes| ,unitless-value-2))
	 unitless-value-2)
	(t (let ((min-1 (unitless-min-value unitless-value-1))
		 (max-1 (unitless-max-value unitless-value-1))
		 (min-2 (unitless-min-value unitless-value-2))
		 (max-2 (unitless-max-value unitless-value-2)))
	     (cond ((< max-1 min-2)
		    unitless-value-1)
		   ((< max-2 min-1)
		    unitless-value-2)
		   ((not handle-overlaps?)
		    unitless-value-1)
		   ;; the following is "partial" in the sense that the two intervals overlap
		   ;; so that the prefered interval is not strictly less than the other but
		   ;; is in some way "more left" on the scale than the other interval
		   ((not (and min-1 max-1 min-2 max-2))
		    unitless-value-1)
		   ((and (<= min-1 min-2)
			 (<= max-1 max-2)
			 (or (< min-1 min-2)
			     (< max-1 max-2)))
		    unitless-value-1)
		   ((and (<= min-2 min-1)
			 (<= max-2 max-1)
			 (or (< min-2 min-1)
			     (< max-2 max-1)))
		    unitless-value-2)
		   ((< (+ min-1 max-1) (+ min-2 max-2)) ;; mid-point comparisons
		    unitless-value-1)
		   ((< (+ min-2 max-2) (+ min-1 max-1))
		    unitless-value-1)
		   (t unitless-value-1))))))

(defun unitless-numeric-values (value-instance)
  "the cardinal values of VALUE-INSTANCE are unitless"
  (mapcar 'second 
	  (remove-if-not #'(lambda (value) 
			     (and (consp value) (numberp (second value))))
			 (km0 `(|the| |value| |of| ,value-instance)))))

(defun unitless-min-value (value-instance)
  "the cardinal values of VALUE-INSTANCE are unitless -- return the min value number"
  (let ((numeric-values (unitless-numeric-values value-instance)))
    (if numeric-values
	(apply 'min numeric-values)
      (let ((numeric-min-values (mapcar 'second 
					(remove-if-not #'(lambda (value) 
							   (and (consp value) (numberp (second value))))
						       (km0 `(|the| |min-value| |of| ,value-instance))))))
	(when numeric-min-values (apply 'min numeric-min-values))))))

(defun unitless-max-value (value-instance)
  "the cardinal values of VALUE-INSTANCE are unitless -- return the max value number"
  (let ((numeric-values (unitless-numeric-values value-instance)))
    (if numeric-values
	(apply 'max numeric-values)
      (let ((numeric-max-values (mapcar 'second 
					(remove-if-not #'(lambda (value) 
							   (and (consp value) (numberp (second value))))
						       (km0 `(|the| |max-value| |of| ,value-instance))))))
	(when numeric-max-values (apply 'max numeric-max-values))))))

(defun unitless-values-consistent-p (unitless-value-1 unitless-value-2)
  (let ((min-1 (unitless-min-value unitless-value-1))
	(max-1 (unitless-max-value unitless-value-1))
	(min-2 (unitless-min-value unitless-value-2))
	(max-2 (unitless-max-value unitless-value-2)))
    (and min-1 max-1 min-2 max-2
	(<= min-1 max-2)
	(<= min-2 max-1))))
