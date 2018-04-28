;;
;; $Id: conversion.lisp,v 1.15 2010/05/19 20:44:51 kbarker Exp $
;;
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;
;; Top-level function called by the Unit-Conversion concept in 
;; the CLIB to convert the values of a list of value term to the 
;; target UoM.
;;
(defun convert-card-vals (vals target)
  (let ((target-UoM (first target))
    card-vals result temp instance-value final-result)
    ;; Extract all the cardinal values and verify consistency.
    (dolist (val vals) 
      (let ((cvals (extract-card-vals (km0 `(|the| |value| |of| ,val)))))
	(cond 
	  ((null cvals) 
	   (setf card-vals nil)
	   (return))
	  ((consistent-card-vals cvals)
	   (setf card-vals (cons (first cvals) card-vals)))
	  (t
	   (setf card-vals nil)
           (return)))))
    ;; Converting the UoM to the target. Check to make sure all the 
    ;; values are convertible.
    (dolist (val card-vals)
      (let ((UoM (third val)))
	(setf temp (my-glconvertunit UoM target-UoM))
	(setf isTemperatureUnit (and (basictempunit UoM) (basictempunit target-UoM)))
	(cond 
	 (isTemperatureUnit
	  (setf result (cons (list :|pair| 
				   (tempconvert (list '* (second val) UoM)
						target-UoM)
				   (first target)) result)))
	 (temp
	  (setf result (cons (list :|pair| (* (second val) temp) (first target)) result)))
	 (t 
	  (setf result nil)
	  (return)))))
    ;; Return the result if there is no violation. Otherwise,
    ;; return nil.
    (setf instance-value (first (km0 `(|the| |instance-of| |of| ,(first vals)))))  
    (dolist (r result final-result)
      (let ((new-instance 
	     (first (km0 `(|a| ,instance-value |with|
			       (|value| (,r)))))))
	(setf final-result (cons new-instance final-result))))))

;;
;; Given a list of values, return all the cardinal elements from 
;; the list.
;;
(defun extract-card-vals (vals)
  (let (result)
    (dolist (x vals result)
      (if (and (consp x) (numberp (second x)))
	  (setf result (cons x result))))))

;;
;; Given a list of cardinal values, this function checks to
;; see if all the values are consistent.
;;
(defun consistent-card-vals (cvals)
  (let ((consistent t) 
	(key-val  (second (first cvals)))
	(key-UoM (intern (string-upcase (string (third (first cvals)))) :km)))
    (dolist (cval (rest cvals))
      (let ((con-UoM (third cval))
	    con-val)
	(setf con-val (my-glconvertunit con-UoM key-UoM))
	(if (not (and con-val (km0 `((,con-val * ,(second cval)) = ,key-val))))
	    (progn
	      (setf consistent nil)
	      (return)))))
     consistent))

(defun my-glconvertunit (UoM1 UoM2)
  (if (equal UoM1 UoM2) 1 (clib-convertunit UoM1 UoM2)))
	

;; additional functions used to compute the max/min of a list of values
(defun find-max-value (values)
  (let ((max1 (find-max-value0 values)))
    (if (> (length max1) 1)
	(first (find-max-value0 max1))
      (first max1))
    ))

(defun find-max-value0 (values)
  (let* ((sorted-values
	  (sort values #'property-gt))
	 (max-val (first sorted-values)))
    (remove-if #'(lambda (x) (property-lt x max-val))
	       sorted-values)
    ))

(defun find-min-value (values)
  (let ((min1 (find-min-value0 values)))
    (if (> (length min1) 1)
	(first (find-min-value0 min1))
      (first min1))
    ))

(defun find-min-value0 (values)
  (let* ((sorted-values
	  (sort values #'property-lt))
	 (min-val (first sorted-values)))
    (remove-if #'(lambda (x) (property-gt x min-val))
	       sorted-values)
    ))

;; INPUTS: a list of values of the form (:pair *cool Drink)
;; OUPUTS: a scalar value that is the max 
;; ASSUMPTIONS: there is exactly 1 common scale among the inputs
(defun find-scalar-max (values)
  (let* (
	 (common-scale (find-common-scale values))
	 (scalar-values 
	  (extract-scalar-values values common-scale))
	 (sorted-values
	  (sort scalar-values
		#'scalar-greater-than))
	)
    `(:|pair| ,(first sorted-values) ,common-scale)
     ))

;; INPUTS: a list of values of the form (:pair *cool Drink)
;; OUPUTS: a scalar value that is the min 
;; ASSUMPTIONS: there is exactly 1 common scale among the inputs
(defun find-scalar-min (values)
  (let* (
	 (common-scale (find-common-scale values))
	 (scalar-values 
	  (extract-scalar-values values common-scale))
	 (sorted-values
	  (sort scalar-values
		#'scalar-less-than))
	)
    `(:|pair| ,(first sorted-values) ,common-scale)
     ))


;; find the common scale shared by all the values
(defun find-common-scale (values)
  (let (;; get all the scales used
	(scales (km0 `(|the2| |of| ,(cons ':|set| values))))
	(common-scale nil)
	(scale-frequency 0)
	(max-scale-frequency 0)
	)
    ;;(format t "scales = ~A~%" scales)
    (dolist (scale scales)
	    ;; find the most frequently occurring scale, which should be the common scale
	    (setf scale-frequency
		  (km-unique0 
		   `(|the| |number| |of|
		     (|allof| ,(cons ':|set| values)
		      |where|
		      ((|the2| |of| |It|) = ,scale)))))
	    (if (> scale-frequency 
		   max-scale-frequency)
		(progn
		  (setf max-scale-frequency scale-frequency)
		  (setf common-scale scale))))
    common-scale
    ))

;; given a list of value pairs, and 1 scale
;; return the values on that scale
(defun extract-scalar-values (values common-scale)
  (km0 `(|forall| ,(cons ':|set| values)
	 |where| ((|the2| |of| |It|) = ,common-scale)
	 (|the1| |of| |It|)))
  )

;; compare 2 scalar values
;; return t if v1 > v2
(defun scalar-greater-than (v1 v2)
  (do ((candidate v1 (km-unique0 `(|the| |greater-than| |of| ,candidate))))
      ((or (not candidate)
	   (equal candidate v2))
       (if (not candidate)
	   nil
	 t))
      )
  )

;; compare 2 scalar values
;; return t if v1 < v2
(defun scalar-less-than (v1 v2)
  (do ((candidate v1 (km-unique0 `(|the| |less-than| |of| ,candidate))))
      ((or (not candidate)
	   (equal candidate v2))
       (if (not candidate)
	   nil
	 t))
      )
  )


;; must use km0 interpreter (not get-vals)
;; get-vals does not climb situation hierarchy
(defun clib-convertunit (unit1 unit2)
  (if (equal unit1 unit2)
      1.0
      (let ((mult1 (car (km0 `(|the| |conversion-multiplier| |of| ,unit1)))) 
            (mult2 (car (km0 `(|the| |conversion-multiplier| |of| ,unit2))))
           )
        (if (and (numberp mult1)
                 (numberp mult2)
                 (not (equal mult2 0))
                 (intersection (immediate-classes unit1) (immediate-classes unit2))
            )
            (/ mult1 mult2)
        )
      )
  )
)

(defun get-clib-base-unit (unit)
  (car (get-vals-of-one (immediate-classes unit) '|base-unit|))
)

(defun get-vals-of-one (km-obj-list slot)
  (if km-obj-list
      (if (listp km-obj-list)
          (let ((val-of-car (km0 `(|the| ,slot |of| ,(car km-obj-list)))))
            (if val-of-car
                val-of-car
                (get-vals-of-one (cdr km-obj-list) slot)
            )
          )
      )
  )
)

(defun combine-units-in-product-list (uplist)
  (combine-units-in-product-list0 nil (expand-uplist-to-derivations uplist))
)

(defun combine-units-in-product-list0 (tmplist uplist)
  (if uplist
      (combine-units-in-product-list0
         (update-unit-in-list (car uplist) tmplist)
         (cdr uplist)
      )
      tmplist
  )
)

(defun expand-uplist-to-derivations (uplist)
  (if uplist
      (let* ((orig-unit  (first (car uplist)))
             (orig-exp   (second (car uplist)))
             (unit-deriv (reduce-unit-to-derivation orig-unit))
             (head-uplist (multiply-exps-in-uplist unit-deriv orig-exp))
            )
        (append head-uplist (expand-uplist-to-derivations (cdr uplist)))
      )
  )
)

(defun multiply-exps-in-uplist (uplist exponent)
  (if uplist
      (cons (list (first (car uplist))
                  (* (second (car uplist)) exponent)
            )
            (multiply-exps-in-uplist (cdr uplist) exponent)
      )
  )
)

(defun unity-unit-only (uplist)
  (and uplist
       (listp uplist)
       (null (cdr uplist))
       (or (equal (caar uplist) '|*unity|)
           (equal (caar uplist) '|*unit|)
       )
  )
)

(defun update-unit-in-list (unit-pair uplist)
  (if uplist
      (if (equal (car unit-pair) (caar uplist))
          (cons (list (first unit-pair)
                      (+ (second unit-pair) (second (car uplist)))
                )
                (cdr uplist)
          )
          (cons (car uplist) (update-unit-in-list unit-pair (cdr uplist)))
      )
      (list unit-pair)
  )
)

(defun simplify-unit-pair-list (uplist)
  (if uplist
      (let ((all-base-units (km0 `(|forall| |?u| |in| (|the| |all-subclasses| |of| |Unit-of-Measurement|)
                                   (|the| |base-unit| |of| |?u|))
                            )
            )
            (uplist-clean (or (strip-unity-from-uplist uplist)
                              (list (list '|*unity| (add-all-uplist-exponents uplist)))    ;; unities only
            ))
           )
        (or (find-matching-base-unit uplist-clean all-base-units)
            uplist-clean
        )
      )
  )
)

(defun strip-unity-from-uplist (uplist)
  (if uplist
      (if (or (equal (first (car uplist)) '|*unity|)
              (equal (first (car uplist)) '|*unit|)
              (equal (second (car uplist)) 0)
          )
          (strip-unity-from-uplist (cdr uplist))
          (cons (car uplist) (strip-unity-from-uplist (cdr uplist)))
      )
  )
)

(defun add-all-uplist-exponents (uplist)
  (if uplist
      (+ (cadar uplist) (add-all-uplist-exponents (cdr uplist)))
      0
  )
)


(defun find-matching-base-unit (uplist base-units)
  (if (and uplist base-units)
      (let ((rbu (reduce-unit-to-derivation (car base-units)))
           )
        (if (uplist-equal rbu uplist)
            (list (list (car base-units) 1))
            (find-matching-base-unit uplist (cdr base-units))
        )
      )
  )
)

(defun uplist-equal (upl1 upl2)
  (and (subsetp upl1 upl2 :test 'equal)
       (subsetp upl2 upl1 :test 'equal)
  )
)

;; generate the conversion multipliers for all known units
;; (only to seed-populate the conversion-multipliers for
;; the first time)
#|
(defun gencm ()
  (let ((dimensions (immediate-subclasses '|Unit-of-Measurement|)))
    (dolist (dim dimensions)
      (let ((units (all-instances dim))
            (baseu (or (car (get-vals dim '|base-unit|))
                       (car (immediate-instances dim))
                   )
            )
           )
        (dolist (unit units)
          (if (and units baseu)
              (format t "echo \"(~a has (conversion-multiplier (~a)))\" >> ~a.km~%"
                        unit
                        (glconvertunit unit baseu)
                        dim
              )
          )
        )
      )
    )
  )
  t
)
|#

