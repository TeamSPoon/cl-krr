;;
;; $Id: user-defined-arithmetic-operators.lisp,v 1.20 2010/05/19 20:44:51 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;; ======================================================================
;;;             DECLARATION of INFIX OVERLOADED Math Operators 
;;; ======================================================================

(defun get-card-val-pair(value-instance)
  (let ((filler (km0 `(|the| |value| |of| ,value-instance))))
    (if (and (null filler)
	     (fboundp 'equation-solve)
	     *EQ-FLAG*)
	(equation-solve value-instance))
    (get-card-val-pair0 value-instance)))

(defun get-card-val-pair0(value-instance)
  (let ((filler (km0 `(|the| |value| |of| ,value-instance))))
    (first (extract-card-vals filler))))
    
(defun unitlessp (input)
  (or (numberp input)
      (unitless-property-value-p input)))

(defun unitless-property-value-p (input)
  (isa input '|Unitless-Value|))

(defun is-property-value (input)
  (and (not (null input))
       (atom input)
       (isa input '|Property-Value|)))

(defun is-vector-value (input)
  (and (not (null input))
       (atom input)
       (isa input '|Property-Vector-Value|)))

(defun extract-unitless-number (input)
  (cond ((numberp input) input)
	((km0 `(,input |isa| |Unitless-Value|))
	 (car (km0 `(|the1| |of| (|the| |value| |of| ,input)))))
	(t nil)))

;; do not unkmify units with new CLib-native conversion
#|
;;Hack for glunkmify to handle conversion of KM's *unity properly.
(defun my-glunkmify (km-uom)
  (cond ((null km-uom) nil)
	((equal km-uom '*) '*)
	((listp km-uom) 
	 (cons (my-glunkmify (car km-uom))
	       (my-glunkmify (cdr km-uom))))
	((equal km-uom '|*unity|) 'unity)
	(t (intern (string-upcase (string (glunkmify km-uom))) :km))))
|#
;; for now just return km uom (should delete calls to my-glunkmify instead)
(defun my-glunkmify (km-uom)
  km-uom
)

(defun negate-property-value (value-instance)
  (let* ((val-pair (get-card-val-pair value-instance))
	 (num-val  (second val-pair))
	 (uom      (third val-pair)))
    (if (null val-pair) nil
        (km0 `(|an| |instance| |of| 
		    (|the| |instance-of| |of| ,VALUE-INSTANCE)
		    |with| 
		    (|value| 
		     ((:|pair| ,(* -1 NUM-VAL) ,UOM))))))))

(defun negate-vector-value (vector-instance)
  (let* ((x-V      (get-x-component vector-instance))
         (x-c      (car (immediate-classes x-V)))
         (y-V      (get-y-component vector-instance))
         (y-c      (car (immediate-classes y-V)))
         (x-slot   (car (km0 `(|the| |x-component-slot| |of| ,VECTOR-INSTANCE))))
         (y-slot   (car (km0 `(|the| |y-component-slot| |of| ,VECTOR-INSTANCE))))
         (m-slot   (car (km0 `(|the| |secondary-slot| |of| ,VECTOR-INSTANCE))))
         (d-slot   '|direction|)
         (m-c      (car (km0 `(|the| |range| |of| (|the| |secondary-slot| |of| ,VECTOR-INSTANCE)))))
         (m-V      (car (km0 `(|the| ,M-SLOT |of| ,VECTOR-INSTANCE))))
         (d-V      (car (km0 `(|the| ,D-SLOT |of| ,VECTOR-INSTANCE))))
         (md-ps    (get-mag-and-dir-from-known-x-and-y-component x-V y-V))
         (m-pair   (or (get-card-val-pair0 m-V)
                       (first md-ps)
                   ))
         (d-pair   (or (get-card-val-pair0 d-V)
                       (second md-ps)
                   ))
         (x-pair   (get-card-val-pair0 x-V))
         (x-num    (second x-pair))
         (x-uom    (third x-pair))
         (y-pair   (get-card-val-pair0 y-V))
         (y-num    (second y-pair))
         (y-uom    (third y-pair))
         (d-num    (second d-pair))
         (d-uom    (third d-pair))
        )
    (if (null d-pair) 
        nil
        (km0 `(|an| |instance| |of| 
                    (|the| |instance-of| |of| ,VECTOR-INSTANCE)
                    |with| 
                    (,M-SLOT ((|a| ,M-C |with| (|value| (,M-PAIR)))))
                    (,D-SLOT ((|a| |Angle-Value| |with|
                                 (|value| ((:|pair| ,(* -1 D-NUM) ,D-UOM)))
                    )))
                    (,X-SLOT ((|a| ,X-C |with|
                                 (|value| ((:|pair| ,(* -1 X-NUM) ,X-UOM)))
                    )))
                    (,Y-SLOT ((|a| ,Y-C |with|
                                 (|value| ((:|pair| ,(* -1 Y-NUM) ,Y-UOM)))
                    )))
                )
        )
    )
  )
)

;Given a unit, the method looks up the synonyms for the unit and determines 
;the mapping to an appropriate unit in the CLIB. For instance if the unit is 
;meter, then the method returns the CLIB UoM |*meter|
(defun get-CLIB-property(unit)
  (let* ((unit_synset (get-synonyms unit)))
    (catch 'found
      (dolist (uom unit_synset)
	(let* ((simplified_unit (intern (string-downcase (string uom)) :km)))
	  (cond ((is-UoM-valid-in-CLIB simplified_unit)
		 (throw 'found simplified_unit))))))))

(defun property-plus (input1 input2)
  (if (and (is-vector-value input1)
           (is-vector-value input2))
      (vector-plus-vector input1 input2)
      (if (and (is-property-value input1)
               (is-property-value input2))
          (property-plus-property input1 input2)
      )
  )
)

(defun property-minus (input1 input2)
  (if (and (is-vector-value input1)
           (is-vector-value input2))
      (vector-minus-vector input1 input2)
      (if (and (is-property-value input1)
	       (is-property-value input2))
          (property-minus-property input1 input2)
      )
  )
)

(defun property-mult (input1 input2)
  (cond ((and (is-property-value input1)
	      (numberp input2))
	 (property-mult-number input1 
			       input2))
	((and (numberp input1)
	      (is-property-value input2))
	 (property-mult-number input2
			       input1))
	((and (is-property-value input1)
	      (is-property-value input2))
	 (property-mult-property input1 input2))))

(defun property-div (input1 input2)
  (cond ((and (is-property-value input1)
	      (numberp input2))
	 (property-div-number input1 
			      input2))
	((and (numberp input1)
	      (unitlessp input2))
	 (property-div-number input2
			      input1))
	((and (is-property-value input1)
	      (is-property-value input2))
	 (property-div-property input1 input2))))

(defun property-plus-property (value-instance1 value-instance2)
  (let ((val-pair1 (get-card-val-pair value-instance1))
	(val-pair2 (get-card-val-pair value-instance2)))
    (cond
     ((or  (null val-pair1) (null val-pair2))   nil)  
     (t (let* ((num-val1   (second val-pair1))
	       (num-val2   (second val-pair2))
;	       (UoM1       (my-glunkmify (third val-pair1)))
;	       (UoM2       (my-glunkmify (third val-pair2)))
;	       (UoC        (my-glconvertunit UoM2 UoM1))
	       (UoM1       (third val-pair1))
	       (UoM2       (third val-pair2))
	       (UoC        (clib-convertunit UoM2 UoM1))
              )
	  (if (numberp UoC) (km0 `(|an| |instance| |of| 
					(|the| |instance-of| |of| ,VALUE-INSTANCE1)
					|with| 
					(|value| 
					 ((:|pair| ((,UOC * ,NUM-VAL2) + ,NUM-VAL1)
						 ,UOM1)))))))))))
	
(defun convert-unit-in-little-v-pair (lv-pair new-km-unit)
  (if (and (third lv-pair) new-km-unit)
      (let ((cardinal (second lv-pair))
            (old-km-unit (third lv-pair))
           )
        (if (equal old-km-unit new-km-unit)
            lv-pair
;            (let ((conv-factor (my-glconvertunit 
;                                 (my-glunkmify old-km-unit) 
;                                 (my-glunkmify new-km-unit)
;                               )
;                 ))
             (let ((conv-factor (clib-convertunit old-km-unit new-km-unit)))
              (if (numberp conv-factor)
                  (list ':|pair| (* conv-factor cardinal) new-km-unit)
                  lv-pair
              )
            )
        )
      )
      lv-pair
  )
)

(defun vector-plus-vector (vector1 vector2)
  (let* ((x1 (get-x-component vector1))
         (x2 (get-x-component vector2))
         (y1 (get-y-component vector1))
         (y2 (get-y-component vector2))
         (x-slot (car (km0 `(|the| |x-component-slot| |of| ,VECTOR1))))
         (y-slot (car (km0 `(|the| |y-component-slot| |of| ,VECTOR1))))
         (m-slot (car (km0 `(|the| |secondary-slot| |of| ,VECTOR1))))
         (m-class (car (km0 `(|the| |range| |of| ,M-SLOT))))
         (d-slot '|direction|)
         (d-class '|Angle-Value|)
         (xresult (property-plus-property x1 x2))
         (yresult (property-plus-property y1 y2))
         (md-res  (get-mag-and-dir-from-known-x-and-y-component xresult yresult))
         (mresult (first md-res))
         (dresult (second md-res))
         (orig-d-unit (or (car (km0 `(|the2| |of| (|the| |value| |of| (|the| |direction| |of| ,VECTOR1)))))
                          (car (km0 `(|the2| |of| (|the| |value| |of| (|the| |direction| |of| ,VECTOR2)))))
         ))
         (dconv   (convert-unit-in-little-v-pair dresult orig-d-unit))
         (vc (car (immediate-classes vector1)))
        )
    (if (and xresult yresult mresult dconv)
        (km0 `(|an| |instance| |of| ,VC |with|
                    (,X-SLOT ,XRESULT)
                    (,Y-SLOT ,YRESULT)
                    (,M-SLOT ((|a| ,M-CLASS |with| (|value| (,MRESULT)))))
                    (,D-SLOT ((|a| ,D-CLASS |with| (|value| (,DCONV)))))
                )
        )
        nil
    )
  )
)

(defun get-x-component (vector)
  (let* ((xp (car (km0 `(|the| (|the| |x-component-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (xv (get-card-val-pair0 xp))
         (yp (car (km0 `(|the| (|the| |y-component-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (yv (get-card-val-pair0 yp))
         (mp (car (km0 `(|the| (|the| |secondary-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (mv (get-card-val-pair0 mp))
         (dp (car (km0 `(|the| |direction| |of| ,VECTOR))))
         (dv (get-card-val-pair0 dp))
       )
    (if xv                            ;; x-component already known
        xp
        (cond ((and mv dv)            ;; calculate from magnitude and direction
               (let* ((newdv (convert-unit-in-little-v-pair dv '|*radian|))
                      (theta (second newdv))
                      (magnum (second mv))
                      (magunit (third mv))
                      (xnum (* magnum (cos theta)))
                      (xc (car (immediate-classes mp)))
                     )
                 (car (km0 `(|an| |instance| |of| ,XC |with|
                                  (|value| ((:|pair| ,XNUM ,MAGUNIT)))))
                 )
               )
              )
              ((and yv dv (> (abs (second yv)) 0.0001))  ;; calculate from y-component and direction
               (let* ((newdv (convert-unit-in-little-v-pair dv '|*radian|))
                      (theta (second newdv))
                      (ynum (second yv))
                      (yunit (third yv))
                      (xnum (/ ynum (tan theta)))
                      (xc (car (immediate-classes yp)))
                     )
                 (car (km0 `(|an| |instance| |of| ,XC |with|
                                  (|value| ((:|pair| ,XNUM ,YUNIT)))))
                 )
               )
              )
              (t nil)
        )
    )
  )
)

(defun get-y-component (vector)
  (let* ((xp (car (km0 `(|the| (|the| |x-component-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (xv (get-card-val-pair0 xp))
         (yp (car (km0 `(|the| (|the| |y-component-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (yv (get-card-val-pair0 yp))
         (mp (car (km0 `(|the| (|the| |secondary-slot| |of| ,VECTOR) |of| ,VECTOR))))
         (mv (get-card-val-pair0 mp))
         (dp (car (km0 `(|the| |direction| |of| ,VECTOR))))
         (dv (get-card-val-pair0 dp))
       )
    (if yv                            ;; y-component already known
        yp
        (cond ((and mv dv)            ;; calculate from magnitude and direction
               (let* ((newdv (convert-unit-in-little-v-pair dv '|*radian|))
                      (theta (second newdv))
                      (magnum (second mv))
                      (magunit (third mv))
                      (ynum (* magnum (sin theta)))
                      (yc (car (immediate-classes mp)))
                     )
                 (car (km0 `(|an| |instance| |of| ,YC |with|
                                  (|value| ((:|pair| ,YNUM ,MAGUNIT)))))
                 )
               )
              )
              ((and xv dv (> (abs (second xv)) 0.0001))  ;; calculate from x-component and direction
               (let* ((newdv (convert-unit-in-little-v-pair dv '|*radian|))
                      (theta (second newdv))
                      (xnum (second xv))
                      (xunit (third xv))
                      (ynum (* xnum (tan theta)))
                      (yc (car (immediate-classes xp)))
                     )
                 (car (km0 `(|an| |instance| |of| ,YC |with|
                                  (|value| ((:|pair| ,YNUM ,XUNIT)))))
                 )
               )
              )
              (t nil)
        )
    )
  )
)

(defun get-mag-and-dir-from-known-x-and-y-component (xprop yprop)
  (if (and xprop yprop)
    (let* ((xv (get-card-val-pair0 xprop))
           (yv (get-card-val-pair0 yprop))
           (xnum (second xv))
           (xunit (third xv))
           (newyv (convert-unit-in-little-v-pair yv xunit)) 
           (ynum  (second newyv))
          )
      (list
        (list ':|pair| (sqrt (+ (expt xnum 2) (expt ynum 2))) xunit)
        (list ':|pair| (atan ynum xnum) '|*radian|)
      )
    )
  )
)

(defun property-minus-property (value-instance1 value-instance2)
  (property-plus-property value-instance1
			  (negate-property-value value-instance2)))

(defun vector-minus-vector (vector-instance1 vector-instance2)
  (vector-plus-vector vector-instance1 (negate-vector-value vector-instance2))
)

(defun is-UoM-valid-in-CLIB(UoM)
  (km0 `((|the| |all-instances| |of| |Unit-of-Measurement|) |includes| ,UOM)))

(defun kmifyunit-safe (unitexpr)
  (let ((unit (kmifyunit unitexpr)))
    (if (equal unit 'UNITY)
        '|*unity|
        unit
    )
  )
)

(defun property-mult-property (value-instance1 value-instance2)
  (let* ((val_pair1  (get-card-val-pair value-instance1))
	 (orig-num1  (second val_pair1))
	 (orig-unit1 (third val_pair1))
         (base-unit1 (car (km0 `(|the| |base-unit| |of| (|the| |classes| |of| ,orig-unit1)))))
         (base-num1  (* orig-num1 (clib-convertunit orig-unit1 base-unit1)))
	 (val_pair2  (get-card-val-pair value-instance2))
	 (orig-num2  (second val_pair2))
	 (orig-unit2 (third val_pair2))
         (base-unit2 (car (km0 `(|the| |base-unit| |of| (|the| |classes| |of| ,orig-unit2)))))
         (base-num2  (* orig-num2 (clib-convertunit orig-unit2 base-unit2)))
	 (prod-unit  (clib-unit-product base-unit1 base-unit2))
	 (prod-class (car (km0 `(|the| |range| |of| 
                                       (|the| |cardinal-unit-class-of| |of| 
                                              (|the| |classes| |of| ,prod-unit))))))
         (prod-num   (* base-num1 base-num2))
        )
      (if (and prod-unit prod-class prod-num)
          (km0 `(|an| |instance| |of| ,prod-class 
                      |with| (|value| ((:|pair| ,prod-num ,prod-unit))))
          )
      )
  )
)
   

(defun property-div-property (value-instance1 value-instance2)
  (let* ((val_pair1  (get-card-val-pair value-instance1))
         (orig-num1  (second val_pair1))
         (orig-unit1 (third val_pair1))
         (base-unit1 (car (km0 `(|the| |base-unit| |of| (|the| |classes| |of| ,orig-unit1)))))
         (base-num1  (* orig-num1 (clib-convertunit orig-unit1 base-unit1)))
         (val_pair2  (get-card-val-pair value-instance2))
         (orig-num2  (second val_pair2))
         (orig-unit2 (third val_pair2))
         (base-unit2 (car (km0 `(|the| |base-unit| |of| (|the| |classes| |of| ,orig-unit2)))))
         (base-num2  (* orig-num2 (clib-convertunit orig-unit2 base-unit2)))
         (quot-unit  (clib-unit-quotient base-unit1 base-unit2))
         (quot-class (car (km0 `(|the| |range| |of|
                                       (|the| |cardinal-unit-class-of| |of|
                                              (|the| |classes| |of| ,quot-unit))))))
         (quot-num   (/ base-num1 base-num2))
        )
      (if (and quot-unit quot-class quot-num)
          (km0 `(|an| |instance| |of| ,quot-class
                      |with| (|value| ((:|pair| ,quot-num ,quot-unit))))
          )
      )
  )
)




(defun get-applicable-property-value-concept(uom value-instance1 value-instance2)
  (let ((applicable (km0 `((|the| |range| |of| 
				  (|the| |cardinal-unit-class-of| |of| 
					 (|the| |instance-of| |of| ,uom)))))))
    (let ((result 
	   (car (intersection (append (km0 `(|the| |instance-of| |of| ,value-instance1))
				      (km0 `(|the| |instance-of| |of| ,value-instance2)))
			      applicable))))
      (if (null result)
	  (car applicable)
	  result))))

(defun property-mult-number (value-instance value-factor)
  (let ((val_pair (get-card-val-pair value-instance))
	num_val UoM)
    (cond
     ((or (null val_pair)
	  (null (numberp value-factor)))
      nil)
     (t 
      (progn 
	(setf num_val (second val_pair))
	(setf UoM (intern (string (third val_pair)) :km))
	(km0 `(|an| |instance| |of| 
		    (|the| |instance-of| |of| ,VALUE-INSTANCE)
		    |with| 
		    (|value| 
		     ((:|pair| (,VALUE-FACTOR * ,NUM_VAL)
			     ,UOM))))))))))

(defun property-div-number (value-instance value-factor)
  (let ((val_pair (get-card-val-pair value-instance)))
    (if (and (not (null val_pair))
	     (not (null (numberp value-factor))))
	(property-mult value-instance (/ 1 value-factor)))))


#|
More or less works. But 
;; a) finding the root of (25 *square-meter) should return (5 *meter) instead of (5 *square-meter)
;; b) what will be a good relation for this infix operator? property-root?
(defun property-expt (input1 input2)
  (if (and (is-property-value input1)
	   (unitless-property-value-p input2))
      (property-expt-unitless-property input1 input2)))

(defun inv-unitless-card-val(input)
  (let ((val (nth 1 input)))
    (if (numberp val)
	(list ':|pair| (/ 1 val) '|*unity|))))

(defun property-expt-unitless-property (value-instance1 root-value-instance2)
  (let ((val-pair1 (get-card-val-pair value-instance1))
	(val-pair2 (get-card-val-pair root-value-instance2)))
    (cond
     ((and (values-consistent val-pair1)
	   (values-consistent val-pair2)
	   (values-consistent (inv-unitless-card-val val-pair2)))
      (bps-clib-small-v-value-into-clib-property-value
       (bps-novak-output-into-clib-small-v-value
	(BPS-SOLVOBJVAR '((= |answer| (|expt| |val| |expt-root|)))
			'|answer|
			`((|val| ,(cdr val-pair1))
			  (|expt-root| ,(cdr (inv-unitless-card-val val-pair2)))))))))))
|#

(setq *user-defined-infix-operators*
      (append *user-defined-infix-operators* 
	      `(
		(|property-plus|  ,#'property-plus)
		(|property-minus| ,#'property-minus)
		(|property-mult|  ,#'property-mult)
		(|property-div|   ,#'property-div))))

#| Overloaded math operator demo

KM> (the value of 
     ((a Mass-Value with
	 (value ((:pair 1 *kilogram))))
      property-plus
      (a Length-Value with
	 (value ((:pair 1000 *meter))))))

NIL
(7 inferences and 163 KB accesses in 0.4 sec [17 lips, 417 kaps]))

KM> (the value of 
     ((a Mass-Value with
	 (value ((:pair 1 *kilogram))))
      property-plus
      (a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))))

KM> ((:pair 2.2 *kilogram))
(14 inferences and 221 KB accesses in 0.5 sec [28 lips, 451 kaps]))

KM> (the value of
     ((a Mass-Value with
	 (value ((:pair 1 *kilogram))))
      property-minus
      (a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))))

KM> ((:pair -0.20000005 *kilogram))
(14 inferences and 195 KB accesses in 0.3 sec [42 lips, 590 kaps]))

KM> (the value of
     ((a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))
      property-minus
      (a Mass-Value with
	 (value ((:pair 1 *kilogram))))))

KM> ((:pair 0.20000005 *kilogram))
(14 inferences and 195 KB accesses in 0.3 sec [46 lips, 650 kaps]))

KM> (the value of
     ((a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))
      property-mult
      5))

KM> ((:pair 6.0 *kilogram))
(10 inferences and 131 KB accesses in 0.2 sec [45 lips, 595 kaps]))

KM> (the value of
     ((a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))
      property-div
      6))

KM> Calling property-div((:pair 0.20000002 *kilogram))
(11 inferences and 133 KB accesses in 0.2 sec [50 lips, 604 kaps]))

KM> (the value of
     (5
      property-mult
      (a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))))

KM> ((:pair 6.0 *kilogram))
(10 inferences and 131 KB accesses in 0.2 sec [50 lips, 655 kaps]))

KM> (the value of
     (
      6
      property-div
      (a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))))

nil
(11 inferences and 133 KB accesses in 0.2 sec [45 lips, 554 kaps]))

KM> (the value of
     (
      (a Mass-Value with
	 (value ((:pair 1.2 *kilogram))))
      property-div
      (a Mass-Value with
	 (value ((:pair 2 *kilogram))))))

KM> ((:pair 0.6 *unity))
(5 inferences and 71 KB accesses in 0.2 sec [25 lips, 355 kaps]))

KM> (the value of 
      ((a Length-Value with
	(value ((:pair 10 *meter))))
     property-mult
     (a Length-Value with
        (value ((:pair 10 *meter))))))
KM> ((:pair 100 *square-meter))
(25 inferences and 510 KB accesses in 0.0 sec [833 lips, 17000 kaps]))

KM> (the value of 
     (((a Length-Value with
	(value ((:pair 10 *meter))))
         property-mult
        (a Length-Value with
        (value ((:pair 10 *meter)))))
 property-mult
     (a Length-Value with
        (value ((:pair 10 *meter))))))

KM> ((:pair 1000.0 *cubic-meter))


(progn (new-situation)
       (km0 '((|a| |Area-Value| |with|
	       (|value| ((:|pair| 100 |*square-meter|))))
	      |property-mult|
	      (|a| |Length-Value| |with|
	       (|value| ((:|pair| 10 |*meter|)))))))
       
KM> (the value of 
     (((a Length-Value with
	 (value ((:pair 10 *meter))))
      property-mult
      (a Length-Value with
         (value ((:pair 10 *meter))))) 
       property-div
       (a Length-Value with
          (value ((:pair 10 *meter))))))
((:pair 10 *meter))
(54 inferences and 1144 KB accesses in 0.1 sec [1080 lips, 22880 kaps]))



|#

(defun clib-unit-product (unit1 unit2)
  (cond ((equal unit1 '|*unity|) unit2)
        ((equal unit2 '|*unity|) unit1)
        (t
          (let ((legal-tr (find-if #'(lambda (tr) (or (and (equal (first tr) unit1) 
                                                           (equal (second tr) unit2))
                                                      (and (equal (first tr) unit2) 
                                                           (equal (second tr) unit1))))
                                   *CLIB-LEGAL-UNIT-PRODUCTS*)
               ))
            (if legal-tr
                (third legal-tr)
            )
          )
        )
  )
)

(defun clib-unit-quotient (unit1 unit2)
  (cond ((equal unit1 unit2) '|*unity|)
        ((equal unit2 '|*unity|) unit1)
        (t
          (let ((legal-tr (find-if #'(lambda (tr) (and (equal (first tr) unit1)
                                                       (equal (second tr) unit2)))
                                   *CLIB-LEGAL-UNIT-QUOTIENTS*)
               ))
            (if legal-tr
                (third legal-tr)
            )
          )
        )
  )
)

;; mults
(defparameter *CLIB-LEGAL-UNIT-PRODUCTS* '(
  (|*cubic-meter| |*kilogram-per-meter-cubed| |*kilogram|)
  (|*hertz| |*meter-per-second| |*meter-per-second-squared|)
  (|*joule| |*hertz| |*watt|)
  (|*kilogram| |*meter-per-second-squared| |*newton|)
  (|*kilogram| |*meter-per-second| |*kilogram-meter-per-second|)
  (|*kilogram| |*square-meter| |*kilogram-meter-squared|)
  (|*meter| |*hertz| |*meter-per-second|)
  (|*meter| |*meter| |*square-meter|)
  (|*meter| |*square-meter| |*cubic-meter|)
  (|*newton| |*meter| |*joule|)
  (|*newton| |*second| |*kilogram-meter-per-second|)
  (|*second| |*hertz| |*unity|)
  (|*second| |*meter-per-second-squared| |*meter-per-second|)
  (|*second| |*meter-per-second| |*meter|)
  (|*square-meter| |*candeal-per-meter-squared| |*candela|)
  (|*square-meter| |*pascal| |*newton|)
  (|*volt| |*ampere| |*watt|)
  (|*watt| |*second| |*joule|)
))

;; divs
(defparameter *CLIB-LEGAL-UNIT-QUOTIENTS* '(
  (|*candela| |*square-meter| |*candela-per-meter-squared|)
  (|*cubic-meter| |*meter| |*square-meter|)
  (|*cubic-meter| |*square-meter| |*meter|)
  (|*joule| |*meter| |*newton|)
  (|*joule| |*newton| |*meter|)
  (|*joule| |*second| |*watt|)
  (|*joule| |*watt| |*second|)
  (|*kilogram-meter-per-second| |*kilogram| |*meter-per-second|)
  (|*kilogram-meter-per-second| |*meter-per-second| |*kilogram|)
  (|*kilogram-meter-per-second| |*newton| |*second|)
  (|*kilogram-meter-per-second| |*second| |*newton|)
  (|*kilogram-meter-squared| |*kilogram| |*square-meter|)
  (|*kilogram-meter-squared| |*square-meter| |*kilogram|)
  (|*kilogram| |*cubic-meter| |*kilogram-per-meter-cubed|)
  (|*meter-per-second-squared| |*hertz| |*meter-per-second|)
  (|*meter-per-second-squared| |*meter-per-second| |*hertz|)
  (|*meter-per-second| |*meter-per-second-squared| |*second|)
  (|*meter-per-second| |*meter| |*hertz|)
  (|*meter-per-second| |*second| |*meter-per-second-squared|)
  (|*meter| |*meter-per-second| |*second|)
  (|*meter| |*second| |*meter-per-second|)
  (|*newton| |*kilogram| |*meter-per-second-squared|)
  (|*newton| |*meter-per-second-squared| |*kilogram|)
  (|*newton| |*square-meter| |*pascal|)
  (|*square-meter| |*meter| |*meter|)
  (|*unity| |*hertz| |*second|)
  (|*unity| |*second| |*hertz|)
  (|*watt| |*ampere| |*volt|)
  (|*watt| |*hertz| |*joule|)
  (|*watt| |*volt| |*ampere|)
))



