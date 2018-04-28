;;
;; $Id: valuecheck.lisp,v 1.21 2008/12/06 01:38:40 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun minchksca (Self TheValue)
  (let* ( (scales   (km0 '(|the| |all-instances| |of| |Scale|)))
          (val      (second (car TheValue)))  ;;gets the constant of value's pair
          (valclass (third (car TheValue)))   ;;gets the class of value's pair
          (minval      (car (km0 `(|the1| |of| (|theoneof| (|the| |min-value| |of| ,Self)               
                                       |where| ((|the2| |of| |It|) = ,valclass))))))
(result
(car
    (remove nil
	    ;;goes thru all the scales and finds the first one that contains both constants
            (mapcar #'(lambda (sca)
                        (let ((seq (km0 `(|the| |elements| |of| (|the| |element| |of| ,sca)))))
                          (if (and (member val    seq)
                                   (member minval seq)) ;from the min-value slot
                              seq
                              nil)))
                    scales);;end mapcar
    );;end remove
);;end car
);;end result
);;end of let's ()
;debuging print messages
;(FORMAT t "val = ~s minval = ~s result = ~s~%" val minval result)
;(PRINT TheValue)
;if a minval was found test whether it is earlier in the scale sequence else just return true
(if minval 
    (>= (POSITION val result) (POSITION minval result))
    t
)
  );;end let
);;end defun


(defun maxchksca (Self TheValue)
  (let* ( (scales   (km0 '(|the| |all-instances| |of| |Scale|)))
          (val      (second (car TheValue)))
          (valclass (third (car TheValue)))
          (maxval      (car (km0 `(|the1| |of| (|theoneof| (|the| |max-value| |of| ,Self)
                                       |where| ((|the2| |of| |It|) = ,valclass))))))
(result
(car
    (remove nil
            (mapcar #'(lambda (sca)
                        (let ((seq (km0 `(|the| |elements| |of| (|the| |element| |of| ,sca)))))
                          (if (and (member val    seq)
                                   (member maxval seq)) ;from the max-value slot
                              seq
                              nil)))
                    scales);;end mapcar
    );;end remove
);;end car
);;end result
);;end of let's ()
;(FORMAT t "val = ~s maxval = ~s result = ~s~%" val maxval result)
;(PRINT TheValue)
(if maxval
    (<= (POSITION val result) (POSITION maxval result))
    t
)
  );;end let
);;end defun

(defun minchkcard (Self TheValue)
  (let* ( (valnum   (second (car TheValue)))
	  (valUoMKM (third (car TheValue))) ;;km needs it in this format
	  (valUoM   (intern (string-upcase (string
				  (third  (car TheValue)))) :km))

	  (minnum (car (km0 `(|the1| |of| (|theoneof| (|the| |min-value| |of| ,Self)
                                       |where| ((|the1| |of| |It|) |isa| |Number|))))))
	  ;;this does not consider the chance that there is two cardinal representations and
	  ;;this somehow grabs a different one than minnum took its value from
	  (minUoM (intern (string-upcase (string
		       (car (km0 `(|the2| |of| (|theoneof| (|the| |min-value| |of| ,Self)
                                       |where| ((|the1| |of| |It|) |isa| |Number|))))))) :km))
	)

;;(format t "~%minnum = ~s minUoM = ~s~%" minnum minUoM)
;;(format t "valnum = ~s valUoM = ~s~%" valnum valUoM)

(if minnum
    (if (not valUoM) ;;if the second part of the pair is not NIL
	(>= valnum minnum) ;;the second part is nil, so just evaluate numbers
        (if (not (km0 `(,valUoMKM |isa| |Unit-of-Measurement|)))  ;;the second part is not a UoM so evaluate like a scalar
	    (if (equal valUoM minUoM) ;;are the reference classes the same
		(>= valnum minnum)
	        t
	    )   
	    (if (km0 `(,valUoMKM |isa| |UoM-Temperature|))
		(>= (tempconvert (list '* valnum valUoM) minUoM) minnum)
	        (>= (* valnum (glconvertunit valUoM minUoM)) minnum)
	    )
	)
    )
  t
)
))

(defun maxchkcard (Self TheValue)
  (let* ( (valnum   (second (car TheValue)))
          (valUoMKM (third (car TheValue))) ;;km needs it in this format
          (valUoM   (intern (string-upcase (string
                                  (third  (car TheValue)))) :km))

          (maxnum (car (km0 `(|the1| |of| (|theoneof| (|the| |max-value| |of| ,Self)
                                       |where| ((|the1| |of| |It|) |isa| |Number|))))))
          (maxUoM (intern (string-upcase (string
                       (car (km0 `(|the2| |of| (|theoneof| (|the| |max-value| |of| ,Self)
                                       |where| ((|the1| |of| |It|) |isa| |Number|))))))) :km))
        )

;(format t "~%maxnum = ~s maxUoM = ~s~%" maxnum maxUoM)
;(format t "valnum = ~s valUoM = ~s~%" valnum valUoM)
(if maxnum
    (if (not valUoM)
	(<= valnum maxnum)
        (if (not (km0 `(valUoMKM |isa| |Unit-of-Measurement|)))
	    (if (equal valUoM maxUoM)
		(<= valnum maxnum)
	        t
	    )
	    (if (km0 `(,valUoMKM |isa| |UoM-Temperature|))
		(<= (tempconvert (list '* valnum valUoM) maxUoM) maxnum)
	        (<= (* valnum (glconvertunit valUoM maxUoM)) maxnum)
	    )
	)
    )
  t
)
))

;;  this is the function that will be called from the constraint
;;  It calls the appopriate function depending on whether the value is a 
;; Cardinal or Scalar
;;  Self = the property-value being checked
;;  TheValue = the particular value that the constraint is checking in the value slot
;; of Property-Value

(defun valuecheck (Self TheValue)
  (if (not TheValue)
      t
      (if (km0 `((|the1| |of| ,TheValue) |isa| |Constant|))
	  (and 
	   (minchksca Self TheValue)
	       (maxchksca Self TheValue)
	  )
	  (and 
	   (minchkcard Self TheValue)
	       (maxchkcard Self TheValue)
	  )
      )
))

;;----------------------------------------------------------------------
;; DESC: Given a list of little-v values, check whether they are 
;;	 consistent. 
;;----------------------------------------------------------------------
(defun values-consistent (all-values)
  (let ((card-values   (extract-card-vals all-values))
        (scalar-values (extract-scalar-vals all-values))
        (categ-values  (extract-categ-vals all-values))
       )
    (and (consistent-card-vals card-values) 
         (consistent-scalar-vals scalar-values)
         (consistent-categ-vals categ-values)
    )
  )
)

(defun extract-categ-vals (val-list)
  (if (null val-list)
      nil
      (if (val-is-categ (car val-list))
          (cons (car val-list) (extract-categ-vals (cdr val-list)))
          (extract-categ-vals (cdr val-list))
      )
  )
)

(defun val-is-categ (val)
  (and (not (listp val))
       (sri-km-query `(,VAL |isa| |Constant|))
       (sri-km-query `(|oneof| (|the| |classes| |of| ,VAL)
                      |where| (|has-value| (|the| |categorical-constant-class-of| |of| |It|))))
  )
)

(defun consistent-categ-vals (val-list)
  (if (null val-list)
      t
      (and (first-val-consistent (car val-list) (cdr val-list))
           (consistent-categ-vals (cdr val-list))
      )
  )
)

(defun first-val-consistent (first-val rest-vals)
  (if (null rest-vals)
      t
      (and (val-pair-consistent first-val (car rest-vals))
           (first-val-consistent first-val (cdr rest-vals))
      )
  )
)

(defun val-pair-consistent (v1 v2)
  (cond ((eq v1 v2) t)
        ((sri-km-query `(|oneof| (|the| |all-instances| |of| |Constant-Exclusion-Set|)
                        |where| ((|the| |exclusive-constants| |of| |It|) |subsumes| (:|set| ,V1 ,V2))))
         nil
        )
        (t t)
  )
)


(defun massage-number (valpair)
  (let* ((pairconv (if (equal (third valpair) '|*radian|)
                       (convert-unit-in-little-v-pair valpair '|*degree|)
                       valpair
                   )
         )
         (numpart  (second pairconv))
         (unitpart (third pairconv))
         (unitstr  (if (equal unitpart '|*unity|)
                       ""
                       (km-name unitpart)
                   )
         )
        )
    (if (numberp numpart)
; The following test was to use scientific notation if the value
; was outside some "natural" range. But it seems that there is
; rarely a correct value outside that range, and it tends to print
; nearly-zero numbers as non-zero. So let's try removing the test.
;        (if (and (< (abs numpart) 100000)
;                 (> (abs numpart) 0.1)
;            )
          (let* ((numstr0 (format nil "~,1f" numpart))
                 (numstr (erase-minus-zero (erase-trailing-zeros numstr0)))
                )
            (format nil "~a ~a" numstr unitstr)
          )
;          (format nil "~a ~a" (erase-trailing-zeros 
;                                (format nil "~,1e" numpart)) unitstr)
;        )
        numpart
    )
  )
)

(defun radians2piradians (num)
  (let* ((numconv (/ (mod num (* 2 pi)) pi))
         (numstr0 (format nil "~,3f pi radians" numconv))
        )
    (cond ((equal numstr0 "0.167 pi radians") "pi/6 radians")
          ((equal numstr0 "0.250 pi radians")  "pi/4 radians")
          ((equal numstr0 "0.333 pi radians") "pi/3 radians")
          ((equal numstr0 "0.500 pi radians")   "pi/2 radians")
          ((equal numstr0 "0.667 pi radians") "2pi/3 radians")
          ((equal numstr0 "0.750 pi radians")  "3pi/4 radians")
          ((equal numstr0 "0.833 pi radians") "5pi/6 radians")
          ((equal numstr0 "1.000 pi radians")     "pi radians")
          ((equal numstr0 "1.167 pi radians") "-5pi/6 radians")
          ((equal numstr0 "1.250 pi radians")  "-3pi/4 radians")
          ((equal numstr0 "1.333 pi radians") "-2pi/3 radians")
          ((equal numstr0 "1.500 pi radians")   "-pi/2 radians")
          ((equal numstr0 "1.667 pi radians") "-pi/3 radians")
          ((equal numstr0 "1.750 pi radians")  "-pi/4 radians")
          ((equal numstr0 "1.833 pi radians") "-pi/6 radians")
          ((equal numstr0 "2.000 pi radians")     "0 pi radians")
          ((equal numstr0 "0.000 pi radians")     "0 pi radians")
          (t numstr0)
    )
  )
)

(defun erase-trailing-zeros (numstring)
  (if (or (equal (subseq numstring (- (length numstring) 3)) "d+0")
          (equal (subseq numstring (- (length numstring) 3)) "e+0")
      )
    (erase-point-zero (subseq numstring 0 (- (length numstring) 3)))
    (erase-point-zero numstring)
  )
)

(defun erase-point-zero (numstring)
  (if (equal (subseq numstring (- (length numstring) 2)) ".0")
      (subseq numstring 0 (- (length numstring) 2))
      numstring
  )
)

(defun erase-minus-zero (numstring)
  (if (equal numstring "-0")
      "0"
      numstring
  )
)

(defun wrap-radians (numstring unitstring)
  (if (and (equal numstring "2")
           (equal unitstring "pi radians")
      )
      "0"
      numstring
  )
)

(defun approx= (x y)
  (< (abs (- x y)) 0.000001)
)

(defun all-cards (vals)
  (= (length vals) 
     (length (extract-card-vals vals))))

(defun all-cats (vals)
  (= (length vals) 
     (length (extract-card-vals vals))))

(defun all-scalars (vals)
  (= (length vals) 
     (length (extract-card-vals vals))))


;; tests whether 2 Property-Value list are of the same type
;; used in constraint checking to avoid comparing cardnials with scalars 
(defun value-pair-same-type-p (val1 val2)
  (or (and (all-cards val1) (all-cards val2))
      (and (all-cats val1) (all-cats val2))
      (and (all-scalars val1) (all-scalars val2))))


;; tests whether 2 Property-Values are of the same type
;; used in constraint checking to avoid e.g. comparing cardinals with scalars 
(defun value-pair-same-type-p (val1 val2)
  (or (and (card-valp val1) (card-valp val2))
      (and (scalar-valp val1) (scalar-valp val2))
      (and (cat-valp val1) (cat-valp val2))))

