;;
;; $Id: user-defined-operators.lisp,v 1.29 2010/05/19 20:44:51 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;; File: user-defined-operators.lisp
;;; Author: Peter Clark
;;; Date: May 2003
;;; Purpose: Simple illustration of user-defined infix operators in KM.

#|
Consider a user-defined infix operator called p-equiv:

KM> (_Value1 p-equiv _Value2)
(t)

p-equiv takes _Value1 and _Value2 and returns some (arbitrary) result
based on those values. This result is computed using a user-defined function,
e.g.:	(defun p-equiv (a b) (equal a b))

To declare user-defined functions, set the global variable 

	*user-defined-infix-operators* 

as illustrated at the end of this file. The value should be a list of
pairs, where a pair is
	(<infix-operator-name> <Lisp-function-implementing-it>)
|#

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;; ======================================================================
;;;		EXAMPLE INFIX OPERATOR IMPLEMENTATION
;;; ======================================================================

#|
Example implementation of a user-defined infix operator:

INPUT is exactly TWO values, which will be fully-evaluated KM expressions (typically two
instances, though could be (:set ...) etc constructs if the user passes those).
RESULT must be either a single or a list of KM values.

You can recursively call KM within these functions, but use the functions
	(km0 <expr>)		; returns a list of values
	(km-unique0 <expr>)	; returns either NIL or a single value
NOTE: DON'T use the functions (km ...) or (km-unique ...).
|#

(defun p-equiv (a b)
  (km-format t "Calling (p-equiv ~a ~a)...~%" a b)
  (equal a b))

(defun my-older-than (a b)
  (km-format t "Calling (my-older-than ~a ~a)...~%" a b)
  (let ( (age1 (km-unique0 `(|the| |age| |of| ,A)))		; in this toy, return a number
	 (age2 (km-unique0 `(|the| |age| |of| ,B))) )
    (cond ((and (numberp age1) (numberp age2))
	   (cond ((> age1 age2) '|*Yes|)
		 ((<= age1 age2) '|*No|)))
	  (t '|*DontKnow|))))

;;; ======================================================================
;;;		INFIX OPERATOR IMPLEMENTATIONS
;;; ======================================================================

;;; ======================================================================
;;; Definitions for property-lte and property-gte. These two functions
;;; are defined in terms of property-lt, property-gt, and property-eq.
;;; ======================================================================
(defun property-lte (val1 val2) (or (property-eq val1 val2) (property-lt val1 val2)))
(defun property-gte (val1 val2) (or (property-eq val1 val2) (property-gt val1 val2)))


;;; ======================================================================
;;; Definitions (and supporting functions) for property-eq.
;;; ======================================================================
(defun property-eq (val1 val2)
  (let ((cat-val1    (cat-valp 	  val1))
	(cat-val2    (cat-valp 	  val2))
	(card-val1   (card-valp   val1))
	(card-val2   (card-valp   val2))
	(scalar-val1 (scalar-valp val1))
	(scalar-val2 (scalar-valp val2)))
    (cond
      ((and card-val1 card-val2 scalar-val1 scalar-val2)
       (and (property-eq-card val1 val2) (property-eq-scalar val1 val2)))
      ((and card-val1 card-val2)
       (property-eq-card val1 val2))
      ((and scalar-val1 scalar-val2)
       (property-eq-scalar val1 val2))
      ((and cat-val1 cat-val2)
       (property-eq-cat val1 val2))
      (t nil))))

(defun property-eq-cat (value-instance1 value-instance2)
  (let ((vals1 (extract-cat-vals (km0 `(|the| |value| |of| ,value-instance1))))
	(vals2 (extract-cat-vals (km0 `(|the| |value| |of| ,value-instance2)))))
    (cond
      ((and vals1 vals2 (null (set-difference vals1 vals2 :test #'equal)))
       t)
      (t nil))))

(defun property-eq-card (value-instance1 value-instance2)
  (let ((vals1 (extract-card-vals (km0 `(|the| |value| |of| ,value-instance1))))
	(vals2 (extract-card-vals (km0 `(|the| |value| |of| ,value-instance2))))
	val1 UoM1 val2 UoM2 UoC)
    (cond
      ((and (null vals1) (null vals2))
       t)
      ((and vals1 vals2 
	    (consistent-card-vals vals1) 
	    (consistent-card-vals vals2))
       (progn
	 (setf val1 (second (first vals1)))
	 (setf val2 (second (first vals2)))
	 (setf UoM1 (third  (first vals1)))
	 (setf UoM2 (third  (first vals2)))
	 (setf isTemperatureUnit (and (basictempunit UoM1) 
				      (basictempunit UoM2)))
	 (setf UoC (my-glconvertunit UoM2 UoM1))
	 (cond (isTemperatureUnit    ;;Special case for temperature values
		(= (tempconvert (list '* val1 UoM1) UoM2) val2))
	       ((and UoC (km0 `((,UoC * ,val2) = ,val1))) t)  ;;All other dimensions
	       (t nil))))
      (t nil))))	

(defun property-eq-scalar (value-instance1 value-instance2)
  (let ((vals1 (extract-scalar-vals (km0 `(|the| |value| |of| ,value-instance1))))
	(vals2 (extract-scalar-vals (km0 `(|the| |value| |of| ,value-instance2)))))
    (cond
      ((and (null vals1) (null vals2))
       t)
      ((and vals1 vals2 (consistent-scalar-vals vals1) (consistent-scalar-vals vals2))
       (if (intersection vals1 vals2 :test #'equal) t nil))
      (t nil))))


;;; ======================================================================
;;; Definitions (and supporting functions) for property-gt.
;;; ======================================================================
(defun property-gt (val1 val2)
  (let ((card-val1   (card-valp   val1))
        (card-val2   (card-valp   val2))
        (scalar-val1 (scalar-valp val1))
        (scalar-val2 (scalar-valp val2)))
    (cond
      ((and card-val1 card-val2)
       (property-gt-card val1 val2))
      ((and scalar-val1 scalar-val2)
       (property-gt-scalar val1 val2))
      (t nil))))

(defun property-gt-card (value-instance1 value-instance2)
  (let ((vals1 (extract-card-vals (km0 `(|the| |value| |of| ,value-instance1))))
        (vals2 (extract-card-vals (km0 `(|the| |value| |of| ,value-instance2))))
        val1 UoM1 val2 UoM2 UoC)
    (cond
      ((and (null vals1) (null vals2))
       t)
      ((and vals1 
	    vals2 
	    (consistent-card-vals vals1) 
	    (consistent-card-vals vals2))
       (progn
	 (setf val1 (second (first vals1)))
	 (setf val2 (second (first vals2)))
	 (setf UoM1 (third  (first vals1)))
	 (setf UoM2 (third  (first vals2)))
	 (setf isTemperatureUnit (and (basictempunit UoM1) 
				      (basictempunit UoM2)))
	 (setf UoC (my-glconvertunit UoM2 UoM1))
	 (cond (isTemperatureUnit    ;;Special case for temperature values
		(> (tempconvert (list '* val1 UoM1) UoM2) val2))
	       ((and UoC (km0 `((,UoC * ,val2) < ,val1))) t)
	       (t nil)))
       )
      (t nil))))

(defun property-gt-scalar (value-instance1 value-instance2)
  (let ((vals1 (extract-scalar-vals (km0 `(|the| |value| |of| ,value-instance1))))
	(vals2 (extract-scalar-vals (km0 `(|the| |value| |of| ,value-instance2)))))
    (cond
      ((and (null vals1) (null vals2)) 
       t)
      ((and vals1 vals2 (consistent-scalar-vals vals1) (consistent-scalar-vals vals2))
       (property-gt-scalar0 vals1 vals2))
      (t nil))))

(defun property-gt-scalar0 (vals1 vals2)
  (let ((consistent t) (no-comp t) pos1 pos2 common-class)
    (dolist (val1 vals1)
      (dolist (val2 vals2)
	(setf common-class nil)
	(setf common-class 
	      (intersection 
		(km0 `(|the| |instance-of| |of| ,(second val1)))
		(km0 `(|the| |instance-of| |of| ,(second val2)))
		:test #'equal))
	(if (and (equal (third val1) (third val2)) common-class)
	    (progn 
	      (setf no-comp nil)
	      (setf pos1 (look-up-scalar-val (second val1) (first common-class)))
	      (setf pos2 (look-up-scalar-val (second val2) (first common-class)))
	      (if (or (null pos1) (null pos2) (>= pos2 pos1))
		  (progn 
		    (setf consistent nil)
		    (return))))))
      (if (not consistent) (return))) 
    (if no-comp nil consistent)))

;;; ======================================================================
;;; Definition for property-lt.
;;; ======================================================================
(defun property-lt (value-instance1 value-instance2)
  (property-gt value-instance2 value-instance1))


;;; ======================================================================
;;; Auxiliary functions.
;;; ======================================================================
(defun look-up-scalar-val (val ref-class)
  (let* ((val-scale (car (find-applicable-scale val ref-class)))
	 (scales (first (km0 `(|the| |element| |of| ,val-scale)))))
    (position val scales)))

(defun find-applicable-scale(val ref-class)
  (remove nil
	  (mapcar #'(lambda(x) 
		      (if (and (member val (car (km `(|the| |element| |of| ,x))))
			       (member ref-class (km `(|the| |element-type| |of| ,x))))
			  x))
		  (km `(|the| |all-instances| |of| |Scale|)))))

(defun consistent-scalar-vals (vals)
  (let ((consistent t)
	(temp-vals (rest vals)))
    (dolist (val vals)
      (dolist (tval temp-vals)
	(if (and (equal (third tval) (third val)) (not (equal (second val) (second tval))))
	    (progn 
	      (setf consistent nil)
	      (return))))
      (if (not consistent) 
	  (return) 
	  (setf temp-vals (rest temp-vals))))
    consistent))

(defun card-valp   (value-instance)
  (extract-card-vals 	 
   (km0 `(|the| |value| |of| ,value-instance))))

(defun cat-valp    (value-instance) 
  (extract-cat-vals    
   (km0 `(|the| |value| |of| ,value-instance))))

(defun scalar-valp (value-instance) 
  (extract-scalar-vals 
   (km0 `(|the| |value| |of| ,value-instance))))

(defun extract-scalar-vals (vals)
  (let (result)
    (dolist (x vals result)
      (if (and (consp x) (not (numberp (second x))))
          (setf result (cons x result))))))

(defun extract-cat-vals (vals)
  (let (result)
    (dolist (x vals result)
      (if (atom x) (setf result (cons x result))))))

;;; ======================================================================
;;; DECLARATION OF THE TEMPORAL OPERATORS.
;;; ======================================================================

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Top level functions to interface with KM.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun time-gte (time-instance1 time-instance2) 
  (or (time-eq time-instance1 time-instance2) (time-gt time-instance1 time-instance2)))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun time-lte (time-instance1 time-instance2) 
  (or (time-eq time-instance1 time-instance2) (time-lt time-instance1 time-instance2)))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun time-eq (time-instance1 time-instance2)
  (let ((time-vals1 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE1)))
	(time-vals2 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE2)))
	time-vals1-canon
	time-vals2-canon)
    (cond
      ((and time-vals1 time-vals2)
	 (setf time-vals1-canon (canonicalize-time-value time-vals1))
	 (setf time-vals2-canon (canonicalize-time-value time-vals2))
       (and (equal (length time-vals1-canon) (length time-vals2-canon))
	    (null (set-difference time-vals1-canon time-vals2-canon :test #'equal))))
      (t nil))))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun time-gt (time-instance1 time-instance2)
  (let ((time-vals1 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE1)))
	(time-vals2 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE2))))
    (comp-time time-vals1 time-vals2 #'>)))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun time-lt (time-instance1 time-instance2)
  (let ((time-vals1 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE1)))
	(time-vals2 (km0 `(|the| |time-value| |of| ,TIME-INSTANCE2))))
    (comp-time time-vals1 time-vals2 #'<)))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions that do the real work.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; Function to compare two time values based on a specified comparison
;; operator.
;;-----------------------------------------------------------------------
(defun comp-time (time-vals1 time-vals2 fn-op)
  (let ((time-val1-canon (canonicalize-time-value time-vals1))
	(time-val2-canon (canonicalize-time-value time-vals2)))
    (cond
      ((and (find-time-value time-val1-canon '|*year|) (find-time-value time-val2-canon '|*year|))
       (comp-year time-val1-canon time-val2-canon fn-op))
      ((and (find-time-value time-val1-canon '|*month|) (find-time-value time-val2-canon '|*month|))
       (comp-month time-val1-canon time-val2-canon fn-op))
      ((and (find-time-value time-val1-canon '|*day|) (find-time-value time-val2-canon '|*day|))
       (comp-day time-val1-canon time-val2-canon fn-op))
      ((and (find-time-dow-value time-val1-canon) (find-time-dow-value time-val2-canon))
       (comp-dow time-val1-canon time-val2-canon fn-op))
      ((and (find-time-value time-val1-canon '|*hour|) (find-time-value time-val2-canon '|*hour|))
       (comp-hour time-val1-canon time-val2-canon fn-op))
      ((and (find-time-value time-val1-canon '|*minute|) (find-time-value time-val2-canon '|*minute|))
       (comp-minute time-val1-canon time-val2-canon fn-op))
      ((and (find-time-value time-val1-canon '|*second|) (find-time-value time-val2-canon '|*second|))
       (comp-second time-val1-canon time-val2-canon fn-op))
      (t nil))))

;;-----------------------------------------------------------------------
;; For comparing years. Handles Cases A-D.
;;-----------------------------------------------------------------------
(defun comp-year (time-vals1 time-vals2 fn-op)
  (let ((time1-year      (find-time-value time-vals1 '|*year|)) 
        (time2-year      (find-time-value time-vals2 '|*year|))
	(time1-has-month (find-time-value time-vals1 '|*month|))
	(time2-has-month (find-time-value time-vals1 '|*month|)))  
    (cond
      ;; Just to be certain, we'll check again to see that there is a year value.
      ((or (not time1-year) (not time2-year)) nil)
      ((funcall fn-op (second time1-year) (second time2-year)) t)
      ((and time1-has-month time2-has-month (= (second time1-year) (second time2-year)))
       (comp-month time-vals1 time-vals2 fn-op))
      (t nil))))

;;-----------------------------------------------------------------------
;; For comparing months. Handles Cases E-G.
;;-----------------------------------------------------------------------
(defun comp-month (time-vals1 time-vals2 fn-op)
  (let ((time1-month   (find-time-value time-vals1 '|*month|)) 
        (time2-month   (find-time-value time-vals2 '|*month|))
	(time1-has-day (find-time-value time-vals1 '|*day|))
	(time2-has-day (find-time-value time-vals1 '|*day|)))  
    (cond
      ((or (not time1-month) (not time2-month)) nil)
      ((funcall fn-op (second time1-month) (second time2-month)) t)
      ((and time1-has-day time2-has-day (= (second time1-month) (second time2-month)))
       (comp-day time-vals1 time-vals2 fn-op))
      (t nil))))

;;-----------------------------------------------------------------------
;; For comparing days. Handles Cases H and I.
;;-----------------------------------------------------------------------
(defun comp-day (time-vals1 time-vals2 fn-op)
  (let ((time1-day    (find-time-value time-vals1 '|*day|)) 
        (time2-day    (find-time-value time-vals2 '|*day|))
	(time1-has-hr (find-time-value time-vals1 '|*hour|))
	(time2-has-hr (find-time-value time-vals1 '|*hour|)))  
    (cond
      ((or (not time1-day) (not time2-day))
       nil)
      ;; We only need to check the hour, since the time values
      ;; will be canonicalized.
      ((funcall fn-op (second time1-day) (second time2-day)) t)
      ((and time1-has-hr time2-has-hr (= (second time1-day) (second time2-day)))
       (comp-hour time-vals1 time-vals2 fn-op))
      (t nil))))

;;-----------------------------------------------------------------------
;; For comparing days. Handles Cases J and K.
;;-----------------------------------------------------------------------
(defun comp-dow (time-vals1 time-vals2 fn-op)
  (let ((time1-dow    (find-time-dow-value time-vals1)) 
        (time2-dow    (find-time-dow-value time-vals2))
	(time1-has-hr (find-time-value time-vals1 '|*hour|))
	(time2-has-hr (find-time-value time-vals1 '|*hour|)))  
    (cond
      ((or (not time1-dow) (not time2-dow)) nil)
      ;; We only need to check the hour, since the time values
      ;; will be canonicalized.
      ((and time1-has-hr time2-has-hr (eql time1-dow time2-dow))
       (comp-hour time-vals1 time-vals2 fn-op))
      ((and (eql fn-op #'<) (dow-lt time1-dow time2-dow)) t)
      ((and (eql fn-op #'>) (dow-gt time1-dow time2-dow)) t)
      (t nil))))

;;-----------------------------------------------------------------------
;; For comparing hours. Handles Case L.
;;-----------------------------------------------------------------------
(defun comp-hour (time-vals1 time-vals2 fn-op)
  (let ((time1-hr (find-time-value time-vals1 '|*hour|))
 	(time2-hr (find-time-value time-vals2 '|*hour|)))
    (cond
      ((or (not time1-hr) (not time2-hr))
       nil)  
      ((= (second time1-hr) (second time2-hr))
       (comp-minute time-vals1 time-vals2 fn-op))
      ((not (eql fn-op #'=)) 
       (funcall fn-op (second time1-hr) (second time2-hr))))))

;;-----------------------------------------------------------------------
;; For comparing minutes. Handles Case M.
;;-----------------------------------------------------------------------
(defun comp-minute (time-vals1 time-vals2 fn-op)
  (let ((time1-min (find-time-value time-vals1 '|*minute|))
 	(time2-min (find-time-value time-vals2 '|*minute|)))
    (cond
      ((or (not time1-min) (not time2-min))
       nil)  
      ((= (second time1-min) (second time2-min))
       (comp-second time-vals1 time-vals2 fn-op))
      ((not (eql fn-op #'=)) 
       (funcall fn-op (second time1-min) (second time2-min))))))

;;-----------------------------------------------------------------------
;; For comparing seconds. Handles Case N.
;;-----------------------------------------------------------------------
(defun comp-second (time-vals1 time-vals2 fn-op)
  (let ((time1-sec (find-time-value time-vals1 '|*second|))
 	(time2-sec (find-time-value time-vals2 '|*second|)))
    (cond
      ((and time1-sec time2-sec)
       (funcall fn-op (second time1-sec) (second time2-sec)))
      (t nil))))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Functions to compare day of week.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;; In the interest of time, it's simplier to just hard code these!
;;-----------------------------------------------------------------------
(defun dow-lt (day1 day2)
  (cond
    ((eql day1 '|*Sunday|)    (member day2 '(|*Monday|)))
    ((eql day1 '|*Monday|)    (member day2 '(|*Tuesday| |*Wednesday| |*Thursday|)))
    ((eql day1 '|*Tuesday|)   (member day2 '(|*Wednesday| |*Thursday| |*Friday|)))
    ((eql day1 '|*Wednesday|) (member day2 '(|*Thursday| |*Friday|))) 
    ((eql day1 '|*Thursday|)  (member day2 '(|*Friday| |*Saturday|)))
    ((eql day1 '|*Friday|)    (member day2 '(|*Saturday| |*Sunday|))) 
    ((eql day1 '|*Saturday|)  (member day2 '(|*Sunday|)))
    (t nil)))

(defun dow-gt (day1 day2)
  (cond
    ((eql day1 '|*Sunday|)    (member day2 '(|*Friday| |*Saturday|)))
    ((eql day1 '|*Monday|)    (member day2 '(|*Sunday|)))
    ((eql day1 '|*Tuesday|)   (member day2 '(|*Monday|)))
    ((eql day1 '|*Wednesday|) (member day2 '(|*Monday| |*Tuesday|))) 
    ((eql day1 '|*Thursday|)  (member day2 '(|*Monday| |*Tuesday| |*Wednesday|)))
    ((eql day1 '|*Friday|)    (member day2 '(|*Tuesday| |*Wednesday| |*Thursday|))) 
    ((eql day1 '|*Saturday|)  (member day2 '(|*Thursday| |*Friday|)))
    (t nil)))


;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Auxiliary functions.
;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun canonicalize-time-value (time-values)
  (let ((canonical-form time-values)
	(has-hour (find-time-value time-values '|*hour|))
	(has-min  (find-time-value time-values '|*minute|)))
    (if (and has-hour (not has-min))
	(setf canonical-form (cons '(:|pair| 0 |*minute|) canonical-form)))
    (if (and (or has-hour has-min) (not (find-time-value time-values '|*second|)))
	(setf canonical-form (cons '(:|pair| 0 |*second|) canonical-form)))
    canonical-form))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun find-time-value (time-values uom-or-constant)
  (let (result)
    (dolist (time-value time-values)
      (if (or (and (consp time-value) (equal (third time-value) uom-or-constant))
	      (and (atom  time-value) (equal time-value uom-or-constant)))
	  (setf result time-value))
      (if result (return)))
    result))

;;-----------------------------------------------------------------------
;;-----------------------------------------------------------------------
(defun find-time-dow-value (time-values)
  (let (result)
    (dolist (time-value time-values)
      (if (and (atom time-value) 
	       (member time-value '(|*Sunday| |*Monday| |*Tuesday| |*Wednesday| |*Thursday| |*Friday| |*Saturday|)))
	  (setf result (cons time-value result))))
    (if (> (length result) 1) nil (first result))))



;;; ======================================================================
;;;		DECLARATION OF INFIX OPERATORS TO KM
;;; ======================================================================
#|
Should be a list of pairs: 
	FIRST element is the KM infix operator. 
	SECOND element is the Lisp implementation of that operator (a Lisp function).
Note the quoting-unquoting. This means this setq must come after the function
definitions themselves.
|#

#| for demo
(setq *user-defined-infix-operators*
      `((|p-equiv| ,#'p-equiv)
        (|my-older-than| ,#'my-older-than)
))
|#

(setq *user-defined-infix-operators*
      (append *user-defined-infix-operators*
	      `((|property-eq| ,#'property-eq)
		(|property-gt| ,#'property-gt)
		(|property-lt| ,#'property-lt)
		(|property-gte| ,#'property-gte)
		(|property-lte| ,#'property-lte)
		(|time-gte| 	,#'time-gte)
		(|time-lte| 	,#'time-lte)
		(|time-eq| 	,#'time-eq)
		(|time-gt|	,#'time-gt)
		(|time-lt| 	,#'time-lt))))

(defun CLIB-same-type(x y)
  (let ((x-instance-of (car (km0 `(|the| |instance-of| |of| ,x))))
	(y-instance-of (car (km0 `(|the| |instance-of| |of| ,y)))))
    (if (equal x-instance-of y-instance-of)
	x)))

(setq *user-defined-infix-operators*
      (append *user-defined-infix-operators* 
	      `((|same-type| ,#'CLIB-same-type))))


#|
 ======================================================================
		DEMO
 ======================================================================

KM> (1 p-equiv 1)
Calling (p-equiv 1 1)...	; print statement is in the code definitions above, just for illustration purposes
(T)

KM> (1 p-equiv 2)
Calling (p-equiv 1 2)...
NIL

KM> (*Fred has (age (20)))
KM> (*Joe has (age (19)))
KM> (*Fred my-older-than *Joe)
Calling (my-older-than *Fred *Joe)...
(*Yes)

KM> (*Joe my-older-than *Fred)
Calling (my-older-than *Joe *Fred)...
(*No)

KM> (*Joe my-older-than *Sue)
Calling (my-older-than *Joe *Sue)...
(*DontKnow)

;; PV examples.
KM> (a Size-Value with
       (value (*big *small *huge))) 

KM> (a Size-Value with
       (value ((:pair *little Tree)
	       (:pair *little Dogs)
	       (:pair *gigantic Apples))))

KM> (a Size-Value with
       (value ((:pair *gigantic Tree)
	       (:pair *gigantic Dogs))))

(a Mass-Value with
   (value ((:pair 1000 *gram)
	   (:pair 1 *kilogram))))

(a Mass-Value with
   (value ((:pair 1.2 *kilogram))))

(a Length-Value with
   (value ((:pair 1000 *meter)
	   (:pair *short Tree))))

(a Length-Value with
   (value ((:pair *long Tree))))

(a Length-Value with
   (value ((:pair *long Pen))))
	   
|#






