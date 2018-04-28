
;; $Id: user-defined-set-aggregation-functions.lisp,v 1.10 2010/05/19 20:44:51 kbarker Exp $

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; function used by set aggregation slot property-sum
;; (see user-defined-set-aggregation-slots.km)
(defun property-sum-function (property-value-list)
  (if (property-value-list-p property-value-list)
      (cond
       ((null property-value-list)       nil)
       ((null (cdr property-value-list)) property-value-list)
       (t (property-plus (car property-value-list)
			 (car (property-sum-function (cdr property-value-list)))))
       )
  )
)

;; function used by set aggregation slot property-mult
;; (see user-defined-set-aggregation-slots.km)
(defun property-mult-function (property-value-list)
 (if (property-value-list-p property-value-list)
     (cond
      ((null property-value-list)       nil)
      ((null (cdr property-value-list)) property-value-list)
      (t (property-mult (car property-value-list)
                        (car 
			 (property-mult-function 
			  (cdr
			   property-value-list)))))
      )
 )
)

(defun property-value-list-p (input)
  (cond ((null input) t)
	((and (listp input)
	      (isa (car input) '|Property-Value|)) 
	 (property-value-list-p (cdr input)))
	(t nil)))

(defun testcase()
(progn
  (testcase1)
(testcase2)
(testcase3))
)

(defun testcase1()
;;Test case #1. Should return 6 *meter
(progn 
  (km '(|*Length-Value1| |has| 
			 (|instance-of| (|Length-Value|))
			 (|value| ((:|pair| 1 |*meter|)))))

  (km '(|*Length-Value2| |has| 
			 (|instance-of| (|Length-Value|))
			 (|value| ((:|pair| 2 |*meter|)))))

  (km '(|*Length-Value3| |has| 
			(|instance-of| (|Length-Value|))
			(|value| ((:|pair| 3 |*meter|)))))

  (format t "~a" (km '(|the| |value| |of| (|the| |property-sum| |of| (:|set| |*Length-Value1|)))))
  (format t "~a" (km '(|the| |value| |of| (|the| |property-sum| |of| (:|set| |*Length-Value1| |*Length-Value2|)))))
  (format t "~a" (km '(|the| |value| |of| (|the| |property-sum| |of| (:|set| |*Length-Value1| |*Length-Value2| |*Length-Value3|)))))
))

(defun testcase2()
;;Test case #2. Should return nil
(km '(|the| |property-sum| |of| (|a| |Thing|))))

(defun testcase3()
;;Test case #3. Should return 6 *cubic-meter
(progn 
  (km '(|*Length-Value1| |has| 
			 (|instance-of| (|Length-Value|))
			 (|value| ((:|pair| 1 |*meter|)))))

  (km '(|*Length-Value2| |has| 
			 (|instance-of| (|Length-Value|))
			 (|value| ((:|pair| 2 |*meter|)))))

  (km '(|*Length-Value3| |has| 
			(|instance-of| (|Length-Value|))
			(|value| ((:|pair| 3 |*meter|)))))

  (format t "~a" (km '(|the| |value| |of| (|the| |property-mult| |of| (:|set| |*Length-Value1|)))))
  (format t "~a" (km '(|the| |value| |of| (|the| |property-mult| |of| (:|set| |*Length-Value1| |*Length-Value2|)))))
  (format t "~a" (km '(|the| |value| |of| (|the| |property-mult| |of| (:|set| |*Length-Value1| |*Length-Value2| |*Length-Value3|)))))
))
