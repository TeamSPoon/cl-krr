;;
;; $Id: eq-solver-atan-hack.lisp,v 1.8 2007/12/30 21:41:33 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)


(defun debug-test(x y)
  (let* ((backup-kb (get-kb))
	 (*SCENARIO* 
	  `((|_X-Speed| |instance-of| |Speed-Value|)
	    (|_X-Speed| |value| (:|pair| ,x |*meter-per-second|))
	    (|_X-Speed| |initial-x-speed-of| |_Projectile-Move|)
	    (|_Y-Speed| |instance-of| |Speed-Value|)
	    (|_Y-Speed| |value| (:|pair| ,y |*meter-per-second|))
	    (|_Y-Speed| |initial-y-speed-of| |_Projectile-Move|)
	    (|_Projectile-Move| |instance-of| |Projectile-Move|)
	    (|_Direction| |instance-of| |Angle-Value|)
	    (|_Direction| |direction-of| |_Projectile-Move|)))
	(*CONTROLLER-TRIPLE-LIST*      (affix-triple-prefix *SCENARIO*))
	(*CONTROLLER-TRIPLE-INSTANCES* (extract-all-instances-from-triple-list *SCENARIO*)))
    (reset-test-bench)
    (ps-assert-triples *SCENARIO* t)
    (ps-km-query '(|the| |text-gen| |of| (|the| |direction| |of| |_Projectile-Move|)) t)
    (put-kb backup-kb)))

(defun debug-test2(x y)
  (let* ((backup-kb (get-kb))
	 (*SCENARIO* 
	  `((|_X-Speed| |instance-of| |Speed-Value|)
	    (|_X-Speed| |value| (:|pair| ,x |*meter-per-second|))
	    (|_X-Speed| |x-speed-of| |_Velocity-Vector-Value|)
	    (|_Y-Speed| |instance-of| |Speed-Value|)
	    (|_Y-Speed| |value| (:|pair| ,y |*meter-per-second|))
	    (|_Y-Speed| |y-speed-of| |_Velocity-Vector-Value|)
	    (|_Velocity-Vector-Value| |instance-of| |Velocity-Vector-Value|)
	    (|_Direction| |instance-of| |Angle-Value|)
	    (|_Direction| |direction-of| |_Velocity-Vector-Value|)))
	(*CONTROLLER-TRIPLE-LIST*      (affix-triple-prefix *SCENARIO*))
	(*CONTROLLER-TRIPLE-INSTANCES* (extract-all-instances-from-triple-list *SCENARIO*)))
    (reset-test-bench)
    (ps-assert-triples *SCENARIO* t)
    (ps-km-query '(|showme| (|the| |direction| |of| |_Velocity-Vector-Value|)) t)
    (ps-km-query '(|the| |text-gen| |of| (|the| |direction| |of| |_Velocity-Vector-Value|)) t)
    (put-kb backup-kb)))

(defun extract-atan-calls(input)
  (cond ((null input) nil)
	((atom input) nil)
	((listp input) 
	 (cond ((atan-call-p input)
		(append (list input)
			(extract-atan-calls (cdr input))))
	       (t (append
		   (extract-atan-calls (car input))
		   (extract-atan-calls (cdr input))))))))

(defun division-call-p(input)
  (and (listp input)
       (equal (length input) 3)
       (equal (car input) '/)))

(defun atan-call-p(input)
  (or (and (listp input)
	   (equal (length input) 3)
	   (equal (car input) '|atan|))
      (and (listp input)
	   (equal (length input) 2)
	   (equal (car input) '|atan|))))

(defun get-denominator-for-division-call (input)
  (if (division-call-p input)
      (car (last input))))

(defun get-numerator-for-division-call (input)
  (if (division-call-p input)
      (cadr input)))

;;Should this be called only once prior the grand call to solvobjvar?
;;Or should this be called repeatedly after every solvobjvar until it converges?
(defun fix-all-atan-calls(eqns defined)
  (if (fix-all-atan-calls-p)
      (fix-all-atan-calls0 eqns defined)
      (values eqns defined)))

(defun fix-all-atan-calls-p()
  (and (boundp '*TURN-OFF-ATAN-FIX*)
       *TURN-OFF-ATAN-FIX*))
  
(defun fix-all-atan-calls0(eqns defined)
  (let ((atan-fixes (determine-all-fixes-for-atan-calls eqns defined)))
    (if (not (null atan-fixes))
	(values (replace-problematic-atan-calls-with-suggested-fixes
		 atan-fixes
		 eqns)
		(append (mapcar 'second atan-fixes)
			defined))
      (values eqns defined))))

(defun replace-problematic-atan-calls-with-suggested-fixes(fixes eqns)
  (cond ((null fixes) 
	 eqns)
	(t (let* ((target-fix (car fixes))
		  (target-expr (first target-fix))
		  (replacement-sym (car (cadr target-fix))))
	     (replace-problematic-atan-calls-with-suggested-fixes 
	      (cdr fixes)
	      (replace-if-equal target-expr
				replacement-sym
				eqns))))))

(defun determine-all-fixes-for-atan-calls(input defined)
  (remove nil
	  (mapcar #'(lambda(atan-call)
		      (let ((atan-val (fix-atan-call atan-call input defined)))
			(if (not (null atan-val))
			    (list atan-call 
				  (list (gensym) (list atan-val '|*radian|))))))
		  (extract-atan-calls input))))

(defun atan-call-argument-is-division-p(input)
  (if (and (atan-call-p input)
	   (= (length input) 2))
      (let ((atan-argument (cadr input)))
	(division-call-p atan-argument))))

(defun atan-call-two-argument-p(input)
  (and (atan-call-p input)
       (= (length input) 3)))
	
(defun parse-atan-call(input)
  (cond ((atan-call-two-argument-p input)
	 (let ((div-numerator   (nth 1 input))
	       (div-denominator (nth 2 input)))
	   (values div-numerator div-denominator)))
	((atan-call-argument-is-division-p input)
	 (let* ((atan-argument (cadr input))
		(div-numerator   (get-numerator-for-division-call  atan-argument))
		(div-denominator (get-denominator-for-division-call atan-argument)))
	   (values div-numerator div-denominator)))))

(defun fix-atan-call(atan-call eqns defined &optional(verbose nil))
  (if (atan-call-p atan-call)
      (multiple-value-bind 
	(div-numerator div-denominator)
	(parse-atan-call atan-call)
      (let* ((fake-dem-symbol (gensym))
	     (fake-dem-expr   (list '= fake-dem-symbol div-denominator))
	     (fake-num-symbol (gensym))
	     (fake-num-expr   (list '= fake-num-symbol div-numerator))
	     (fake-expr       (cons fake-dem-expr
				    (cons fake-num-expr eqns)))
	     (denominator-val-pair (bps-solvobjvar fake-expr
						   fake-dem-symbol
						   defined))
	     (numerator-val-pair   (bps-solvobjvar fake-expr
						   fake-num-symbol
						   defined))
	     (denominator-val      (car  denominator-val-pair))
	     (denominator-uom      (cadr denominator-val-pair))
	     (numerator-val        (car  numerator-val-pair))
	     (numerator-uom        (cadr numerator-val-pair)))
	(if (and (numberp denominator-val)
		 (numberp numerator-val))
	    (atan numerator-val denominator-val))))))


