;;
;; $Id: controller-multislot-query.lisp,v 1.13 2009/08/27 22:40:09 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Checks if a slot and its inverse is the same as target
(defun abs-slot-equal-p (slot target)
  (or (equal slot               target)
      (equal (invert-slot slot) target)))

(defun is-spatial-dimension-query-p(slot-name frame-instance)
  (cond ((abs-slot-equal-p slot-name '|location|)   frame-instance)
	((abs-slot-equal-p slot-name '|is-at|) frame-instance)
	((abs-slot-equal-p slot-name '|site|) frame-instance)
	((abs-slot-equal-p slot-name '|related-to|) frame-instance)))
  
(defun is-participant-dimension-query-p(slot-name frame-instance)
  (cond 
	;;Pattern for "What is the purpose of X?"
        ((abs-slot-equal-p slot-name '|purpose|) frame-instance)
	
	;;Pattern for "What is the function of X?",
        ;;            "What is the purpose of X?", 
        ;;            "What is the role of X?"
	((abs-slot-equal-p slot-name '|plays|)   frame-instance)

	((abs-slot-equal-p slot-name '|object|)  frame-instance)

	;;Deprecated. Old pattern for "What is the role of X?"
	((abs-slot-equal-p slot-name '|related-to|)
	 (let ((question-inst-type (car (get-concept-for-kb-instance
					 (ps-slot-lookup frame-instance slot-name)))))
	   (cond ((eql question-inst-type '|Role|) frame-instance))))))

(defun is-meronymic-dimension-query-p(slot-name frame-instance)
  (cond 
        ;;Pattern for "What are the parts of X?"
         ((abs-slot-equal-p slot-name '|has-part|)
	  (let ((question-inst-type (car (ps-km-query `(|the| |Structure| ,slot-name |of| ,frame-instance)))))
	    (cond ((not (null question-inst-type ))
		   (car (ps-slot-lookup frame-instance '|is-part-of|)))
		  (t frame-instance))))

	;;Pattern for "What is the structure of X?" and 	
	;;            "what is the element of X?"
	((abs-slot-equal-p slot-name '|element|)    
	 (let ((question-inst-type (car (get-concept-for-kb-instance frame-instance))))
	   (cond ((eql slot-name '|element-of|) ;;Hack to handle bad parse for
		  frame-instance)               ;;"What is the structure of a cell?"
		 ((eql question-inst-type '|Aggregate|) 
		  (car (ps-slot-lookup frame-instance '|element|))))))
		
	;;Deprecated. Old pattern for "What is the structure of X?"
	((abs-slot-equal-p slot-name '|related-to|) 
	 (let ((question-inst-type (car (get-concept-for-kb-instance
					 (ps-slot-lookup frame-instance slot-name)))))
	   (cond ((eql question-inst-type '|Structure|) t))))))

(defun is-agentive-dimension-query-p(slot-name frame-instance)
  (cond ((abs-slot-equal-p slot-name '|agent|)      frame-instance)
	((abs-slot-equal-p slot-name '|donor|)      frame-instance)
	((abs-slot-equal-p slot-name '|purpose|)    frame-instance)
	((abs-slot-equal-p slot-name '|instrument|) frame-instance)))

(defun make-viewpoint-query-slot-for-dimension-participant (frame-instance expected-filler)
  (mapcar #'(lambda(slot)
	      (make-viewpoint-query-slot-entry slot frame-instance expected-filler))
	  (cons '|purpose| 
		(all-instances '|Participant-Relation|))))

(defun make-viewpoint-query-slot-for-dimension-meronymic (frame-instance expected-filler)
  (mapcar #'(lambda(slot)
	      (make-viewpoint-query-slot-entry slot frame-instance expected-filler))
	  (all-instances '|Meronymic-Relation|)))

(defun make-viewpoint-query-slot-for-dimension-spatial (frame-instance expected-filler)
  (mapcar #'(lambda(slot)
	      (make-viewpoint-query-slot-entry slot frame-instance expected-filler))
	  (all-instances '|Spatial-Relation|)))

(defun make-viewpoint-query-slot-for-dimension-agentive (frame-instance expected-filler)
  (mapcar #'(lambda(slot)
	      (make-viewpoint-query-slot-entry slot frame-instance expected-filler))
	  (all-instances '|Agentive-Relation|)))

