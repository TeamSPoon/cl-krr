;; 
;;    $Id: get-ch-ent.lisp,v 1.23 2010/05/19 20:44:51 kbarker Exp $ 
;; 
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defvar *gce-trace* nil)

(defmacro do-while (test &rest body)
   `(do ()
        ((not ,test))
        ,@body))

;; obsolete ??
;; hash with Chemical-Entities indexed on nested-atomic-chemical-formula
(defparameter *nacf-hash* (make-hash-table :test 'string= :size 1024))

;; obsolete ??
;; hash with Chemicals indexed on has-basic-structural-unit
(defparameter *hbsu-hash* (make-hash-table :test 'string= :size 1024))

(defun nested-chf-term (ch-ent)
  (km-unique0 `(|the| |term| |of| 
	 (|the| |nested-atomic-chemical-formula| |of| ,ch-ent))))

;; returns the class of the hbsu of ch 
(defun hbsu-class (ch)
  (let* ((all-hbsu (km0 `(|the| |has-basic-structural-unit| |of| ,CH)))
	 ;; consider only those Chemicals that:
	 ;; - have 1 hbsu 
	 ;; - the hbsu has a meningful nested-atomic-chemical-formula
	 (hbsu (if (and (= (length all-hbsu) 1)
			(has-cf all-hbsu))
		   all-hbsu))
	 (hbsu-classes (km0 `(|the| |instance-of| |of| ,HBSU)))
	 (hbsu-classes-inst (mapcar #'(lambda (c)
					(cons (km-unique0 `(|an| |instance| |of| ,C)) c))
				    hbsu-classes)))	 
    (format *gce-trace* "~%hbsu: ~A, hbsu-class ~A" hbsu hbsu-classes)
    (choose-hbsu-class hbsu-classes-inst)))

;; chooses the instance that has a value for nacf and is the most specific among inst-list
;; 
(defun choose-hbsu-class (pair-list)
  (let ((result-inst nil)
	(result-class nil))
    (dolist (pair pair-list result-class)
	    (let ((inst (car pair))
		  (class (cdr pair)))
	      (if (has-cf inst)
		  (if (null result-inst)
		      (progn
			;; null result => choose this one for now
			(setf result-inst inst)
			(setf result-class class))
		    ;; result not null, find the most specific
		    (if (km-unique0 `(,INST |isa| ,RESULT-CLASS))
			(progn
			  (setf result-inst inst)
			  (setf result-class class)))))))))
	      

;; returns most specific class in classes
(defun most-spec (classes)
  (let ((result (first classes)))
    (dolist (class (cdr classes) result)
	    (if (km-unique0 `(,result |subsumes| ,class))
		(setf result class)))))

(defun has-cf (instance)
  (or (has-nacf instance)
      (has-hcf instance)))

(defun has-nacf (instance)
  (km-unique0 `(|has-value| 
		  (|the| |term| |of| 
		       (|the| |nested-atomic-chemical-formula| |of| ,INSTANCE)))))


(defun has-hcf (instance)
  (km-unique0 `(|has-value| 
		  (|the| |term| |of| 
		       (|the| |has-chemical-formula| |of| ,INSTANCE)))))

;; obsolete
(defun identify-ch-ent-cached (ch-ent)
  (let* ((nested-chf (nchf-to-string (nested-chf-term ch-ent)))
	 (class (gethash nested-chf *nacf-hash*)))
    (if class
	;; hit
	(km-unique0 `(|an| |instance| |of| ,CLASS))
#|      
      ;; miss
      (let* ((new-inst (identify-ch-ent ch-ent))
	     (new-class (km-unique0 `(|the| |instance-of| |of| ,NEW-INST))))
	(if new-inst
	    (progn
	      (setf (gethash nested-chf *nacf-hash*)
		    new-class)
	      new-inst)))
|#
      )))

;; obsolete
(defun identify-ch-ent-class (nchf)
  (gethash (nchf-to-string nchf) *nacf-hash*))


;; input: a seq chemical formula
;; output: an instance of the ChEnt in the KB that has the same seq-chf
(defun init-nacf-hash ()
  (let ((old-ind-classif *indirect-classification*)
	(old-rec-classif *recursive-classification*))
    
    ;; turn off indirect & recursive classification
    (setq *indirect-classification* nil)
    (setq *recursive-classification* nil)
    
    (let* ((subcls '(|Chemical-Entity|)))
      (do-while (not (null subcls))
		(let* ((subcl (first subcls))
		       (inst (km-unique0 `(|an| |instance| |of| ,subcl)))
		       (next (immediate-subclasses subcl))
		       (nested-chf (nested-chf-term inst)))
		  
		  (format *gce-trace* "~%Inspecting: ~A, next ~A" subcl next)
		  
		  (if nested-chf
		      (setf (gethash (nchf-to-string nested-chf) *nacf-hash*)
			    subcl))
		  (if next 
		      (setf subcls (append (cdr subcls) next))
		    (setf subcls (cdr subcls))))))
    
    (setq *indirect-classification* old-ind-classif)
    (setq *recursive-classification* old-rec-classif)))


(defun init-hbsu-hash ()
  (let ((old-ind-classif *indirect-classification*)
	(old-rec-classif *recursive-classification*))
    
    ;; turn off indirect & recursive classification
    (setq *indirect-classification* nil)
    (setq *recursive-classification* nil)
    
    (let* ((subcls '(|Chemical|)))
      (do-while (not (null subcls))
		(let* ((subcl (first subcls))
		       (inst (km-unique0 `(|an| |instance| |of| ,subcl)))
		       (next (immediate-subclasses subcl))
		       (hbsu-class (hbsu-class inst))
		       (old-class (gethash (hbsu-to-string hbsu-class) *hbsu-hash*))
		       (new-class (if old-class 
				      (most-spec (list subcl old-class))
				    subcl)))
		  
		  (format *gce-trace* "~%Inspecting: ~A, next ~A" subcl next)
		  (if old-class 
		      (format *gce-trace* "~%Clash: ~A, old class ~A" subcl old-class))

		  (if (and hbsu-class 
			   (not (eq hbsu-class `|Chemical-Entity|)))
		      (setf (gethash (hbsu-to-string hbsu-class) *hbsu-hash*)
			    new-class))
		  (if next 
		      (setf subcls (append (cdr subcls) next))
		    (setf subcls (cdr subcls))))))
    
    (setq *indirect-classification* old-ind-classif)
    (setq *recursive-classification* old-rec-classif)))

;; obsolete
;; input: a seq chemical formula
;; output: an instance of the ChEnt in the KB that has the same seq-chf
(defun identify-ch-ent (ch-ent)
  (let ((old-ind-classif *indirect-classification*)
	(old-rec-classif *recursive-classification*)
	(reif-inst nil))
    
    ;; turn off indirect & recursive classification
    (setq *indirect-classification* nil)
    (setq *recursive-classification* nil)

    (let* ((subcls (immediate-classes ch-ent))
	   (still-looking t)
	   (nested-chf (nested-chf-term ch-ent))
	   (special-case (identify-ch-ent-special-case nested-chf)))
      (if special-case
	  (progn
	    (setq *indirect-classification* old-ind-classif)
	    (setq *recursive-classification* old-rec-classif)
	    (return-from identify-ch-ent special-case)))
      (do-while (and still-looking subcls)
		(let* ((subcl (first subcls))
		       (inst (km-unique0 `(|an| |instance| |of| ,subcl)))
		       (next (immediate-subclasses subcl)))
		  
		  (format *gce-trace* "~%Inspecting: ~A, next ~A" subcl next)
		  
		  (if (chf-eq nested-chf (nested-chf-term inst))
		      (progn
			(setf still-looking nil)
			(setf reif-inst inst))
		    )
		  (if next 
		      (setf subcls (append (cdr subcls) next))
		    (setf subcls (cdr subcls))))))
    
    (setq *indirect-classification* old-ind-classif)
    (setq *recursive-classification* old-rec-classif)
    reif-inst))

;; obsolete
;; special cases
;; for now only OH-, recognized from HO as well as OH
(defun identify-ch-ent-special-case (nested-chf)
  (if (and (= (length nested-chf) 3)
	   (or (and (eq (third (second nested-chf)) '|H|)
		    (eq (third (third nested-chf)) '|O|))
	       (and (eq (third (second nested-chf)) '|O|)
		    (eq (third (third nested-chf)) '|H|))))
      (km-unique0 '(|an| |instance| |of| |OH-Minus|))))
  


(defun is-water (input)
  (or (chf-eq input `(:|seq| (:|pair| 2 |H|) (:|pair| 1 |O|)))
      (chf-eq input `(:|seq| (:|pair| 1 |H-Plus|) (:|pair| 1 |OH-Minus|)))
      (chf-eq input `(:|seq| (:|pair| 1 |H|) (:|pair| 1 |O|) (:|pair| 1 |H|)))))


(defun is-ha (input)
  (or (chf-eq input `(:|seq| (:|pair| 1 |H|) (:|pair| 1 |A|)))
      (chf-eq input `(:|seq| (:|pair| 1 |H-Plus|) (:|pair| 1 |A|)))))

;; (:seq (:pair # H) (:pair # X))
(defun is-anonymous-acid (input)
  (KM-UNIQUE0 
   `(((|the2| |of| (|the1| |of| ,INPUT)) = |H-Plus|)
       |and|
       (|the| |all-superclasses| |of|
	    (|the2| |of| (|the2| |of| ,INPUT)))
       = (|Thing|))))

(defun hbsu-to-string (x)
  (string-upcase (format nil "~A" x)))

;; obsolete
(defun is-weak-base (input)
  (or (chf-eq input '(:|seq| (:|pair| 1 |N|) (:|pair| 3 |H|)))
      (chf-eq input '(:|seq| (:|pair| 6 |C|) 
			     (:|pair| 5 |H|)
			     (:|pair| 1 |N|)
			     (:|pair| 2 |H|)))
      (chf-eq input '(:|seq| (:|pair| 2 
				    (:|seq| (:|pair| 1 |C|)
					  (:|pair| 3 |H|)))
			     (:|pair| 1 |N|)
			     (:|pair| 1 |H|)))
      (chf-eq input '(:|seq| (:|pair| 2 |C|)
			     (:|pair| 5 |H|)
			     (:|pair| 1 |N|)
			     (:|pair| 2 |H|)))
      (chf-eq input '(:|seq| (:|pair| 2 |H|)
			     (:|pair| 1 |N|)
			     (:|pair| 1 |N|)
			     (:|pair| 2 |H|)))
      (chf-eq input '(:|seq| (:|pair| 1 |H|)
			    (:|pair| 1 |O|)
			    (:|pair| 1 |N|)
			    (:|pair| 2 |H|)))
      (chf-eq input '(:|seq| (:|pair| 1 |C|)
                            (:|pair| 3 |H|)
                            (:|pair| 1 |N|)
                            (:|pair| 2 |H|)))
      (chf-eq input '(:|seq| (:|pair| 5 |C|)
                            (:|pair| 5 |H|)
                            (:|pair| 1 |N|)))
      (chf-eq input '(:|seq| (:|pair| 3
				    (:|seq| (:|pair| 1 |C|)
					  (:|pair| 3 |H|)))
                            (:|pair| 1 |N|)))
      )

  )

;; input: a non reified Chemical with some has-basic-structural-unit
;; output: an instance of the Ch in the KB that has the same has-basic-structural-unit class
(defun identify-ch-cached (ch)
  (let* ((hbsu-class (hbsu-to-string (hbsu-class ch)))
	 (class (gethash hbsu-class *hbsu-hash*)))
    (if class
	;; hit
	(km-unique0 `(|an| |instance| |of| ,CLASS))
      )))

#|
;; temporary fix 
;; by pzyeh during Halo Pilot??
;; numerical precision was causing some odd failure somewhere. 
;; Unsure if still necessary. But commented out to remove compiler warning. [01Feb08]
(DEFUN NUMBER-EQ (N1 N2) (AND (NUMBERP N1) (NUMBERP N2) (< (ABS (-
N1 N2)) 1E-20)))
|#

