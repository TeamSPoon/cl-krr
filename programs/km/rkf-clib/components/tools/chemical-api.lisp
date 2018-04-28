;;
;; $Id: chemical-api.lisp,v 1.51 2010/05/19 20:44:51 kbarker Exp $
;;


(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;##################################################################
;;; compute chemicals based on defs
;;;##################################################################
(defvar *chemical-recognition-debug* nil)

;; chemicals that are known to not have a chemical formula
;; will not try to add them again to *chemical-defs-table*
(defvar *known-chemicals-with-no-formula* nil)


;;; each entry is of the form 
;;; (
;;;  1. frame-name 
;;;  2. (name1-str name2-str ...) 
;;;  3. has-chemical-formula 
;;;  4. nested-atomic-chemical-formula 
;;;  5. atomic-chemical-formula 
;;;  6. charge 
;;;  7. acid name
;;;  8. molecular name
;;; )

;; chem def table accessor functions
(defun chem-def-entry-class-name (entry)
  (first entry))

(defun chem-def-entry-name (entry)
  (second entry))

(defun chem-def-entry-has-chf (entry)
  (third entry))

(defun chem-def-entry-nacf (entry)
  (fourth entry))

(defun chem-def-entry-acf (entry)
  (fifth entry))

(defun chem-def-entry-charge (entry)
  (sixth entry))

(defun chem-def-entry-charge-value (entry)
  (second (chem-def-entry-charge entry)))

(defun chem-def-entry-acid-name (entry)
  (seventh entry))

(defun chem-def-entry-molecular-name (entry)
  (eighth entry))

;; ===================================================
;;
;; TODO: change to use &key
 
(defun compute-non-ion-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge &key(whole-entry nil) (acid-name nil) (molecular-name)) 
  (compute-chemical-from-defs name has-chem-form nested-atom-chem-form atom-chem-form charge 
			      :acid-name acid-name :molecular-name molecular-name 
			      :instance t 
			      :test #'(lambda (entry) (and (chem-def-entry-charge entry)
							   (= (second (chem-def-entry-charge entry)) 0)))
			      :whole-entry whole-entry))

(defun compute-ion-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge &key(whole-entry nil) (acid-name nil) (molecular-name))
  (compute-chemical-from-defs name has-chem-form nested-atom-chem-form atom-chem-form charge
			      :acid-name acid-name :molecular-name molecular-name 
			      :instance t 
			      :test #'(lambda (entry) (and (chem-def-entry-charge entry)
							   (not (= (second (chem-def-entry-charge entry)) 0))))
			      :whole-entry whole-entry))

(defun compute-cation-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge &key(whole-entry nil) (acid-name nil) (molecular-name))
  (compute-chemical-from-defs name has-chem-form nested-atom-chem-form atom-chem-form charge
			      :acid-name acid-name :molecular-name molecular-name 
			      :instance t 
			      :test #'(lambda (entry) (and (chem-def-entry-charge entry)
							   (> (second (chem-def-entry-charge entry)) 0)))
			      :whole-entry whole-entry))

(defun compute-anion-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge &key(whole-entry nil) (acid-name nil) (molecular-name))
  (compute-chemical-from-defs name has-chem-form nested-atom-chem-form atom-chem-form charge
			      :acid-name acid-name :molecular-name molecular-name 
			      :instance t 
			      :test #'(lambda (entry) (and (chem-def-entry-charge entry)
							   (< (second (chem-def-entry-charge entry)) 0)))
			      :whole-entry whole-entry))


;; ============================================================================================
;;
;;; [dgt 6/13/08] - created version for SRI wrapped with with-standard-aura-parameters
;;; This CAN be called in *Global
;;; input: parts, or formula or formula and charge or name
;;; output: one or more
;;; - instance of the particular chemical-entity type or the class itself (depending on :instance nil/t)
;;; - or the whole entry matching (if :whole-entry is t)

(defun sri-compute-chemical-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge &key (instance t) (whole-entry nil) (acid-name nil) (molecular-name))
  (with-standard-bps-parameters ()
     (compute-chemical-from-defs name has-chem-form nested-atom-chem-form atom-chem-form charge 
				 :instance instance :whole-entry whole-entry :try-expand t)))
;;
;; ============================================================================================

;; because atomic-chemical does not care about the order in the seq
;; we need to use set comparison for equality
;; example: (atomic-chemical-equal '(:|seq| (:|pair| 1 C) (:|pair| 4 H) (:|pair| 1 O)) '(:|seq| (:|pair| 1 C) (:|pair| 4 H) (:|pair| 1 O))) => t
(defun atomic-chem-form-equal (atom-chem1 atom-chem2)
  (let ((s1 (cdr atom-chem1))
	(s2 (cdr atom-chem2)))
    (and
     (null (set-difference s1 s2 :test #'equal))
     (null (set-difference s2 s1 :test #'equal)))
    ))


(defun get-chemical-entities-that-match (name has-chem-form nested-atom-chem-form atom-chem-form charge &key (acid-name nil) (molecular-name) (test nil) )
  (format *chemical-recognition-debug* "~% Searching for: ~%  name=~s ~%  has-chem-form=~s ~%  nested-atom-chem-form=~s ~%  atom-chem-form=~s ~%  charge=~s"
	  name has-chem-form nested-atom-chem-form atom-chem-form charge)
  (let ((result))
    (dolist (entry *chemical-defs-table* result)
      (let ((names-entry (chem-def-entry-name entry))
	    (acid-names-entry (chem-def-entry-acid-name entry))
	    (molecular-names-entry (chem-def-entry-molecular-name entry))
	    (has-chem-form-entry (chem-def-entry-has-chf entry))
	    (nested-atom-chem-form-entry (chem-def-entry-nacf entry))
	    (atom-chem-form-entry (chem-def-entry-acf entry))
	    (charge-entry (chem-def-entry-charge entry)))
	(format *chemical-recognition-debug* "~% Trying: ~a ~a ~s ~s ~s ~s" 
		(chem-def-entry-class-name entry) names-entry has-chem-form-entry nested-atom-chem-form-entry atom-chem-form-entry charge-entry)
	(cond ((and name names-entry
		    (member (if (stringp name) (string-downcase (remove-junk-from-names name)) (format nil "~a" name)) 
			    (mapcar #'remove-junk-from-names names-entry) :test #'equal)
		    (if test (funcall test entry) t))
	       (push entry result))

	       ((and acid-name acid-names-entry
		    (member (if (stringp acid-name) (string-downcase acid-name) (format nil "~a" acid-name)) acid-names-entry :test #'equal)
		    (if test (funcall test entry) t))
		(push entry result))

	       ((and molecular-name molecular-names-entry
		     (member (if (stringp molecular-name) (string-downcase molecular-name) (format nil "~a" molecular-name)) molecular-names-entry :test #'equal)
		     (if test (funcall test entry) t))
		(push entry result))
	      	      
	      ;; by chem form
	      ((and (valid-chem-formula has-chem-form) 
		    (valid-chem-formula has-chem-form-entry)
		    (equal has-chem-form-entry has-chem-form)
		    (if test (funcall test entry) t))
	       (if (or (null charge) 
		       (null charge-entry)
		       (charge-eq charge charge-entry))
		   (push entry result)))
	      
	      ;; by nested chem form
	      ((and (valid-chem-formula nested-atom-chem-form)
		    (valid-chem-formula nested-atom-chem-form-entry)
		    (equal nested-atom-chem-form nested-atom-chem-form-entry)
		    (if test (funcall test entry) t))
	       (if (or (null charge)
		       (null charge-entry)
		       (charge-eq charge charge-entry))
		   (push entry result)))
	      
	      ;; by atomic chem form
	      ((and (valid-chem-formula atom-chem-form)
		    (valid-chem-formula atom-chem-form-entry)
		    (atomic-chem-form-equal atom-chem-form atom-chem-form-entry)
		    (if test (funcall test entry) t))
	       (if (or (null charge)
		       (null charge-entry)
		       (charge-eq charge charge-entry))
		   (push entry result)))
	      )))))



;TODO: when chemical def is not found create a Chemical-Entity with that atomic-chemical-formula
;Guess whether it is acid/base?  Guess if it is ionic or molecular?
;TODO: we should find the function to go from string to atomic-chemical-formula
(defun compute-chemical-from-defs (name has-chem-form nested-atom-chem-form atom-chem-form charge 
					&key (instance t) (test nil) (whole-entry nil) 
					(acid-name nil) (molecular-name) (try-expand nil))
  (if (or name acid-name molecular-name
	  (valid-chem-formula has-chem-form)
	  (valid-chem-formula nested-atom-chem-form)
	  (valid-chem-formula atom-chem-form))

      (let ((chemical-entities (get-chemical-entities-that-match name has-chem-form nested-atom-chem-form atom-chem-form charge 
								 :acid-name acid-name :molecular-name molecular-name :test test)))
	(if (and (null chemical-entities)
		 try-expand
		 (extend-chemical-definitional-property-table))
	    (setf chemical-entities 
		  (get-chemical-entities-that-match name has-chem-form nested-atom-chem-form atom-chem-form charge 
						    :acid-name acid-name :molecular-name molecular-name :test test)))
	
	;;Check that the chemical entities are actually in the KB
	(remove-if #'null
		   (mapcar #'(lambda (entry)
			       (if (and entry 
					(chem-def-entry-class-name entry)
					(known-frame (chem-def-entry-class-name entry)))
				   (if whole-entry
				       entry
				     (if instance 
					 (first (km0 `(|a| ,(chem-def-entry-class-name entry))))
				       (chem-def-entry-class-name entry)))))
			   chemical-entities)))))


;;Issues KM query
(defun km-timeout (km-query &optional (timeout 30) (timeout-val nil))
  (sys:with-timeout (timeout timeout-val) (km km-query)))



;;===============================================================================
;; When compute-chemical-from-defs fails 
;; we extend the definition table with the chemicals in the KB that are not in the definition table
(defun extend-chemical-definitional-property-table ()
  (let* ((all-ions (km '(|the| |all-subclasses| |of| |Ion|)))
	 (all-chemical-entities (km '(|the| |all-subclasses| |of| |Chemical-Entity|)))
	 (existing-chemical-entities (mapcar #'chem-def-entry-class-name *chemical-defs-table*)) 
	 (all-chem-ents-to-skip (union existing-chemical-entities *known-chemicals-with-no-formula*)) ;; skip those already in table and those w/o a formula
	 (new-ions (set-difference all-ions  all-chem-ents-to-skip))
	 (new-chemical-entities (set-difference all-chemical-entities all-chem-ents-to-skip))) 
    (dolist (chem-ent new-ions)   
      (add-chemical-definition chem-ent))
    (dolist (chem-ent new-chemical-entities)   
      (add-chemical-definition chem-ent))))
 
;; ===========================================================================
;; new add-chemical-definition - actually adds inst to *chemical-dsfs-table*
(defun add-chemical-definition (chemical-entity)
  (with-standard-bps-parameters ()
     (disable-classification)
     (if (member chemical-entity *chemical-defs-table* :key #'car)
	 (return-from add-chemical-definition))

     (format t "~% Building def table entry for: ~a" chemical-entity)				
     (let* ((inst (first (km-timeout `(km::|a| ,chemical-entity))))
	    (name (mapcar #'(lambda (n) (if (stringp n) (string-downcase n) (format nil "~a" n))) 
			  (km-timeout `(km::|the| km::|has-chemical-name| km::|of| ,inst))))
	    (has-chem-form (first (km-timeout `(km::|the| km::|term| km::|of| (km::|the| km::|has-chemical-formula| km::|of| ,inst)))))
	    (nested-atom-chem-form (first (km-timeout `(km::|the| km::|term| km::|of| (km::|the| km::|nested-atomic-chemical-formula| km::|of| ,inst)))))
	    (atom-chem-form (first (km-timeout `(km::|the| km::|term| km::|of| (km::|the| km::|atomic-chemical-formula| km::|of| ,inst)))))
	    (charge (first (km-timeout `(km::|the| km::|value| km::|of| (km::|the| km::|charge| km::|of| ,inst)))))
	    
	    (acid-name (mapcar #'(lambda (n) (if (stringp n) (string-downcase n) (format nil "~a" n))) 
			       (km-timeout `(km::|the| km::|has-acid-name| km::|of| ,inst)))) ;; record acid name as well for Anions
	    (molecular-name (mapcar #'(lambda (n) (if (stringp n) (string-downcase n) (format nil "~a" n))) 
				    (km-timeout `(km::|the| km::|has-molecular-name| km::|of| ,inst)))) ;; record molecular name as well for Atoms
	    ;;(parts (km0 `(km::|the| km::|has-part| km::|of| ,inst)))
	    (entry (list chemical-entity name has-chem-form nested-atom-chem-form atom-chem-form charge acid-name molecular-name))) 
       (if (valid-chem-def-entry entry) 
	   (push entry *chemical-defs-table*)
	 (progn
	   (format t "~% - Invalid chemical definition not added: ~a" entry)
	   (push (chem-def-entry-class-name entry) *known-chemicals-with-no-formula*))))))


;; check whether chemical def entry is valid
;; i.e. has values for at least a key bu which lookup is done.
;; in this particular case all keys depend on each other, so if one of them is missing a value, there is a problem
(defun valid-chem-def-entry (entry)
  (or (valid-chem-formula (chem-def-entry-has-chf entry))
      (valid-chem-formula (chem-def-entry-nacf entry))
      (valid-chem-formula (chem-def-entry-acf entry))))


;; tests whether chem-formula is valid.
;; i.e. - non null and starting with :seq and having something other than :seq
(defun valid-chem-formula (chem-formula)
  (and chem-formula
       (equal (first chem-formula) ':|seq|)
       (cdr chem-formula)
       (remove-if #'null (all-term-types chem-formula))))

(defun charge-eq (ch1 ch2)
  (and ch1 ch2 
       (consp ch1) (consp ch2)
       (equal (second ch1) (second ch2))))
