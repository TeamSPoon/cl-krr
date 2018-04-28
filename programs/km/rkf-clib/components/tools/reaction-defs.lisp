;;
;; $Id: reaction-defs.lisp,v 1.30 2008/12/09 22:19:25 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; build the table on the first reaction balancing call
;; later, cache the table
;; also extend the table on the fly
(defvar *reaction-definitions-table* nil) 
(setf *reaction-definitions-table* nil )

;;entries look like this: 
;;(|Reaction-1C1O2--1CO2| ((:seq (:pair 1 C)) (:seq (:pair 2 O))) ((:seq (:pair 1 C) (:pair 2 O))))

;; this is a list of all the reactions that don't have all their raw-materials and results defined
;; all ground reactions should be balancable
(defvar *non-ground-reactions* nil)

;;|Reaction-1AuCl33Ag--1Au3AgCl| |Reaction-1CaCO31Al--Al_CO3_31Ca-Plus-2| ; broken atm
(setf *non-ground-reactions*  '(
 |Dissociation-Reaction| |Water-Producing-Reaction| |Equilibrium-Reaction| |Hydrolysis|
 |Oxidation-Reaction| |Single-Displacement-Reaction| |Partial-Dissociation-Reaction| 
 |Acid-Dissociation-Reaction| |Neutralization-Reaction| |Identify-Reaction| |Acid-Base-Reaction|
 |Combustion-Reaction| |Hydrocarbon-Combustion-Reaction| |Oxidation-Reduction-Reaction| |Precipitation-Reaction|
 |Nonmetal-Nonmetal-Reaction| |Metathesis-Reaction| |Gas-Solution-Reaction-b| |Gas-Solution-Reaction-a| 
 |Complete-Dissociation-Reaction| |Decomposition-Reaction| |Metal-Carbonate-Decomposition| |Carbonate-Decomposition-Reaction|
 |Combination-Reaction| |Metal-Nonmetal-Combination-Reaction| |Binary-Ionic-Compound-Decomposition|)
      )

;; turn on/off debugging
(defvar *reaction-identification-debugging*)
(setf *reaction-identification-debugging* nil)

;; prevent recursive identification
(defvar *use-reaction-identification*)
(setf *use-reaction-identification* t)

;; keep reactions instances already proccessed to aviod duplicate work
(defvar *processed-reactions-cache*)
(setf *processed-reactions-cache* nil)

(defvar *during-reactrion-identification*)
(setf *during-reactrion-identification* nil)

(defun clear-processed-reactions-cache ()
  (setf *processed-reactions-cache* nil))

(defun get-acf-term-from-chemical (chemical)
  (car (km-timeout `(|the| |term| |of| 
		     (|the| |atomic-chemical-formula| |of| 
		      (|the| |has-basic-structural-unit| |of| ,chemical))) 
		   30 
		   '((:|seq| (:|pair| 1 |Timeout|))))))

;;TODO: retry non-ground reactions a couple of times before adding them to the list for good
;; [dgt] 07/11/08 removed superflous situation saving - already done in sri-km-query
(defun extend-reaction-definitions-table ()
   (format *reaction-identification-debugging* "~%~% Extending reactions table")
   (let ((extended nil)
	 (*use-reaction-identification* nil)
	 (reactions (km '(|the| |all-subclasses| |of| |Reaction|))))
     (format *reaction-identification-debugging* "~% ~a" reactions)
     (dolist (reaction reactions)
       (if (or (member reaction *non-ground-reactions*)
	       (member reaction *reaction-definitions-table* :key #'car))
	   (format *reaction-identification-debugging* "~%  -- ~a already in table" reaction)
	 (progn
	   (disable-classification)
	   (let* ((inst (first (km-timeout `(|a| ,reaction))))
		  (raw-materials (km-timeout `(|the| |raw-material| |of| ,inst)))
		  (results (km-timeout `(|the| |result| |of| ,inst)))
		  (raw-acfs (mapcar #'get-acf-term-from-chemical raw-materials))
		  (res-acfs (mapcar #'get-acf-term-from-chemical results)))
	     (format *reaction-identification-debugging* "~%   ~a --> ~a" raw-acfs res-acfs)
	     (if (and (not (member nil raw-acfs))
		      (not (member nil res-acfs)))
		 (progn 
		   (setf extended t)
		   (push (list reaction raw-acfs res-acfs) *reaction-definitions-table*)
		   (if *reaction-identification-debugging* 
		       (format t "~%  -- Building def table entry for: ~a" reaction)))
	       (progn (push reaction *non-ground-reactions*) 
		      (if *reaction-identification-debugging* 
			  (format t "~%  -- Skipping non-ground reaction: ~a" reaction))))))))
     extended))


;;; added short-circuit return on first match
(defun get-reactions-that-match (raw-acfs res-acfs)
  ;; if raw materials are missing, exit. DO NOT ID ONLY ON RESULT!!!
  (if (contains-nil raw-acfs) (return-from get-reactions-that-match))
  (let ((matched-raw-mat)
	(matched-res))
  (dolist (entry *reaction-definitions-table*)
    (format *reaction-identification-debugging* "~% -- matching against ~a" (first entry))
    (let ((raw-materials-acfs-match? (if (not (contains-nil raw-acfs)) (acfs-matchp (second entry) raw-acfs)))
	  (results-acfs-match? (if (not (contains-nil res-acfs)) (acfs-matchp (third entry) res-acfs))))
             ;; if both raw-mat and results are fully specified and they both should match !!
      (cond ((and (not (contains-nil raw-acfs))
		  (not (contains-nil res-acfs))
		  raw-materials-acfs-match? 
		  results-acfs-match?)
	     (return-from get-reactions-that-match (values (first entry) t)))
	    
	    ;; if raw-mat are specified, they need to match
	    ((and (not (contains-nil raw-acfs))
		  raw-materials-acfs-match?) 
	     (push (first entry) matched-raw-mat))

	    ;; if results are specified, they need to match
	    ((and (not (contains-nil res-acfs))
		  results-acfs-match?)
	     (push (first entry) matched-res)))))
  (if matched-raw-mat 
      (values (first matched-raw-mat) nil)
    (values (first matched-res) nil))))


(defun contains-nil (list)
  (member nil list))

(defun acfs-matchp (acfs1 acfs2)
  (and (= (length acfs1)
	  (length acfs2))
       (not (set-difference acfs1 acfs2 :test #'atomic-chem-form-equal))))

;;jchaw, default is to identify and unify with an instance of identified reaction.
(defun reaction-identify-me (reaction &key (do-unification? t) (do-caching t) (raw-mat) (res))
  (with-standard-bps-parameters ()
     (disable-classification)
     (format *reaction-identification-debugging* "~% Identifying ~a instance-of: ~a" reaction (km `(|the| |instance-of| |of| ,reaction)))
     ;; prevent identification when instructed
     (if (not *use-reaction-identification*) 
	 (progn
	   (format *reaction-identification-debugging* "~% Reaction identification turned off, exiting")
	   (return-from reaction-identify-me)))
       
     ;; some supers of the reactions already in reaction-table
     (if (and do-caching
	      (some #'(lambda (x) (member x *reaction-definitions-table* :key #'car))
		    (km0 `(|the| |instance-of| |of| ,reaction))))
	 (let* ((*use-reaction-identification* nil)
		(raw-materials) (results) (raw-acfs) (res-acfs)
		(identification-result nil)
		(unify-reactions-output nil)
		(reaction-class (caar (some #'(lambda (x) (member x *reaction-definitions-table* :key #'car))
					    (km0 `(|the| |instance-of| |of| ,reaction))))))
	   (format *reaction-identification-debugging* "~% Already identified")
	   (km0 `(,reaction |now-has| (|instance-of| (|Reaction|))))
	   (setf raw-materials (km0 `(|the| |raw-material| |of| ,reaction)))
	   (setf results (km0 `(|the| |result| |of| ,reaction)))
	   (setf raw-acfs (mapcar #'get-acf-term-from-chemical raw-materials))
	   (setf res-acfs (mapcar #'get-acf-term-from-chemical results))
	   (setf unify-reactions-output 
		 (unify-reactions reaction raw-materials results reaction-class do-unification?))
	   (format *reaction-identification-debugging* "~% Before returning")
	   (return-from reaction-identify-me (values t unify-reactions-output))))
       
     ;; cache look-up
     (if (and do-caching 
	      (assoc reaction *processed-reactions-cache*))
	 ;; no need for unification here!
	   (progn
	     (setf reaction-class (cdr (assoc reaction *processed-reactions-cache*)))
	     (format *reaction-identification-debugging* "~% Mapped onto [cached]: ~a" reaction-class)
	     (setf *during-reaction-identification* nil)
	     (return-from reaction-identify-me (values t nil))))
     
     ;; actual search of table
     (let* ((*use-reaction-identification* nil)
	    (raw-materials (km0 `(|the| |raw-material| |of| ,reaction)))
	    (results (km0 `(|the| |result| |of| ,reaction)))
	    (raw-acfs (mapcar #'get-acf-term-from-chemical raw-materials))
	    (res-acfs (mapcar #'get-acf-term-from-chemical results))
	    (identification-result nil)
	    (unify-reactions-output nil))
       (format *reaction-identification-debugging*  "~% Looking up: ~a --> ~a" raw-acfs res-acfs)
       (if (and (member nil raw-acfs) (member nil res-acfs))
	   (format *reaction-identification-debugging* "~% ... chemical formulas contain nil. Giving up!")
	 (multiple-value-bind (reaction-class all-matched)
	     (get-reactions-that-match raw-acfs res-acfs)
	   ;;if you find one, unify
	   (if reaction-class 
	       (progn 
		 (format *reaction-identification-debugging* "~% Mapped onto: ~a" reaction-class)
		 (if all-matched
		     ;; both raw-materials and results matched, don't unify, just change super for Reaction instance
		     (setf unify-reactions-output 
			   (unify-reactions reaction raw-materials results reaction-class nil))
		   (setf unify-reactions-output 
			 (unify-reactions reaction raw-materials results reaction-class do-unification?)))
		 (setf identification-result reaction-class))
	     
	     ;;if you don't find one then extend reaction table and retry
	     (if (extend-reaction-definitions-table)
		 (multiple-value-bind (reaction-class all-matched)
		     (get-reactions-that-match raw-acfs res-acfs)
		   ;; need to invalidate cache - there might be reactions that were not identified but can now be identified
		   (clear-processed-reactions-cache)
		   ;;if you find one, unify
		   (if reaction-class 
		       (progn 
			 (format *reaction-identification-debugging* "~% Mapped onto: ~a" reaction-class)
			 ;; the unification here is quite tricky as the raw-materials and results need to be aligned
			 ;; we cannot rely on KM's unification mechanism
			 ;;jchaw
			 (if all-matched
			     ;; both raw-materials and results matched, don't unify, just change super for Reaction instance
			     (setf unify-reactions-output 
				   (unify-reactions reaction raw-materials results reaction-class nil))
			   (setf unify-reactions-output 
				 (unify-reactions reaction raw-materials results reaction-class do-unification?)))
			 (setf identification-result reaction-class))))))))
       (push (cons reaction identification-result) *processed-reactions-cache*)
       ;;jchaw
       (values (not (null identification-result)) unify-reactions-output)
       )))
  
;; 
;; unifies two reactions r1 and r2 by unifying their raw-materials and results in the proper order
;; NOTE: we know that at leat the raw-materials of these reactions match, se we only need to align them and then unify them
;; 
;;jchaw, default is to find and unify  mappings between components of reaction and identified reaction.
(defun unify-reactions (r1-instance r1-raw-materials r1-results r2-class &optional(do-unification? t))
  (with-standard-bps-parameters ()
    (disable-classification)
    (if (not do-unification?) 
	(progn
	  (km0 `(,r1-instance |now-has| (|instance-of| (,r2-class))))
	  (return-from unify-reactions )))
    
    (let* ((r2-instance (car (km0 `(|a| ,r2-class))))
	   (r2-raw-materials (km0 `(|the| |raw-material| |of| ,r2-instance)))
	   (r2-results (km0 `(|the| |result| |of| ,r2-instance)))
	   (triple-mappings nil)
	   (instance-mappings nil)
	   (raw-mat-mappings (find-reaction-participant-mappings r1-raw-materials r2-raw-materials))
	   (res-mappings (find-reaction-participant-mappings r1-results r2-results)))
      (format *reaction-identification-debugging* "~% Changing reaction ~a to be a ~a" r1-instance r2-class)
     
      ;; unify raw-materials
      (mapcar #'(lambda (mapping)
		  (let ((r1-raw-mat (car mapping))
			(r2-raw-mat (cdr mapping)))
		    (format *reaction-identification-debugging* "~%  -- unifying raw-mat: ~a (~a) with ~a (~a)" 
			    r1-raw-mat (chem-string-name r1-raw-mat) r2-raw-mat (chem-string-name r2-raw-mat))
		    (km0 `(,r2-raw-mat &! ,r1-raw-mat))
		    ;; [jchaw 07Sep08] update triple mapping list, assigning default score of 1
		    (push (list (list (list r1-instance '|raw-material| r1-raw-mat)
				      (list r2-instance '|raw-material| r2-raw-mat)) 1)
			  triple-mappings)
		    ;; [jchaw 07Sep08] update mappings list
		    (push (cons r1-raw-mat r2-raw-mat) instance-mappings)))
		  raw-mat-mappings)
      
      ;; unify results
      (mapcar #'(lambda (mapping)
		  (let ((r1-res (car mapping))
			(r2-res (cdr mapping)))
		    (format *reaction-identification-debugging* "~%  -- unifying result: ~a (~a)  with ~a (~a)" 
			    r1-res (chem-string-name r1-res) r2-res (chem-string-name r2-res))
		    (km0 `(,r2-res &! ,r1-res))
		    ;; [jchaw 07Sep08] update triple mapping list, assigning default score of 1
		    (push (list (list (list r1-instance '|result| r1-res)
				      (list r2-instance '|result| r2-res)) 1)
			  triple-mappings)
		    ;; [jchaw 07Sep08] update mappings list)
		    (push (cons r1-res r2-res) instance-mappings)))
	      res-mappings)

      ;; assert new instance-of
      (format *reaction-identification-debugging* "~%  -- unifying reactions ~a and ~a" r2-instance r1-instance)
      ;;(showme r2-instance)
      ;;(showme r1-instance)
      ;; this should be a unification, not a change of suppers because the "recognized" class might be more general than the new one
      (km0 `(,r2-instance & ,r1-instance)) ;;|now-has| (|instance-of| (,r2-class)))) 
      (push (cons r1-instance r2-instance) instance-mappings) ;;update instance mappings list
      (list triple-mappings instance-mappings 1) ;; [jchaw 07Sep08] return both triple and instance mappings list, also assign default score of 1.
      )))

(defun find-reaction-participant-mappings (p1-set p2-set)
  (let ((mappings)
	(p1-mapped)
	(p2-mapped))

    (dolist (p1 p1-set)
      (let ((p1-mapping (find p1 p2-set :test #'chem-string-name=)))
	(if p1-mapping
	    (progn
	      (push p1 p1-mapped)
	      (push p1-mapping p2-mapped)
	      (push (cons p1 p1-mapping) mappings)))))
    
    (if (= (length p1-mapped)
	   (length p1-set))
	;; all mapped
	mappings
      (append mappings (mapcar #'(lambda (x y) 
				   (cons x y))
			       (set-difference p1-set p1-mapped) 
			       (set-difference p2-set p2-mapped))))))


(defun chem-string-name<= (c1 c2)
  (string<= (chem-string-name c1)
	    (chem-string-name c2)))

(defun chem-string-name= (c1 c2)
  (string= (chem-string-name c1)
	   (chem-string-name c2)))

(defun chem-string-name (chem)
  (let ((chem-name (km0 `(|the| |string-name| |of| 
			  (|the| |atomic-chemical-formula| |of| 
			   (|the| |has-basic-structural-unit| |of| ,chem))))))
    (if chem-name 
	(car chem-name)
      "")))


(defun km-query-disabled-classif (query)
  (with-standard-bps-parameters ()
   (disable-classification)
   (km query)))

;TODO: do the same thing for chemicals and chemical-entities
