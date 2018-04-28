;; 
;;    $Id: chf-util.lisp,v 1.4 2010/05/19 20:44:51 kbarker Exp $ 
;; 
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; Utilities for nested-atomic-chemical-formula

;; tests equality between 2 nested-chf
;; exact order matters
(defun chf-eq (nested-ch1 nested-ch2)
  (cond 
   ((and (null nested-ch1) (null nested-ch2))
    t)

   ;; symbols
   ((and (symbolp nested-ch1) 
	 (symbolp nested-ch2)) 
    (eq nested-ch1 nested-ch2))
   
   ;;pairs
   ((and (is-pair nested-ch1) 
	 (is-pair nested-ch2))
    (and (eq (second nested-ch1) 
	     (second nested-ch2))
	 (chf-eq (third nested-ch1) 
		 (third nested-ch2))))
   
   ;; seq
   ((and (is-seq nested-ch1) 
	 (is-seq nested-ch2))
    (pair-list-eq (cdr nested-ch1) (cdr nested-ch2)))

   ;; not aligned
   (t nil)))

(defun pair-list-eq (l1 l2)
  (cond ((and (null l1) (null l2))
	 t)
	((and (listp l1) (listp l2))
	 (and (chf-eq (car l1) (car l2))
	      (pair-list-eq (cdr l1) (cdr l2))))
	((and (null l1) (null l2))
	 t)
	(t nil)))



(defun is-pair (list)
  (and (listp list)
       (eq (first list) ':|pair|)))


(defun is-seq (list)
  (and (listp list)
       (eq (first list) ':|seq|)))


(defun is-triple (list)
  (or (is-seq list) (is-pair list)))

;; prints a nested-chf to a string
(defun nchf-to-string (x)
  (string-upcase (format nil "~A" x)))

;; =====================================================================
;; Utility functions


;; takes an atomic-chemical-formula and check if it is an acid
;; E.g. > (is-acid-chf '(:|seq| (:|pair| 1 H) (:|pair| 1 |Cl|)))
;;      T
;; NOTE: assumes H or H-Plus appears as the first element in the formula 
;; H2O is not an acid 
(defun is-acid-chf (chf)
  (or (and (eq (term-type (first (all-terms chf))) 'H)
	   (not (is-water chf)))
      (and (eq (term-type (first (all-terms chf))) '|H-Plus|)
	   (not (eq (term-type (first (all-terms chf))) '|O-Minus-2|)))))
    
(defun get-atomic-ions (atom &key (charge nil))
  (compute-ion-from-defs nil `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) charge :whole-entry t))

(defun get-atomic-cations (atom &key (charge nil))
  (compute-cation-from-defs nil `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) charge :whole-entry t))

(defun get-atomic-anions (atom &key (charge nil))
  (compute-anion-from-defs nil `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) `(:|seq| (:|pair| 1 ,ATOM)) charge :whole-entry t))

(defun is-metal-atom-p (atom)
  (km `((|the| |all-superclasses| |of| ,ATOM) |includes| |Metal-Atom|)))

(defun is-non-metal-atom-p (atom)
  (km `((|the| |all-superclasses| |of| ,ATOM) |includes| |Non-Metal-Atom|)))

(defun get-ions (ion-chf &key (charge nil))
  (compute-ion-from-defs nil ion-chf ion-chf ion-chf charge :whole-entry t))

(defun get-cations (ion-chf &key (charge nil))
  (compute-cation-from-defs nil ion-chf ion-chf ion-chf charge :whole-entry t))

(defun get-anions (ion-chf &key (charge nil))
  (compute-anion-from-defs nil ion-chf ion-chf ion-chf charge :whole-entry t))



;; ====================================================================
;;
;; Accessor functions for chemical formulas
;;
;; chemical-formula = (:|seq| (:|pair coeff Atom)*)

(defun all-terms (chf)
  (cdr chf))

(defun term-type (term)
  (third term))

(defun term-coeff (term)
  (second  term))

(defun all-term-types (chf)
  (remove-duplicates (mapcar #'term-type (all-terms chf))))

