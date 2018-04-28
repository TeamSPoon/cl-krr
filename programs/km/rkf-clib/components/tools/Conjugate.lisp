;; 
;; $Id: Conjugate.lisp,v 1.4 2010/05/19 20:44:50 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;This file contains support LISP functions for Compute-Conjugate-Acid and
;;Compute-Conjugate-Base.

(defun inc_H_atom_count (input pos)
  (cond ((eq (car (cddr input)) 'H)
	 (list 
	  (append
	   (cons (car input)
		 (list
		  (+ 1
		     (car (cdr input)))))
	   (cddr input))))
	(t
	 (cond ((eq pos 'back)
		(cons
		 input
		 (list (list ':|pair| '1 'H))))
	       (t 
		(list 
		 (list ':|pair| '1 'H)
		 input))))))

(defun dec_H_atom_count (input)
  (cond ((or (eq (nth 2 input) 'H)
	     (eq (nth 2 input) 'h))
	 (cond ((> (nth 1 input) 1)
		(list (nth 0 input) 
		      (- (nth 1 input) 1)
		      (nth 2 input)))
	       (t nil)))
	(t input)))

(defun Get-Conjugate-Acid-Atomic-Formula-Back (input)
  (list
   (cons ':|seq|
	  (append
	   (cdr (butlast (car input) 1))
	   (inc_H_atom_count 
	    (nth (- (length (car input)) 1) (car input))
	    'back)))))

(defun Get-Conjugate-Acid-Atomic-Formula-Front (input)
  (list (cons ':|seq|
	  (append (inc_H_atom_count 
		   (car (cdr (car input)))
		   'front)
		  (cddr (car input))))))

(defun Get-Conjugate-Acid-Atomic-Formula (input)
  (list
   (Get-Conjugate-Acid-Atomic-Formula-Front input)
   (Get-Conjugate-Acid-Atomic-Formula-Back input)))




(defun Get-Conjugate-Base-Atomic-Formula-Front (input)
  (list (cons ':|seq|
	  (cond ((dec_H_atom_count (car(cdr(car input))))
		 (cons
		  (dec_H_atom_count (car(cdr(car input))))
		  (cddr (car input))))

		(t (cddr (car input)))))))

(defun Get-Conjugate-Base-Atomic-Formula-Back (input)
  (list (cons ':|seq|
	      (append
	       (cdr (butlast (car input) 1))
	       (list (dec_H_atom_count 
		      (nth (- (length (car input)) 1) 
			   (car input))))))))

(defun Get-Conjugate-Base-Atomic-Formula (input)
  (list
   (Get-Conjugate-Base-Atomic-Formula-Front input)
   (Get-Conjugate-Base-Atomic-Formula-Back input)))

;Test-cases					   
(Get-Conjugate-Acid-Atomic-Formula '((:seq (:pair 1 S)(:pair 4 O))))
(Get-Conjugate-Acid-Atomic-Formula-Back '((:seq (:pair 1 H) 
					   (:pair 1 S)
					   (:pair 4 O))))
(Get-Conjugate-Acid-Atomic-Formula '((:seq (:pair 6 C)
					   (:pair 5 H)
					   (:pair 1 N)
					   (:pair 2 H))))
(Get-Conjugate-Acid-Atomic-Formula '((:seq (:pair 2
						  (:pair 1 C)
						  (:pair 3 H))
					   (:pair 1 N)
					   (:pair 1 H))))

(Get-Conjugate-Base-Atomic-Formula '((:seq (:pair 1 H)
					   (:pair 1 Cl))))

(Get-Conjugate-Base-Atomic-Formula '((:seq (:pair 1 H)
					   (:pair 1 O)
					   (:pair 1 N)
					   (:pair 1 H))))

(Get-Conjugate-Base-Atomic-Formula '((:seq (:pair 6 C)
					   (:pair 5 H)
					   (:pair 1 N)
					   (:pair 2 H))))

;(trace Get-Conjugate-Base-Atomic-Formula Get-Conjugate-Base-Atomic-Formula-Back Get-Conjugate-Base-Atomic-Formula-Front)

;(trace Get-Conjugate-Acid-Atomic-Formula Get-Conjugate-Acid-Atomic-Formula-Back Get-Conjugate-Acid-Atomic-Formula-Front)

