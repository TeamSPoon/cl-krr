;; 
;; $Id: get-db.lisp,v 1.15 2007/11/07 23:28:11 mrglass Exp $
;;
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;; There is an assumption that the periodic table will only contain one
;;; occurance of any given element
;;;
;;; There is an assumption of a string input for the element name

;;; usage: (get-db "Gold" 'atomic_weight)
;;;        (get-db "Au" 'atomic_weight)


(defun get-atom (term-name term-value)
  (let ((table (loop for item in (first *allelements*)
		   when (listp item)
		   collect item)))
    (loop for item in table
	for match = (loop for tag in item
			when (and (listp tag) (equal (first tag) term-name)
				  (equal (second tag) term-value))
			return tag)
	when (not (null match))
	return item)))

(defun get-atom-value (term-name term-value slot-value)
  (let ((table (loop for item in (first *allelements*)
		   when (listp item)
		   collect item)))
    (loop for item in table
	for match = (loop for tag in item
			when (and (listp tag) (equal (first tag) term-name)
				  (equal (second tag) term-value))
			return tag)
	for svalue = (loop for tag in item
			 when (and (listp tag) (equal (first tag) slot-value))
			 return tag)
	when (and (not (null match)) (not (null svalue)))
	return (if (numberp (read-from-string (second svalue)))
		   (read-from-string (second svalue))
		 (second svalue)))))

(defun get-db (term-name term-value slot-value)
   (cond ((string= term-value "Nil")
	 nil)
	(t
	 (get-atom-value term-name term-value slot-value)))
    )
