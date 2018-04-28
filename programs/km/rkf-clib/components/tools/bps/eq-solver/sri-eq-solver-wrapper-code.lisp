;;
;; $Id: sri-eq-solver-wrapper-code.lisp,v 1.2 2008/02/14 00:14:11 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun sri-resolve-conflicting-symbols(current-eq-expr-list 
				       current-eq-symbol-table 
				       new-eq-expr-list 
				       new-eq-symbol-table)
  (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
    (reset-eq-solver)
    (resolve-conflicting-symbols current-eq-expr-list 
				 current-eq-symbol-table 
				 new-eq-expr-list 
				 new-eq-symbol-table))))

(defun sri-resolve-all-conflicting-symbols(current-eq-expr-list 
					   current-eq-symbol-table 
					   new-eq-expr-list 
					   new-eq-symbol-table)
  (with-restoring-situations-reasoning ()
  (let ((*LOGGING* t))
    (in-situation *CONTROLLER-KM-SITUATION*)
    (reset-eq-solver)
    (resolve-all-conflicting-symbols current-eq-expr-list 
				     current-eq-symbol-table 
				     new-eq-expr-list 
				     new-eq-symbol-table))))

