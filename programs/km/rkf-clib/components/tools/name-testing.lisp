;;
;;    $Id: name-testing.lisp,v 1.4 2010/05/19 20:44:51 kbarker Exp $
;;

;;;----------------------------------------
;;; Testing functions

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defstruct name-test
  name
  formula)

(defparameter *naming-tests* nil)

(defun define-translate-test (name formula)
  (push (make-name-test :name name :formula formula) *naming-tests*))

(defun get-formula-for-name (name)
  (km-unique `(|the| |term| |of|
		     (|the| |nested-atomic-chemical-formula| |of|
			  (|the| |output| |of|
			       (|a| |Compute-Compound-from-Chemical-Name| |with|
				  (|input| (,NAME))))))))

(defun n2f (name)
  ;; Convenience function
  (get-formula-for-name name))

(defun test-name-to-formula ()
  (dolist (test *naming-tests*)
    (let ((formula (get-formula-for-name (name-test-name test))))
      (format t "~&Formula for ~S: ~S (~:[Fail; expected ~S~;Success~])"
	      (name-test-name test) formula
	      (equal formula (name-test-formula test)) (name-test-formula test)))))

(defun get-name-for-formula (formula)
  (km-unique `(|the| |has-chemical-name| |of|
		     (|the| |output| |of|
			  (|a| |Compute-Compound-from-Chemical-Formula| |with|
			     (|input| (,FORMULA)))))))

(defun f2n (formula)
  ;; Convenience
  (get-name-for-formula formula))

(defun test-formula-to-name ()
  (dolist (test *naming-tests*)
    (let ((name (get-name-for-formula (name-test-formula test))))
      (format t "~&Naming ~S: ~S (~:[Fail; expected ~S~;Success~])"
	      (name-test-formula test) name
	      (string-equal name (name-test-name test)) (name-test-name test)))))

;;;----------------------------------------
;;; Test cases

(define-translate-test "sodium dihydrogen phosphate"
    '(:|seq| (:|pair| 1 |Na|) (:|pair| 2 |H|) (:|pair| 1 |P|) (:|pair| 4 |O|)))
(define-translate-test "carbon disulfide" '(:|seq| (:|pair| 1 |C|) (:|pair| 2 |S|)))
(define-translate-test "calcium sulfate" '(:|seq| (:|pair| 1 |Ca|) (:|pair| 1 |S|) (:|pair| 4 |O|)))
(define-translate-test "dibromine pentoxide" '(:|seq| (:|pair| 2 |Br|) (:|pair| 5 |O|)))
(define-translate-test "barium sulfide" '(:|seq| (:|pair| 1 |Ba|) (:|pair| 1 |S|)))
(define-translate-test "arsenic(III) oxide" '(:|seq| (:|pair| 2 |As|) (:|pair| 3 |O|)))
(define-translate-test "ammonium dihydrogen phosphate"
    '(:|seq| (:|pair| 1 |N|) (:|pair| 4 |H|) (:|pair| 2 |H|) (:|pair| 1 |P|) (:|pair| 4 |O|)))
(define-translate-test "ammonium hydroxide" 
    '(:|seq| (:|pair| 1 |N|) (:|pair| 4 |H|) (:|pair| 1 |O|) (:|pair| 1 |H|)))
(define-translate-test "dihydrogen oxide" '(:|seq| (:|pair| 2 |H|) (:|pair| 1 |O|)))
(define-translate-test "iron(III) oxide" '(:|seq| (:|pair| 2 |Fe|) (:|pair| 3 |O|)))
(define-translate-test "magnesium nitrate"
    '(:|seq| (:|pair| 1 |Mg|) (:|pair| 2 (:|seq| (:|pair| 1 |N|) (:|pair| 3 |O|)))))
#+ignore
(define-translate-test "mercury(II) acetate"
    '(:|seq| (:|pair| 2 (:|seq| (:|pair| 1 |C|) (:|pair| 3 |H|) (:|pair| 1 |C|) (:|pair| 1 |O|) (:|pair| 1 |O|)))
	     (:|pair| 1 |Hg|)))
(define-translate-test "potassium hydrogen sulfite"
    '(:|seq| (:|pair| 1 |K|) (:|pair| 1 |H|) (:|pair| 1 |S|) (:|pair| 3 |O|)))
(define-translate-test "sodium sulfide" '(:|seq| (:|pair| 2 |Na|) (:|pair| 1 |S|)))
(define-translate-test "barium chloride" '(:|seq| (:|pair| 1 |Ba|) (:|pair| 2 |Cl|)))
(define-translate-test "diphosphorus trioxide" '(:|seq| (:|pair| 2 |P|) (:|pair| 3 |O|)))
(define-translate-test "ammonium hydrogen phosphate"
    '(:|seq| (:|pair| 2 (:|seq| (:|pair| 1 |N|) (:|pair| 4 |H|))) (:|pair| 1 |H|) (:|pair| 1 |P|) (:|pair| 4 |O|)))
(define-translate-test "oxygen difluoride" '(:|seq| (:|pair| 1 |O|) (:|pair| 2 |F|)))
(define-translate-test "hydrogen peroxide" '(:|seq| (:|pair| 2 |H|) (:|pair| 2 |O|)))
(define-translate-test "iron(II) chloride" '(:|seq| (:|pair| 1 |Fe|) (:|pair| 2 |Cl|)))
(define-translate-test "magnesium nitride" '(:|seq| (:|pair| 3 |Mg|) (:|pair| 2 |N|)))
(define-translate-test "mercury(II) iodide" '(:|seq| (:|pair| 1 |Hg|) (:|pair| 2 |I|)))
;;;o-H3PO4 -- Huh?
;;(define-translate-test o-H3PO4)
(define-translate-test "potassium hydroxide" '(:|seq| (:|pair| 1 |K|) (:|pair| 1 |O|) (:|pair| 1 |H|)))

(define-translate-test "potassium hydrogen carbonate"
    '(:|seq| (:|pair| 1 |K|) (:|pair| 1 |H|) (:|pair| 1 |C|) (:|pair| 3 |O|)))
(define-translate-test "potassium phosphate" '(:|seq| (:|pair| 3 |K|) (:|pair| 1 |P|) (:|pair| 4 |O|)))
(define-translate-test "sodium hydride" '(:|seq| (:|pair| 1 |Na|) (:|pair| 1 |H|)))
(define-translate-test "aluminum nitrate"
    '(:|seq| (:|pair| 1 |Al|) (:|pair| 3 (:|seq| (:|pair| 1 |N|) (:|pair| 3 |O|)))))

;;; EOF
