;;
;; $Id: solver.lisp,v 1.8 2007/08/27 22:39:49 mrglass Exp $
;;

;;;;;;;
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;
(defun check-consistency (a x b)
  ;; Returns T if Ax=b for all rows of the system, NIL otherwise.
  (let ((all-nums t))
     (loop for i fixnum from 0 below (array-dimension x 0)
        do (setf all-nums (and all-nums (numberp (aref x i)))))
     (if all-nums
        (loop for i fixnum from 0 below (length b)
           for res = (progn
              #+never
              (loop for j from 0 below (array-dimension a 1)
                 do (format t "~d " (aref a i j)))
              (- (aref b i)
                 (loop for j fixnum from 0 below (array-dimension a 1)
                    sum (* (aref a i j) (aref x j)))))
           always
           (progn
              ;; (format t "~%b[~d] = ~d; res = ~d" i (aref b i) res)
              (zerop res))))))
     

;;;
;;; Find the defect of the system.  Meant to be used on a reduced,
;;; underdetermined system.  The defect is defined as the
;;; dimensionality of the embedding space (the x vector) minus the
;;; dimensionality of the actual solution space (the span of A).  A
;;; fully determined system has a defect of 0.  An underdetermined
;;; system starts with a defect that is the rows - cols of A.  Every
;;; zero row for which b is also zero increases the defect (it was a
;;; linearly dependent equation).

(defun find-defect (a b)
  (let ((def (- (array-dimension a 1) (array-dimension a 0))))
    (loop for i fixnum from 0 below (array-dimension a 0)
	  do
	  (let ((rowzerop (loop for j fixnum from 0 below (array-dimension a 1)
				always (zerop (aref a i j)))))
	    (when rowzerop
	      (if (zerop (aref b i))
		  (incf def)
	          (setf def (1+ (array-dimension a 1)))))))
    def))


;;;
;;; Get a sample solution for the system.  The "undetermined" values
;;; for x are those at the end of the vector.  These can be set to any
;;; desired values, but by default they are zero.  This function
;;; attempts to back-substitute the initial n elements of x (where n
;;; is the dimensionality of the solution space).
;;;

(defun get-x-sample (a b x n)
  ;; Do back substitution in the triangular A matrix to solve for x.
  (loop for z from 0 below n
	for l = (- n z 1)
	for ir = l
	do
	(if (zerop (aref a ir l))

	    (let ((k (1+ l)))
	      ;; Skip over to the first nonzero element:
	      (loop while (and (< k (length x)) (zerop (aref a ir k)))  do (incf k))
	      (when (< k (length x))
		(let ((den (aref a ir k))
		      (sum (loop for c from (1+ k) below (length x)
				 summing (* (aref a ir c) (aref x c)))))
		  (setf (aref x k) (/ (- (aref b ir) sum) den))
		  )))
	  (let* ((sum (loop for c from (1+ l) below (length x)
			    summing (* (aref a ir c) (aref x c))))
		 (val (/ (- (aref b ir) sum) (aref a ir l))))
	    (setf (aref x l) val)))))



;;; Given a permuted upper-triangular matrix, compute its determinant
;;; -- just the product of its diagonal:
(defun compute-ut-determinant (a permute n)
  (let ((det 1))
    (loop for i fixnum from 0 below n
	  for diag = (aref a (aref permute i) i)
	  do (setf det (* det diag)))
    det))




(defun first-nonzero-entry (a k)
  (let ((j 0)
	(n (array-dimension a 1)))
    (loop while (and (< j n) (zerop (aref a k j))) do (incf j))
    (unless (>= j n) j)))

(defun pseudo-back-substitution (a b x)
  ;;     ---Lastly, do the back substitution in the triangular A matrix to
  ;;     |  solve for X.
  (let ((rows (array-dimension a 0))
	(cols (array-dimension a 1))
	bogus)
    (unless (or (loop for v across x always (numberp v))
		(< rows (length x)))
      (loop while (not bogus)
	    for z from 0 below rows
	    for l = (- rows z 1)
	    for c0 = (first-nonzero-entry a l)
	    do
	    (if  (null c0)
		(unless (zerop (aref b l)) (setq bogus t))
	      (progn
		(let* ((sum (loop for c from (1+ c0) below cols
				  summing
				  (progn
				    (unless (aref x c) (setf (aref x c) 0))
				    (* (aref a l c) (aref x c)))))
		       (val (/ (- (aref b l) sum) (aref a l c0)))
		       (old (aref x c0)))
		  (if (and (numberp old) (/= old val))
		      (setf bogus t)
		    (setf (aref x c0) val)))))))
      bogus))

;;;
;;; A permuted back-substitution that assumes the system is square or
;;; overdetermined:
;;;
(defun back-substitution (a b x permute n)
  ;; Do back substitution in the triangular A matrix to solve for x.
  (loop for z from 0 below n
	for l = (- n z 1)
	for ir = (aref permute l)
	do
	(let* ((sum (loop for c from (1+ l) below n
			  summing (* (aref a ir c) (aref x c))))
	       (val (/ (- (aref b ir) sum) (aref a ir l))))
	  (setf (aref x l) val))))



(defun de-permute-matrix (matrix permute &optional (new (make-array (array-dimensions matrix))))
  "De-permutes a matrix according to the permutation vector PERMUTE.
This stuff is assumed to have been created by SOLVE-INTEGER-SYSTEM.  A
new array is created and returned.  This array will be in upper
triangular form."
  (let ((rows (array-dimension matrix 0))
	(cols (array-dimension matrix 1)))
    (loop for i fixnum from 0 below rows
	  do
;;	  (loop for j fixnum from 0 below i do (setf (aref new i j) 0))
	  (loop for j from 0 below cols
		do (setf (aref new i j)
			 (aref matrix (aref permute i) j))))
    new))


(defun de-permute-vector (vec permute &optional (new (make-array (length vec))))
  "De-permutes a matrix according to the permutation vector PERMUTE.  This stuff
is assumed to have been created by SOLVE-LINEAR SYSTEM.  A new array is created
and returned.  This array will be in upper triangular form."
  (fill new nil)
  (let ((rows (length permute)))
    (dotimes (i rows)
      (setf (aref new i) (aref vec (aref permute i))))
    new))



;;;
;;; Returns 5 values: DIM DET X A B
;;;
;;; DIM - the dimensionality of the solution space.  A dimensionality
;;; of 0 is a unique point solution, 1 is a line solution, 2 is a
;;; plane, and so on.  Negative dimensionality means that the system
;;; has no solutions.
;;;
;;; DET - the determinant of the system.  A nonzero determinant for a
;;; square system implies that there is a single point solution.
;;;
;;; X - a solution vector (if one exists)
;;; A - a reduced (echelon) form of the input A matrix
;;; B - a reduced form for the input B matrix.
;;;
;;; 
;;; Both the input and output matrices and vectors are constrained to
;;; the domain of the rationals.
;;;

(defun reduce-integer-system (a &key b (x (make-array (array-dimension a 1))))
  (let* ((rows   (array-dimension a 0))
	 (cols   (array-dimension a 1))
	 (rownum (make-array rows))
	 (max (make-array rows))
	 (det 1)
	 (defect (- cols rows))
	 bigrow )

    ;; For each row, find the maximum value across columns:
    (loop for i fixnum from 0 below rows
	  do
	  (setf (aref rownum i) i)
	  (setf (aref max i) 0)
	  (loop for j fixnum from 0 below cols
		for v = (abs (aref a i j))
		when (>  v (aref max i))
		do (setf (aref max i) v)))
    ;; (format t "~%Found maximum values.")
    ;; 
    (loop for j from 0 below (1- (if (> cols rows) rows cols))
	  for row = (aref rownum j)
	  do (let ((bigval 0))
	       ;; Find the row with the largest value in column j.
	       ;; (format t "~%Pivoting on column ~d." j)
	       (loop for i from j below rows
		     for tstrow = (aref rownum i)
		     for maxval = (aref max tstrow)
		     do
		     ;; (format t "~%...reducing row ~d." i)
		     (if (zerop maxval)
			 (setq det 0)
		       (let ((tstval (/ (abs (aref a tstrow j)) maxval)))
			 (when (> tstval bigval)
			   (setq bigval tstval
				 bigrow i)))))
	       (if (zerop bigval)
		   (setq det 0)
		 (progn
		   ;; switch current row with pivot
		   (setf (aref rownum j) (aref rownum bigrow))
		   (setf (aref rownum bigrow) row)
		   (setq det (- det)))))

	  ;; Eliminate the leading coefficients:
	  ;; (format t "~%Eliminating leading coefficients for column ~d" j)
	  (let ((pivrow (aref rownum j))
		div)
	    ;; (format t "~%Pivot row = ~d" pivrow)
	    (when (< pivrow rows)
	      (setf div (aref a pivrow j))
	      ;; (format t "~%div = ~d" div)
	      (unless (zerop div)
		(loop for i fixnum from (1+ j) below rows
		      do
		      (let ((ir (aref rownum i)))
			;; (format t "~%ir = ~d." ir)
			(let ((u (/ (aref a ir j) (aref a pivrow j))))
			  (setf (aref a ir j) 0) ;; This element becomes 0
			  ;; Subtract rows in A to eliminate:
			  (loop for j2 fixnum from (1+ j) below cols
				do (setf (aref a ir j2)
					 (- (aref a ir j2) (* (aref a pivrow j2) u))))
			  ;; Do the same for B:
			  (setf (aref b ir) (- (aref b ir) (* (aref b pivrow) u))))
			))
		;; (format t "~%elimination is done.")
		))))
    ;; (format t "~%Done.")

    ;; compute the determinant.
    (setf det (* det (compute-ut-determinant a rownum (min rows cols))))
    (if (< rows cols)
	(setf defect (find-defect a b))
      (if (zerop det)
	  ;; If this is simply a linearly dependent system, find its defect:
	  (when (= rows cols) (setf defect (find-defect a b)))
	  (back-substitution a b x rownum (min rows cols))))
    
    (let ((a-permuted (de-permute-matrix a rownum))
	  (b-permuted (de-permute-vector b rownum))
	  (dim (- cols defect)))
      
      (when (and (<= rows cols) (> cols defect))
	;; This is an underdetermined system, so sample the solution
	;; space (it's not a point).
	(fill x 0) ;; all underdetermined values of x are set to 0 (is this safe?)
	(get-x-sample a-permuted b-permuted x dim))

      (when (and (> rows cols)
		 (zerop det)
		 (pseudo-back-substitution a-permuted b-permuted x))
	(setf dim -1))
      ;; If the system is consistent, then we found a valid solution
      ;; (in x).  If rows are greater than columns, the system is
      ;; overdetermined but valid.
      (if (check-consistency a-permuted x b-permuted)
	  (when (> rows cols) (setf dim 0))
	  (setf dim -1) ;; otherwise the system is inconsistent.
	  )
      
      (values dim det x a-permuted b-permuted))))


