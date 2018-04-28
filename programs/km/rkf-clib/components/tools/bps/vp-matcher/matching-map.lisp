;;
;; $Id: matching-map.lisp,v 1.1 2008/04/08 21:23:49 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun dump-matching-map(triple-lst filename)
  (write-table filename
	       '("input triples" "candidate matches" "Unmatched concepts")
	       (format-output-for-get-candidate-concepts
		(get-candidate-concepts-powerset (normalize-triple-list triple-lst)))))

(defun format-output-for-get-candidate-concepts(table)
  (mapcar #'(lambda (row)
	      (let ((input-triples      (nth 0 row))
		    (candidate-matches  (nth 1 row))
		    (unmatched-concepts (nth 2 row)))
		(list (format-output-for-triples input-triples)
		      (format-output-for-candidate-matches candidate-matches)
		      (format-output-for-unmatched-concepts unmatched-concepts))))
	      table))

(defun format-output-for-triples(input)
  (let ((s (make-string-output-stream))
	(triple-lst (de-isolate-triple-grp input)))
    (dolist (x (extract-non-instance-triples triple-lst))
      (format s "~a~%" x))
    (format s "~%")    
    (dolist (x (extract-instance-triples triple-lst))
      (format s "~a~%" x))
    (get-output-stream-string s)))

(defun format-output-for-unmatched-concepts(input)
  (let ((s (make-string-output-stream)))
    (format s "~a entries~%" (length input))
    (dolist (x (sort input 
		     #'(lambda(x y) (string< (format nil "~a" x) 
					     (format nil "~a" y)))))
      (format s "~a~%" x))
    (get-output-stream-string s)))

(defun format-output-for-candidate-matches(input)
    (let ((s (make-string-output-stream)))
      (format s "~a entries~%" (length input))
    (dolist (x (sort input
		     #'(lambda(x y) (> (abs (nth 3 x)) (abs (nth 3 y))))))
      (let ((concept          (nth 0 x))
	    (triple-map-lst   (nth 1 x))
	    (instance-map-lst (nth 2 x))
	    (avg-score        (nth 3 x)))
	(format s "~a ~a~%" concept (round-fraction avg-score))
	(dolist (y (sort triple-map-lst #'(lambda(x y) (> (abs (cdr (nth 0 x))) (abs (cdr (nth 0 y)))))))
	  (format-output-for-triple-map s y instance-map-lst))))
    (get-output-stream-string s)))

(defun format-output-for-triple-map(s entry instance-map)
  (let* ((triple-map     (nth 0 entry))
	 (transformation (nth 1 entry))
	 (sibling-match? (nth 2 entry))
	 (lhs   (nth 0 (car triple-map)))
         (rhs   (nth 1 (car triple-map)))
	 (score (abs (cdr triple-map))))
    (format s "   ~a" (round-fraction score))
    (cond ((and (not (null transformation))
		(equal transformation 'flexible))
	   (format s " flexible")))
    (cond ((and (not (null sibling-match?))
		(equal sibling-match? 'sibling-match))
	   (format s " sibling")))
    (format s "~%")
    (format s "   ~a~%" lhs)
    (if (mapped-p (car lhs) (car rhs) instance-map)
	(format s "   ~a~%" rhs)
        (format s "   ~a~%" (invert-triple rhs)))))

(defun get-candidate-concepts-powerset(scenario)
  (sort
   (mapcar #'(lambda(c)
	       	 (multiple-value-bind
		     (shotgun-matches non-matching-concept-lst)
		     (get-candidate-concepts-n c)
	       (list c shotgun-matches non-matching-concept-lst)))
	   (remove nil 
		   (powerset (isolate-all-triples scenario))))
   #'(lambda(x y)(< (length (car x)) (length (car y))))))

;;all isolated triple match
(defun get-candidate-concepts-n(scenario)
  (let ((isolated-triple-lst (isolate-all-triples scenario)))
    (if (not (null isolated-triple-lst))
	(get-candidate-concepts-n-aux isolated-triple-lst))))

(defun get-candidate-concepts-n-aux(isolated-triple-lst)
  (cond ((and (listp isolated-triple-lst)
	      (= (length isolated-triple-lst) 1))
	 (get-candidate-concepts-1+ (car isolated-triple-lst)))
	(t 
	 (multiple-value-bind
	     (1+matches non-matching-concept-lst)
	     (get-candidate-concepts-1+  (car isolated-triple-lst))
	   (multiple-value-bind
	       (cdr-1+matches cdr-non-matching-concept-lst)
	       (get-candidate-concepts-n-aux (cdr isolated-triple-lst))
	     (values (merge-same-matches 1+matches cdr-1+matches)
		     (union non-matching-concept-lst cdr-non-matching-concept-lst)))))))

(defun merge-same-matches(x-lst y-lst)
  (let ((result nil))
    (dolist (x x-lst)
      (dolist (y y-lst)
	(if (equal (car x) (car y))
	    (push (list (car x)
			(append (nth 1 x) (nth 1 y))
			(append (nth 2 x) (nth 2 y))
			(/ (+ (abs (nth 3 x)) (abs (nth 3 y))) 2))
		  result))))
    result))

;;1+ isolated triple match
;;returns two things. 
;;a) 1+ matches
;;b) non-matching-concept-lst
(defun get-candidate-concepts-1+(isolated-triple-lst
				 &optional (concept-lst *all-user-defined-concepts*))
  (cond ((null isolated-triple-lst) (values nil concept-lst))
	((null concept-lst)         ())
	(t (multiple-value-bind 
	       (1+matches non-matching-concept-lst)
	       (sieve-concept-list-for-triple-match 
		(car isolated-triple-lst)
		concept-lst)
	     (multiple-value-bind
		 (cdr-1+matches cdr-non-matching-concept-lst)
		 (get-candidate-concepts-1+
		  (cdr isolated-triple-lst)
		  non-matching-concept-lst)
	       (values (append 1+matches cdr-1+matches)
		       cdr-non-matching-concept-lst))))))

;;returns two things. 
;;a) matched concepts and its matching info
;;b) unmatched concept-lst
(defun sieve-concept-list-for-triple-match(isolated-triple 
					   &optional(concept-lst *all-user-defined-concepts*))
  (let ((debug  nil)
	(result nil))
    (if debug (format t "~a~%" isolated-triple))
    (mapcar #'(lambda(concept)
		(multiple-value-bind
		    (root graph)
		    (procure-graph concept)
		  (multiple-value-bind
		      (triple-map-list instance-map-list taxonomic-score transformations sibling-match?)
		      (ps-triple-match isolated-triple (normalize-triple-list graph))
		  (if (not (null triple-map-list))
		      (progn (if debug (format t "~a(~a) " concept 
					       (round-fraction taxonomic-score)))
			     (push (list concept 
					 (list (insert-at-end sibling-match? (insert-at-end transformations triple-map-list)))
					 instance-map-list taxonomic-score)
				   result))))))
	    concept-lst)
    (if debug (format t "~%"))
    (values 
     (sort (copy-list result) #'(lambda(x y) (> (abs (nth 3 x))
						(abs (nth 3 y)))))
     (set-difference concept-lst (mapcar 'car result)))))

#|
(defun idiot()
  (multiple-value-bind
      (scenario compute-question yn-question)
      (ps-parse-question 
       "Hydrogen and oxygen react.
      What is the result of the reaction?")
    (dump-matching-map scenario "pset-match.html")))
|#



