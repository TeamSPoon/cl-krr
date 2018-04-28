;
;; $Id: misc.lisp,v 1.36 2009/02/14 20:05:19 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun mutate-hash-table (key value hash-table &optional(verbose nil))
  (let ((old-value (gethash key hash-table)))
    (cond ((not (null old-value))
	   (let ((appended-value (if (null old-value)
				     (list value)
				     (cons value old-value))))
	     (if verbose (format t "BPS: Value for ~a already present, ~a, in hash-table. Appending. as ~a ~%" key old-value appended-value))
	     (setf (gethash key hash-table) appended-value)))
	  (t 
	   (setf (gethash key hash-table) (list value))))))

(defun replace-all (string part replacement &optional (test #'char=))
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun replace-if (test new list)
  (cond ((null list) nil)
        ((atom list)	 
	 (if (funcall test list) new list))
	((funcall test list) new)
        (t
         (cons (replace-if test new (first list))
               (replace-if test new (rest list))))))

(defun replace-if-equal (test-val new list)
  (cond ((null list) nil)
	((equal test-val list) new)
        ((atom list) list)
        (t
         (cons (replace-if-equal test-val new (first list))
               (replace-if-equal test-val new (rest list))))))

;;Returns subset of lst that satisfies predicate.
(defun sieve (lst predicate)
  (cond ((null lst) nil)
	((funcall predicate (car lst)) (cons (car lst)
					     (sieve (cdr lst) predicate)))
	(t (sieve (cdr lst) predicate))))

;;Returns first N elements in lst.
(defun firstn (lst n)
  (cond ((< n 0) nil)
	((< (length lst) n)	lst)
	(t (reverse (last (reverse lst) n)))))

;;Returns the rest of lst after the 1st N elements.
(defun aftern (lst n)
  (cond ((>= n (length lst)) nil)
	((and (> n 0)
	      (> (length lst) n))
	 (aftern (cdr lst) (1- n)))
	(t lst)))

;;Inserts element at end of a list
(defun insert-at-end (x lst)
  (reverse (cons x (reverse lst))))

;;Removes last element of a list
(defun remove-last (lst)
  (reverse (cdr (reverse lst))))

;;LIFO insertion of elements.
(defun stack-insert (nodes stack)
  (append nodes stack))

(defun sorted-queue-insert-sub (node ds)
  (cond ((endp ds) (list node))
	((cheaper-nodep node (car ds)) (cons node ds))
	(t (cons (car ds) 
		 (sorted-queue-insert-sub node (cdr ds))))))

(defun sorted-queue-insert(nodes ds)
  (cond ((endp nodes) ds)
	(t (sorted-queue-insert (cdr nodes)
				(sorted-queue-insert-sub (car nodes) ds)))))

;;Natural number predicate
(defun natp (x)
  (and (integerp x)
       (>= x 0)))

(defun nest-flat-list (flat-list)
  (if (null (cadr flat-list)) 
      (list (car flat-list))
      (append (list (car flat-list))
	      (list (nest-flat-list (cdr flat-list))))))

(defun flip-linear-tree(vp-linear-tree)
  (nest-flat-list (reverse (flatten vp-linear-tree))))

(defun get-time-of-day()
  (let ((day-names '("Monday" "Tuesday" "Wednesday"
		     "Thursday" "Friday" "Saturday"
		     "Sunday")))
    (multiple-value-bind
      (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
      (format nil "~2,'0d:~2,'0d:~2,'0d, ~a, ~d/~2,'0d/~d (GMT~@d)"
	      hour
	      minute
	      second
	      (nth day-of-week day-names)
	      month
	      date
	      year
	      (- tz)))))

(defun pair-up (lst1 lst2)
  (if (and (listp lst1)
	   (listp lst2)
	   (equal (length lst1) (length lst2)))
      (mapcar #'(lambda(x y)
		  (list x y))
	      lst1 lst2)))

(defun make-list-elements-into-string (x)
  (cond ((null x) ())
	((atom x) (string x))
	(t (cons (make-list-elements-into-string (car x))
		 (make-list-elements-into-string (cdr x))))))

;;Replaces element in list if replacement exist. If not, original value is preserved.
(defun replace-elements-in-list(input map)
  (cond ((null input) ())
        ((atom input) (let* ((replacement 
			      (cdr (assoc input map :test 'equal))))
			(if (null replacement)
			    input
			  (if (atom replacement)
			      replacement
			    (car replacement)))))
	(t (cons (replace-elements-in-list (car input) map)
		 (replace-elements-in-list (cdr input) map)))))

;;Reorders source list such that elements in target are at head of list.
(defun promote-elements (target source)
  (cond ((null target) source)
	(t (let ((target-elem (car target)))
	     (cond ((member target-elem source)
		    (promote-elements (cdr target)
				      (cons target-elem (remove target-elem source))))
		   (t (promote-elements (cdr target)
					source)))))))

;;Stringifies something.
(defun stringify (x)
  (cond ((stringp x) x)
        ((symbolp x) (copy-seq (symbol-name x)))   ; avoids Sun bug
	(t (princ-to-string x))))

;;Inverts map
;; a) (invert-map '((a . b) (c . d)))     => '((b . a) (d . c))
;; b) (invert-map '((a (1 2)) (b (2 3)))) => '(((1 2) a) ((2 3) b))
(defun invert-map(map)
  (mapcar #'(lambda(entry)
	      (cond ((consp (cdr entry))
		     (list (cadr entry) (car entry)))
		    (t (cons (cdr entry) (car entry)))))
	  map))

;;Inverts all slots in list
(defun invert-slot-list (slot-list)
  (mapcar #'(lambda(slot)
	      (invert-slot slot))
	  slot-list))

;;Predicate to test if input is a km aggregate.
(defun km-aggregatep (input)
  (or (km-setp  input)
      (km-pairp input)
      (km-bagp  input)
      (km-seqp  input)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'mappend)
    (defun mappend (fn &rest lsts)
      (apply #'append (apply #'mapcar fn lsts)))))

(defun divide-lst(x count)
  (cond ((null x)())
	(t (cons (firstn x count)
		 (divide-lst (aftern x count) count)))))

(defun all-pairs(input)
  (mappend #'(lambda(x)
	      (mapcar #'(lambda(y)
			  (list x y))
		      input))
	  input))

;Return the sequence with elements in random order.
;e.g., (scramble '(0 1 2 3 4 5 6 7 8 9)) => (3 2 0 5 6 4 9 1 7 8)
(defun scramble (sequence)
   (let ((length (length sequence)))
     (dotimes (i (1- length) sequence)
       (rotatef
        (elt sequence i)
        (elt sequence (random (+ i (- length  i)))))))) 

(defun powerset(l)
  (if (null l)
    '(nil)
    (let ((ps (powerset (cdr l))))
      (append ps (mapcar #'(lambda (x) (cons (car l) x)) ps)))))

;;checks if (x y) is in ((a b) (c d) (.. ..)) assoc list.
(defun mapped-p (x y instance-map)
  (not 
   (null 
    (remove nil
	    (mapcar #'(lambda(z)
			(or (equal (list x y) z)
			    (equal (list x y) (reverse z))))
		    instance-map)))))

;;(intersection x y z) using the 'equal
(defun intersection-n-arg(input)
  (cond ((null input) ())
	((and (listp input)
	      (= (length input) 1))
	 (car input))
	(t (intersection (car input)
			 (intersection-n-arg (cdr input))))))

;;Assumes x [0,1] and returns a float, up to 2 decimal places.
(defun round-fraction(x)
  (cond ((null x) 0)
	(t (/ (round (* x 100.0))
	      100.0))))

(defun reverse-assoc (x map)
  (assoc x (invert-map map)))

(defun safe-<(input-x input-y)
  (let ((x (if (numberp input-x) input-x most-positive-fixnum))
	(y (if (numberp input-y) input-y most-positive-fixnum)))
    (cond ((and (numberp x)
		(numberp y))
	   (< x y))
	(t nil))))

(defun safe->(input-x input-y)
  (let ((x (if (numberp input-x) input-x 0))
	(y (if (numberp input-y) input-y 0)))
    (cond ((and (numberp x)
		(numberp y))
	   (> x y))
	  (t nil))))

(defun dump-clib-concept-cache()
  (write-table "clib-concept-cache.html"
	       '(concept graph)
	       (mapcar #'(lambda(x)
			   (multiple-value-bind 
			       (concept root graph)
			       (parse-clib-concept-cache-entry x)
			     (list concept 
				   (htmlify-km-assertion-list 
				    (triples-to-km-assertions 
				     (remove-irrelevant-km-triples
				      graph))))))
		       *clib-persistent-hash*)))

(defun parse-clib-concept-cache-entry(x)
  (let* ((concept (nth 0 x))
	 (content (nth 1 x))
	 (root    (nth 0 content))
	 (graph-content (nth 1 content))
	 (graph (nth 0 graph-content)))
    (values concept 
	    root 
	    graph)))

(defun count-substring-occurrence(sub str)
  (cond ((null (search sub str)) 0)
	(t (1+ (count-substring-occurrence sub 
					   (subseq str 
						   (+ (search sub str)
						      (length sub))
						   (length str)))))))


#|
(cross-product '((a b) (c d e) (f g h i))))
returns
((A C F) (A C G) (A C H) (A C I) (A D F) (A D G) (A D H)
 (A D I) (A E F) (A E G) (A E H) (A E I) (B C F) (B C G)
 (B C H) (B C I) (B D F) (B D G) (B D H) (B D I) (B E F)
 (B E G) (B E H) (B E I))
|#
(defun cross-product (lst)
  (cond ((null lst) nil)
	((null (cdr lst))  (car lst))
	(t (get-cross-prod (car lst)
			   (cross-product (cdr lst))
			   #'list))))

(defun flatten-to-triple-lst(input)
  (cond ((atom input) nil)
	((equal input (flatten input)) (list input))
	(t (append (flatten-to-triple-lst (car input))
		 (flatten-to-triple-lst (cdr input))))))

;;Converts internal time to seconds.
(defun convert-internal-time-to-seconds(x)
  (float (/ x internal-time-units-per-second)))

;; (defun get-assoc-count(idx assoc-lst)
;;   (cond ((null assoc-lst)             0)
;; 	((null (assoc idx assoc-lst)) 0)
;; 	(t 
;; 	 (1+ 
;; 	  (get-assoc-count idx
;; 			   (cdr (member (assoc idx assoc-lst) assoc-lst)))))))

;; (defun get-all-assoc-values-for(idx assoc-lst)
;;   (cond ((null (assoc idx assoc-lst)) ())
;; 	(t (append 
;; 	    (cadr (assoc idx assoc-lst))
;; 	    (get-all-assoc-values-for idx (remove-if #'(lambda(x) (equal x (assoc idx assoc-lst))) assoc-lst))))))

;; (defun merge-assoc-lst(assoc-lst)
;;   (mapcar #'(lambda(idx)
;; 	      (list idx (get-all-assoc-values-for idx assoc-lst)))
;; 	  (remove-duplicates (mapcar 'car assoc-lst))))

(defun collate-assoc-list(alist)
  (remove-duplicates
   (mapcar #'(lambda(key)
	       (list key
		     (remove-duplicates
		      (mapcar 'cadr
			      (remove-if-not 
			       #'(lambda(entry)
				   (equal (car entry) key))
			       alist))
		      :test 'equal)))
	   (mapcar 'car alist))
   :test 'equal))

(defun make-delimited-string(lst separator)
  (let ((s (make-string-output-stream)))
    (if (not (null lst))
	(format s "~a" (car lst)))
    (dolist (x (cdr lst))
      (format s "~a~a" separator x))
    (get-output-stream-string s)))

(defun break-string(string delimiter-str)
  (if (numberp (search delimiter-str string))
      (progn 
	(cons (subseq string 
		      0
		      (search delimiter-str string))
	      (break-string
	       (subseq string 
		       (1+ (search delimiter-str string))
		       (length string))
	       delimiter-str)))
      (list string)
))

