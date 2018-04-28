;;
;; $Id: question-extraction.lisp,v 1.12 2008/10/19 19:25:45 jchaw Exp $
;;

#| 
  question-extraction.lisp -- given a CPL encoding of a question, translate it to (the slot of X) form of KM 
|#
(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun find-all-triples-by-pattern (match-pattern triples)
  (remove-if-not #'(lambda (triple) (minimatch triple match-pattern)) triples)
  )

#| sample input:     
    (_Duration2363 instance-of Duration-Value) 
    (_Fall2361 instance-of Fall) 
    (_What2362 equal _Duration2363) 
    (_Duration2363 duration-of _Fall2361) 
    (_What2362 input-word (:pair "what" n)) 
    (_What2362 det det-what) 
    (_Fall2361 input-word (:pair "fall" n)) 
    (_Fall2361 det det-the) 
    (_Duration2363 input-word (:pair "duration" n)) 
    (_Duration2363 det det-the) 
    (_What2362 query-varp t) |#
#| sample output:
    ((the value of (the duration of _Fall2361)))
|#
(defun ps-extract-question0 (triples query-vars)
  (let (;(query-vars (mapcar #'first (find-all-triples-by-pattern  '(?x |query-varp| |t|) triples)))
	(retval nil))
    (dolist (query-var query-vars)
	    (setf retval
		  (append retval
			  (extract-1-question-var query-var triples)))
	    )
    retval
    ))

;;Are these triples in KM memory when ps-extract-question is called?
;;We need to be careful of cases where some query-var may have correct 
;;query-triples, while other query-vars are to find specific (pictionary)
;;matches to KB instances, i.e they are suppose to become instance-of triples.
(defun ps-extract-question(triples query-vars)
  (let* ((extracted-queries (ps-extract-question0 triples query-vars))	 
	 #|;;Should we really be filtering out such queries?
	 (temp-result       (remove-if #'(lambda(query) 
					   (not (forward-relationp (nth 1 query))))
				       extracted-queries))
	 |#
	 (temp-result extracted-queries)
        )
;    (if (null temp-result) 
;	(mapcar #'(lambda(x) `(|the| |instance-of| |of| ,x)) query-vars)
;        temp-result)
    temp-result
    ))

(defun extract-1-question-var (query-var triples)
  (let* (;(queried-value (third (first (find-all-triples-by-pattern `(,query-var |equal| ?x) triples)))) ;; assume it only equals to one thing
	 (queried-value query-var)
	 (interesting-triples (remove-if-not #'real-triplep triples)) ;; now filter all noisy slots
	 )
  ;; convert rest of them to query paths
    (mapcar #'triple2query
	    (append
	     (find-all-triples-by-pattern `(?x ?y ,queried-value) interesting-triples)
	     (mapcar #'inverse-triple (find-all-triples-by-pattern `(,queried-value ?x ?y) interesting-triples))))
    ))

(defun real-triplep (triple)
  (let ((slot (second triple)))
    (and (not (eql slot '|related-to|))   ;;CPL superfluous slot.
	 (or (ps-km-query `(,slot |isa| |Relation|))
	     (ps-km-query `(,slot |isa| |Property|))
	     (ps-km-query `(,slot |isa| |KM-Slot-Group|)))) ;; added for chemical-equation and equation slots
    ))

(defun triple2query (triple)
  `(|the| ,(second triple) |of| ,(first triple)))

(defun inverse-triple (triple)
  (let ((inverse-slot (first (ps-km-query `(|the| |inverse| |of| ,(second triple))))))
    `(,(third triple) ,inverse-slot ,(first triple))
    ))