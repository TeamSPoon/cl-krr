;;
;; $Id: ps-query-expansion.lisp,v 1.8 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *ps-query-expansion-data* nil)

(defun query-to-triple-lst(query)
  (let* ((frame  (nth 3 query))
	 (slot   (nth 1 query))
	 (filler (ps-instantiate-concept 
		  (ranges-of slot))))
    (values 
     (properly-terminate-triple-list
      (list 
       (list frame slot filler)))
     frame slot filler)))

(defun get-relations-in-query-path(query-path)
  (remove-if #'(lambda(x) (or (km-instancep x)
			      (member x '(|the| |of|))))
		 (flatten query-path)))

;; bad alternative query path is when number of relations in query-path != number of relations in triple-lst
;; This is caused by 
;; a) loops in triple-lst
;; b) reflexive triples in triple-lst
(defun bad-alternative-query-path?(query-path triple-lst)
  (let ((query-path-relation-count (length (get-relations-in-query-path query-path)))
	(triple-lst-relation-count (length (mapcar 'triple-relation (extract-non-instance-triples triple-lst)))))
    
    (not (= query-path-relation-count triple-lst-relation-count))))

(defun reset-ps-query-expansion()
  (reset-semantic-matcher)
  (setf *ps-query-expansion-data* nil))

(defun parse-ps-query-expansion-data()
  (values (cadr (assoc 'query *ps-query-expansion-data*))
	  (cadr (assoc 'candidate-lst *ps-query-expansion-data*))
	  (cadr (assoc 'applicable-rewrites *ps-query-expansion-data*))))

(defun mutate-ps-query-expansion-data(query candidate-lst applicable-rewrites)
  (progn 
    (reset-ps-query-expansion)
    (push (list 'query query) *ps-query-expansion-data*)
    (push (list 'candidate-lst candidate-lst) *ps-query-expansion-data*)
    (push (list 'applicable-rewrites applicable-rewrites) *ps-query-expansion-data*)))

(defun ps-alternative-queries-aux(query applicable-rewrites)
  (reset-semantic-matcher)
  (multiple-value-bind
      (query-graph query-frame query-slot query-filler)
      (query-to-triple-lst query)
    (let* ((pzyeh-graph (ps-convert-to-semantic-matcher-form query-graph))
	   (alternative-lst (back-chain-on-consequent pzyeh-graph applicable-rewrites))
	   (result nil))
      (values 
       alternative-lst
       (mapcar #'(lambda(alternative)
		   (let* ((triple-lst (convert-to-petes-form alternative))
			  (query-path (ps-get-query-path query-frame query-filler triple-lst)))
		     (if (not (bad-alternative-query-path? query-path triple-lst))
			 query-path)))
	       alternative-lst)
       applicable-rewrites))))

(defun ps-alternative-queries(&optional(query nil))
  (let ((debug nil))
  (if (not (null query))
      (progn 
	(mutate-ps-query-expansion-data query (list query) 0)
	(if debug (format t "0 step rewrite (results for original query).~%"))
	(list query))
    (ps-additional-alternative-queries))))

(defun ps-additional-alternative-queries(&optional(only-novel? nil))
  (let ((debug nil))
    (multiple-value-bind 
	(query prev-candidate-lst prev-applicable-rewrites)
	(parse-ps-query-expansion-data)
      (multiple-value-bind
	  (alternative-lst candidate-lst applicable-rewrites)	
	  (ps-alternative-queries-aux query (1+ prev-applicable-rewrites))
	(let ((result (remove nil
			      (if only-novel? 
				  (set-difference candidate-lst prev-candidate-lst :test 'equal)
				(union candidate-lst prev-candidate-lst :test 'equal)))))
	  (if debug 
	      (format t "~a step rewrite, returning ~A queries, ~A novel~%" (1+ prev-applicable-rewrites)
		      (length result)
		      (length (set-difference candidate-lst prev-candidate-lst :test 'equal))))
	  (mutate-ps-query-expansion-data query 
					  (union candidate-lst prev-candidate-lst :test 'equal)
					  applicable-rewrites)
	  result)))))

(defun ps-km-query-lst(query-lst)
  (let ((debug       nil)
	(breadcrumbs nil))
    (values
     (remove-duplicates
      (mappend #'(lambda(query)
		   (let ((*silent-spypoints* (determine-relations-to-spy-in-query-path query)))
		     (clear-silent-spy-log)
		     (let ((result (ps-km-query query debug)))
		       (setf breadcrumbs 
			     (union 
			      (properly-terminate-triple-list
			       (get-triples-from-subgoal-lst 
				(get-fillers-for-km-query-lst
				 (inspect-silent-spy-log))))
			      breadcrumbs
			      :test 'ps-triple-equal))
		       result)))
	       query-lst))
     breadcrumbs)))

(defun determine-relations-to-spy-in-query-path (query-path)
  (mapcar #'(lambda(relation)
	      (list '|the| relation '|of| '?y))
	  (remove-duplicates (get-relations-in-query-path query-path))))

(defun get-valid-km-flex-query(query)
  (let ((slot  (nth 1 query))
	(frame (nth 3 query)))
    (cond ((and (kb-objectp slot)
		(kb-objectp frame))   query)
	  ((not (= (length query) 4)) nil)
	  ((null query)               nil)
	  (t 
	   (let ((new-frame (ps-km-query frame)))
	     (if (and (not (null new-frame))
		      (= (length new-frame) 1)
		      (kb-objectp (car new-frame)))
		 (progn
		   (format t "Warning: ~s is not a flex query. Reduced ~S to be kb object ~S.~%" query frame new-frame)
		   `(|the| ,slot |of| ,(car new-frame)))
	       (progn (format t "Warning: ~s is not a flex query.~%" query)
		      nil)))))))
	
;;Ideally query is of the form (the ?x of <kb-object>)
;;Otherwise, will try to reduce something in (the ?x of something) to be a <ob-object>).
;;If reduction fails, no additional results will be returned in subsequent (ps-alternative-queries) call.
(defun ps-km-flex-query(&optional(input-query nil))
  (if (and (null input-query)
	   (not (null *ps-query-expansion-data*)))
      (ps-km-query-lst (ps-alternative-queries))
    (let ((new-query (get-valid-km-flex-query input-query)))
      (reset-ps-query-expansion)
      (if (null new-query)
	  (ps-km-query input-query)
	(ps-km-query-lst (ps-alternative-queries new-query))))))

(defun testcase(&optional(concept '|Cell|) (depth 2))
  (let ((original-query `(|the| |has-part| |of| ,(ps-instantiate-concept concept))))
    (dotimes (x depth)
      (multiple-value-bind
	  (result result-triples)
	  (if (zerop x) (ps-km-flex-query original-query)
	    (ps-km-flex-query))
	(multiple-value-bind
	    (skolem-frames non-skolem-frames)
	    (ps-get-frames-for-triples result-triples)
	  (format t "   result(~a)           : ~A~%" (length result) result)
	  (format t "   KM frames(~a triples): ~A~%" (length result-triples) (append skolem-frames non-skolem-frames)))))))
