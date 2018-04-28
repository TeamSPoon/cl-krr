;;
;; $Id: debugger-apply-command.lisp,v 1.3 2006/10/24 21:57:39 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun apply-query-on-viewpoint-command (vp-inst query-prefixes)
  (let* ((vp-result-triple
	  (apply-query-on-viewpoint vp-inst
				    query-prefixes)))
    (format nil "Querying ~s, ~% i.e. ~s~%    => ~s~%"
	    (first vp-result-triple)
	    (second vp-result-triple)
	    (third vp-result-triple))))

(defun apply-query-on-all-viewpoint-command (query-prefixes)
  (if *CLOSEDLIST*
      (reduce #'(lambda (x y) 
		  (format nil "~a~a" x y))
	      (mapcar #'(lambda (z) 
			  (apply-query-on-viewpoint-command z 
							    query-prefixes)) 
		      *CLOSEDLIST*))
      ""))

(defun apply-query-on-viewpoint-result-command (vp-inst query-prefixes)
  (let* ((vp-result-triple
	  (apply-result-slot-query-on-viewpoint vp-inst
						query-prefixes)))
    (format nil "Querying ~s, ~% i.e. ~s~%    => ~s~%"
	    (first vp-result-triple)
	    (second vp-result-triple)
	    (third vp-result-triple))))

(defun apply-query-on-all-viewpoint-result-command (query-prefixes)
  (if *CLOSEDLIST*
      (reduce #'(lambda (x y) 
		  (format nil "~a~a" x y))
	      (mapcar #'(lambda (z) 
			  (apply-query-on-viewpoint-result-command z 
								   query-prefixes)) 
		      *CLOSEDLIST*))
      ""))

(defun apply-command(operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|user|        param))
	   (apply-user-query (cdr operands)))
	  (t "Error: Unknown APPLY operands"))))

(defun apply-user-query(operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|viewpoint-query|          param))
	   (apply-user-viewpoint-query (cdr operands)))
	  ((or (equal '|viewpoint-result-query|   param)) 	   
	   (apply-user-viewpoint-result-query (cdr operands)))
	  (t "Error: Unknown APPLY USER operands"))))

(defun apply-user-viewpoint-query(operands)
  (let* ((param (first  operands)))
    (cond ((viewpoint-existp  param) 
	   (apply-query-on-viewpoint-command param 
					     *viewpoint-query-prefixes*))
	  ((equal '|all|  param)
	   (apply-query-on-all-viewpoint-command *viewpoint-query-prefixes*))
	  (t "Error: Unknown APPLY USER VIEWPOINT-QUERY operands"))))

(defun apply-user-viewpoint-result-query(operands)
  (let* ((param (first  operands)))
    (cond ((viewpoint-existp  param) 
	   (apply-query-on-viewpoint-result-command param 
						    *viewpoint-result-query-prefixes*))
	  ((equal '|all|  param)
	   (apply-query-on-all-viewpoint-result-command *viewpoint-result-query-prefixes*))
	  (t "Error: Unknown APPLY USER VIEWPOINT-RESULT-QUERY operands"))))

