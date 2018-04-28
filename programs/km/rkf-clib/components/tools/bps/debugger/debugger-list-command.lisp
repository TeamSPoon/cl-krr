;;
;; $Id: debugger-list-command.lisp,v 1.8 2007/06/19 21:24:55 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun parse-list-command (input)
  (let* ((command (car input)))
    (cond ((null command)  (list-viewpoint-hierarchy))
	  ((equal '|satisfyp|  command)   (list-satisfyp-command (cdr input)))
	  ((equal '|concept|   command)   (list-concept-command)))))

(defun list-viewpoint-hierarchy ()
  (if (not (null *CLOSEDLIST*))
      (progn (format t "-------------------~%")
	     (format t "Viewpoint hierarchy~%")
	     (format t "-------------------~%")
	     (if *ADHOC-MODE*
		 (tree-printer t (get-ancestors (first (last *CLOSEDLIST*))) nil)
	         (tree-printer t (get-descendants (root-viewpoint)) nil))
	     (format t "~%")
	     (format t "--------------------------------------------------~%")
	     (format t "Viewpoints were examined in the following sequence~%")
	     (format t "--------------------------------------------------~%")
	     (format t "~a~%"  *CLOSEDLIST*)
	     (format t "~%")
	     t)
       nil))

(defun list-satisfyp-command (operands)
  (if (null operands)
      (progn (satisfyp-tree-printer t
				    (get-descendants (root-viewpoint))
				    *PRUNING-CONDITION*)
	     (sieve-operation *PRUNING-CONDITION*))
      (let* ((term-cond-str (string-upcase (string (car operands))))
	     (pruning-condition (intern term-cond-str :km)))
	(progn (satisfyp-tree-printer t
				      (get-descendants (root-viewpoint))
				      pruning-condition)
	       (sieve-operation pruning-condition)))))

(defun sieve-operation (pruning-condition)
  (progn 
    (format t "~%The following Viewpoints satisfy pruning-condition, ~a.~%" pruning-condition)	   
    (format t "~a~%" (sieve-for-satisfiable-viewpoints))
    t))

(defun list-concept-command()
  (if (not (null *CLOSEDLIST*))
      (progn (format t "-------------------~%")
	     (format t "Viewpoint hierarchy~%")
	     (format t "-------------------~%")
	     (if *ADHOC-MODE*
		 (mapped-concept-tree-printer (get-ancestors   (first (last *CLOSEDLIST*))))
    	         (mapped-concept-tree-printer (get-descendants (root-viewpoint))))
	     (format t "~%")
	     (format t "--------------------------------------------------~%")
	     (format t "Viewpoints were examined in the following sequence~%")
	     (format t "--------------------------------------------------~%")
	     (format t "~a~%" *CLOSEDLIST*)
	     (format t "~%")
	     t)))

