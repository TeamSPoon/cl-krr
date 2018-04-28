;;
;; $Id: debugger-group-by-command.lisp,v 1.2 2006/10/24 21:57:39 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun group-command (operands)
  (if (and (equal '|by|   (first operands))
	   (equal '|user| (second operands)))
      (group-by-command (cddr operands))
      "Error: Unknown GROUP operands"))

(defun group-by-command (operands)
  (let* ((param (first  operands)))
    (cond ((equal param '|viewpoint-query|) 
	   (cluster-viewpoint-list-by-query *CLOSEDLIST*
					    *VIEWPOINT-QUERY-PREFIXES*))
	  ((equal param '|viewpoint-result-query|) 
	   (cluster-viewpoint-list-by-query *CLOSEDLIST*
					    *VIEWPOINT-RESULT-QUERY-PREFIXES*))
	  (t "Error: Unknown GROUP BY USER operands"))))

