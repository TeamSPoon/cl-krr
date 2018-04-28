;;
;; $Id: debugger-help-command.lisp,v 1.2 2006/10/24 21:57:39 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun help-command (operands)
  (let* ((output
	  '("ls"
	    "begin"
	    "continue"
	    "next"
	    "step"
	    "push"
	    "pop"
	    "apply"
	    "group-by"
	    "query"
	    "see"
	    "reset"
	    "adhoc"
	    "watch"
	    "km"
	    "lisp" 
	    "set"
	    "sieve"
	    "satisfyp"
	    "try"
	    "pick"
	    "unpick")))
    (mapcar #'(lambda(x)
		(format t "~a~%" x))
	    output)
    t))