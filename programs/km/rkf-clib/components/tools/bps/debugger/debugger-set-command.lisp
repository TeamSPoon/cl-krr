;;
;; $Id: debugger-set-command.lisp,v 1.3 2006/10/24 21:57:39 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun set-command(operands)
  (let* ((param (first  operands))
	 (value (second operands)))
    (cond ((or (equal '|queue_fn|              param))  (mutate-queue_fn              value))
	  ((or (equal '|heuristic|             param))  (mutate-heuristic             value))
	  ((or (equal '|pruning-condition|     param))  (mutate-pruning-condition value))
	  ((or (equal '|clock|                 param))  (mutate-param-clock           value))
	  (t "Error: Unknown SET operands"))))