;;
;; $Id: debugger-stepper-commands.lisp,v 1.11 2008/08/26 19:17:06 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun step-command(operands)
  (if *adhoc-mode* "STEP command not available in adhoc mode"
                   (perform-step)))

(defun next-command(operands)
  (if *adhoc-mode* 
      "NEXT command not available in adhoc mode"
      (next-viewpoint)))

;;Attempts the next viewpoint in *openlist*.
(defun next-viewpoint()
  (progn
    (format t "current-step ~A~%" *current-step*)
    (if (= *current-step* 1)
	(perform-step))
    (step-through-clock)))

(defun step-through-clock()
  (progn 
    (format t "current-step ~A~%" *current-step*)
    (if (> *current-step* 1)
	(progn (perform-step)
	       (step-through-clock)))))

(defun continue-command (operands)
  (if *adhoc-mode* "CONTINUE command not available in adhoc mode"
    (progn (step-through-clock)
	   (solve)
	   (sieve-operation *pruning-condition*))))

