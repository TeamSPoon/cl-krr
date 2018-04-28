;;
;; $Id: debugger-query-command.lisp,v 1.3 2006/10/24 21:57:39 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun query-viewpoint-result (vp-inst)
  (let* ((query-prefixes nil))
    (apply-query-on-viewpoint-result-command vp-inst query-prefixes)))

(defun query-all-viewpoint-result()
  (let* ((query-prefixes nil))
    (apply-query-on-all-viewpoint-result-command query-prefixes)))

(defun query-viewpoint-result-for-type (vp-inst)
  (let* ((query-prefixes '((|the| |instance-of| |of|))))
    (apply-query-on-viewpoint-result-command vp-inst query-prefixes)))

(defun query-all-viewpoint-result-for-type()
  (let* ((query-prefixes '((|the| |instance-of| |of|))))
    (apply-query-on-all-viewpoint-result-command query-prefixes)))

(defun query-result-type-command (operands)
  (let* ((param (first  operands)))
    (cond ((viewpoint-existp  param)    (query-viewpoint-result-for-type param))
	  ((or (equal '|all|  param))   (query-all-viewpoint-result-for-type))
	  (t "Error: Unknown QUERY RESULT TYPE operands."))))

(defun query-result-command (operands)
  (let* ((param (first  operands)))
    (cond ((viewpoint-existp  param)    (query-viewpoint-result param))
	  ((or (equal '|type|  param))  (query-result-type-command (cdr operands)))
	  ((or (equal '|all|  param))   (query-all-viewpoint-result))
	  (t "Error: Unknown QUERY RESULT operands."))))

(defun query-viewpoint-for-matched-model (vp-inst)
  (let* ((query-prefixes '((|the| |viewpoint-target| |of|)
			   (|the| |instance-of| |of|))))
    (apply-query-on-viewpoint-command vp-inst query-prefixes)))

(defun query-all-viewpoint-for-matched-model()
  (let* ((query-prefixes '((|the| |viewpoint-target| |of|)
			   (|the| |instance-of| |of|))))
    (apply-query-on-all-viewpoint-command query-prefixes)))

(defun query-viewpoint-answer-page-command(operands)
  (let* ((param (first  operands))
	 (query-prefixes '((|the| |viewpoint-answer-page| |of|))))
    (cond ((viewpoint-existp  param)   
	   (apply-query-on-viewpoint-command param query-prefixes))
	  ((or (equal '|all|  param))  
	   (apply-query-on-all-viewpoint-command query-prefixes))
	  (t "Error: Unknown QUERY VIEWPOINT ANSWER PAGE operands."))))

(defun query-viewpoint-answer-detail-command(operands)
  (let* ((param (first  operands))
	 (query-prefixes '((|the| |viewpoint-detail| |of|))))
    (cond ((viewpoint-existp  param)   
	   (apply-query-on-viewpoint-command param query-prefixes))
	  ((or (equal '|all|  param))  
	   (apply-query-on-all-viewpoint-command query-prefixes))
	  (t "Error: Unknown QUERY VIEWPOINT ANSWER DETAIL operands."))))

(defun query-viewpoint-answer-command (operands)
  (let* ((param (first  operands))
	 (query-prefixes '((|the| |viewpoint-answer| |of|))))
    (cond ((viewpoint-existp     param)   (apply-query-on-viewpoint-command param query-prefixes))
	  ((or (equal '|page|    param))  (query-viewpoint-answer-page-command   (cdr operands)))
	  ((or (equal '|detail|  param))  (query-viewpoint-answer-detail-command (cdr operands)))
	  ((or (equal '|all|     param))  (apply-query-on-all-viewpoint-command query-prefixes))
	  (t "Error: Unknown QUERY VIEWPOINT ANSWER operands."))))

(defun query-viewpoint-model-command (operands)
  (let* ((param (first  operands)))
    (cond ((viewpoint-existp  param)    (query-viewpoint-for-matched-model param))
	  ((or (equal '|all|  param))   (query-all-viewpoint-for-matched-model))
	  (t "Error: Unknown QUERY VIEWPOINT MODEL operands."))))

(defun query-viewpoint-command (operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|model|  param))  (query-viewpoint-model-command  (cdr operands)))
	  ((or (equal '|answer| param))  (query-viewpoint-answer-command (cdr operands)))
	  (t "Error: Unknown QUERY VIEWPOINT operands."))))
	  
(defun query-command (operands)
  (let* ((param (first operands)))
    (cond ((or (equal '|result|  param))     (query-result-command    (cdr operands)))
	  ((or (equal '|viewpoint|  param))  (query-viewpoint-command (cdr operands)))
	  (t "Error: Unknown QUERY operands."))))

