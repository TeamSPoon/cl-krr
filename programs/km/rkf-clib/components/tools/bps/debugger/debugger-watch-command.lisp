;;
;; $Id: debugger-watch-command.lisp,v 1.6 2008/02/02 18:58:04 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun set-watch-param-openlist()
  (setq *watch-param-openlist* 
	(not *watch-param-openlist*)))

(defun set-watch-param-closedlist()
  (setq *watch-param-closedlist* 
	(not *watch-param-closedlist*)))

(defun set-watch-param-queue_fn()
  (setq *watch-param-queue_fn* 
	(not *watch-param-queue_fn*)))

(defun set-watch-param-heuristic()
  (setq *watch-param-heuristic* 
	(not *watch-param-heuristic*)))

(defun set-watch-param-pruning-condition()
  (setq *watch-param-pruning-condition* 
	(not *watch-param-pruning-condition*)))

(defun set-watch-param-clock()
  (setq *watch-param-clock* 
	(not *watch-param-clock*)))

(defun set-watch-param-current-node()
  (setq *watch-param-current-node* 
	(not *watch-param-current-node*)))

(defun set-watch-param-current-vp-inst()
  (setq *watch-param-current-vp-inst* 
	(not *watch-param-current-vp-inst*)))

(defun set-watch-param-current-step()
  (setq *watch-param-current-step* 
	(not *watch-param-current-step*)))

(defun set-watch-param-cpl-scenario()
  (setq *watch-param-cpl-scenario* 
	(not *watch-param-cpl-scenario*)))

(defun set-watch-param-cpl-compute-question()
  (setq *watch-param-cpl-compute-question* 
	(not *watch-param-cpl-compute-question*)))

(defun set-watch-param-viewpoint-query()
  (setq *watch-param-viewpoint-query-prefixes* 
	(not *watch-param-viewpoint-query-prefixes*)))

(defun set-watch-param-viewpoint-result-query()
  (setq *watch-param-viewpoint-result-query-prefixes* 
	(not *watch-param-viewpoint-result-query-prefixes*)))

(defun set-watch-clone-counter-list()
  (setq *watch-param-clone-counter-list*
	(not *watch-param-clone-counter-list*)))

(defun set-watch-param-all-variables()
  (progn (setq *watch-param-openlist*                          t)
	 (setq *watch-param-closedlist*                        t)
	 (setq *watch-param-queue_fn*                          t)
	 (setq *watch-param-heuristic*                         t)
	 (setq *watch-param-pruning-condition*                 t)
	 (setq *watch-param-clock*                             t)
	 (setq *watch-param-current-node*                      t)
	 (setq *watch-param-current-vp-inst*                   t)
	 (setq *watch-param-current-step*                      t)
	 (setq *watch-param-cpl-scenario*                      t)
	 (setq *watch-param-cpl-compute-question*              t)
	 (setq *watch-param-viewpoint-query-prefixes*          t)
	 (setq *watch-param-viewpoint-result-query-prefixes*   t)
	 (setq *watch-param-clone-counter-list*                t)
	 (format t "All variables are now being watched.~%")))

(defun reset-watch-variables()
  (progn (setq *watch-param-openlist*                        nil)
	 (setq *watch-param-closedlist*                      nil)
	 (setq *watch-param-queue_fn*                        nil)
	 (setq *watch-param-heuristic*                       nil)
	 (setq *watch-param-pruning-condition*               nil)
	 (setq *watch-param-clock*                           nil)
	 (setq *watch-param-current-node*                    nil)
	 (setq *watch-param-current-vp-inst*                 nil)
	 (setq *watch-param-current-step*                    nil)
	 (setq *watch-param-cpl-scenario*                    nil)
	 (setq *watch-param-cpl-compute-question*            nil)
	 (setq *watch-param-viewpoint-query-prefixes*        nil)
	 (setq *watch-param-viewpoint-result-query-prefixes  nil)
	 (setq *watch-param-clone-counter-list*              nil)
	 (format t "Watch variables are reset.~%")))

(defun watch-command(operands)
  (let* ((param (first  operands)))
    (cond ((equal '|openlist|               param)
	   (set-watch-param-openlist))
	  ((equal '|closedlist|             param)
	   (set-watch-param-closedlist))
	  ((equal '|queue_fn|               param)
	  (set-watch-param-queue_fn))
	  ((equal '|heuristic|              param)
	   (set-watch-param-heuristic))
	  ((equal '|pruning-condition|      param)
	   (set-watch-param-pruning-condition))
	  ((equal '|clock|                  param)
	   (set-watch-param-clock))
	  ((equal '|current-node|           param)
	   (set-watch-param-current-node))
	  ((equal '|current-vp-inst|        param)
	   (set-watch-param-current-vp-inst))
	  ((equal '|current-step|           param)
	   (set-watch-param-current-step))
	  ((equal '|cpl-scenario|           param)
	   (set-watch-param-cpl-scenario))
	  ((equal '|cpl-compute-question|   param)
	   (set-watch-param-cpl-compute-question))
	  ((equal '|viewpoint-query|        param)
	   (set-watch-param-viewpoint-query))
	  ((equal '|viewpoint-result-query| param)
	   (set-watch-param-viewpoint-result-query))
	  ((equal '|clone-counter-list| param)
	   (set-watch-param-clone-counter-list))
	  ((equal '|all|                    param) 
	   (set-watch-param-all-variables))
	  ((equal '|off|                    param)
	   (reset-watch-variables))
	  (t "Error: Unknown WATCH operands"))))

