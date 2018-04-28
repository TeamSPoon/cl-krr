;;
;; $Id: controller-mutate-operations.lisp,v 1.12 2008/10/20 21:52:38 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun mutate-clone-counter-list(new-clone-counter-list)
  (progn 
    (if *watch-param-clone-counter-list*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CLONE-COUNTER-LIST* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CLONE-COUNTER-LIST* has value ~a~%" *clone-counter-list*)
	       (format t " After mutation, *CLONE-COUNTER-LIST* has value ~a~%" new-clone-counter-list)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *clone-counter-list* new-clone-counter-list)))

(defun mutate-cpl-scenario(new-cpl-scenario)
  (progn 
    (if *watch-param-cpl-scenario*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CPL-SCENARIO* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CPL-SCENARIO* has value ~a~%" *cpl-scenario*)
	       (format t " After mutation, *CPL-SCENARIO* has value ~a~%" new-cpl-scenario)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *cpl-scenario* new-cpl-scenario)))

(defun mutate-cpl-compute-question(new-cpl-compute-question)
  (progn 
    (if *watch-param-cpl-compute-question*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CPL-COMPUTE-QUESTION* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CPL-COMPUTE-QUESTION* has value ~a~%" *cpl-compute-question*)
	       (format t " After mutation, *CPL-COMPUTE-QUESTION* has value ~a~%" new-cpl-compute-question)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *cpl-compute-question* new-cpl-compute-question)))

(defun mutate-openlist(new_openlist)
  (progn 
    (if *watch-param-openlist*
	(progn (format t "--------------------------------------------~%")
	       (format t "Data Structure *OPENLIST* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation")
	       (print (extract-configs-from-openlist *OPENLIST*))
	       (format t "~%")
	       (format t " After mutation")
	       (print (extract-configs-from-openlist new_openlist))
	       (format t "~%")
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *openlist* (remove-duplicates new_openlist))
    (if (= (get-BPSCurStage) 3)
	(set-BPSMaxPosition (length *OPENLIST*)))))

(defun mutate-closedlist(new_closedlist)
  (progn 
    (if *watch-param-closedlist*
	(progn (format t "--------------------------------------------~%")
	       (format t "Data Structure *CLOSEDLIST* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation")
	       (print *CLOSEDLIST*)
	       (format t "~%")
	       (format t " After mutation")
	       (print new_closedlist)
	       (format t "~%")
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *closedlist* (remove-duplicates new_closedlist))
    (if (= (get-BPSCurStage) 3)
	(set-BPSCurPosition (length *CLOSEDLIST*)))))

(defun mutate-queue_fn(new_queue_fn)
  (progn 
    (if *watch-param-queue_fn*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *QUEUE_FN* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *QUEUE_FN* has value ~a~%" *queue_fn*)
	       (format t " After mutation, *QUEUE_FN* has value ~a~%" new_queue_fn)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *queue_fn* new_queue_fn)))

(defun mutate-ignoredlist(new-val)
  (setq *ignoredlist* new-val))

(defun mutate-heuristic(new_heuristic)
  (progn 
    (if *watch-param-heuristic*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *HEURISTIC* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *HEURISTIC* has value ~a~%" *heuristic*)
	       (format t " After mutation, *HEURISTIC* has value ~a~%" new_heuristic)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *heuristic* new_heuristic)))

;; (defun mutate-pruning-condition(new_pruning-condition)
;;   (progn 
;;     (if *watch-param-pruning-condition*
;; 	(progn (format t "--------------------------------------------~%")
;; 	       (format t "Variable *PRUNING-CONDITION* is being mutated~%")
;; 	       (format t "--------------------------------------------~%")))
;;     (setq *PRUNING-CONDITION* new_pruning-condition)))

(defun mutate-clock(new_clock)
  (progn 
    (if *watch-param-clock*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CLOCK* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CLOCK* has value ~a~%" *clock*)
	       (format t " After mutation, *CLOCK* has value ~a~%" new_clock)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *clock* new_clock)))

(defun mutate-CURRENT-NODE(new_CURRENT-NODE)
  (progn 
    (if *watch-param-CURRENT-NODE*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CURRENT-NODE* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CURRENT-NODE* has value ~a~%" *CURRENT-NODE*)
	       (format t " After mutation, *CURRENT-NODE* has value ~a~%" new_CURRENT-NODE)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *CURRENT-NODE* new_CURRENT-NODE)))

(defun mutate-CURRENT-VP-INST(new_CURRENT-VP-INST)
  (progn 
    (if *watch-param-CURRENT-VP-INST*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CURRENT-VP-INST* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CURRENT-VP-INST* has value ~a~%" *CURRENT-VP-INST*)
	       (format t " After mutation, *CURRENT-VP-INST* has value ~a~%" new_CURRENT-VP-INST)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *CURRENT-VP-INST* new_CURRENT-VP-INST)))

(defun mutate-CURRENT-STEP(new_CURRENT-STEP)
  (progn 
    (if *watch-param-CURRENT-STEP*
	(progn (format t "--------------------------------------------~%")
	       (format t "Variable *CURRENT-STEP* is being mutated~%")
	       (format t "--------------------------------------------~%")
	       (format t "Before mutation, *CURRENT-STEP* has value ~a~%" *CURRENT-STEP*)
	       (format t " After mutation, *CURRENT-STEP* has value ~a~%" new_CURRENT-STEP)
	       (format t "--------------------------------------------~%")
	       (format t "~%")))
    (setq *CURRENT-STEP* new_CURRENT-STEP)))

(defun mutate-VIEWPOINT-RESULT-QUERY-PREFIXES(new_VIEWPOINT-RESULT-QUERY-PREFIXES)
  (let ((existing-km-query (attach-query 
			    (cons '<viewpoint-result> 
				  *viewpoint-result-query-prefixes*)))
	(new-km-query (attach-query 
		       (cons '<viewpoint-result> 
			     new_viewpoint-result-query-prefixes))))
  (progn 
    (if *watch-param-VIEWPOINT-RESULT-QUERY-PREFIXES*
	(progn (format t "--------------------------------------------------~%")
	       (format t "Variable *VIEWPOINT-RESULT-QUERY-PREFIXES* is being mutated~%")
	       (format t "--------------------------------------------------~%")
	       (format t "Before mutation, the KM query was ~a~%" 
		       existing-km-query)
	       (format t " After mutation, the KM query is ~a~%"
		       new-km-query)
	       (format t "--------------------------------------------------~%")
	       (format t "~%")))
    (setq *VIEWPOINT-RESULT-QUERY-PREFIXES* 
	  new_VIEWPOINT-RESULT-QUERY-PREFIXES))))

(defun mutate-VIEWPOINT-QUERY-PREFIXES(new_VIEWPOINT-QUERY-PREFIXES)
  (let ((existing-km-query (attach-query (cons '<viewpoint> 
					       *viewpoint-query-prefixes*)))
	(new-km-query (attach-query (cons '<viewpoint> 
					  new_viewpoint-query-prefixes))))
    (progn 
      (if *watch-param-VIEWPOINT-QUERY-PREFIXES*
	(progn (format t "--------------------------------------------------~%")
	       (format t "Variable *VIEWPOINT-QUERY-PREFIXES* is being mutated~%")
	       (format t "--------------------------------------------------~%")
	       (format t "Before mutation, the KM query was ~a~%" 
		       existing-km-query)
	       (format t " After mutation, the KM query is ~a~%"
		       new-km-query)
	       (format t "--------------------------------------------------~%")
	       (format t "~%")))
    (setq *VIEWPOINT-QUERY-PREFIXES* 
	  new_VIEWPOINT-QUERY-PREFIXES))))
