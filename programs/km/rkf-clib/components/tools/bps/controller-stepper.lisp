;;
;; $Id: controller-stepper.lisp,v 1.65 2009/05/29 20:32:27 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Stepper function that invokes the appropriate step depending on *CURRENT-STEP* value.
(defun perform-step()
  (cond ((= *CURRENT-STEP* 1) (step1))
	((= *CURRENT-STEP* 2) (step2))
	((= *CURRENT-STEP* 3) (step3))))

(defun increment-step()
  (progn
  (if (= *current-step* *max-step*)
      (setf *current-step*  1)
      (setf *current-step*  (1+ *current-step*)))
  nil))

;;Restarts clock cycle, i.e. set *CURRENT-STEP* to 1.
(defun restart-clock()
  (setq *CURRENT-STEP* 1))

(defun get-next-node-in-openlist (&optional(verbose t))
  (progn
    (topup-openlist)
    (if (not (null *OPENLIST*))
	(get-next-node-in-openlist0 verbose))))

(defun get-next-node-in-openlist0 (&optional(verbose t))
  (let* ((target-node            (car *OPENLIST*))
	 (target-vp-inst         (get-config target-node)))
    (if (not (reasonable-viewpoint-p target-vp-inst))
	(progn
	  (mutate-openlist       (cdr *OPENLIST*))     ;pop off the top value. 
	  (mutate-ignoredlist    (insert-at-end target-vp-inst *IGNOREDLIST*))
	  (mutate-closedlist     (insert-at-end target-vp-inst *CLOSEDLIST*))
	  (format t "BPS: Skipping #~a ~a (~a)~%"
		  (get-viewpoint-ordering target-vp-inst)
		  (format-mapped-concepts (get-mapped-concepts target-vp-inst))
		  (get-pruned-vp-reason target-vp-inst))
	  (get-next-node-in-openlist verbose))
        target-node)))

;;Asserts context protrayed in Viewpoint
(defun step1 ()
  (progn 
    (if (> (get-viewpoint-depth (car (get-next-node-in-openlist))) *CONTROLLER-MAX-DEPTH*)
	(mutate-clock 0))
    (if (and (> *CLOCK* 0)
	     (not (null *OPENLIST*)))
	(progn 
	  (setq *CURRENT-NODE*       (get-next-node-in-openlist))
	  (setq *CURRENT-VP-INST*    (get-config *CURRENT-NODE*))
	  (increment-step)))))

(defun step2()
  (if *CONTROLLER-DRYRUN* 
      (step2-dryrun)
      (step2-non-dryrun)))

(defun step2-dryrun ()
  (progn
    ;(ps-show-mapped-concepts t *CURRENT-VP-INST*)
    (increment-step)
    nil))

;;Invoke pruning condition to see if scenario is Viewpoint
;;satisfies it.
(defun step2-non-dryrun ()
  (let ((*logging* t))
    (bps-set-checkpoint *CURRENT-VP-INST*)
    (multiple-value-bind
	(goal-viewpoint? answerable-query-list)
	(test-viewpoint *CURRENT-VP-INST*)
      (format t "BPS: Trying #~a ~a => ~a" 
	      (get-viewpoint-ordering *CURRENT-VP-INST*)
	      (format-mapped-concepts (get-mapped-concepts *CURRENT-VP-INST*))
	      goal-viewpoint?)
      (if goal-viewpoint?
	  (if (duplicate-answer? *CURRENT-VP-INST*) (format t " (duplicate)")))
      (format t "~%")
      (if *CONTROLLER-EPISODIC*
	  (store-bps-episode *CURRENT-VP-INST* goal-viewpoint?))
      (let ((asserted-triples 
	     (ps-update-triple-list
	      (get-simplified-context-for-viewpoint *CURRENT-VP-INST*))))
	(cond ((and goal-viewpoint? 
		    (not (duplicate-answer? *CURRENT-VP-INST*)))
	       (progn 
		 (restart-clock)
		 (mutate-openlist      (cdr *OPENLIST*)) ;pop off the top value. 
		 (mutate-closedlist    (insert-at-end *CURRENT-VP-INST* *CLOSEDLIST*))
		 ;(ps-km-query 
		 ; `(,*CURRENT-VP-INST* |now-has| 
		 ;		       (|viewpoint-asserted-triple-list| 
		 ;			,(affix-triple-prefix asserted-triples))))		 
		 (mutate-clock 0) ;;reset clock, solution found.
		 (insert-answer-viewpoint *CURRENT-VP-INST*)
		 *CURRENT-VP-INST*
	       )
	      )
	      (t      
	       (progn
		 (bps-undo *CURRENT-VP-INST*)
		 (fill-viewpoint-answerable-query-slot *CURRENT-VP-INST* 
						       answerable-query-list)
		 (ps-km-query 
		  `(,*CURRENT-VP-INST* |now-has| 
				       (|viewpoint-asserted-triple-list| 
					,(affix-triple-prefix asserted-triples))))
		 (increment-step)
		 nil)))))))

;;Loads up the next Viewpoint candidate in *OPENLIST* onto
;;*CURRENT-NODE* data structure.
(defun step3 ()
  (progn
    (set-BPSCurStage 3)
    (mutate-openlist      (cdr *OPENLIST*))  ;;;pop off the top value. 
    (mutate-closedlist    (insert-at-end *CURRENT-VP-INST* *CLOSEDLIST*))
    (mutate-clock         (1- *CLOCK*))
 ;;    (if (promising-viewpoint-p *CURRENT-VP-INST*)
;; 	(progn
;; 	  (format t "BPS: Growing openlist for promising viewpoint, ~a.~%" *CURRENT-VP-INST*)
;; 	  (grow-openlist (list *CURRENT-VP-INST*))))
    
    (increment-step)))

#|

;;deprecated (defun skip-next-step()
  (progn
    (increment-step)
    (increment-step)))

;;deprecated (defun remove-candidates-from-list (candidate-list target-list)
  (if (null candidate-list)
      target-list
      (remove-candidates-from-list (cdr candidate-list)
		   (remove (car candidate-list) target-list))))

;;Scenario in Viewpoint does not satisfy pruning-condition.
;;So we expand the Viewpoint and queue candidate Viewpoints 
;;in *OPENLIST*
;;deprecated (defun step3 ()
  (progn
;;   (if (not *controller-inspection-viewpoint*)  ;;Do not expand if inspection viewpoint.
;; 	(let* ((query-frame-concept (get-concept-for-kb-instance (get-query-frame-in-vp-query-entry (car (get-viewpoint-query *current-vp-inst*)))))
;; 	       (candidate-concepts        (get-related-concepts query-frame-concept *all-user-defined-concepts*)))
;; 	  (update-gather-graph-hash candidate-concepts)
;; 	  (mutate-openlist   
;; 	   (expand *CURRENT-NODE*
;; 		   *CURRENT-VP-INST*
;; 		   *OPENLIST*
;; 		   *CLOSEDLIST* 
;; 		   *queue_fn*
;; 		   *heuristic*
;; 		   *ALL-USER-DEFINED-CONCEPTS*
;; 		   ))))
    (increment-step)
    t))
|#

