;;
;; $Id: controller-search.lisp,v 1.19 2008/10/29 19:55:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Determines which NODE has a lower associated COST
(defun cheaper-nodep(node1 node2)
  (< (get-cost node1) (get-cost node2)))

;;Returns an instance of the NODE data structure given some
;;CONFIG, HISTORY, and COST
(defun make-node (config history cost)
  (list config history cost))

;;Returns a plain-vanilla instance of the NODE data structure
(defun new-node (config)
  (make-node config nil 0))

;;Returns the CONFIG instance in a NODE instance
(defun get-config (node)
  (nth 0 node))

;;Returns the HISTORY instance in a NODE instance
(defun get-history(node)
  (nth 1 node))

;;Returns the COST instance in a NODE instance
(defun get-cost(node)
  (nth 2 node))

;;Returns the static cost of a node, i.e. the distance from root
(defun calc-static-cost (node)
  (length (get-history node)))

;f=g+0
(defun neutral-heuristic (node)
  (let* ((cost (calc-static-cost node)))
    cost))

;;Determines if a CONFIG instance is valid
(defun configp (config)
  (not (null config)))

;;Determines if two CONFIG instances are the same
(defun same-configp (config1 config2)
  (equal config1 config2))

(defun make-config (viewpoint-defn)
  viewpoint-defn)

(defun calculate-cost(heuristic board new_config)
  (neutral-heuristic board))

(defun solve()
  (if (not (zerop *clock*))
      (progn
	(topup-openlist)
	(if *openlist*
	    (progn 
	      (perform-step)
	      (solve))))))

(defun begin-solve (vp-inst queue_fn heuristic)
  (let* ((node (new-node vp-inst)))
    (setup-search (list node) queue_fn heuristic)
    (solve)))

;;Depth-First Search
(defun dfs-unbounded (vp-inst)
  (begin-solve vp-inst 'stack-insert 'neutral-heuristic))

;;Breadth-First Search
(defun bfs (vp-inst)
  (begin-solve vp-inst 'sorted-queue-insert 'neutral-heuristic))

(defun setup-search(openlist &optional
			     (queue_fn 'sorted-queue-insert)
			     (heuristic 'neutral-heuristic)
			     (closedlist nil))
  (progn
    (mutate-openlist   openlist)
    (mutate-closedlist closedlist)
    (mutate-queue_fn   queue_fn)
    (mutate-heuristic  heuristic)))

(defun topup-openlist()
  (if (null *OPENLIST*)
      (grow-openlist)))

(defun grow-openlist(&optional(unexpanded-viewpoint-list nil))
  (cond ;(*controller-inspection-viewpoint* nil) ;;Do not expand if inspection viewpoint. No longer necessary.
        ((null unexpanded-viewpoint-list)  (grow-openlist0 (determine-unexpanded-viewpoints)))
	(t                                 (grow-openlist0 unexpanded-viewpoint-list))))

(defun grow-openlist0(unexpanded-viewpoint-list)
  (if unexpanded-viewpoint-list
      (let* ((unexpanded-viewpoint (car unexpanded-viewpoint-list))
	     (new-vp-instances     (expand nil
					   unexpanded-viewpoint
					   *OPENLIST*
					   *CLOSEDLIST* 
					   *queue_fn*
					   *heuristic*
					   *ALL-USER-DEFINED-CONCEPTS*)))
	(if (not (null new-vp-instances))
	    (progn (mutate-openlist new-vp-instances) t)
	    (grow-openlist0 (cdr unexpanded-viewpoint-list))))))

(defun determine-unexpanded-viewpoints()
  (remove-if-not 'is-unexpanded-viewpoint-p *CLOSEDLIST*))

(defun is-unexpanded-viewpoint-p(vp-inst)
  (and (not (member vp-inst *IGNOREDLIST*))
       (or (isa vp-inst '|Slot-Value-Viewpoint|)
	   (isa vp-inst '|Multislot-Value-Viewpoint|))
       (null (remove-if-not #'(lambda(x)
				(or (isa x '|Slot-Value-Viewpoint|)
				    (isa x '|Multislot-Value-Viewpoint|)))
			    (get-viewpoint-children vp-inst)))))
