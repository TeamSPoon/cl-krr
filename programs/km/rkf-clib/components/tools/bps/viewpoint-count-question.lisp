;;
;; $Id: viewpoint-count-question.lisp,v 1.12 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun output-count-viewpoint-for-lookup-viewpoint(sl-vp-inst)
  (multiple-value-bind
      (simplified-scenario full-scenario)
      (get-simplified-context-for-viewpoint sl-vp-inst)
    (multiple-value-bind
	(vp-answerable-query-filler yn-query-lst sl-query-lst)
	(get-viewpoint-answerable-query sl-vp-inst)
      (let* ((debug nil)
	     (cloned-vp-inst (ps-clone-viewpoint sl-vp-inst))
	     (count-query-lst (convert-slot-lookup-query-to-count-query full-scenario sl-query-lst))
	     (vp-defn
	      `(|a| |Count-Viewpoint| |with|
		(|viewpoint-scenario|   (,(cons ':|set| (affix-triple-prefix full-scenario))))
		(|viewpoint-parent|	 (,sl-vp-inst))
		;(|viewpoint-filter|    ,(quasi-get-concept-for-kb-instance (cadr (car cpl-question)) cpl-scenario)) ;; needed for filtering. Commented out for now.
		(|viewpoint-query|     (,(cons ':|seq| count-query-lst))))))
	(let ((result (car (ps-km-query vp-defn debug))))
	  (ps-km-query `(,sl-vp-inst |now-has| (|viewpoint-child| (,result))) debug)
	  result)))))

(defun convert-slot-lookup-query-to-count-query(scenario sl-query-lst)
  (remove nil
	  (mapcar #'(lambda(entry)
		      (let* ((triple (cdr (nth 2 entry)))
			     (count   (ps-count-items 
				       (triple-head triple)
				       (triple-relation triple)
				       scenario)))
			(if (or (consp count)
				(and (numberp count)
				     (not (zerop count))))
			    (list ':|pair| '|*count-query| 
				  `(:|triple| 
				     ,(triple-head triple)
				     ,(triple-relation triple)
				     ,count)))))
		  sl-query-lst)))

(defun ps-count-items(frame relation scenario &optional(concept-filter '|Thing|))
  (let ((debug nil))
    (or 
     (ps-km-query `(|oneof| (|constraints-for| (|the| ,relation |of| ,frame))
		    |where| ((|?constr| == (#'(LAMBDA () (STRING (CAADR (KM-UNIQUE0 '|It|))))))
			     |and|
			     ((|?constr| = "at-least") |or| (|?constr| = "exactly") |or| (|?constr| = "at-most"))))
		  debug)
     (length (ps-km-query
	      `(|allof| (|the| ,relation |of| ,frame) |where| (|It| |isa| ,concept-filter))
	      debug)))))

;;03Nov08
;; (defun viewpoint-count (cpl-scenario cpl-question)
;;   (let* ((question-instance (second (first cpl-question)))
;; 	 (question-instance-concept (get-concept-for-instance-in-triple-list question-instance cpl-scenario))
;; 	 (query-path  (car (ps-extract-question cpl-scenario (list question-instance))))
;; 	 (query-slot  (nth 1 query-path))
;; 	 (query-frame (nth 3 query-path))
;; 	 (elem-count  (count-isa-elements query-path
;; 					  (car question-instance-concept)))) ;;Only pick the 1st concept in instance-of slot.
;;     (ps-slot-lookup query-frame query-slot) ;; Touch the vp-query to trigger classification. Is this a feature or bug in KM?
;;                                             ;; Shouldn't all instances be classified at instantiation?
;;     `(|a| |Count-Viewpoint| |with|
;;       (|viewpoint-filter|  ,(quasi-get-concept-for-kb-instance (cadr (car cpl-question)) cpl-scenario))
;;       (|viewpoint-scenario| 	 (,(cons ':|set| (affix-triple-prefix cpl-scenario))))
;;       (|viewpoint-query|	 ((:|pair| |*count-query| (:|triple| ,QUERY-FRAME ,QUERY-SLOT ,ELEM-COUNT)))))))

;;03Nov08
;; ;;Counts number of elements in KM query result which are subclasses of some concept.
;; (defun count-isa-elements(query-path concept)
;;   (let ((km-query `(|allof| ,query-path |where| (|It| |isa| ,concept))))
;;     (length (ps-km-query km-query))))

;;Predicate for testing is question is a COUNT question type.
(defun is-count-question-p (cpl-question)
  (let ((target-question (first cpl-question)))
    (and (consp target-question)
	 (eql (first (first cpl-question)) 'HOW-MANY))))
