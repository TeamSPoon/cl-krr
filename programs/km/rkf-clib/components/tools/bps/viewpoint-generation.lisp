;;
;; $Id: viewpoint-generation.lisp,v 1.3 2009/06/22 02:14:14 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-generate-viewpoint (viewpoint-filler-lst)
  (let ((vp-inst (ps-instantiate-concept '|Viewpoint|)))
    (dolist (filler viewpoint-filler-lst)
      (assert-viewpoint-filler vp-inst filler))
    vp-inst
))

(defun assert-viewpoint-filler(vp-inst filler)
  (let ((debug nil)
	(slot  (nth 0 filler))
	(value (nth 1 filler)))
    (if (and (not (null slot))
	     (not (null value)))
	(progn
	  (cond ((atom value)
		 (ps-km-query `(,vp-inst |now-has| (,slot (,value))) debug))
		((listp value)
		 (cond ((km-aggregatep value)
			(ps-km-query `(,vp-inst |now-has| (,slot (,value))) debug))
		       (t (ps-km-query `(,vp-inst |now-has| (,slot (,(cons ':|set| value)))) debug)))))
	  (ps-classify-viewpoint vp-inst)))))

(defun ps-classify-viewpoint(vp-inst)
  (let ((debug nil))
  (cond ((= (length (get-viewpoint-query vp-inst)) 1)
	 (ps-km-query `(,vp-inst |now-has| (|instance-of| (|Slot-Value-Viewpoint|))) debug))
	((> (length (get-viewpoint-query vp-inst)) 1)
	 (ps-km-query `(,vp-inst |now-has| (|instance-of| (|Multislot-Value-Viewpoint|))) debug)))
))

;;Old code that should be refactored and eventually deprecated.
;;BUT these are required for now.

(defun generate-viewpoint (viewpoint-source
                           viewpoint-scenario
                           viewpoint-query
                           &optional 
                           (viewpoint-filter          nil)
                           (viewpoint-target          nil)
                           (viewpoint-model-graph     nil)
                           (viewpoint-score           nil)
                           (viewpoint-query-matched   t)
                           (viewpoint-parent          nil)
                           (viewpoint-correspondence  nil))
  (let* ((viewpoint-type    (if (= (length viewpoint-query) 1)
                               '|Slot-Value-Viewpoint|
                               '|Multislot-Value-Viewpoint|))
         (vp-inst         (ps-instantiate-concept viewpoint-type)))
    (if (not (null viewpoint-source))         (ps-frame-now-has vp-inst `(|viewpoint-source|         (,(cons ':|set| viewpoint-source)))))
    (if (not (null viewpoint-target))         (ps-frame-now-has vp-inst `(|viewpoint-target|         (,(cons ':|seq| viewpoint-target)))))
    (if (not (null viewpoint-filter))         (ps-frame-now-has vp-inst `(|viewpoint-filter|         (,viewpoint-filter))))
    (if (not (null viewpoint-scenario))       (ps-frame-now-has vp-inst `(|viewpoint-scenario|       (,(cons ':|set| viewpoint-scenario)))))
    (if (not (null viewpoint-model-graph))    (ps-frame-now-has vp-inst `(|viewpoint-model-graph|    (,(cons ':|set| viewpoint-model-graph)))))
    (if (not (null viewpoint-score))          (ps-frame-now-has vp-inst `(|viewpoint-score|          (,viewpoint-score))))
    (if (not (null viewpoint-query))          (ps-frame-now-has vp-inst `(|viewpoint-query|          (,(cons ':|seq| viewpoint-query)))))
    (if (not (null viewpoint-query-matched))  (ps-frame-now-has vp-inst `(|viewpoint-query-matched|  (,viewpoint-query-matched))))
    (if (not (null viewpoint-parent))         (ps-frame-now-has vp-inst `(|viewpoint-parent|         (,viewpoint-parent))))
    (if (not (null viewpoint-correspondence)) (ps-frame-now-has vp-inst `(|viewpoint-correspondence| ,viewpoint-correspondence)))
    vp-inst))
