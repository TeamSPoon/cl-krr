;;
;; $Id: write-out-sme-kb.lisp,v 1.2 2008/04/09 21:09:09 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun bps-dump-sme-kb()
  (mapcar #'(lambda(x)
	      (multiple-value-bind
		  (skolem-assertions non-skolem-assertions)
		  (bps-dump-sme-concept x)
		(list x (append skolem-assertions non-skolem-assertions))))
	  *all-user-defined-concepts*))

(defun bps-dump-sme-concept(x)
  (multiple-value-bind
      (root graph)
      (bps-AURA-GET-GRAPH x)
    (ps-get-frames-for-triples 
     (normalize-triple-list graph))))

(defun write-out-sme-kb()
  (write-table "bps-kb-dump.html" 
	       '(concept km-frame) 
	       (mapcar #'(lambda(x)
			   (list (car x)
				 (htmlify-km-assertion-list (cadr x))))
		       (bps-dump-sme-kb))))