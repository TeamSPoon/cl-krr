;;
;; $Id: debugger-find-similar-command.lisp,v 1.3 2006/02/01 21:06:37 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun find-similar-concepts (concept)
  (let ((concept-triples (aggregate-instance-types
			  (remove-irrelevant-km-triples
			   (remove-duplicates (cadr (get-km-prototype-def concept))
					      :test #'(lambda (x y) 
							(or (equal x y) 
							    (equal x (invert-triple y)))))))))
    (match-triples-to-candidates concept-triples (remove concept *all-user-defined-concepts* :test #'eql))))

