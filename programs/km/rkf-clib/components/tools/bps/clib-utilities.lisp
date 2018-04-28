;;
;; $Id: clib-utilities.lisp,v 1.2 2008/08/18 19:30:18 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun clib-get-sibling-uoms(uom)
  (ps-km-query `(|the| |instances| |of|
		 (|the| |instance-of| |of| ,uom))))

;;returns true if concept is member of applicable concepts for slot-range.
(defun concept-applicable-for-slot-range?(concept slot)
  (let ((filler-concept-lst (ranges-of slot)))
    (not (null
	  (intersection (cons concept (all-subclasses concept))
			filler-concept-lst)))))
