;;
;; $Id: preferred-uom-code.lisp,v 1.2 2008/08/18 20:22:06 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-all-uoms-in-viewpoint(vp-inst)
  (get-all-uoms-in-triple-list 
   (get-simplified-context-for-viewpoint vp-inst)))

(defun install-preferred-uom(current-uom query-path)
  (let* ((debug nil)
	 (preferred-uom (get-preferred-unit-for-query-path current-uom query-path)))
    (if (not (null preferred-uom))
	(let ((km-instance (car (ps-km-query query-path)))) ;;Safe to assume exactly one KM property-value instance.
	  (if debug (format t "BPS: Installing preferred unit ~A~%" preferred-uom))
	  (ps-km-query `(,km-instance |now-has| (|preferred-unit| (,preferred-uom))) debug)))))

(defun find-preferred-uom (uom triple-list)
  (let* ((uoms-present (get-all-uoms-in-triple-list triple-list))
	 (alternatives (clib-get-sibling-uoms uom)))
    (or (car (intersection alternatives uoms-present)) ;;default
	(car (intersection alternatives (mappend 'decompose-uom uoms-present)))
	uom)))

(defun decompose-uom(uom)
  (let ((novak-uom (my-glunkmify uom)))
    (remove-if-not 'clib-uom-p
		   (mapcar 'kmifyunit
			   (flatten (glnoticeunit novak-uom))))))

(defun get-preferred-unit-for-query-path(current-uom query-path)
  (if (not (null (get-user-specified-preferred-unit query-path)))
      (get-user-specified-preferred-unit query-path)
    (find-preferred-uom current-uom *CONTROLLER-TRIPLE-LIST*)))

(defun get-user-specified-preferred-unit(query-path)
  (let* ((slot  (second query-path))
	 (frame (intern (ps-unclone-naively (fourth query-path)) :km))
	 (filler (car (quasi-ps-slot-lookup frame slot *CONTROLLER-INPUT-SCENARIO*))))
    (car (quasi-ps-slot-lookup filler '|preferred-unit| *CONTROLLER-INPUT-SCENARIO*))))

(defun extract-instance-for-query-path-list (query-path-list)
  (remove nil
	  (mapcar #'(lambda(query-path)
		      (let ((target-instance (car (last query-path))))
			target-instance))
		  query-path-list)))
					     
(defun sieve-query-path-list-not-having-instance (query-path-list instance-list)
  (remove nil 
	  (mapcar #'(lambda(query-path)
		      (let ((target-instance (car (last query-path))))
			(if (not (member target-instance instance-list))
			    query-path)))
		  query-path-list)))

