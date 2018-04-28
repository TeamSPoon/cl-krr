;;
;; $Id: persistent-hash.lisp,v 1.5 2007/07/04 21:27:29 jchaw Exp $
;;

(unless (find-package :km)   (make-package :km))
(unless (find-package :aura) (make-package :aura))
(in-package :km)
(setq *using-km-package* t)

(defun write-hash (htable hash-file)
  (if (not (null htable))
      (let (stream)
	(setf stream (open hash-file :direction :output :if-exists :rename-and-delete :if-does-not-exist :create))
	(format stream ";;;~%")
	(format stream ";;; $Id: persistent-hash.lisp,v 1.5 2007/07/04 21:27:29 jchaw Exp $~%")
	(format stream ";;;~%~%")
	(format stream "(unless (find-package :km)   (make-package :km))~%")
	(format stream "(unless (find-package :aura) (make-package :aura))~%")
	(format stream "(in-package :km)~%")
	(format stream "(setq *using-km-package* t)~%")
	(format stream "(defparameter *persistent-hash* '(")
	(maphash #'(lambda (key val) (format stream "(~s ~s)~%" key val)) htable)
	(format stream "))~%")
	(close stream))))
  
(defun load-hash (htable hash-file)
  (let ((result (load hash-file :if-does-not-exist nil))
        (count 0))
    (cond
     (result
      (clrhash htable)
      (dolist (key+val *persistent-hash*)
        (setf (gethash (first key+val) htable) (second key+val))
        (setf count (+ count 1)))
      count)
     (t nil))))
