;
;; $Id: controller-macros.lisp,v 1.5 2009/08/23 20:04:35 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defconstant +standard-bps-parameters+
  '((KM::*BUILT-IN-REMOVE-SUBSUMERS-SLOTS* '(|instance-of| |classes| |superclasses| |member-type|))
    (KM::*BUILT-IN-REMOVE-SUBSUMEES-SLOTS* '(|subclasses| |prototype-of| |domain| |range|))
    (KM::*RECURSIVE-CLASSIFICATION* T)
    (KM::*CLASSIFICATION-ENABLED*   T)
    (KM::*PROTOTYPE-CLASSIFICATION-ENABLED* T)
    (KM::*AM-IN-SITUATIONS-MODE* T)
    (KM::*MAX-PADDING-INSTANCES* 2) ;; kbarker testing
    (KM::*purely-deductive-QA*   nil) ;; kbarker testing
    ))

(defmacro with-restoring-situations-reasoning (() &body body)
 (let ((g-prev-situation (gensym "prev-situation"))
       (g-prev-situation-mode (gensym "prev-situation-mode")))
   `(let ((,g-prev-situation (curr-situation))
          (,g-prev-situation-mode km::*am-in-situations-mode*))
      (unwind-protect (progn ,@body)
        (in-situation ,g-prev-situation)
        (setq km::*am-in-situations-mode* ,g-prev-situation-mode)))))

(defmacro with-standard-bps-parameters (() &body body)
 `(with-standard-aura-parameters ()
   (let ,+standard-bps-parameters+
     (with-restoring-situations-reasoning ()
      (progn
	(ps-non-global-km-situation)
	,@body)))))

(defmacro lhs (x) `(cadr  ,x))
(defmacro rhs (x) `(caddr ,x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'while)
    (defmacro while (test &rest forms)
      `(loop (unless ,test (return)) ,@forms))))

