;;
;; $Id: regression-parameters.lisp,v 1.2 2006/04/27 19:26:49 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *regression-remarks*                            "")
(defparameter *regression-include-rule-engine-output*         t)
(defparameter *regression-check-answers*                      nil)