;;
;; $Id: debugger-parameters.lisp,v 1.13 2008/08/26 19:13:15 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *watch-param-openlist*                        nil)
(defparameter *watch-param-closedlist*                      nil)
(defparameter *watch-param-queue_fn*                        nil)
(defparameter *watch-param-heuristic*                       nil)
(defparameter *watch-param-pruning-condition*               nil)
(defparameter *watch-param-clock*                           nil)
(defparameter *watch-param-current-node*                    nil)
(defparameter *watch-param-current-vp-inst*                 nil)
(defparameter *watch-param-current-step*                    nil)
(defparameter *watch-param-cpl-scenario*                    nil)
(defparameter *watch-param-cpl-compute-question*            nil)
(defparameter *watch-param-viewpoint-query-prefixes*        nil)
(defparameter *watch-param-viewpoint-result-query-prefixes* nil)
(defparameter *watch-param-clone-counter-list*              nil)
(defparameter *adhoc-mode*                                  nil)

(defparameter *debugger-show-touch-all-query-paths*         nil)

