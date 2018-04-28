;;
;; $Id: tester-config.lisp,v 1.1 2008/02/26 00:29:15 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)

;;===============================================================
;; Params that control the behaviour of the tester
;;===============================================================

;; list of clib authors
(defconstant *CLIB-AUTHORS*
  '("tecuci" "kbarker" "porter" "jchaw" "mrglass" "onue5" "aiyer" "vaibhav" "hari5851" "kunal"))
	
;; default receiver if no author could be found
(defconstant *DEFAULT-RECEIVER* "tecuci")

;; whether the tester should email the author on error
(defvar *send-email-on-error* t)

(defvar *test-result-file* "/projects/rkf/util/test-result.txt")
