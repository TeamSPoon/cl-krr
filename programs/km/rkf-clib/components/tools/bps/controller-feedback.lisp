;;
;; $Id: controller-feedback.lisp,v 1.8 2006/05/19 20:37:10 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *bps-current-stage*      0)
(defparameter *bps-max-stage*          0)
(defparameter *bps-current-position*   0)
(defparameter *bps-max-position*       0)

(defun set-BPSCurStage(in)
  (setq *bps-current-stage* in))

(defun set-BPSMaxStage(in)
  (setq *bps-max-stage* in))

(defun set-BPSCurPosition(in)
  (setq *bps-current-position* in))

(defun set-BPSMaxPosition(in)
  (setq *bps-max-position* in))

(defun get-BPSCurStage()
  *bps-current-stage*)

(defun get-BPSMaxStage()
  *bps-max-stage*)

(defun get-BPSCurPosition()
  *bps-current-position*)

(defun get-BPSMaxPosition()
  *bps-max-position*)

(defun get-BPS-Feedback ()
  (list *bps-current-stage*
	*bps-max-stage*
	*bps-current-position*
	*bps-max-position*))

(defun reset-bps-feedback()
  (progn
    (set-BPSCurStage    0)
    (set-BPSMaxStage    0)
    (set-BPSCurPosition 0)
    (set-BPSMaxPosition 0)))