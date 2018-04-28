;;
;; $Id: viewpoint-mutator-routines.lisp,v 1.3 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;Mutates the slotvalue of slot in some Viewpoint instance
(defun set-slotvalue-for-viewpoint-instance(slotname vp-instance value)
  (car (ps-km-query `(,vp-instance |now-has| (,slotname (,value))) t)))

;;Mutates the filler for viewpoint-scenario slot in some Viewpoint instance
(defun set-viewpoint-scenario(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-scenario|
					 vp-instance
					 value))

;;Mutates the filler for viewpoint-score slot in some Viewpoint instance
(defun set-viewpoint-score(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-score|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-model-graph slot in some Viewpoint instance
(defun set-viewpoint-model-graph(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-model-graph|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-correspondence slot in some Viewpoint instance
(defun set-viewpoint-correspondence(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-correspondence|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-source slot in some Viewpoint instance
(defun set-viewpoint-source(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-source|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-target slot in some Viewpoint instance
(defun set-viewpoint-target(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-target|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-query slot in some Viewpoint instance
(defun set-viewpoint-query(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-query|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-answerable-query slot in some Viewpoint instance
(defun set-viewpoint-answerable-query(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-answerable-query|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-query-matched slot in some Viewpoint instance
(defun set-viewpoint-query-matched(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-query-matched|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-answer slot in some Viewpoint instance
(defun set-viewpoint-answer(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-answer|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-parent slot in some Viewpoint instance
(defun set-viewpoint-parent(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-parent|
					 vp-instance 
					 value))

;;Mutates the filler for viewpoint-parent slot in some Viewpoint instance
(defun set-viewpoint-filter(vp-instance value)
  (set-slotvalue-for-viewpoint-instance '|viewpoint-filter|
					 vp-instance 
					 value))

;;Instantiates a Slot-Value-Viewpoint instance
(defun create-slot-value-viewpoint()
  (car (ps-km-query '(|a| |Slot-Value-Viewpoint|))))
