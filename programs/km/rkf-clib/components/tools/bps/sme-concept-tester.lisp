;;
;; $Id: sme-concept-tester.lisp,v 1.5 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-test-all-sme-concepts()
  (let* ((*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           nil))  ;disable KM classification for non-root viewpoints.
  (dolist (x *all-user-defined-concepts*)
    (progn
      (if (not (ps-test-sme-concept x))
	  (format t "~a fail!~%" x)
	;(format t "~a ok!~%" x)
)))))

(defun ps-test-sme-concept(sme-concept)
  (with-standard-bps-parameters ()
  (let* ((*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           nil))  ;disable KM classification for non-root viewpoints.
  (multiple-value-bind
      (concept-root concept-graph)
      (ps-get-new-graph sme-concept)
    (delete-frames-in-triple-lst concept-graph)
    (ps-test-triples concept-root concept-graph))))
)

(defun ps-test-triples(head triple-lst)
  (let ((debug nil)
	(*on-error* 'abort)
	(current-triple nil))
    (delete-frames-in-triple-lst triple-lst)
    (handler-case
     (progn
       (ps-assert-triples triple-lst debug)
       (dolist (triple (extract-non-instance-triples 
			(reverse (order-km-triple-lst triple-lst))))
	 (let* ((frame (triple-head triple))
	       (slot (triple-relation triple))
	       (filler (triple-tail triple))
	       (path (cond ((equal head frame)
			    `(|the| ,slot |of| ,frame))
			   (t `(|the| ,slot |of|  
				,(ps-get-query-path head frame triple-lst))))))
	   (setq current-triple triple)
	   ;(format t "Testing (the ~a of ~a)~%" slot frame)
	   (ps-km-query `(|the| ,slot |of| ,frame) debug)
	   ;(ps-km-query path t)
))
       (delete-frames-in-triple-lst triple-lst)
       t)
     (error (condition)
	    (progn
	      (if (null current-triple)
		  (values nil "ps-assert-triples failed!")
		(values nil (format nil "failed ~A.. ~A~%" current-triple
				    condition))))))))

#|
(ps-test-triples
 '((|_Solubility-Value26322| |instance-of| |Solubility-Value|)
     (|_Solubility-Value26326| |instance-of| |Solubility-Value|)
     (|_Solubility-Value26322| |value| (:|pair| |*soluble| |Chemical|))
     (|_Solubility-Value26322| |value| (:|pair| |*insoluble| |Chemical|))
     (|_Solubility-Value26326| |value| (:|pair| |*insoluble| |Chemical|)) 
     (|_Solubility-Value26326| |value| (:|pair| |*soluble| |Chemical|))))
|#

#|

(defun testcase(&optional
		(sme-kb-directory "/projects/rkf/storage/jchaw/tester/chemistry-sme-kb/aura/kb/"))
  (dolist (candidate (all-instances '|Property-Value|))
    (if (null (values-consistent (ps-slot-lookup candidate '|value|)))
	(let ((command (format nil "/lusr/gnu/bin/grep -l \"~a has\" ~a*.km" candidate sme-kb-directory)))
      (dolist (associated-with (excl.osi:command-output command))
	(format t "Bad ~a, defined in ~a.km~%" candidate (pathname-name associated-with))))
)))

|#
