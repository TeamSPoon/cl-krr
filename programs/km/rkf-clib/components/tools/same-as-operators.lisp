;;
;; $Id: same-as-operators.lisp,v 1.1 2008/09/08 16:02:08 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun property-group-text-description-same-as(self thevalue)
  (LET ((DEBUG T)
	(SELF-STRING (KM-UNIQUE0 
		      `(|the| |text-gen| |of| ,Self)))
	(THEVALUE-STRING 
	 (KM-UNIQUE0 
	  `(|the| |text-gen| |of| ,TheValue))))
       (IF DEBUG (FORMAT T "(doing property-group-text-description-same-as test between ~a(~s) and ~a(~s), using text-gen(Property-Group).~%"
			 SELF
			 SELF-STRING
			 THEVALUE
			 THEVALUE-STRING))
       (AND (STRINGP SELF-STRING)
	    (STRINGP THEVALUE-STRING)
	    (STRING= SELF-STRING THEVALUE-STRING))))