;;
;; $Id: debugger-problem-solving-report.lisp,v 1.3 2006/12/06 17:14:43 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun get-problem-solving-report(&optional(filename "debug.html"))
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "<HTML>")
    (format stream "<PRE>")
    (tree-printer stream (get-descendants (root-viewpoint)) nil)
    (format stream "</PRE>")
    (format stream "<TABLE BORDER=\"1\">")
    (format stream "<TR><TH>Context<TH>Viewpoint Instance</TH></TD>")
    (dolist (vp-inst *CLOSEDLIST*)
      (get-problem-solving-report-process-vp-inst stream vp-inst))
    (format stream "</TABLE>")
    (format stream "created at ~a<BR><BR>" (get-time-of-day))
    (format stream "</HTML>")))

(defun get-problem-solving-report-process-vp-inst(stream vp-inst)
  (progn
    (format stream "<TR>~%")
    (format stream "<TD><PRE>~a</PRE><HR><pre>~a</pre></TD>" 
	    (htmlify-km-assertion-list 
	     (triples-to-km-assertions
	      (get-simplified-context-for-viewpoint vp-inst)))
	    (ps-km-query `(|the| |viewpoint-answer| |of| ,vp-inst)))
    (format stream "<TD><PRE>~a</PRE>"
	    (showme vp-inst (all-situations) (all-theories) nil t))
    (let ((viewpoint-targets (cdr (car (ps-km-query `(|the| |viewpoint-target| |of| ,vp-inst))))))
      (if viewpoint-targets
	  (progn 
	    (format stream "<HR><PRE>")
	    (dolist (target viewpoint-targets)
	      (ls-equation target (get-equation-provenance-for-concept target) stream))
	    (format stream "</PRE>")))
    (format stream "</TD>")
    (format stream "</TR>~%"))))
