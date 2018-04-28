;;
;; $Id: halo2-snapshot-diff.lisp,v 1.5 2008/06/09 01:10:52 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)
(use-package :net.html.parser)
(require :phtml)

#|

;;Example
1) Need to re-define (get-snapshot-list) and (start)
   a) (get-snapshot-list) returns list of top-level directory names containing
      previous results.
   b) Inside (start), need to specify directory containing the snapshots.

For example, we have three snapshots for physics, "current", "old", and "older". They
are located in "/projects/rkf/storage/jchaw/tester/diff-work"

time-diff.html and relevant HTML files will be created in the directory specified in (start).

2) To generate the time diff, run (start).

Thanks!

;;RE-DEFINE! 
;;List of all snapshot dir names.
(defun get-snapshot-list(target)
  (mapcar 'pathname-name (remove-if 'pathname-type (directory "./resultset/"))))

;;RE-DEFINE! 
;;need to specify main directory containing snapshots
;;In this case, the snapshot directories "current", "old", and "older" are located
;;in /projects/rkf/storage/jchaw/tester/diff-work
(defun start()
  (generate-temporal-diff-page-for-target 
   "./resultset"))
|#

(defun get-snapshot-target-names(target)
  (mapcar #'(lambda(x)
	      (format nil "~a/~a" target x))
	  (get-snapshot-list target)))

(defun get-snapshot-target-name-and-writedate(target)
  (mapcar #'(lambda(x)
	      (let ((target-name (format nil "~a/~a" target x)))
		(cons target-name
		      (file-write-date target-name))))
	  (get-snapshot-list target)))
  
(defun sort-snapshot-targets(input)
  (sort input #'(lambda(x y)
			 (let ((x-date (file-write-date x))
			       (y-date (file-write-date y)))
			   (and (numberp x-date)
				(numberp y-date)
				(> x-date y-date))))))

(defun load-all-snapshot-for-target(target)
  (let ((target-list (get-snapshot-target-names target)))
    (sort-snapshot-targets
     (remove nil
	     (mapcar #'(lambda(x)
			 (progn
			   (load-tester-set x)
			   (if (completed-processing-p x) x)))
		     target-list)))))

(defun generate-temporal-diff-page-for-target(&optional (target "."))
  (let ((filename (format nil "~a/time-diff.html" target)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (let ((grp-lst (divide-lst (remove-if #'(lambda(x) (string-equal x ".")) 
					    (load-all-snapshot-for-target target))
				 5)))
      (format stream "<HTML><BODY>")
      (dolist (x grp-lst)
	(format stream "~a" (generate-diff-table target x))
	(format stream "<BR><BR>"))
      (format stream "</body>")
      (format stream "</HTML>")))))

(defun get-label-for-snapshot-target (x)
  (format nil "~a<BR>(~a ago)" x (simplify-universal-time (- (get-universal-time)
						     (file-write-date x))
						  )))

(defun generate-filename-for-target-pair-table-for-parameter(dest-dir source target parameter)
  (format nil "~a/time-diff-~a.html" 
	  dest-dir
	  (excl:md5-string (format nil "~a~a~a" source target parameter) :return :integer)))

(defun generate-target-pair-table-for-parameter(dest-dir source target parameter q-number-list)
  (let ((filename
	 (generate-filename-for-target-pair-table-for-parameter 
	  dest-dir source target parameter)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "<TABLE BORDER=\"1\">")
      (format stream "<TR>")
      (format stream "<TH>q-number</TH>")
      (format stream "<TH>~a</TH>"       (stringify-universal-time (file-write-date source)))
      (format stream "<TH>~a</TH>"       (stringify-universal-time (file-write-date target)))
      (format stream "</TR>")
      (dolist (q-number q-number-list)
	(format stream "~a" (generate-target-pair-line-item-for-parameter source target parameter q-number)))
      (format stream "</TABLE>"))
    filename))

(defun generate-target-pair-line-item-for-parameter(source target parameter q-number)
  (let ((source-sym (reintern-symbol-for-package (format nil "*~a-~a*" q-number parameter)
						 source))
	(target-sym (reintern-symbol-for-package (format nil "*~a-~a*" q-number parameter)
						 target))
	(s          (make-string-output-stream)))
    (format s "<TR>")
    (format s "<TD>~a</TD>" q-number)
    (format s "<TD>~a</TD>" (deref-symbol source-sym))
    (format s "<TD>~a</TD>" (deref-symbol target-sym))
    (format s "</TR>")
    (get-output-stream-string s)))

(defun generate-diff-table(target target-list)
  (let ((diff-results-for-target-pair 
	 (generate-diff-for-target-pair-list
	  (all-pairs target-list)))
	(stream (make-string-output-stream)))
    (format stream "<TABLE BORDER=\"1\">")
    (format stream "<TR>")
    (format stream "<TH>&nbsp;</TH>")
    (dolist (x target-list)
      (format stream "<TH>~a</TH>" (get-label-for-snapshot-target x)))
    (format stream "</TR>")
    (dolist (x target-list)
      (progn
	(format stream "<TR>")
	(format stream "<TD>~a</TD>" (get-label-for-snapshot-target x))
	(dolist (y target-list)
	  (let* ((pairing (list x y))
		 (diff-result (assoc pairing diff-results-for-target-pair :test 'equal)))
	    (format stream "<TD>~a</TD>" 
		    (pretty-print-diff-results target
					       x y 
					       (cadr diff-result)))))
	(format stream "</TR>")
	))
    (format stream "</TABLE>")
    (get-output-stream-string stream)))

(defun remove-all-old-time-diff-html-files()
  (dolist (x (directory "time-diff*.html"))
    (delete-file x)))

(defun pretty-print-diff-results(dest-dir source target diff-result)
  (let ((outputstream (make-string-output-stream)))
    (if (null diff-result)
	(format outputstream "&nbsp;")
      (dolist (item diff-result)
	(let ((parameter (car item))
	      (affected-q-numbers (cadr item)))
	  (if (not (null affected-q-numbers))
	      (let* ((abs-filename
		      (generate-target-pair-table-for-parameter 
		       dest-dir source target parameter affected-q-numbers))
		     (filename (format nil "~A.html" (pathname-name abs-filename))))
		(format outputstream "~a<a href=\"~a\">(~a)</a><BR>"
			parameter
			filename
			(length affected-q-numbers)))))))
    (get-output-stream-string outputstream)))

(defun generate-diff-for-target-pair-list(target-pair-list)
  (let ((parameter-list	'(question-id question-orig question comments answer-snippets bps-answer)))
    (mapcar #'(lambda(pair)
		(let ((source (first  pair))
		      (target (second pair)))
		  (list pair (result-diff source target parameter-list))))
	    target-pair-list)))

(defun result-diff(source target parameter-list) 
  (if (not (equal source target) )
      (progn
	(load-tester-set source)
	(load-tester-set target)
	(result-diff-for-parameter-list source target parameter-list))))
				    
(defun create-parameter-list-for-q-number(q-number parameter-list)
  (mapcar #'(lambda(parameter)
	      (string-upcase (format nil "*~a-~a*" q-number parameter)))
	  parameter-list))

(defun result-diff-for-parameter-list(source target parameter-list)
  (mapcar #'(lambda(parameter)
	      (list parameter (result-diff-for-parameter source target parameter)))
	  parameter-list))

(defun result-diff-for-q-number-parameter (source target q-number parameter)
  (let ((source-sym (reintern-symbol-for-package (format nil "*~a-~a*" q-number parameter)
						 source))
	(target-sym (reintern-symbol-for-package (format nil "*~a-~a*" q-number parameter)
						 target)))
    (if (not (string-equal 
	      (unhtmlify (stringify (deref-symbol source-sym)))
	      (unhtmlify (stringify (deref-symbol target-sym)))))
	q-number)))

(defun unhtmlify(input)
  (handler-case
   (stringify 
    (remove-explanation-tags 
     (net.html.parser::parse-html input)))
   (error (condition)
	  input)))

(defun result-diff-for-parameter(source target parameter)
  (remove nil 
	  (mapcar #'(lambda(q-number)
		      (result-diff-for-q-number-parameter source target q-number parameter))
		  (get-formulation-set source))))

(defun completed-processing-p(target)
  (and (completed-cpl-processing-p target)
       (completed-bps-processing-p target)))

(defun completed-cpl-processing-p(target)
  (progn
    (load-tester-set target)
    (null
     (remove nil
	     (set-difference (get-formulation-set target)
			     (identify-cpl-processed-q-numbers target))))))

(defun completed-bps-processing-p(target)
  (progn
    (load-tester-set target)
    (null
     (remove nil (set-difference (identify-good-cpl-interpretations target)
				 (identify-bps-processed-q-numbers target))))))

(defun remove-explanation-tags(x) ;;removes javascript onclick links to bpsxxx
  (remove nil
   (flatten 
    (cond ((or (null x)
	       (symbolp x)) ())
	  ((stringp x) x)
	  ((equal (car x) ':ONCLICK) ())
	  (t (cons (remove-explanation-tags (car x) )
		   (remove-explanation-tags (cdr x))))))))

