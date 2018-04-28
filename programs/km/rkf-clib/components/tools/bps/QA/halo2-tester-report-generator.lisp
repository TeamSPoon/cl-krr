;;
;; $Id: halo2-tester-report-generator.lisp,v 1.2 2008/04/18 17:49:48 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun ps-get-domain-for-kb(kb-name)
  (multiple-value-bind 
    (sme domain date)
    (ps-parse-kb-name kb-name)
    domain))

(defun ps-get-directory-listing(dirname)
  (mapcar #'(lambda(entry)
	      (pathname-name entry))
	  (directory dirname)))

(defun ps-get-question-list-for-domain(domain)
  (cond ((or (string= domain "phys") (string= domain "physics")) 
	 (ps-get-question-list-for-physics))
	((or (string= domain "chem") (string= domain "chemistry")) 
	 (ps-get-question-list-for-chemistry))
	((or (string= domain "bio")  (string= domain "biology")) 
	 (ps-get-question-list-for-biology))))

(defun ps-make-domain-proper(domain)
  (cond ((or (string= domain "phys") (string= domain "physics")) 
	 "physics")
	((or (string= domain "chem") (string= domain "chemistry")) 
	 "chemistry")
	((or (string= domain "bio") (string= domain "biology")) 
	 "biology")))

(defun ps-get-question-list-for-physics()
  (ps-get-directory-listing "/projects/rkf/storage/jchaw/halo/question/physics/"))

(defun ps-get-question-list-for-chemistry()
  (ps-get-directory-listing "/projects/rkf/storage/jchaw/halo/question/chemistry/"))

(defun ps-get-question-list-for-biology()
  (ps-get-directory-listing "/projects/rkf/storage/jchaw/halo/question/biology/"))
    
(defun ps-parse-kb-name(kb-name)
  (let* ((1st-hyphen (search "-" kb-name))
	 (2nd-hyphen (+ (1+ 1st-hyphen) (search "-" (subseq kb-name (1+ 1st-hyphen) (length kb-name)))))
	 (sme        (subseq kb-name 0 1st-hyphen))
	 (domain     (ps-make-domain-proper (subseq kb-name (1+ 1st-hyphen) 2nd-hyphen)))
	 (date       (subseq kb-name (1+ 2nd-hyphen) (length kb-name))))
    (values sme domain date)))

(defun ps-generate-all-job-list()
  (let ((kb-lst       (directory "/projects/rkf/storage/jchaw/halo/kb/"))
	(engine-lst   (directory "/projects/rkf/storage/jchaw/halo/engine/"))
	(outputstream (make-string-output-stream))
	(directory "/projects/rkf/storage/jchaw/halo/question/"))
    (dolist (engine-entry engine-lst)
      (dolist (kb-entry kb-lst)
	(let* ((kb-name (pathname-name kb-entry))
	       (domain  (ps-get-domain-for-kb kb-name)))
	  (dolist (question-entry (ps-get-question-list-for-domain domain))
	    (format outputstream "~a/~a/~a/~%" (pathname-name engine-entry)
		                               (pathname-name kb-entry)
					       (pathname-name question-entry))))))
    (with-open-file (stream "/projects/rkf/storage/jchaw/halo/joblist" 
			    :direction :output
  			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "~a" (get-output-stream-string outputstream)))))

(defun resurrect-cpl()
  (progn
    (format t "BPS: Begin resurrecting CPL.~%")
    (ps-load-cpl-miscellaneous)
    (initialize-cpl)
    (register-user-defined-concepts-with-CPL)			  
    (format t "BPS: Done resurrecting CPL.~%")))

(defun ps-load-cpl-miscellaneous()
  (progn
    (load "/projects/rkf/storage/jchaw/engines/main-system/cpl/translations.lisp")
    (load "/projects/rkf/storage/jchaw/engines/main-system/cpl/km/dispatch-macros.lisp")))

(defun ps-generate-condor-state(&optional(image-name "condor-image.dxl"))
  (progn
    (setq excl:*restart-init-function* 
	  '(lambda () 
	     (progn
	       (in-package :km) 
	       (format t "Tester: Calling *restart-init-function*~%"))))
    (setq excl:*restart-app-function* 
	  `(lambda () 
	     (let ((argv (aftern (sys:command-line-arguments :application nil) 3)))
	       (in-package :km) 
	       (format t "Command arguments(~a) : ~a~%" (length argv) argv)
	       (let ((mode (car argv)))
		 (format t "mode : ~a~%" (string-downcase mode))
		 (cond ((string= (string-downcase mode) "report")
			(generate-report))
		       ((string= (string-downcase mode) "report-diff")
			(generate-temporal-diff-page-for-target))
		       (t (let ((kb-location (cadr argv)))
			    (if (null kb-location)
				(setq *CONTROLLER-KB-LOCATION* "./aura/")
			        (setq *CONTROLLER-KB-LOCATION* (format nil "~a/aura/" kb-location)))
			    (resurrect-cpl)
			    (ps-load-user-defined-lib *CONTROLLER-KB-LOCATION*)
			    (crank-up-problem-solver)
			    (reset-test-bench)
			    (load-tester-set)
			    (setq *controller-persistent-hash-mutable* nil)
			    (cond ((string= (string-downcase mode) "bps")
				   (format t "BPS: Doing BPS processing.~%")
				   (generate-bps-set))
				  ((string= (string-downcase mode) "cpl")
				   (progn
				     (format t "BPS: Doing CPL processing.~%")
				     (generate-cpl-set)))
				  ((string= (string-downcase mode) "continue")
				   (progn
				     #|
				     (setq *HALO2-TESTER-UNCACHED-CONCEPTS* 	(get-uncached-user-defined-concepts))
				     (setq *ALL-USER-DEFINED-CONCEPTS* (set-difference *ALL-USER-DEFINED-CONCEPTS*
				     (get-uncached-user-defined-concepts)
				     :test 'equal))
				     |#
			      (cond ((missing-cpl-set)
				     (generate-cpl-set))
				    ((missing-bps-set)
				     (generate-bps-set)))))
			   ((string= (string-downcase mode) "update")
			    (progn
			      (update-tester-set)))
			   ((string= (string-downcase mode) "debug")
			    (let ((kb-location (cadr argv)))
			      (if (null kb-location)
				  (setq *CONTROLLER-KB-LOCATION*              "./aura/")
				(setq *CONTROLLER-KB-LOCATION*              (format nil "~a/aura/" kb-location)))
			      ;(ps-load-user-defined-lib *CONTROLLER-KB-LOCATION*)
			      ;(crank-up-problem-solver)
			      (setq *controller-persistent-hash-mutable* nil)
			      (format t "~%")
			      (format t "This version of the Basic-Problem-Solver was compiled on ~a.~%" 				   
				      (stringify-universal-time 
				       (file-write-date 
					(format nil "~a/condor-image.dxl" kb-location))))
			      (format t "~%")
			      (tpl:start-interactive-top-level *terminal-io* 
							       #'tpl:top-level-read-eval-print-loop
							       nil)))
			   ((string= (string-downcase mode) "cache")
			    (format t "BPS: Creating concept cache.~%")
			    (setq *controller-persistent-hash-mutable* t)
			    (prefetch-concept-cache))
			   (t
			    (progn
			      (setq *controller-persistent-hash-mutable* t)
			      (if (not (null (file-write-date "condor-image.dxl")))
				  (progn
				    (format t "~%")
				    (format t "This version of the Basic-Problem-Solver was compiled on ~a.~%" 
					    (stringify-universal-time (file-write-date "condor-image.dxl")))
				    (format t "~%")))
			      (tpl:start-interactive-top-level *terminal-io* 
							       #'tpl:top-level-read-eval-print-loop
							       nil))))
			    (reload-tester-set)
			    (generate-report))))
	       (excl:exit)))))
    (format t "~%BPS: Generating LISP image for Condor processing.   ")
    (excl:dumplisp :name image-name)
    (format t "[Done]~%")))

(defun generate-full-account-result-table(&optional (target ".")
						    (filename "full.html"))
  (let ((q-number-list (get-formulation-set)))
    (generate-result-table-for-q-number-list filename
					     (get-formulation-set)
					     target)))

(defun generate-result-table-p(&optional (target ".")
					(filename "index.html"))
  (let ((target-date      (file-write-date (format nil "~a/~a" target filename)))
	(most-recent-file (car (sort-filelist-by-write-date 
				(union 
				 (union (directory "formulation-Q*.lisp")
					(directory "cpl-Q*.lisp"))
				 (directory "bps-Q*.lisp"))))))
    (cond ((or (null target-date)
	       (null most-recent-file)) t)
	  (t (< target-date
		(file-write-date most-recent-file))))))
		      
(defun generate-result-table-for-q-number-list(filename q-number-list
							&optional (target "."))
  (let ((tmp-filename (gen-temp-filename))
	(q-number-list (sort-q-number-list q-number-list)))
    (with-open-file (stream tmp-filename
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (load-tester-set target)
      (format stream "<HTML><BODY>")
      (format stream "created at ~a, ~a entries.<BR><BR>" (get-time-of-day) (length q-number-list))
      (format stream "~a<BR><BR>" (generate-index-for-q-number-list q-number-list))
      (format stream "<TABLE BORDER=\"1\">")
      (format stream "<TR><TH width=\"50%\">Formulation/CPL processing<TH width=\"50%\">BPS Answer</TH></TD>")
      (dolist (q-number q-number-list)
	(format t ".")
	(generate-result-table-item stream q-number))
      (format stream "</TABLE>")
      (put-google-analytic-tag stream)
      (format stream "</body>")
      (format stream "</HTML>")
      (format t "~%"))
    (tester-rename-file tmp-filename filename)))

(defun generate-index-for-q-number-list(q-number-list)
  (let ((s (make-string-output-stream)))
    (dolist (q-number q-number-list)
      (format s "<a href=\"#~a\">~a</a>  " q-number q-number))
      (get-output-stream-string s)))

(defun q-number-<-operator(q-number1 q-number2)
  (let* ((string1 (format nil "~a" q-number1))
	 (string2 (format nil "~a" q-number2))
	 (number1 (parse-integer (subseq string1 2 (length string1))))
	 (number2 (parse-integer (subseq string2 2 (length string2)))))
    (<= number1 number2)))

(defun sort-q-number-list(q-number-list)
  (sort (check-q-number-list (copy-list q-number-list)) 'q-number-<-operator))
  
(defun check-q-number-list(q-number-list)
  (remove nil
	  (mapcar #'(lambda(x)
	      (let* ((str (format nil "~a" x)))
		(handler-case (if (numberp (parse-integer (subseq str 2 (length str))))
				  x)
			      (error (condition)
				     nil))))
	  q-number-list)))

(defun generate-result-table-item(stream q-number)
  (let ((formulation-output (generate-formulation-output-string q-number))
	(comments-output    (generate-comments-output-string    q-number))
	(cpl-output         (generate-cpl-output-string         q-number))
	(bps-output         (generate-bps-output-string         q-number)))
    (format stream "<TR id=\"~a\">~%" q-number)
    (format stream "<TD>~a ~a <hr>~a</TD>" 
	           formulation-output 
		   comments-output 
		   cpl-output)
    (format stream "<TD>~a</TD>"    bps-output)
    (format stream "</TR>")))

(defun generate-formulation-output-string(q-number)
  (let ((stream (make-string-output-stream)))
    (let ((target-cpl-formulation      (intern (format nil "*~a-QUESTION*" q-number)      :km))
	  (target-original-formulation (intern (format nil "*~a-QUESTION-ORIG*" q-number) :km)))
      (cond ((and (boundp target-cpl-formulation))
	     (let ((grade-value (grade q-number))
		   (question-id (if (boundp (intern (format nil "*~a-QUESTION-ID*" q-number) :km))
				    (eval (intern (format nil "*~a-QUESTION-ID*" q-number) :km))
				"?")))
		 (format stream "<B>~a[~a](~a)</B><BR>" q-number question-id grade-value)
		 (if (boundp target-original-formulation)		 
		     (format stream "<B>Original formulation</B><BR>~a<BR>" 
			     (eval target-original-formulation)))
		 (format stream "<B>CPL Formulation</B><BR><PRE>~a</PRE>" 
			 (ps-make-cpl-string-proper (eval target-cpl-formulation)))))
	    (t (format stream "<B>~a Formulation</B> <PRE>No such formulation</PRE>" q-number))))
    (get-output-stream-string stream)))

(defun generate-comments-output-string(q-number)
  (let ((target-comments (intern (format nil "*~a-COMMENTS*" q-number) :km)))
    (cond ((and (boundp target-comments)
		(not (null (eval target-comments))))
	   (format nil "<B>~a Comments</B><BR>~a<BR>" q-number
		   (ps-make-cpl-string-proper (eval target-comments))))
	  (t (format nil "<B>~a Comments</B> <PRE>No comments</PRE>" q-number)))))
  
(defun generate-cpl-output-string(q-number &optional (prefix-tag "cpl"))
  (let ((s (make-string-output-stream))
	(target-cpl-scenario     (intern (string-upcase (format nil "*~a-~a-SCENARIO*"     q-number prefix-tag)) :km))
	(target-cpl-questions    (intern (string-upcase (format nil "*~a-~a-QUESTIONS*"    q-number prefix-tag)) :km))
	(target-cpl-yn-questions (intern (string-upcase (format nil "*~a-~a-YN-QUESTIONS*" q-number prefix-tag)) :km))
	(target-cpl-timings      (intern (string-upcase (format nil "*~a-~a-TIME*"         q-number prefix-tag)) :km))
	(target-cpl-clib-version (intern (string-upcase (format nil "*~a-~a-CLIB-VERSION*" q-number prefix-tag)) :km))
	(target-cpl-version      (intern (string-upcase (format nil "*~a-~a-VERSION*"      q-number prefix-tag)) :km)))
    (cond ((and (boundp target-cpl-scenario)
		(boundp target-cpl-questions)
		(boundp target-cpl-yn-questions))
	   (progn
	     (format s "<B>CPL-Scenario</B><BR><PRE>~a</PRE>~%"          (htmlify-km-assertion-list (triples-to-km-assertions 
												     (remove-irrelevant-km-triples
												     (eval target-cpl-scenario)))))
	     (format s "<B>CPL-Compute-Questions</B><BR><PRE>~a</PRE>~%" (eval target-cpl-questions))
	     (format s "<B>CPL-YN-Questions</B><BR><PRE>~a</PRE>~%"      (eval target-cpl-yn-questions))
	     (if (and (boundp target-cpl-timings)
		      (boundp target-cpl-clib-version) 
		      (boundp target-cpl-version))		 
		 (format s "<HR>Interpreted in <B>~a seconds</B>, using CPL <B> ~a </B> and CLIB dated <B>~a</B>~%" 
			 (eval target-cpl-timings)
			 (eval target-cpl-version)
			 (eval target-cpl-clib-version)))
	     (get-output-stream-string s)))
	  (t "Not yet CPL processed."))))

(defun htmlify-km-assertion-list(km-assertion-list)
  (let ((s (make-string-output-stream)))
  (dolist (km-assertion km-assertion-list)
    (format s "~a~%" (htmlify-km-assertion km-assertion)))
  (get-output-stream-string s)))

(defun htmlify-km-assertion(km-assertion)
  (let ((s (make-string-output-stream))
	(header (format nil "~a ~a"(first km-assertion) (second km-assertion))))
    (format s "(~a" header)
    (dolist (item (cddr km-assertion))
      (format s "~%    ~a" item))
    (format s ")~%" header (third km-assertion))
    (get-output-stream-string s)))

(defun lisp-error-string-p(input)
  (let ((begin (search "#<" input))
	(end   (search ">" (reverse input))))
    (and (numberp begin)
	 (numberp end)
	 (zerop begin)
	 (zerop end))))

(defun format-lisp-error-string(input)
  (if (lisp-error-string-p input)
      (format nil "LISP error : ~a~%"  (subseq input 2 (search "@" input)))
      input))

(defun generate-bps-output-string(q-number)
  (let ((target-bps-answer         (intern (format nil "*~a-BPS-ANSWER*"       q-number) :km))
	(target-bps-time           (intern (format nil "*~a-BPS-TIME*"         q-number) :km))
	(target-bps-dryrun         (intern (format nil "*~a-BPS-DRYRUN*"       q-number) :km))
	(target-bps-expl-time      (intern (format nil "*~a-EXPL-TIME*"        q-number) :km))
	(target-bps-version        (intern (format nil "*~a-BPS-VERSION*"      q-number) :km))
	(target-bps-clib-version   (intern (format nil "*~a-BPS-CLIB-VERSION*" q-number) :km))
	(target-bps-lisp-error-tag (intern (format nil "*~a-BPS-LISP-ERROR-TAG*" q-number) :km)))
    (cond ((or (boundp target-bps-lisp-error-tag)
	       (boundp target-bps-answer))
	   (let ((s (make-string-output-stream)))
	     (cond ((boundp target-bps-lisp-error-tag)
		    (format s "~a" (format-lisp-error-string (car (eval target-bps-lisp-error-tag)))))
		   ((boundp target-bps-answer)
		    (let ((viewpoint-answer-page (format-lisp-error-string (eval target-bps-answer))))
		      (format s "~a" viewpoint-answer-page))))
	     (format s "<HR>")
	     (if (probe-file (format nil "./graph-~a" q-number))
		 (if (and (boundp target-bps-dryrun)
			  (eval target-bps-dryrun))
		     (format s "<a href=\"./graph-~a\">Problem-solving tree (dryrun processing)</a><BR>" q-number)
		   (format s "<a href=\"./graph-~a\">Problem-solving tree (actual processing)</a><BR>" q-number)))
	     (if (and (boundp target-bps-time)
		      (boundp target-bps-expl-time))
		 (format s "Solved in <B>~a seconds</B> and  explained in <B>~a seconds</B>.<BR> "
			 (eval target-bps-time)
			 (eval target-bps-expl-time)))
	     (generate-bps-statistic s q-number)
	     (if (and (boundp target-bps-version)
		      (boundp target-bps-clib-version))
		 (format s "BPS dated <B> ~a </B><BR> CLIB dated <B>~a</B>~%" 
			 (stringify-universal-time (eval target-bps-version))
			 (eval target-bps-clib-version)))
	     (get-output-stream-string s)))
	  (t "Not yet BPS processed."))))

(defun generate-bps-statistic(stream q-number)
  (let ((target-bps-openlist      (intern (format nil "*~a-BPS-OPENLIST*"     q-number) :km))
	(target-bps-closedlist    (intern (format nil "*~a-BPS-CLOSEDLIST*"   q-number) :km))
	(target-bps-ignoredlist   (intern (format nil "*~a-BPS-IGNOREDLIST*"  q-number) :km)))
    (if (and (boundp target-bps-openlist)
	     (boundp target-bps-closedlist)
	     (boundp target-bps-ignoredlist))
	(format stream "Selected [~a], Examined [~a], and Ignored [~a] viewpoints<BR>"
		(+ (length (eval target-bps-openlist))
		   (length (eval target-bps-closedlist)))
		(length (eval target-bps-closedlist))
		(length (eval target-bps-ignoredlist))))))

#|
				     (defun update-tester-set()
(progn 
(do-update-zapping)
(generate-tester-set)))
				     |#

(defun update-tester-set()
  (progn
    (reload-tester-set)
    (let ((target (nth (ps-random (total-formulation-count)) *q-formulation-set*)))
      (do-cpl-processing target)
      (reload-tester-set)
      (do-bps-processing target))))

(defun do-update-zapping()
  (let* ((filelist (union (directory "cpl-*.lisp")
			  (directory "bps-*.lisp")))
	 (threshold (ceiling (* 0.9 (total-formulation-count)))))
    (if (= (/ (length filelist) 2) 
	   (total-formulation-count))
	(excl:while (> (length (directory "cpl-*.lisp")) threshold)
	  (let ((q-number (determine-oldest-question-target)))
	    (if (not (null q-number))
		(progn 
		  (format t "HALO2 Tester: Zapping ~a.~%" q-number)
		  (zap-cpl q-number))
	        (return)))))))

(defun determine-oldest-question-target()
  (let* ((filelist (directory "cpl-*.lisp"))
	 (result   (car filelist)))
    (dolist (target filelist)
      (if (< (file-write-date target)
	     (file-write-date result))
	  (setq result target)))
    (if (not (null result))
	(let ((temp-str (pathname-name result)))
	  (subseq temp-str (length "cpl-") (length temp-str))))))


;; Code for generating report tables.

(defun generate-bps-lisp-error-result-table(&optional (target ".")
						      (filename "bps-lisp-error.html"))
  (let ((q-number-list (find-bps-lisp-errors)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-cpl-lisp-error-result-table(&optional (target ".")
				       (filename "cpl-lisp-error.html"))
  (let ((q-number-list (find-cpl-lisp-errors)))
  (generate-result-table-for-q-number-list filename
					   q-number-list
					   target)
  q-number-list))

(defun generate-cpl-empty-scenario-error-result-table (&optional (target ".")
				       (filename "cpl-nil-scenario-error.html"))
  (let ((q-number-list (identify-broken-cpl-interpretations-with-empty-scenarios)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-cpl-empty-query-error-result-table (&optional (target ".")
				       (filename "cpl-empty-query-error.html"))
  (let ((q-number-list (identify-broken-cpl-interpretations-with-empty-query)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-bps-timeout-error-result-table(&optional (target ".")
							  (filename "bps-timeout-error.html"))
  (let ((q-number-list (find-bps-timeout-errors)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-good-bps(&optional (target ".")
						    (filename "good-bps.html"))
  (let ((q-number-list (identify-plausible-bps-answers)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-correct-good-bps(&optional (target ".")
							    (filename "correct-good-bps.html"))
  (let ((q-number-list (identify-correct-bps-answers)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-incorrect-good-bps(&optional (target ".")
							    (filename "incorrect-good-bps.html"))
  (let ((q-number-list (identify-incorrect-bps-answers)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-unknown-good-bps(&optional (target ".")
							    (filename "unknown-good-bps.html"))
  (let ((q-number-list (identify-unknown-bps-answers)))
    (generate-result-table-for-q-number-list filename
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-bad-bps(&optional (target "."))
  (let ((q-number-list (identify-broken-bps-answers)))
    (generate-result-table-for-q-number-list "bad-bps.html" 
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-bad-bps-having-nil-answers(&optional (target "."))
  (let ((q-number-list (identify-broken-bps-answers-having-nil-answers)))
    (generate-result-table-for-q-number-list "bad-bps-having-nil-answers.html" 
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-good-cpl(&optional (target "."))
  (let ((q-number-list (identify-good-cpl-interpretations)))
    (generate-result-table-for-q-number-list "good-cpl.html" 
					     q-number-list
					     target)
    q-number-list))

(defun generate-result-table-for-bad-cpl(&optional (target "."))
  (let ((q-number-list (identify-broken-cpl-interpretations)))
    (generate-result-table-for-q-number-list "bad-cpl.html" 
					     q-number-list
					     target)
    q-number-list))

(defun generate-report-correctness-for-bad-cpl(stream)
    (let* ((bad-cpl-count           (length (generate-result-table-for-bad-cpl)))
	   (cpl-lisp-error-count    (length (generate-cpl-lisp-error-result-table)))
	   (cpl-timeout-error-count 0) ;;FIXME
	   (cpl-empty-scenario-error-count    (length (generate-cpl-empty-scenario-error-result-table)))
	   (cpl-empty-query-error-count    (length (generate-cpl-empty-query-error-result-table))))
  (if (not (zerop bad-cpl-count))
      (progn 
	(format stream "<li><a href=\"bad-cpl.html\">~a</a> had invalid interpretations.</li>" bad-cpl-count)
	(format stream "<ul>")
	(if (not (zerop cpl-lisp-error-count))    
	    (format stream "<li><a href=\"cpl-lisp-error.html\">~a</a> were due to LISP errors.</li>" cpl-lisp-error-count))
	(if (not (zerop cpl-timeout-error-count)) 
	    (format stream "<li><a href=\"cpl-lisp-error.html\">~a</a> were due to timeout errors.</li>" cpl-timeout-error-count))
	(if (not (zerop cpl-empty-scenario-error-count)) 
	    (format stream "<li><a href=\"cpl-nil-scenario-error.html\">~a</a> interpretations had empty scenarios.</li>"    cpl-empty-scenario-error-count))
	(if (not (zerop cpl-empty-query-error-count ))
	    (format stream "<li><a href=\"cpl-empty-query-error.html\">~a</a> interpretations had some scenario but no specified query.</li>"  cpl-empty-query-error-count))
	(format stream "</ul>")))))

(defun generate-report-correctness()
    (let* ((good-cpl                (generate-result-table-for-good-cpl))
	   (good-cpl-count          (length good-cpl))
	   (bad-cpl-count           (length (generate-result-table-for-bad-cpl)))
	   (cpl-lisp-error-count    (length (generate-cpl-lisp-error-result-table)))
	   (cpl-timeout-error-count 0) ;;FIXME
	   (cpl-empty-scenario-error-count    (length (generate-cpl-empty-scenario-error-result-table)))
	   (cpl-empty-query-error-count    (length (generate-cpl-empty-query-error-result-table)))
	   (total-bps-count         (length (intersection (current-bps-set) good-cpl)))
	   (good-bps-count          (length (generate-result-table-for-good-bps)))
	   (bad-bps-count           (length (generate-result-table-for-bad-bps)))
	   (bps-lisp-error-count    (length (generate-bps-lisp-error-result-table)))
	   (bps-timeout-error-count (length (generate-bps-timeout-error-result-table)))
	   (bps-nil-answer-count    (length (generate-result-table-for-bad-bps-having-nil-answers)))
	   (stream (make-string-output-stream)))
      (format stream "<ul>")
      (generate-report-correctness-for-bad-cpl stream)
        (format stream "<li><a href=\"good-cpl.html\">~a</a> were plausibly interpreted. BPS attempted ~a interpretations.</li>" 
		good-cpl-count 
		total-bps-count)
  	(format stream "<ul>")
          (format stream "<li><a href=\"good-bps.html\">~a</a> had plausible answers. But <a href=\"bad-bps.html\">~a</a> were not answered.</li>" 
		  good-bps-count 
		  bad-bps-count)
	   (format stream "<ul>")
	   (format stream "<li>Of the ~a plausible answers</li>" good-bps-count)
	    (format stream "<ul>")
  	      (format stream "<li><a href=\"correct-good-bps.html\">~a</a> had correct answers.</li>"     (length (generate-result-table-for-correct-good-bps)))
	      (format stream "<li><a href=\"incorrect-good-bps.html\">~a</a> had incorrect answers.</li>" (length (generate-result-table-for-incorrect-good-bps)))
	      (format stream "<li><a href=\"unknown-good-bps.html\">~a</a> were not graded.</li>"         (length (generate-result-table-for-unknown-good-bps)))
	    (format stream "</ul>")
	   (format stream "<li>For the ~a that were not answered</li>" bad-bps-count)
	    (format stream "<ul>")
  	      (format stream "<li><a href=\"bps-lisp-error.html\">~a</a> were due to LISP errors.</li>"            bps-lisp-error-count)
	      (format stream "<li><a href=\"bps-timeout-error.html\">~a</a> were due to timeout errors.</li>"      bps-timeout-error-count)
	      (format stream "<li><a href=\"bad-bps-having-nil-answers.html\">~a</a> attempts returned nil.</li>"  bps-nil-answer-count)
	    (format stream "</ul>")
	   (format stream "</ul>")
	  (format stream "</ul>")
	  (format nil "~a" (get-output-stream-string stream))))

(defun generate-report-performance()
  (let* ((avg-cpl-time            (get-mean-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (median-cpl-time         (get-median-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (max-cpl-time            (get-max-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (min-cpl-time            (get-min-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (avg-qa-time             (get-mean-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (median-qa-time          (get-median-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (max-qa-time             (get-max-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (min-qa-time             (get-min-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (avg-bps-time            (get-mean-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (median-bps-time         (get-median-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (max-bps-time            (get-max-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (min-bps-time            (get-min-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (avg-expl-time           (get-mean-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))
	   (median-expl-time        (get-median-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))
	   (max-expl-time           (get-max-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))
	   (min-expl-time           (get-min-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))	   
	   (90pct-cpl-time          (get-90pct-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (90pct-qa-time           (get-90pct-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (90pct-bps-time          (get-90pct-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (90pct-expl-time         (get-90pct-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))
	   (75pct-cpl-time          (get-75pct-for-time-pair-list (get-cpl-processing-times (identify-good-cpl-interpretations))))
	   (75pct-qa-time           (get-75pct-for-time-pair-list (get-qa-processing-times (identify-plausible-bps-answers))))
	   (75pct-bps-time          (get-75pct-for-time-pair-list (get-bps-processing-times (identify-plausible-bps-answers))))
	   (75pct-expl-time         (get-75pct-for-time-pair-list (get-expl-processing-times (identify-plausible-bps-answers))))
	   (stream (make-string-output-stream)))
          (format stream "<ul>")
            (format stream "<li>CPL processing (for plausible interpretations)</li>")
            (format stream "<ul>")
              (format stream "<li>CPL interpretation averaged ~a secs.</li>" avg-cpl-time)
              (format stream "<li>The median time for CPL interpretation was ~a secs.</li>" median-cpl-time)
              (format stream "<li>The 75th pecentile time for CPL interpretation was ~a secs.</li>" 75pct-cpl-time)
              (format stream "<li>The 90th pecentile time for CPL interpretation was ~a secs.</li>" 90pct-cpl-time)
              (format stream "<li>CPL interpretation ranged from ~a to ~a secs.</li>" min-cpl-time max-cpl-time)
            (format stream "</ul>")

            (format stream "<li>QA processing (for plausible answers)</li>")
            (format stream "<ul>")
 	      (format stream "<li>QA processing averaged ~a secs.</li>" avg-qa-time)
	      (format stream "<li>The median time for QA processing was ~a secs.</li>" median-qa-time)
	      (format stream "<li>The 75th percentile time for QA processing was ~a secs.</li>" 75pct-qa-time)
	      (format stream "<li>The 90th percentile time for QA processing was ~a secs.</li>" 90pct-qa-time)
	      (format stream "<li>QA processing ranged from ~a to ~a secs.</li>" min-qa-time max-qa-time)

            (format stream "<li>BPS processing (for plausible answers)</li>")
            (format stream "<ul>")
 	      (format stream "<li>BPS processing averaged ~a secs.</li>" avg-bps-time)
	      (format stream "<li>The median time for BPS processing was ~a secs.</li>" median-bps-time)
	      (format stream "<li>The 75th percentile time for BPS processing was ~a secs.</li>" 75pct-bps-time)
	      (format stream "<li>The 90th percentile time for BPS processing was ~a secs.</li>" 90pct-bps-time)
	      (format stream "<li>BPS processing ranged from ~a to ~a secs.</li>" min-bps-time max-bps-time)
            (format stream "</ul>")
            (format stream "<li>Explanation processing (for plausible answers)</li>")
            (format stream "<ul>")
 	      (format stream "<li>Explanation processing averaged ~a secs.</li>" avg-expl-time)
	      (format stream "<li>The median time for explanation processing was ~a secs.</li>" median-expl-time)
	      (format stream "<li>The 75th percentile time for explanation processing was ~a secs.</li>" 75pct-expl-time)
	      (format stream "<li>The 90th percentile time for explanation processing was ~a secs.</li>" 90pct-expl-time)
	      (format stream "<li>Explanation processing ranged from ~a to ~a secs.</li>" min-expl-time max-expl-time)
            (format stream "</ul>")
          (format stream "</ul>")
            (format stream "</ul>")
	  (format nil "~a" (get-output-stream-string stream))))

(defun generate-report (&optional (target ".")
				  (filename "index.html"))
  (let ((tmp-filename (gen-temp-filename)))
  (with-open-file (stream tmp-filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (reload-tester-set target)
    (generate-performance-csv "performance.csv")
    (let ((total            (total-formulation-count))
	  (total-cpl-count  (length (current-cpl-set))))
    (generate-full-account-result-table)
    (format stream "<HTML><BODY>")
    (format stream "There are ~a formulations. "         total)
    (format stream "Of these, ~a were attempted by CPL." total-cpl-count)
    (format stream "<BR>")
    (format stream "This report was created at ~a" (get-time-of-day))
    (format stream "<TABLE BORDER=\"1\">")
    (format stream "<TR><TH>Correctness<TH>Performance</TH></TD>")
    (format stream "<TR>~%")
    (format stream "<TD>~a</TD>" (generate-report-correctness))
    (format stream "<TD>~a</TD>" (generate-report-performance))
    (format stream "</TR>")	  
    (format stream "</TABLE>")	  
    (if (probe-file "time-diff.html") 
	(format stream "A <a href=\"full.html\">full account</a> of the processing, and a <a href =\"time-diff.html\">catalog of differences</a> among snapshots.")
        (format stream "A <a href=\"full.html\">full account</a> of the processing."))
    (format stream "<br>")
    (format stream "<br>")
    (format stream "~a" (include-html-code-for-line-graph nil))
    (put-google-analytic-tag stream)
    (format stream "</body>")
    (format stream "</HTML>")
    (format t "~%")))
    (tester-rename-file tmp-filename filename)))

(defun include-html-code-for-line-graph(stream)
  (format stream 
	  "<script type=\"text/javascript\" src=\"amline/swfobject.js\"></script>
	<div id=\"flashcontent\">
		<strong>You need to upgrade your Flash Player</strong>
	</div>
	<script type=\"text/javascript\">
		// <![CDATA[		
		var so = new SWFObject(\"amline/amline.swf\", \"amline\", \"1260\", \"300\", \"8\", \"#FFFFFF\");
		so.addVariable(\"path\", \"./\");
		so.addVariable(\"settings_file\", escape(\"amline/halo2-tester-performance.xml\"));  // you can set two or more different settings files here (separated by commas)
		so.addVariable(\"data_file\",
		escape(\"performance.csv\"));
		so.addVariable(\"preloader_color\", \"#999999\");
		so.write(\"flashcontent\");
		// ]]>
	</script>"))

