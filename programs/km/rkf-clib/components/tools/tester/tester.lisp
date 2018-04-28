;;
;; $Id: tester.lisp,v 1.21 2008/03/14 18:11:47 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)

;;===============================================================
;; Some global constant declarations
;;===============================================================
(defconstant *CLIB-AUTHORS*
  '("tecuci" "kbarker" "porter" "jchaw" "mrglass" "onue5" "aiyer" "vaibhav" "hari5851" "kunal"))
	
(defconstant *DEFAULT-RECEIVER* "tecuci")

;; all file related to the output of tester should be dumped here.
(defconstant *output-dir* "/projects/rkf/util/test-results/")

(defvar *reserved-km-keywords* nil)

(defvar *reserved-shaken-keywords*
  '(|SHAKEN-Slot-Group| |SHAKEN-Attribute-Group| |every| |forall| |node-coordinate| |edge-coordinate| |node-visibility| |edge-visibility| |node-never-visible| |edge-never-visible| |internally-expanded| |node-unviewed| |edge-unviewed| |user-defined-node-label| |user-defined-edge-label| |node-comment| |edge-comment| |view-accessors| |user-equation-expression| |edge-documentation| |slot-value-condition| |lmap-expanded| |lmap-expansion-state| |Big-Node| |Condition-Node| |Group-Node| |Property-Node| |Equation-Big-Node| |Preparatory-Event| |Trigger-Node| |Salient-Node| |Negation-Node| |contains-node| |contains-node-of| |contains-edge| |big-node-is-open| |big-node-is-open-of| |big-nodes| |big-node-of| |every| |value| |equation-uses| |equation-expression| |equation-symbol| |equation| |component| |SHAKEN-Table| |SHAKEN-Table-Column| |SHAKEN-Table-Header-Column| |has-table-column| |has-table-column-of| |has-table-column-header| |has-table-column-header-of| |has-table-rows-content| |has-column-content| |has-table-column-order| |SHAKEN-Column-Content-Order-Constant| |has-column-content-order| |SHAKEN-Partition| ))

(defun set-reserved-km-keywords ()
  (setf *reserved-km-keywords*
	(append '(|TheValue| |TheValues| :|default| |range2| |situation-specific| |anonymous-instancep| |PAIR-FILTER| |get-justification|);; <- ***
		*built-in-classes-with-nonfluent-instances-relation*
		*additional-keywords* *constraint-keywords*
		*downcase-km-lisp-exprs* *built-in-frames*
		*reserved-keywords* *reserved-shaken-keywords*
		'(|nil| |+/-| |property-eq| |property-gt| |property-lt| |property-lte| |property-gte| |in| |taxonomy| |now-has| |original-slot|))))

;; list of all core clib components
(defvar *clib-core-components* nil)

;;===============================================================
;; Some global variable declarations
;;===============================================================
(defvar *all-well-formed* t)
(defvar *file-pathes* nil)
(defvar *send-email-on-error* t)

;; Variables used to check whether concepts are defined.
(defvar *all-concepts* nil)
(defvar *declared-concepts* nil)

;; [dgt] keep this?
(defvar *excluded-concepts* nil)
(defvar *excluded-directories* nil)

(defvar *initial-clib* nil)

;;===============================================================
;; top-level function to test all the components within the 
;; component library. 
;; [dgt] 1/20/08 - added build parameter
;;===============================================================
(defun tester (&optional (km-path "km") (clib-path "./") (send-mail-p t) (gen-tax-p t) 
			 (result-file "test-result.txt") (skip-core-concepts nil) (build "halo2"))
  ;; initialization
  (load km-path) ;; load km
  (load (concatenate 'string clib-path "tools/km-state-vars.lisp")) ;; load km state vars from file
  (set-reserved-km-keywords)
  (load (concatenate 'string clib-path "tools/load-clib-build.lisp")) ;; load clib loader
  (setf *abort-on-error-report* nil)
  (setf *abort-on-error-report* t)

  ;; load-clib and test it
  (with-open-file (stream result-file :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)
    (generate-tester-header stream build)
    (let* ((components  (load-clib-build km-path clib-path build 'check-km-file))
	   (*all-concepts* (remove-duplicates *all-concepts* :test #'km-symbol-name-equal))
	   test-result1 test-result2 test-result3)
      ;; store kb 
      (store-kb) 

      ;; 1) Check for undefined concepts/frames
      (setf test-result1 (check-undefined-frames stream (remove-reserved-km-keywords *all-concepts*) build clib-path))
      ;; 2) Compute the values for all slots on a component. Skip concepts in core if told to do so. 
      ;;    Why? Because tester is called twice by the nightly script. Once to test the regular CLib 
      ;;    distribution and again to test the Halo distribution. Both the Reg. and Halo distribution 
      ;;    share the same upper ontology (i.e. "core"), so we don't want to waste work. Especially
      ;;    since the tester takes quite some time to run.
      ;;(setf test-result2 (test-all-classes stream components)) 
      ;; 3) Run the test cases for each component. Once again, we can specify whether concepts in core
      ;;    can be skipped.
      (setf test-result3 (test-all-components components stream skip-core-concepts))
      ;; 4) Process the results. 
      (if (or test-result1 test-result2 test-result3)
	  (progn
	    (format stream "~%~%======================================================================~%")
	    (format stream "                            FAILED CASE HANDLING~%")
	    (format stream "======================================================================~%")
	    (handle-failed-cases test-result1 stream send-mail-p build)
	    ;;(handle-failed-cases test-result2 stream send-mail-p build)
	    (handle-failed-cases test-result3 stream send-mail-p build)
	    (if send-mail-p (send-mails build)))
	(if (and gen-tax-p *all-well-formed*) 
	    (excl::shell (format nil "/projects/rkf/util/update-comp.sh")))))))

(defun print-local-time ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (format nil "~a:~a:~a ~a/~a/~a" hour minute second month date year))) 


;;===============================================================
;; Function to handle all the components whoses test cases
;; failed during the testing process.
;;===============================================================
(defun handle-failed-cases (f-list stream &optional (send-mail-p t) build)
  (let (author)
    (if send-mail-p
        (dolist (x f-list)
	  (setq author (look-up-author (table-lookup-path (first x))))
		(if (and author
			 (member author *CLIB-AUTHORS* :test #'equal))
		    ;; author found
		    (progn 
		      (update-file-for-author author x build)
		      (format stream "~%Informed author: ~A, of error in ~A!~%" author (first x)))
		  ;; No author was found, email default receiver.
		  (progn 
		    (format stream "~%No author found for component: ~A, informed default receiver (~a)~%" (first x) *DEFAULT-RECEIVER*)
		    (update-file-for-author *DEFAULT-RECEIVER* x build :orphan t)))))))


;;===============================================================
;; Functions to look up the author and mail the author of
;; a component if an error occurred.
;;===============================================================
(defun look-up-author (component)
  (excl::shell (format nil "/lusr/bin/cvs log ~A > cvs-log.tmp" component))
  (with-open-file (s "cvs-log.tmp")
		  (loop
		   (let ((cvs-line (read-line s nil 'eof)))
		     (if (equal cvs-line 'eof)
			 (return)
		       (multiple-value-bind (matched whole-match author)
					    (excl::match-regexp "author: \\(\\w+\\);" cvs-line)
					    (if matched
						(progn 
						  (close s)
						  (delete-file "cvs-log.tmp")
						  (return author)))))))))

;; ===========================================================================
;; sends email to authors of components with problems
;; 
(defun send-mails (build)
  ;; find all the mail msgs
  (let ((files (directory (concatenate 'string *output-dir* "*-err.msg")))
	(author-name nil)
	matched? matched-str)
    ;; send them out one by one
    (dolist (file files)
	    ;; get the author name
	    (multiple-value-setq (matched? matched-str author) 
	      (excl::match-regexp "/\\([a-z]+\\)-err.msg" (format nil "~A" file)))
	    (when matched?
	      (excl::shell (format nil "mail -s \"CLib errors for build ~a\" ~A < ~A" build author file)))
	    ;;(delete-file file)
	    )))
;; ==============================================================================

;; ==============================================================================
;; updates the file corresponding to author with failed-case
;;
(defun update-file-for-author (author failed-case build &key (orphan nil))
  (let* ((comp      (first  failed-case))
	 (case      (second failed-case))
	 (reason    (third  failed-case)) 
	 (first-time? (not (probe-file (concatenate 'string *output-dir* author "-err.msg"))))
	 (file-name (concatenate 'string *output-dir* author "-err.msg")))
    (with-open-file (stream file-name :direction :output :if-exists :append :if-does-not-exist :create)
      (if first-time? (generate-tester-header stream build))
      (format stream "~% An error was found in the component: ~A" comp)
      ;; Determine what message to send
      (cond ((equal case 'syntax-error)
	     (format stream "~% - This component is not well-formed."))
	    ((or (equal reason 'undefined-error) 
		 (equal reason 'implicit-declaration-error))
	     (format stream "~%    This is because the following term(s) were not defined:")
	     (format stream "~%     -- ~A" case)	
	     (if (equal reason 'undefined-error)
		 (progn
		   (format stream "~%    Possible Reasons:")
		   (format stream "~%    1) The term does not have a definition;")
		   (format stream "~%    2) The term may be defined but not-well formed.")) 
	       (format stream "~%    KM found a reference to this term, but the term was not explicitly defined anywhere in the KB.")))
	    (t 
	     (format stream "~% - This component failed on the following test case:")
	     (format stream "~%	~A" case)
	     (if reason (format stream "~%   -- This is due to the following:~%~A" reason))))
      
      ;; Determine if the author is known
      (if orphan
	  (format stream "~%You received this message because the original author could not be found.~%")))))
;; ========================================================================================

;;===============================================================
;; Test all the components specified in the components list
;; This function will return a list of the following form:
;; 
;;	( (<component> <failed-case> [<reason>]) ...)
;;
;; for each failed test case.
;;===============================================================
(defun test-all-components (components-list stream &optional (skip-core-concepts nil))
  (let ((failed-cases nil) result)
    (format stream "~%~%======================================================================~%")
    (format stream "                          TEST RESULT~%")
    (format stream "======================================================================~%")
    (dolist (comp components-list)
      (cond
	((or (not skip-core-concepts) (not (member comp *clib-core-components* :test #'equal)))
 	 (setf result (test-component comp stream))
	 (if result (setq failed-cases (cons result failed-cases)))
	 (restore-kb :unintern-symbols t)
	 )))
    failed-cases))


;;===============================================================
;; This function will test a single component logging the result
;; in a pre-specified file. The return values of this function
;; are:
;;	1) NIL, if the test was successful
;;	2) A list of the form 
;;
;;		(<component> <failed-case> [<reason>]), 
;;	
;;	   if the test fails. Note, the third value is optional
;;	   dependent upon whether or not there was a reason for
;;	   the failure.
;;===============================================================
(defun test-component (component stream)
  (let ((test-cases (km `(|the| |test-case| |of| ,(intern component :km)) :fail-mode 'fail))
	(all-errors nil) (all-warnings nil))
    (format t "TESTING: ~A~%" component)
    (format stream "~%~%Test Result for ~A~%~%" component)
    (if (null test-cases)
        (format stream "   WARNING: ~A.km component has no test cases.~%" component)
      (progn
	(km '(|new-situation|))
	(dolist (c (unquote (first test-cases)))
	  (multiple-value-bind (answer error-msg error-str warnings)
	      (km c :fail-mode 'fail)
	    (cond (error-str 
		   (format stream "   ERROR: Test case ~A failed for ~A.km!~%" c component)
		   (format stream "   Due to the following:~%~%~A~%" error-str)
		   (push (list component c error-msg) all-errors))
		  (warnings
		   (push (list component c warnings) all-warnings))
		  ((null answer)
		   (format stream "   ERROR: Test case ~A failed for ~A.km!~%" c component)
		   (push (list component c) all-errors)))))
	(if (null all-errors)
	    (format stream "   ~A.km passed~%" component)
	  (if all-warnings
	      (progn
		(format stream "   WARNINGS: The test cases of ~A.km generated ~a warnings:~%" component (length all-warnings))
		(pprint-list stream warnings))))
	all-errors))))


;;===============================================================
;; Functions originally provided by Pete Clark to compute all 
;; the values of all the slots on a component. The original 
;; code has been modified to suit our purposes. 
;;===============================================================
(defun test-all-classes (stream components &optional (skip-core-concepts nil))
  (let* ((all-classes 	   ;; remove classes in the excluded concept list
	  (remove-if 
	   #'(lambda (i)
	       (or (member i (mapcar #'cdr *excluded-concepts*) :test #'equal)
		   (not (member i (mapcar #'cdr *all-concepts*) :test #'equal))
		   (and skip-core-concepts (member (string i) *clib-core-components* :test #'equal))))
	   (km '(|the| |all-subclasses| |of| |Thing|))))
	 (agg-slot (km '(|the| |all-instances| |of| |Set-Aggregation-Slot|)))
         (all-slots (set-difference
		     (set-difference (km '(|the| |all-instances| |of| |Slot|)) 
				     '(|has-example| |is-example-of| |element| |element-type| |element-of| |element-type-of|) 
				     :test #'equal)
		     agg-slot :test #'equal)
		    )
         (expr-error-pairs (mapcan #'(lambda (class)
                                       (test-a-class class all-slots)
				        (restore-kb :unintern-symbols t))
                                   (sort all-classes #'string< :key #'symbol-name))))
    (format t "all subclasses of Thing is ~S~%" (km '(|the| |all-subclasses| |of| |Thing|)))
    (format t "all-classes are ~S~%" all-classes)
    ;;; Report errors:
    (km-format stream "~%~%======================================================================~%")
    (km-format stream "              ERROR REPORT~%")
    (km-format stream "======================================================================~%")
    (mapc #'(lambda (expr-error-pair)
              (let ( (comp 	(first  expr-error-pair))
		     (expr 	(second expr-error-pair))
                     (error-str (third  expr-error-pair)) )
                (km-format stream "~%ERROR doing ~a~%" expr)
                (km-format stream "~a~%" error-str)
                (terpri)))
          expr-error-pairs)
    (km-format stream "~%~a errors found.~%" (length expr-error-pairs))
    expr-error-pairs))

(defun test-a-class (class slots)
  (new-situation)
  (km-format t "~a...~%" class) ;; (km-format t "slots = ~S~%" slots)
  (let ((instance nil)
	(error-msg "")
	(required-slots (km `(|the| |required-slot| |of| ,class) :fail-mode 'fail)))
    (multiple-value-setq
     (instance error-msg)
     (km-unique `(|a| ,class)))
    (when error-msg
      (setf error-msg (list (list (string class) `(|a| ,class) error-msg))))
    (append error-msg
	    (remove nil
		    (mapcar #'(lambda (slot) ;;(format t " slot = ~S~%" slot)
					;(new-situation)
				(multiple-value-bind (answer error-str)
						     (km `(|the| ,slot |of| ,instance) :fail-mode 'fail)
						     (cond 
						      (error-str 
						       (list (string class) `(|the| ,slot |of| (|a| ,class)) error-str))
						      ((and (member slot required-slots :test #'eql) (null answer))
						       (list (string class) 
							     `(|the| ,slot |of| (|a| ,class)) 
							     "ERROR! No value found for a required slot.")))))
			  slots)))))

;;===============================================================
;; Set of functions used to check a file for:
;; - matching parentheses
;; - missing concepts
;; SIDE EFFECTS:
;; - updates *all-concepts*
;;           *excluded-concepts*  
;;===============================================================
(defun check-km-file (file-name)
  (let ((stream (open file-name :direction :input :if-does-not-exist nil))
        read-result curr-concepts)
    (if stream
	(loop (setf read-result (my-case-sensitive-read stream nil nil))
	  (cond ((eql read-result nil)  ;; End of File.
		 (close stream)
		 (if (not (member
			   (first (last (pathname-directory file-name)))
			   *EXCLUDED-DIRECTORIES*
			   :test #'equal))
		     (setf *all-concepts* (append curr-concepts *all-concepts*))
		   (setf *excluded-concepts* (append curr-concepts *excluded-concepts*)))
		 (return t))
		((eql read-result 'km-syntax-error)
		 (close stream)
		 (return nil))
		(t (setf curr-concepts (extract-concepts file-name read-result curr-concepts)))))
      (progn
        (format t "~%WARNING File: ~A does not exist!~%" file-name)
	nil))))

;; Same as case-sensitive read, except returns a value if there is an error.
(defun my-case-sensitive-read (&optional stream (eof-err-p t) eof-val rec-p)
					;(cond (*case-sensitivity*
  (let ( (old-readtable-case (readtable-case *readtable*)) )
    (handler-case
     (prog2
	 (setf (readtable-case *readtable*) :preserve)
	 (read stream eof-err-p eof-val rec-p)
       (setf (readtable-case *readtable*) old-readtable-case))
     (error (error)			; make sure readtable-case gets reset!
	    (setf (readtable-case *readtable*) old-readtable-case)
	    'km-syntax-error))))
					;(t (read stream eof-err-p eof-val rec-p))))

(defun extract-concepts (file frame curr-concepts)
  (let ((fframe (flatten-list frame)) 
	(result curr-concepts)
	(pos 0) 
	declared-instances)
    (setf pos (or (position (intern '|has|) fframe)
		  (position (intern '|now-has|) fframe)))
    (setf declared-instances (get-instances-list frame))
    ;;(format t "~% frame: ~a, instances: ~S" frame declared-instances)
    (if (and (numberp pos) (> pos 0))
        (setf *declared-concepts* (cons (intern (elt fframe (- pos 1)) 'km) *declared-concepts*)))
    (if declared-instances 
	(setf *declared-concepts* (append declared-instances *declared-concepts*)))
    
    (dolist (e fframe result)
	    (if (and (not (stringp e))
		     (not (numberp e))
		     (not (is-comment-p e))
		     (not (eql (elt (string e) 0) #\?));; <- This is to catch user defined variables.
		     (not (member e *reserved-km-keywords* :test #'eql))
		     (not (member e result :test #'(lambda (x y) (eql x (cdr y))))))
		(setf result (cons (cons (pathname-name file) e) result)))))
  ;;(format t "~% file: ~a OK" file)
  )


;; ==============================================================================================
;; This function is similar to the function scan-kb defined
;; in km.lisp
(defun check-undefined-frames (stream frames build clib-path)
  (let ((known-frames *declared-concepts*) ;;(get-all-concepts)) 
	(declared-frames (remove-duplicates *declared-concepts*))
	result
	(path-to-build-root (get-path-to-build-root clib-path)))

    (format t "~% === check-undefined-frames ===")
    (format t "~% declared: ~a" known-frames)
    (format t "~% path to build root: ~a" path-to-build-root)

    (format stream "~%~%======================================================================~%")
    (format stream "                          KB CHECK~%")
    (format stream "======================================================================~%")
				
    (dolist (frame frames result)
      ;; is the symbol accessible in the km package?
      (if (not (member (intern (get-concept frame) 'km) *declared-concepts* :test #'equal)) ;;km-symbol-name-equal))
	  (progn 
	    (format stream "~%WARNING: In file ~A, the concept ~A was not declared anywhere in the KB~%" 
		    (km-filepath (get-file-name frame) :build build :path-to-build-root path-to-build-root)
		    (string (get-concept frame)))
	    (setf result (cons (list (get-file-name frame) (string (get-concept frame)) 'undefined-error) result)))
	
	;; if so was it or its inverse declared?
	(if (and (not (anonymous-instancep (get-concept frame))) ;; this is added to exclude local variable definitions
		 (not (member (get-concept frame) *declared-concepts* :test #'km-symbol-name-equal))
		 (not (member (get-inverse (get-concept frame)) *declared-concepts* :test #'km-symbol-name-equal)))
	    (progn
	      (format stream "~% --- Concept: ~a, inverse ~a" (get-concept frame) (get-inverse (get-concept frame)))
	      (format stream "~%WARNING: In file ~A, the concept ~A was not explicitly declared anywhere in the KB~%"
		      (km-filepath (get-file-name frame) :build build :path-to-build-root clib-path) 
		      (string (get-concept frame)))
	      (setf result (cons (list (get-file-name frame) (string (get-concept frame)) 'implicit-declaration-error) result))))))))

(defun get-path-to-build-root (clib-path)
  (let* ((path-list (pathname-directory (excl::probe-directory clib-path))))
    (if (string= (car (last path-list)) "components") 
	(make-pathname :directory (butlast path-list))
      clib-path)))

(defun km-symbol-name-equal (s1 s2)
  (if (not (or (listp s1) (listp s2)))
      (string= (string s1) (string s2))))
 
(defun to-string-case-sensitive (symbol)
  (format nil "~S" symbol))

;; ===============================================================
;; extracts declared instances in expr (a list of km expressions)
;;
(defun get-instances-list (expr)
  (cond
   ((not (consp expr)) nil)
   ;; catches  (x has (instances (...)))
   ((and (consp (car expr)) 
	 (not (consp (caar expr))) 
	 (km-symbol-name-equal (caar expr) '|instances|))
    (cadar expr))
   ;; catches (x has (instance-of (...)))
   ((and (not (consp (car expr))) 
	 (consp (third expr)) 
	 (km-symbol-name-equal (car (third expr)) '|instance-of|))
    (list (car expr)))
   (t 
    (get-instances-list (cdr expr)))))	

;; =================================================================


(defun flatten-list (x)
  (cond
   ((null x) nil)
   ((atom x) (list x))
   ((and (consp x) (or (eql (car x) 'quote) (eql (car x) 'function))) nil)
   (t (append (flatten-list (car x)) (flatten-list (cdr x))))))

(defun get-concept-name (x) (string (get-concept x)))
(defun get-concept   (x) (cdr x))
(defun get-file-name (x) (car x))
(defun get-inverse   (x) (car (km `(|the| |inverse| |of| ,(intern x 'km)) :fail-mode 'fail)))
(defun is-comment-p (x)
  (or (equal (string x) "comment")
      (eq (position #\[ (string x)) 0)))

;;===============================================================
;; Some miscellaneous auxiliary functions
;;===============================================================
(defun table-lookup-path (component)
  (or (first (member component *file-pathes* 
		     :test #'(lambda (x y) (equal x (pathname-name y)))))
      *DEFAULT-RECEIVER*))

(defun find-duplicates (vals)
  (let (result duplicates)
    (dolist (val vals duplicates)
	    (if (member val result :test #'equal)
		(setf duplicates (cons val duplicates))
	      (setf result 	   (cons val result))))))


;; ===========================================================================
;; check whether component is well formed and if so, loads it
;; if not, reports error to *standard-error*
;;         and sends an email to the author of the last modification to that file
;; also, adds core components to *clib-core-components*
(defun check-and-load-component (component build)
  (let ((well-formed (check-km-file component)))
    (if well-formed ;; Component is well-formed. Load it up!
	(progn
	  (load-kb component)
	  (if (member "core" (pathname-directory component) :test #'string=)
	      (push component *clib-core-components*))
	  (push component *file-pathes*))
      
      ;; Component has a syntax error, inform the author of the most recent change
      (let ((author (look-up-author component)))
	;; Remember that there was a syntax error!
	(setf *all-well-formed* nil)
	(format *standard-error* "ERROR: Component: ~A, not well-formed!~%" (pathname-name component))
	(if *send-mail-on-error*
	    (progn 
	      (update-file-for-author author (list (pathname-name component) build 'syntax-error))
	      (format stream "~%Informed author: ~A, of error!~%" author)))))))

;; ==========================================================================================

(defun remove-reserved-km-keywords (components)
  (remove-if  #'(lambda (i)
		  (or (member (intern (cdr i) 'km) (mapcar #'first *user-defined-infix-operators*) :test #'equal)
		      ;; dgt - hack 6/22/05 - remove km keywords from all-concepts
		      (member (intern (cdr i) 'km) *reserved-km-keywords* :test #'equal)))
	      components))


;;(trace tester test-all-classes test-all-components)


(defun pprint-list (stream list)
  (dotimes (i (length list))
    (format stream "~% [~a/~a] ~a" i (length list) (nth i list))))

(defmacro generate-tester-header (stream build)
  `(,(format stream "~%~%======================================================================~%")
    ,(format stream "~% CLIB TEST RESULTS FOR BUILD: ~a" build)
    ,(format stream "~% Date: ~a" (print-local-time))
    ,(format stream "~%~%======================================================================~%")))
      