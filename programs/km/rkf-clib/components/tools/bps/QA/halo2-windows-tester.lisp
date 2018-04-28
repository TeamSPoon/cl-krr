;;
;; $Id: halo2-windows-tester.lisp,v 1.11 2008/03/25 20:18:31 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *timeout*                                          600)
(defparameter *work-count*                                       0)
(defparameter *HALO2-TESTER-UNCACHED-CONCEPTS*                   nil)

(defun generate-formulation-set(&optional (target-q-set *question-set*))
    (let ((q-set target-q-set))
    (dotimes (idx              (length q-set) 0)
      (let*  ((entry           (nth idx q-set))
	      (question-id     (nth 0 entry))
	      (question-orig   (nth 1 entry))
	      (question        (nth 2 entry))
	      (answer-snippets (nth 3 entry))
	      (comments        (nth 4 entry))
	      (q-number        (format nil "Q-~a" idx)))
	(save-question-file q-number question-id question-orig question answer-snippets comments)))))

(defun process-boeing-question-set-physics()
  (progn
    (process-boeing-question-set *ie-physics-question-set*)
    (load "question-set.lisp")
    (generate-formulation-set)
    (reload-tester-set)))

(defun process-boeing-question-set-biology()
  (progn
    (process-boeing-question-set *ie-biology-question-set*)
    (load "question-set.lisp")
    (generate-formulation-set)
    (reload-tester-set)))

(defun process-boeing-question-set-chemistry()
  (progn
    (process-boeing-question-set *ie-chemistry-question-set*)
    (load "question-set.lisp")
    (generate-formulation-set)
    (reload-tester-set)))

(defun check-answer-snippets(input)
  (dotimes (idx (length input))
    (progn
      (if (not (listp (nth 4 (nth idx input))))
	  (format t "bad answer-snippet: ~a~%~a~%" idx (nth 0 (nth idx input)))))))

(defun process-boeing-question-set(&optional (input *boeing-question-set*))
    (with-open-file (stream "question-set.lisp"
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "(unless (find-package :km) (make-package :km))~%")
      (format stream "(in-package :km)~%")
      (format stream "(defparameter *question-set* '(")
      (mapcar #'(lambda(elem)
		  (let ((question-id     (nth 0 elem))
			(question-orig   (nth 2 elem))
			(question        (nth 3 elem))
			(answer-snippets (nth 4 elem))
			(comments        (nth 5 elem)))
		    (if (not (null question))
			(format stream "(~s ~s ~s ~s ~s)~%~%" 
				       question-id
				       question-orig
				       question 
				       answer-snippets 
				       comments))))
	      input)
      (format stream "))~%")))

(defun save-triple-list(triple-list)
  (let ((s (make-string-output-stream)))
    (dolist (triple triple-list)
      (format s "~s~%" triple))
    (get-output-stream-string s)))

(defun save-cpl-scenario(q-number cpl-scenario &optional (prefix-tag "cpl"))
  (let ((s (make-string-output-stream)))
    (format s "(setq *~a-~a-scenario* '(~%" q-number prefix-tag)
    (format s "~a" (save-triple-list cpl-scenario))
    (format s "))~%")
    (get-output-stream-string s)))
  
(defun save-cpl-question(q-number cpl-question &optional (prefix-tag "cpl"))
  (let ((s (make-string-output-stream)))
    (format s "(setq *~a-~a-questions* '~s)~%" q-number prefix-tag cpl-question)
    (get-output-stream-string s)))

(defun save-cpl-yes-no-question(q-number cpl-yes-no-question &optional (prefix-tag "cpl"))
  (let ((s (make-string-output-stream)))
    (format s "(setq *~a-~a-yn-questions* '~s)~%" q-number prefix-tag cpl-yes-no-question)
    (get-output-stream-string s)))

;;Saves question and tag.
(defun save-question-file(q-number 
			  question-id 
			  question-orig 
			  question 
			  answer-snippets 
			  comments)
  (let ((question-file (format nil "formulation-~a.lisp" q-number)))
  (with-open-file (stream question-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "(unless (find-package :km) (make-package :km))~%")
    (format stream "(in-package :km)~%")
    (format stream "(setq *using-km-package* t)~%")
    (format stream "(setq *~a-question-id* ~s)~%" q-number question-id)
    (format stream "(setq *~a-question-orig* ~s)~%" q-number question-orig)
    (format stream "(setq *~a-question* ~s)~%" q-number question)
    (format stream "(setq *~a-comments* ~s)~%" q-number comments)
    ;(format stream "(setq *~a-USE-MATCHER* ~a)~%" q-number *USE-MATCHER*)
    ;(format stream "(setq *~a-USE-LOOSESPEAK* ~a)~%" q-number *USE-LOOSESPEAK*)
    (format stream "(setq *~a-answer-snippets* \'~s)~%" q-number answer-snippets)
    (format stream "(setq *~a-tag* ~s)~%" q-number "unknown") ;;FIXME: Backward compatibility. To be removed.
    (format stream "(if (boundp '*q-formulation-set*) (setq *q-formulation-set* (cons '~a *q-formulation-set*)) (setq *q-formulation-set* (list '~a)))~%" q-number q-number))))

(defun load-tester-set(&optional (target-directory "."))
 (reload-tester-set target-directory))

(defun reload-tester-set(&optional (target-directory "."))
  (progn
    (load-tester-set-failsafe target-directory)
    (setq *q-formulation-set* (remove-duplicates *q-formulation-set*))
    (setq *q-cpl-set*         (remove-duplicates *q-cpl-set*))
    (setq *q-bps-set*         (remove-duplicates *q-bps-set*))))

(defun reset-tester-set()
  (progn 
    (setq *q-formulation-set*        nil)
    (setq *q-cpl-set*                nil)
    (setq *q-bps-set*                nil)))

;;Loads all formulation, cpl and bps processed files.
(defun load-tester-set-failsafe (&optional (target-directory "."))
  (progn
    (reset-tester-set)
    (dolist (element (get-list-of-question-related-files target-directory))
      (load element))))

;;Returns list of formulation, cpl, and bps processing files for tester set.
(defun get-list-of-question-related-files (&optional (dir-path "."))
  (let ((dir-list (directory dir-path)) result)
    (dolist (element dir-list result)
      (let ((type (pathname-type element))
	    (name (pathname-name element)))
	(cond ((and 
		(string-equal type
			      "lisp")
		(not (null (search "formulation-" name)))
		(zerop (search "formulation-" name)))
	       (setf result (cons element result)))
	      ((and 
		(string-equal type
			      "lisp")
		(not (null (search "cpl-" name)))
		(zerop (search "cpl-" name)))
	       (setf result (cons element result)))
	      ((and 
		(string-equal type
			      "lisp")
		(not (null (search "bps-" name)))
		(zerop (search "bps-" name)))
	       (setf result (cons element result))))))))

(defun total-formulation-count(&optional (target-directory "."))
  (progn 
    (load-tester-set target-directory)
    (length (remove-duplicates (get-formulation-set target-directory)))))

(defun get-formulation-set(&optional (target-directory "."))
   (sort-question-number-set *q-formulation-set*))

(defun sort-question-number-set(input)
  (remove nil
	  (sort (copy-list input) #'(lambda(x y) 
			  (let ((x-num (extract-question-number x))
				(y-num (extract-question-number y)))
			    (and (numberp x-num)
				 (numberp y-num)
				 (< x-num y-num)))))))

;;Extracts number value from 'Q-1234
(defun extract-question-number(input)
  (let* ((target-string (string-upcase (stringify input)))
	(pos           (search "Q-" target-string)))
    (and (numberp pos)
	 (string-to-number (subseq target-string (+ pos 2) (length target-string))))))

(defun missing-cpl-set(&optional (directory ".") 
				 (prefix-tag "cpl"))
  (let ((result '()))
    (load (format nil "~a/question-set.lisp" directory))
    (dotimes (x (total-formulation-count))
      (if (not (probe-file (format nil "cpl-Q-~a.lisp" x)))
	  (setq result (cons (intern (format nil "Q-~a" x) :km) result))))
    result))

(defun ps-randomly-pick(list)  
  (if (and (listp list)
	   (not (null list)))
      (nth (ps-random (1- (length list))) list)))

(defun ps-random(x &optional(verbose t))
  (if (zerop x) x
    (handler-case (let ((result (ps-random0 x verbose)))
		    (if (null result)
			(random x)
		      result))
		  (error (condition)
			 (random x)))))

(defun ps-random0(x &optional(verbose t))
  (let ((query (format nil "GET /integers/?num=1&min=0&max=~a&col=1&base=10&format=plain&rnd=new" x))
	(s (make-string-output-stream))
	(socket (acl-socket:make-socket :remote-host "www.random.org" :remote-port 80)))
    (if verbose (format t "rnd query: ~a~%" query))
    (format socket "~a~%" query)
    (force-output socket)
    (LOOP :for line = (READ-LINE socket nil nil)
	  :while line
	  :do (format s "~a" line))
    (close socket)
    (parse-integer (get-output-stream-string s))))

;;Do CPL processing for all un processed problem formulations.
(defun generate-cpl-set(&optional(directory ".")
				 (prefix-tag "cpl"))
(let ((target-set (missing-cpl-set directory prefix-tag)))
	(cond ((null target-set) ())
	      (t (let ((target (ps-randomly-pick target-set)))
		   (format t "Halo Tester: CPL Processing ~a~%" target)
		   (handler-case (do-cpl-processing target prefix-tag)
                             (error (condition)
 		                        (do-cpl-processing-failure-processing target "LISP Error")))
		   (increment-work-count)
		   (generate-cpl-set directory prefix-tag))))))

(defun ps-test-no-error(expr &optional(timeout *timeout*)
			              (verbose nil))
  (progn 
    (if verbose (format t "Evaluating: ~a (~a)~%" expr timeout))
    (sys:with-timeout (timeout
		       (values nil 
			       (format nil "Timeout'ed after ~a seconds" timeout)))
		      (handler-case (values t (eval expr))
				    (error (condition)
					   (values nil (format nil "~S" condition)))))))

(defun do-processing(target  &optional(prefix-tag "cpl")
					 (timeout *timeout*))
  (progn 
    (do-cpl-processing target prefix-tag timeout)
    (do-bps-processing target prefix-tag timeout)))
  
(defun do-cpl-processing(target &optional(prefix-tag "cpl")
					 (timeout *timeout*))
  (multiple-value-bind
    (successful return-val)
    (ps-test-no-error `(do-cpl-processing0 (quote ,target) ,prefix-tag))
    (if (not successful) 
	(do-cpl-processing-failure-processing target return-val))
    (reload-tester-set)))

;;Do CPL processing for specific problem formulation
(defun do-cpl-processing0(q-number &optional (prefix-tag "cpl"))
  (let ((target-file (format nil "~a-~a.lisp" prefix-tag q-number))
  	  (target-question (intern (format nil "*~a-QUESTION*" q-number) :km))
	   start-time end-time)
    (with-open-file (stream target-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (let ((km-state   (get-kb))
	    (start-time (get-universal-time)))
	(ls-clear-cache)
      (multiple-value-bind
	(scenario compute-questions yes-no-questions)
	(ps-parse-question  (eval target-question))
	(setq end-time (get-universal-time))
	(put-kb km-state)
	(format stream "(unless (find-package :km) (make-package :km))~%")
	(format stream "(in-package :km)~%")
	(format stream "(setq *using-km-package* t)~%")
	(format stream "~a" (save-cpl-scenario        q-number scenario          prefix-tag))
	(format stream "~a" (save-cpl-question        q-number compute-questions prefix-tag))
	(format stream "~a" (save-cpl-yes-no-question q-number yes-no-questions  prefix-tag))
	(format stream "(setq *~a-cpl-time* ~a)" q-number (- end-time start-time))
	(format stream "(setq *~a-cpl-version* '~s)~%" q-number *cpl-version*)
	(format stream "(setq *~a-cpl-clib-version* '~s)~%" q-number (car (ps-km-query '(|the| |description| |of| |Version|))))
	(format stream "(setq *~a-cpl-timestamp* '~a)~%"         q-number (get-universal-time))
	(format stream "(setq *~a-cpl-hostname* ~s)~%"         q-number (get-hostname))
	(format stream "(if (boundp '*q-~a-set*) (setq *q-~a-set* (cons '~a *q-~a-set*)) (setq *q-~a-set* (list '~a)))~%" 
		prefix-tag
		prefix-tag
		q-number 
		prefix-tag
		prefix-tag
		q-number))))))

(defun do-cpl-processing-failure-processing(q-number failure-reason)
  (let ((target-file (format nil "cpl-~a.lisp" q-number))
	(target-question (intern (format nil "*~a-QUESTION*" q-number) :km)))
    (with-open-file (stream target-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (let* ((explanation `(,failure-reason)))
      (format stream "(unless (find-package :km) (make-package :km))~%")
      (format stream "(in-package :km)~%")
      (format stream "(setq *using-km-package* t)~%")
      (format stream "(setq *~a-cpl-scenario*     '())~%" q-number)
      (format stream "(setq *~a-cpl-questions*    '())~%" q-number)
      (format stream "(setq *~a-cpl-yn-questions* '())~%" q-number)
      (format stream "(setq *~a-cpl-ERROR-TAG* '~s)~%" q-number explanation)
      (format stream "(setq *~a-cpl-version* '~s)~%" q-number *cpl-version*)
      (format stream "(setq *~a-cpl-clib-version* '~s)~%" q-number (car (ps-km-query '(|the| |description| |of| |Version|))))
      (format stream "(setq *~a-cpl-timestamp* '~a)~%"         q-number (get-universal-time))
      (format stream "(setq *~a-cpl-hostname* ~s)~%"         q-number (get-hostname))
      (format stream "(if (boundp '*q-cpl-set*) (setq *q-cpl-set* (cons '~a *q-cpl-set*)) (setq *q-cpl-set* (list '~a)))~%" q-number q-number)))))

(defun dump-bps-tree (&optional(verbose nil))
  (let ((s (make-string-output-stream)))
    (dolist (vp-inst (get-ordered-viewpoint-list))
      (if verbose (format t "Saving definition for ~A~%" vp-inst))
      ;(showme vp-inst (all-situations) (all-theories) s t)
      )
    (get-output-stream-string s)))

(defun get-hostname()
  (car (excl.osi:command-output "hostname")))

(defun increment-work-count()
  (setq *work-count* (1+ *work-count*)))

(defun missing-bps-set(&optional (directory "."))
  (let ((result '()))
    (load (format nil "~a/question-set.lisp" directory))
    (dotimes (x (total-formulation-count))
      (if (not (probe-file (format nil "bps-Q-~a.lisp" x)))
	  (setq result (cons (intern (format nil "Q-~a" x) :km) result))))
    result))

(defun ps-punt(q-number)
  (do-bps-processing-failure-processing 
   q-number 
   (format nil "Punt!")))

(defun do-bps-processing-failure-processing(q-number failure-reason)
  (let ((target-file (format nil "bps-~a.lisp" q-number))
	(target-question (intern (format nil "*~a-QUESTION*" q-number) :km))
	(target-cpl-scenario     (intern (format nil "*~a-CPL-SCENARIO*"     q-number) :km))
	(target-cpl-questions    (intern (format nil "*~a-CPL-QUESTIONS*"    q-number) :km))
	(target-cpl-yn-questions (intern (format nil "*~a-CPL-YN-QUESTIONS*" q-number) :km)))
    (with-open-file (stream target-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (let* ((explanation `(,failure-reason)))
      (format stream "(unless (find-package :km) (make-package :km))~%")
      (format stream "(in-package :km)~%")
      (format stream "(setq *using-km-package* t)~%")
      (format stream "(setq *~a-bps-LISP-ERROR-TAG* '~s)~%"    q-number explanation)
      (format stream "(setq *~a-bps-closedlist* '~a)~%"        q-number *CLOSEDLIST*)
      (format stream "(setq *~a-bps-ignoredlist* '~a)~%"       q-number *IGNOREDLIST*)
      (format stream "(setq *~a-bps-openlist* '~a)~%"          q-number *OPENLIST*)
      (format stream "(setq *~a-bps-uncached-concepts* '~a)~%" q-number *HALO2-TESTER-UNCACHED-CONCEPTS*)
      (format stream "(setq *~a-bps-timestamp* '~a)~%"         q-number (get-universal-time))
      (format stream "(setq *~a-bps-hostname* ~s)~%"           q-number (get-hostname))
      (format stream "(setq *~a-bps-clib-version* '~s)~%"      q-number (car (ps-km-query '(|the| |description| |of| |Version|))))
      (format stream "(if (boundp '*q-bps-set*) (setq *q-bps-set* (cons '~a *q-bps-set*)) (setq *q-bps-set* (list '~a)))~%" q-number q-number)))))

(defun do-bps-processing(target &optional(timeout *timeout*))
  (let ((backup-kb (get-kb)))
    (multiple-value-bind
	(successful return-val)
	(ps-test-no-error `(do-bps-processing0 (quote ,target)) timeout)
      (if (not successful)
	  (let ((*CONTROLLER-DRYRUN* t))
	    (ps-punt target)
	    (ps-test-no-error `(do-bps-processing0 (quote ,target)))
	    (do-bps-processing-failure-processing target return-val))))
    (reload-tester-set)
    (put-kb backup-kb)))

;;Perform problem-solving for specific cpl formulation
(defun do-bps-processing0(q-number)
  (let ((target-file (format nil "bps-~a.lisp" q-number))
	(target-bps-graph-directory (format nil "graph-~a" q-number))
	(target-question (intern (format nil "*~a-QUESTION*" q-number) :km))
	(target-cpl-scenario     (intern (format nil "*~a-CPL-SCENARIO*"     q-number) :km))
	(target-cpl-questions    (intern (format nil "*~a-CPL-QUESTIONS*"    q-number) :km))
	(target-cpl-yn-questions (intern (format nil "*~a-CPL-YN-QUESTIONS*" q-number) :km))
	bps-start-time bps-end-time
	expl-start-time expl-end-time)
    (with-open-file (stream target-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (ps-assert-triples (eval target-cpl-scenario) t)
      (let ((km-state   (get-kb))
	    (bps-start-time (get-universal-time)))
      (let* ((vp-inst (basic-problem-solver (eval target-cpl-scenario)
					    (eval target-cpl-questions)
					    (eval target-cpl-yn-questions))))
	(setq bps-end-time (get-universal-time))
	(setq expl-start-time (get-universal-time))
	(let* ((*EQ-FLAG*                           t))
	  (multiple-value-bind
	      (answer explanation expl-time expl-error-str)
	      (ps-get-explanation vp-inst)
	  (let ((answer (km `(|the| |viewpoint-answer| |of| ,vp-inst)))
		(bps-vp-tree-defn (dump-bps-tree)))
	    (format stream "(unless (find-package :km) (make-package :km))~%")
	    (format stream "(in-package :km)~%")
	    (format stream "(setq *using-km-package* t)~%")
	    (format stream "(setq *~a-bps-viewpoint-answer* '~s)~%" q-number answer)
	    (format stream "(setq *~a-bps-answer* '~s)~%" q-number explanation)
	    (format stream "(setq *~a-bps-dryrun* ~a)~%" q-number *CONTROLLER-DRYRUN*)
	    (format stream "(setq *~a-bps-closedlist-vp-tree-defn* '~s)~%" q-number bps-vp-tree-defn)
	    (format stream "(setq *~a-bps-root-vp* '~a)~%"                 q-number (get-root-viewpoint))
	    (format stream "(setq *~a-bps-answer-vp* '~a)~%"               q-number vp-inst)
	    (format stream "(setq *~a-bps-closedlist* '~a)~%"              q-number *CLOSEDLIST*)
	    (format stream "(setq *~a-bps-ignoredlist* '~a)~%"             q-number *IGNOREDLIST*)
	    (format stream "(setq *~a-bps-openlist* '~a)~%"                q-number *OPENLIST*)
	    (format stream "(setq *~a-bps-time* ~a)" q-number (- bps-end-time bps-start-time))
	    (format stream "(setq *~a-expl-time* ~a)" q-number expl-time)
	    (format stream "(setq *~a-bps-clib-version* '~s)~%" q-number (car (ps-km-query '(|the| |description| |of| |Version|))))
	    (format stream "(setq *~a-bps-timestamp* '~a)~%"         q-number (get-universal-time))
	    (format stream "(setq *~a-bps-hostname* ~s)~%"         q-number (get-hostname))
	    (format stream "(setq *~a-bps-uncached-concepts* '~a)~%" q-number *HALO2-TESTER-UNCACHED-CONCEPTS*)
	    (format stream "(if (boundp '*q-bps-set*) (setq *q-bps-set* (cons '~a *q-bps-set*)) (setq *q-bps-set* (list '~a)))~%" q-number q-number))))))
    )))

(defun do-bps-processing-for-q-number-list(input)
(dolist (x input)
(do-bps-processing x)))

;;changed
(defun gen-temp-filename(&optional(dir "."))
  (let ((target (format nil "~ahalo2-tester-~a-~a.tmp" dir (get-hostname) (gensym))))
    (if (probe-file target)
	(gen-temp-filename)
        target)))

(defun generate-performance-csv(filename)
  (let ((tmp-filename (gen-temp-filename)))
    (with-open-file (stream tmp-filename
			    :direction :output
				       :if-exists :supersede
						  :if-does-not-exist :create)
      (dolist (q-number (get-formulation-set))
	(let ((idx (extract-question-number q-number)))
	  (multiple-value-bind 
	    (cpl-time bps-time expl-time total-time)
	    (get-performance-for-q-number q-number)
	    (if (and (numberp cpl-time) 
		     (numberp bps-time)
		     (numberp expl-time)
		     (numberp total-time))
		(format stream "~a;~a;~a;~a;~a~%" q-number total-time cpl-time bps-time expl-time))))))
    (tester-rename-file tmp-filename filename)
    ))

(defun get-performance-for-q-number(q-number)
  (let ((cpl-time-symbol  (intern (string-upcase (format nil "*~a-CPL-TIME*"  q-number)) :km))
	(bps-time-symbol  (intern (string-upcase (format nil "*~a-BPS-TIME*"  q-number)) :km))
	(expl-time-symbol (intern (string-upcase (format nil "*~a-EXPL-TIME*" q-number)) :km)))
	  (if (and (boundp cpl-time-symbol)
		   (boundp bps-time-symbol)
		   (boundp expl-time-symbol))
	      (let ((cpl-time  (eval cpl-time-symbol))
		    (bps-time  (eval bps-time-symbol))
		    (expl-time (eval expl-time-symbol)))
		(if (and (numberp cpl-time)
			 (numberp bps-time)
			 (numberp expl-time))
		    (values cpl-time bps-time expl-time 
			    (+ cpl-time bps-time expl-time)))))))

;;changed
(defun tester-rename-file (x y &optional(verbose t))
  (rename-file (merge-pathnames x) (merge-pathnames y)))

(defun current-bps-set(&optional (directory ".")
				 (prefix-tag "bps"))
  (let ((result (current-bps-set0 directory prefix-tag)))
    #|
    (if (> (length result)
	   (total-formulation-count directory))
    (zap-all-processing))
    |#
        result))

(defun current-bps-set0(&optional (directory ".")
				 (prefix-tag "bps"))
  (mapcar #'(lambda(entry)
	      (let ((name (pathname-name entry)))
		(intern (subseq name (length "bps-") (length name)) :km)))
	  (directory (format nil "~a/bps-Q*.lisp" directory))))

(defun current-cpl-set(&optional (directory ".")
				 (prefix-tag "cpl"))
  (let ((result (current-cpl-set0 directory prefix-tag)))
    #|
    (if (> (length result)
	   (total-formulation-count directory))
	(zap-all-processing))
    |#
        result))

(defun current-cpl-set0(&optional (directory ".")
				 (prefix-tag "cpl"))
  (mapcar #'(lambda(entry)
	      (let ((name (pathname-name entry)))
		(intern (subseq name (length "cpl-") (length name)) :km)))
	  (directory (format nil "~a/cpl-Q*.lisp" directory))))

(defun is-answer-pseudo-gradable-p(q-number &optional(target "."))
  (let ((answer-snippet  (reintern-symbol-for-package
			  (string-upcase (format nil "*~a-answer-snippets*"   q-number)) 
			  target))
	(q-number-answer-symbol (reintern-symbol-for-package
				 (string-upcase (format nil "*~a-bps-answer*" q-number)) 
				 target)))
    (and (boundp answer-snippet)
	 (boundp q-number-answer-symbol)
	 (not (null (prune-answer-snippets (eval answer-snippet))))
	 (listp (eval answer-snippet)))))

(defun prune-answer-snippets (answer-snippet)
  (cond  ((null answer-snippet) nil)
	 ((listp (car answer-snippet)) 
	  (mapcar #'(lambda (x) (remove-if #'(lambda (y) (string= y "")) x)) answer-snippet))
	 (t (remove-if #'(lambda (x) (string= x "")) answer-snippet))))

(defun grade(q-number &optional(target "."))
  (cond ((not (is-answer-pseudo-gradable-p q-number target)) "unknown")
	((pseudo-grade q-number target) "correct")
	(t "incorrect")))

(defun pseudo-grade-disjunctive (answer-snippet-list q-number-answer-string)
 (if (listp (car answer-snippet-list))
     ;; disjunctive answer-snippet
     (eval
      (cons 'or
            (mapcar #'(lambda (answer-snippets)
                        (eval (cons 'and
                                    (mapcar #'(lambda(snippet)
                                                (numberp (search (string-upcase snippet)
                                                                 (string-upcase q-number-answer-string))))
                                            answer-snippets))))
                    answer-snippet-list)))
   (eval (cons 'and
               (mapcar #'(lambda(snippet)
                           (numberp (search (string-upcase snippet)
                                            (string-upcase q-number-answer-string))))
                       answer-snippet-list)))))

(defun pseudo-grade(q-number &optional(target "."))
 (if (is-answer-pseudo-gradable-p q-number target)
     (let* ((answer-snippets (deref-symbol
			       (reintern-symbol-for-package
				(string-upcase (format nil
						       "*~a-answer-snippets*" q-number))
				target)))
            (q-number-answer  (deref-symbol
				(reintern-symbol-for-package
				 (string-upcase (format nil
							"*~a-bps-answer*" q-number))
				 target)))
            (q-number-answer-string (format nil "~a" q-number-answer)))
       (if (pseudo-grade-disjunctive answer-snippets q-number-answer-string)
           q-number))))

(defun reintern-symbol-for-package(sym-str &optional(target "."))
  (if (or (null target) (equal target "."))
      (intern (string-upcase sym-str) :km)
      (intern (format nil "~a::~a" target (string-upcase sym-str)) :km)))

(defun deref-symbol(sym)
  (if (boundp sym)
      (eval sym)))

;;Returns question numbers where CPL processing ended with LISP errors.
(defun find-cpl-lisp-errors(&optional (target ".")) ;;FIXME, how about timeout errors in CPL?
  (let ((tmp (reintern-symbol-for-package "*cpl-lisp-errors*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
			       (mapcar #'(lambda (q-number)
					   (let ((sym (reintern-symbol-for-package
						       (string-upcase (format nil "*~a-cpl-lisp-error*" q-number))
						       target)))
					     (if (deref-symbol sym)
						 q-number)))
				       (get-formulation-set target))))))
      (deref-symbol tmp))))

;;Returns question numbers where BPS processing ended with LISP errors.
(defun find-bps-lisp-errors(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*bps-lisp-errors*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
	  (mapcar #'(lambda (q-number)
		      (let ((sym (reintern-symbol-for-package
				  (string-upcase (format nil "*~a-bps-lisp-error-tag*" q-number))
				  target)))
			(if (and (deref-symbol sym)
				 (numberp (search "#<" (car (deref-symbol sym)))))
			    q-number)))
		  (identify-good-cpl-interpretations target))))))
      (deref-symbol tmp))))

;;Returns question numbers where BPS processing ended with LISP errors.
(defun find-bps-timeout-errors(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*bps-timeout-errors*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
			      (mapcar #'(lambda (q-number)
					  (let ((target-answer 
						 (reintern-symbol-for-package
						  (string-upcase (format nil "*~a-bps-answer*" q-number))
						  target))
						(target-lisp-error 
						 (reintern-symbol-for-package
						  (string-upcase (format nil "*~a-bps-lisp-error-tag*" q-number))
						  target)))
					    (if (or (and (boundp target-lisp-error)
							 (not (null (search "Timeout" (car (eval target-lisp-error))))))
						    (and (boundp target-lisp-error)
							 (not (null (search "Punt"    (car (eval target-lisp-error)))))))
						q-number)))
				      (identify-good-cpl-interpretations target))))))
      (deref-symbol tmp))))

(defun identify-plausible-bps-answers(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*plausible-bps-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(set-difference (identify-bps-processed-q-numbers target)
				      (union (identify-broken-bps-answers target)
					     (identify-broken-cpl-interpretations target))))))
      (deref-symbol tmp))))

(defun identify-broken-bps-answers(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*broken-bps-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote
		      ,(remove nil
			       (mapcar #'(lambda (q-number)
					   (if (is-bps-answer-broken-p q-number target)
					       q-number))
				       (set-difference (current-bps-set target)
						       (identify-broken-cpl-interpretations target)))))))
      (deref-symbol tmp))))

(defun identify-broken-bps-answers-having-nil-answers(&optional(target "."))
  (let ((tmp (reintern-symbol-for-package "*broken-bps-answers-having-nil-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 		      
		      ,(remove nil
			       (mapcar #'(lambda (q-number)
					   (if (is-bps-answer-nil-p q-number target)
					       q-number))
				       (set-difference (get-formulation-set target)
						       (identify-broken-cpl-interpretations target)))))))
      (deref-symbol tmp))))

(defun is-bps-answer-nil-p(q-number &optional(target "."))
  (let ((answer-target (reintern-symbol-for-package
			(format nil "*~a-bps-viewpoint-answer*" q-number)
			target)))
    (and (boundp answer-target)
	 (null (eval answer-target))
	 (null (member q-number (find-bps-lisp-errors target)))
	 (null (member q-number (find-bps-timeout-errors target))))))

(defun is-bps-answer-broken-p(q-number &optional (target "."))
  (let ((answer-target (reintern-symbol-for-package
			(format nil "*~a-bps-viewpoint-answer*" q-number)
		       target))
	(expl-target   (reintern-symbol-for-package
			(format nil "*~a-bps-answer*" q-number)
			target)))
    (cond ((not (boundp answer-target)) t)   
	  (t (or 
	      (is-cpl-interpretation-broken-p q-number target)
	      (null (eval answer-target))
	      (not  (null (member q-number (find-bps-lisp-errors target))))
	      (not  (null (member q-number (find-bps-timeout-errors target)))))))))

(defun identify-good-cpl-interpretations(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*good-cpl-interpretations*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 		      
		      ,(set-difference (identify-cpl-processed-q-numbers target)
				       (identify-broken-cpl-interpretations target)))))
      (deref-symbol tmp))))

(defun identify-bps-processed-q-numbers(&optional (target "."))
  (intersection (get-bps-set target)
		(get-formulation-set target)))

(defun identify-cpl-processed-q-numbers(&optional (target "."))
  (intersection (get-cpl-set target)
		(get-formulation-set target)))

(defun identify-broken-cpl-interpretations(&optional(target "."))
  (let ((tmp (reintern-symbol-for-package "*broken-cpl-interpretations*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
			       (mapcar #'(lambda (q-number)
					   (if (is-cpl-interpretation-broken-p q-number target)
					       q-number))
				       (get-formulation-set target))))))
      (deref-symbol tmp))))

(defun identify-broken-cpl-interpretations-with-empty-query(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*broken-cpl-interpretations-with-empty-query*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
			      (mapcar #'(lambda (q-number)
					  (if (does-cpl-interpretation-have-empty-query-p q-number target)
					      q-number))
				      (set-difference (identify-broken-cpl-interpretations target)
						      (identify-broken-cpl-interpretations-with-empty-scenarios target)))))))
      (deref-symbol tmp))))

(defun does-cpl-interpretation-have-empty-query-p(q-number &optional (target "."))
  (let ((questions-target (reintern-symbol-for-package
			   (string-upcase (format nil "*~a-cpl-questions*"    q-number))
			   target))
	(yn-questions-target (reintern-symbol-for-package
			      (string-upcase (format nil "*~a-cpl-yn-questions*" q-number))
			      target)))
    (and (boundp questions-target)
	 (boundp yn-questions-target)
	 (null (eval questions-target))
	 (null (eval yn-questions-target))
	 (null (member q-number (find-cpl-lisp-errors target))))))

(defun identify-broken-cpl-interpretations-with-empty-scenarios(&optional(target "."))
  (let ((tmp (reintern-symbol-for-package "*broken-cpl-interpretations-with-empty-scenarios*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
			       (mapcar #'(lambda (q-number)
					   (if (does-cpl-interpretation-have-empty-scenario-p q-number target)
					       q-number))
				       (identify-broken-cpl-interpretations target))))))
      (deref-symbol tmp))))

(defun does-cpl-interpretation-have-empty-scenario-p(q-number &optional (target "."))
  (let ((scenario-target  (reintern-symbol-for-package
			   (string-upcase (format nil "*~a-cpl-scenario*"     q-number))
			   target)))
    (and (boundp scenario-target)
	 (null (eval scenario-target))
	 (null (member q-number (find-cpl-lisp-errors target))))))

(defun is-cpl-interpretation-broken-p(q-number &optional(target "."))
  (let ((scenario-target      (reintern-symbol-for-package
			       (string-upcase (format nil "*~a-cpl-scenario*"     q-number)) target))
	(questions-target     (reintern-symbol-for-package
			       (string-upcase (format nil "*~a-cpl-questions*"    q-number)) target))
	(yn-questions-target  (reintern-symbol-for-package
			       (string-upcase (format nil "*~a-cpl-yn-questions*" q-number)) target)))
    (cond ((or (not (boundp scenario-target))
	       (not (boundp questions-target))
	       (not (boundp yn-questions-target))) nil)
	  (t (or (null (eval scenario-target))
		 (and (null (eval questions-target))
		      (null (eval yn-questions-target)))
		 (not (null (member q-number (find-cpl-lisp-errors target)))))))))

(defun get-cpl-set(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package '*q-cpl-set* target)))
    (cond ((boundp tmp)
	   (sort-question-number-set (eval tmp))))))

(defun get-bps-set(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package '*q-bps-set* target)))
    (cond ((boundp tmp)
	   (sort-question-number-set (eval tmp))))))

;;Identify correct bps answers from plausible set. 
;;Uses answer snippets supplied by users.
(defun identify-correct-bps-answers(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*correct-bps-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
	  (mapcar #'(lambda(q-number)
		      (if (is-answer-pseudo-gradable-p q-number target)
			  (pseudo-grade q-number target)))
		  (identify-plausible-bps-answers target))))))
      (deref-symbol tmp))))

;;Identify incorrect bps answers from plausible set
;;Uses answer snippets supplied by users.
(defun identify-incorrect-bps-answers(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*incorrect-bps-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
	  (mapcar #'(lambda(q-number)
		      (if (is-answer-pseudo-gradable-p q-number target)
			  (if (not (pseudo-grade q-number target))
			      q-number)))
		      (identify-plausible-bps-answers target))))))
      (deref-symbol tmp))))

;;Identify unknown bps answers from plausible set
;;Uses answer snippets supplied by users.
(defun identify-unknown-bps-answers(&optional (target "."))
  (let ((tmp (reintern-symbol-for-package "*unknown-bps-answers*" target)))
    (if (not (boundp tmp))
	(eval `(setq ,tmp
		     (quote 
		      ,(remove nil
	  (mapcar #'(lambda(q-number)
		      (if (not (is-answer-pseudo-gradable-p q-number target))
			  q-number))
		  (identify-plausible-bps-answers target))))))
      (deref-symbol tmp))))

(defun get-cpl-processing-times(&optional(q-number-list *q-formulation-set*))
  (sort-q-number-time-pair-list
   (remove nil
	  (mapcar #'(lambda (q-number)
		      (let ((target (intern (string-upcase (format nil "*~a-cpl-time*" q-number)) :km)))
			(if (and (boundp target)
				 (numberp (eval target)))
			    (list q-number (eval target)))))
		  q-number-list))))

(defun get-qa-processing-times(&optional(q-number-list *q-formulation-set*))
  (sort-q-number-time-pair-list
   (remove nil
	   (mapcar #'(lambda (q-number)
		       (let ((target-bps-time  (intern (string-upcase (format nil "*~a-bps-time*" q-number)) :km))
			     (target-expl-time (intern (string-upcase (format nil "*~a-expl-time*" q-number)) :km)))
			 (if (and (boundp target-bps-time)
				  (boundp target-expl-time)
				  (numberp (eval target-bps-time))
				  (numberp (eval target-expl-time)))
			     (list q-number (+ (eval target-bps-time)
					       (eval target-expl-time))))))
		   q-number-list))))

(defun get-bps-processing-times(&optional(q-number-list *q-formulation-set*))
  (sort-q-number-time-pair-list
   (remove nil
	  (mapcar #'(lambda (q-number)
		      (let ((target (intern (string-upcase (format nil "*~a-bps-time*" q-number)) :km)))
			(if (and (boundp target)
				 (numberp (eval target)))
			    (list q-number (eval target)))))
		  q-number-list))))

(defun get-expl-processing-times(&optional(q-number-list *q-formulation-set*))
  (sort-q-number-time-pair-list
   (remove nil
	  (mapcar #'(lambda (q-number)
		      (let ((target (intern (string-upcase (format nil "*~a-expl-time*" q-number)) :km)))
			(if (and (boundp target)
				 (numberp (eval target)))
			    (list q-number (eval target)))))
		  q-number-list))))

(defun get-mean-for-time-pair-list(input)
  (let* ((time-list (extract-time-component-from-time-pair-list input))
	 (time-list-count (length time-list)))
    (if (not (zerop time-list-count))
	(round (/ (eval 
		     (cons '+ time-list))
		    time-list-count)))))

(defun get-median-for-time-pair-list(input)
  (let* ((time-list (extract-time-component-from-time-pair-list input))
	 (time-list-count (length time-list)))
    (if (not (zerop time-list-count))
	(nth (floor (/ time-list-count 2))
	     time-list))))

(defun get-75pct-for-time-pair-list(input)
  (let* ((time-list (reverse (extract-time-component-from-time-pair-list input)))
	 (time-list-count (length time-list)))
    (if (not (zerop time-list-count))
	(nth (floor (* time-list-count 0.75))
	     time-list))))

(defun get-90pct-for-time-pair-list(input)
  (let* ((time-list (reverse (extract-time-component-from-time-pair-list input)))
	 (time-list-count (length time-list)))
    (if (not (zerop time-list-count))
	(nth (floor (* time-list-count 0.9))
	     time-list))))

(defun get-max-for-time-pair-list(input)
  (let* ((time-list (extract-time-component-from-time-pair-list input)))
    (car time-list)))

(defun get-min-for-time-pair-list(input)
  (let* ((time-list (extract-time-component-from-time-pair-list input)))
    (car (last time-list))))

(defun extract-time-component-from-time-pair-list(input)
  (remove nil
	  (mapcar #'(lambda(time-pair)
		      (if (numberp (cadr time-pair))
			  (cadr time-pair)))
		  input)))

(defun q-number-time-pair-gt(x y)
  (let ((time_x (cadr x))
	(time_y (cadr y)))
  (if (and (not (null time_x))
	   (not (null time_y)))
      (> time_x time_y))))

(defun sort-q-number-time-pair-list(input)
  (sort (copy-list input) 'q-number-time-pair-gt))

(defun generate-tester-set(&optional count)
  (progn
    (reload-tester-set ".")
    (if (missing-cpl-set)
	(progn
	  (generate-cpl-set)))
    (reload-tester-set ".")
    (if (missing-bps-set)
	(generate-bps-set count))))

;;Perform problem-solving for all cpl formulations which are not solved yet.
(defun generate-bps-set(&optional count)
  (let ((*max-clock* 256))
    (let ((*CONTROLLER-PERSISTENT-HASH-MUTABLE*  nil)
           (target-set (missing-bps-set)))
       (cond ((null target-set) t)
             ((and (not (null count))
		   (zerop count)) ())
             (t (let ((target (ps-randomly-pick target-set)))
                  (format t "Halo Tester: BPS Processing ~a~%" target)
                  (do-bps-processing target)
                  (increment-work-count)
                  (generate-bps-set (when count (1- count)))
                  ))))))

(defun put-google-analytic-tag(stream))







