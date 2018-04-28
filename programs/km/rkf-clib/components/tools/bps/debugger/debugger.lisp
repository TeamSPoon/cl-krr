;;
;; $Id: debugger.lisp,v 1.51 2008/06/16 18:52:10 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun reset-debugger-parameters()
  (progn (setq *current-step*                       1)))

(defun reset-debugger()
  (progn
    (dialog-reset)
    (reset-problem-solver)
    (reset-controller-parameters)))

(defun ps-debugger-version () 
  (format t "      ---------------------------------------------~%")
  (format t "       A debugger for the Basic Problem Solver~%")
  (format t "      ---------------------------------------------~%")
  t)

(defun make-list-from-string (string)  
  (let ((old-readtable-case (readtable-case *readtable*)))
    (progn
      (unwind-protect
          (progn
            (setf (readtable-case *readtable*) :preserve)
            (setf result (with-input-from-string (stream string) 
                                                 (DO ((WORD-IN-STRING              
                                                       (READ STREAM NIL 'END-OF-STRING)  
                                                       (READ STREAM NIL 'END-OF-STRING))  
                                                      (STRING-LIST))
                                                     ((EQ WORD-IN-STRING 'END-OF-STRING)
                                                      (NREVERSE STRING-LIST))        
                                                     (PUSH WORD-IN-STRING           
                                                           STRING-LIST)))))
        (setf (readtable-case *readtable*) old-readtable-case))
      result)))

(DEFUN MAKE-STRING-FROM-LIST (A-LIST)
  (let* ((xstring ""))
    (IF (NULL A-LIST) NIL
	(SETF XSTRING (CONCATENATE 'STRING XSTRING
				   (CONCATENATE 'STRING (STRING (CAR A-LIST)) " ")
				   (MAKE-STRING-FROM-LIST (CDR A-LIST)))))))

(defun viewpoint-existp (vp-inst)
  (eval (cons 'or (mapcar #'(lambda (x) (equal vp-inst x)) *CLOSEDLIST*))))

(defun invalid-vp-inst-err-msg (vp-target)
  (concatenate 'STRING "Error: " 
	               (string vp-target)
	               " is not a Viewpoint instance in the problem solving history."))

(defun km-command (operands)
  (progn 
    (km (car operands))
))  

(defun lisp-command (operands)
  (handler-case (eval (car operands))
		(error (condition) (format t "LISP ERROR! ~S~%" condition))))

(defun reset-command (operands)
  (progn 
    (reset-debugger)
    (format t "Basic Problem Solver is reset.~%")))

(defun begin-command(operands)
  (cond ((= (length operands) 2)
	 (begin-command-lisp-parameters operands))
	((= (length operands) 1)
	 (begin-command-string operands))
	(t "Error: Unknown BEGIN operands")))

(defun break-question-string(input)
  (let ((target_pos (search "." input)))
    (cond ((null target_pos)                  (list input))
	  ((= (1+ target_pos) (length input)) (list input))
	  (t (cons (subseq input 0 (1+ target_pos))
		   (break-question-string (subseq input 
						  (1+ target_pos) 
						  (length input))))))))

(defun insert-carriage-return-for-question-string(input)
  (let ((s (make-string-output-stream)))
    (dolist (item (break-question-string input))
      (format s "~a~%" item))
    (get-output-stream-string s)))

(defun begin-command-string (operands)
  (let* ((*prototype-classification-enabled* t)  ;Enable KM prototype classification for non-root viewpoints.
	 (*classification-enabled*           t)  ;Enable KM classification for non-root viewpoints.
    (question-string (ps-make-cpl-string-proper (string (first operands)))))
    (reset-debugger)
    (aura-dialog-interpret question-string)
    (multiple-value-bind
      (scenario question yes-no-questions)
      (triples-for-solver)
      (problem-solver-context-setup scenario question))))

(defun begin-command-lisp-parameters (operands)
  (let* ((scenario (eval (intern (string-upcase (string (first operands))) :km)))
	 (question (eval (intern (string-upcase (string (second operands))) :km))))
    (reset-command nil)
    (problem-solver-context-setup scenario question)))

;;Potential problem. 
;;We no longer use just create root-viewpoint using (get-initial-viewpoint).
;;Instead, we now create the crown of the search graph using (get-initial-viewpoint-lst).
(defun problem-solver-context-setup(scenario question)
  (let* ((vp-inst  (car (get-initial-viewpoint-lst 
			 scenario
			 (ps-get-query-triples scenario question))))
	 (node     (new-node vp-inst)))
    (mutate-openlist (list node))
    (next-viewpoint)
    (if *adhoc-mode*
	(progn
	  (try-all-concepts-command)
	  (print-candidate-list)
	  ))
    t))

(defun push-user-command(operands)
  (let* ((param (first  operands))
	 (path (second operands)))
    (cond ((or (equal '|viewpoint-query|        param))  
	   (push-viewpoint-query-prefixes path))
	  ((or (equal '|viewpoint-result-query| param))  
	   (push-viewpoint-result-query-prefixes path))
	  (t "Error: Unknown PUSH USER operands"))))

(defun push-command(operands)
  (let* ((param (first  operands))
	 (value (second operands)))
    (cond ((equal '|user| param) 
	   (push-user-command (cdr operands)))
	  (t "Error: Unknown PUSH operands"))))

(defun pop-openlist (elem)
  (cond ((zerop elem) t)
	(t (let* ((temp-node (car *OPENLIST*)))
	     (progn
	       (mutate-openlist (cdr *OPENLIST*))
	       (mutate-closedlist (insert-at-end (get-config temp-node) 
						 *closedlist*))
	       (pop-openlist (1- elem)))))))

(defun pop-openlist-command(operands)
  (let* ((param (first  operands)))
    (cond ((null param)     (pop-openlist 1))
	  ((integerp param) (if (>= (length *openlist*) param)
				(pop-openlist param)
			        "Error: POP OPENLIST operand is larger than size of OPENLIST"))
	  (t "Error: Unknown POP OPENLIST operands"))))

(defun pop-user-command(operands)
  (let* ((param (first  operands))
	 (value (second operands)))
    (cond ((or (equal '|viewpoint-query|        param))
	   (pop-viewpoint-query-prefixes))
	  ((or (equal '|viewpoint-result-query| param))  
	   (pop-viewpoint-result-query-prefixes))
	  (t "Error: Unknown POP USER operands"))))

(defun pop-command (operands)
  (let* ((param (first  operands))
	 (value (second operands)))
    (cond ((equal '|user|    param)
	   (pop-user-command     (cdr operands)))
	  ((equal '|openlist| param)
	   (pop-openlist-command (cdr operands)))
	  (t "Error: Unknown POP operands"))))

(defun find-similar-command (operands)
  (cond ((and (= (length operands) 1))
	 (let* ((concept (car operands))
		(KB-candidates (find-similar-concepts concept)))
	   (if (null KB-candidates)
	       (format t "The ~a concept was not found to be similar to any concept in the KB.~%" concept)
	       (progn
		 (format t "The ~a concept was found similar to ~%" concept)
		 (dolist (candidate KB-candidates)
		   (format t "     ~a concept by ~a degree.~%"
			   (car candidate) (cadr candidate)))))))
	(t "Error: Unknown FIND-SIMILAR operands")))

(defun find-match-command (operands)
  (cond ((and (= (length operands) 1)
	      (kb-viewpointp (first operands)))
	 (let* ((vp-inst (car operands))
		(KB-candidates (find-matches-for-viewpoint vp-inst)))
	   (if (null KB-candidates)
	       (format t "~a does not match any concept in the KB.~%" vp-inst)
	       (progn
		 (format t "~a was found to match ~%" vp-inst)
		 (dolist (candidate KB-candidates)
		   (format t "     ~a concept by ~a degree.~%"
			   (car candidate) (cadr candidate)))))))
	(t "Error: Unknown FIND-MATCH operands")))

(defun parse (input)
  (let* ((command (car input)))
    (cond ((null input) "")
	  ((or (equal '|ls|           command))   (parse-list-command    (cdr input)))
	  ((or (equal '|begin|        command))   (begin-command         (cdr input)))
	  ((or (equal '|continue|     command))   (continue-command      (cdr input)))
	  ((or (equal '|next|         command))   (next-command          (cdr input)))
	  ((or (equal '|step|         command))   (step-command          (cdr input)))
	  ((or (equal '|push|         command))   (push-command          (cdr input)))
	  ((or (equal '|pop|          command))   (pop-command           (cdr input)))
	  ((or (equal '|apply|        command))   (apply-command         (cdr input)))
	  ((or (equal '|group|        command))   (group-command         (cdr input)))
	  ((or (equal '|query|        command))   (query-command         (cdr input)))
	  ((or (equal '|see|          command))   (see-command           (cdr input)))
	  ((or (equal '|reset|        command))   (reset-command         (cdr input)))
	  ((or (equal '|adhoc|        command))   (adhoc-command         (cdr input)))
	  ((or (equal '|watch|        command))   (watch-command         (cdr input)))
	  ((or (equal '|km|           command))   (km-command            (cdr input)))
	  ((or (equal '|lisp|         command))   (lisp-command          (cdr input)))
	  ((or (equal '|set|          command))   (set-command           (cdr input)))
	  ((or (equal '|help|         command))   (help-command          (cdr input)))
	  ((or (equal '|why|          command))   (why-command           (cdr input)))
	  ((or (equal '|whynot|       command))   (whynot-command        (cdr input)))
	  ((or (equal '|find-similar| command))   (find-similar-command  (cdr input)))
	  ((or (equal '|find-match|   command))   (find-match-command    (cdr input)))
	  ((or (equal '|try|          command)
	       (equal '|pick|         command)
	       (equal '|unpick|       command))   (parse-adhoc-command input))
	  ((or (equal '|q|            command)    
	       (equal '|quit|         command))   (return-from parse nil))
	  (t                                      "ps-debug : Parse error"))))

(defun ps-debug()
  (let ((*logging* t)
	(*prototype-classification-enabled* nil)  ;disable KM prototype classification for non-root viewpoints.
	(*classification-enabled*           nil))  ;disable KM classification for non-root viewpoints.
    (handler-case (ps-debug0)
		  (error 
		   (error)
		   (cerror "Ignore error and return."
			   (progn 			     
			     (reset-problem-solver)
			     (reset-debugger)
			     (format t "Cleaning up and exiting (ps-debug)~%") 
			     (tpl:top-level-read-eval-print-loop)))))))

(defun ps-debug0()
  (let* ((input  nil)
	 (output nil))
    (progn 
	  (rlclearrules)
      (rldefrules physrules)   
      (ps-debugger-version)
      (if (not (= *current-step* 1))
	  (progn (format t "Cleaning up half-steps...~%")
		 (step-through-clock)))
      (catch 'exit
	(loop
	 (if *ADHOC-MODE*
	     (format t    "(ps-debug <adhoc>) ")
	     (format t    "(ps-debug) "))
	 (setf input  (make-list-from-string (read-line)))
	 (setf output (parse input))
	 (if (not (equal output ""))
	     (format t "~a~%" output)))))))

