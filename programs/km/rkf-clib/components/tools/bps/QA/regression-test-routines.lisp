;;
;; $Id: regression-test-routines.lisp,v 1.24 2010/05/19 20:45:12 kbarker Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun reset-regression-tester()
  (progn
    (setq *REGRESSION-REMARKS*        "")
    ;(cl-user::gc)
    ;(cl-user::gc t)
    ))

(defun ps-iterate-question-list(output header question-list)
  (cond ((streamp output)
	 (ps-iterate-question-list-output-to-stream output header question-list))
	((stringp output)
	 (ps-iterate-question-list-output-to-filename output header question-list))
	(t (progn 
	     (format t "Error: Not a Stream or a filename for PS-ITERATE-QUESTION-LIST arg0.~%")
	     nil))))

(defun ps-generate-header-output-for-regression-table(stream header)
  (progn
    (format stream "<HTML>")
    (format stream "<B>~a</B><BR>" header)
    (format stream "created at ~a<BR><BR>" (get-time-of-day))
    (format stream "Using CPL ~a, with CLIB dated ~a<BR>" *CPL-VERSION* (car (km0 '(|the| |description| |of| |Version|))))
    (cond ((and *USE-LOOSESPEAK* *USE-MATCHER*)
	   (format stream "Both Loosespeak and Semantic Matcher were used in question formulation.<BR>"))
	  (*USE-LOOSESPEAK*
	   (format stream "Loosespeak was used, but Semantic Matcher was turned off in question formulation.<BR>"))
	  (*USE-MATCHER*
	   (format stream "Semantic Matcher was used, but Loosespeak was turned off in question formulation.<BR>")))
    (format stream "<TABLE BORDER=\"1\">")
    (format stream "<TR><TH>Num
                        <TH>Question
                        <TH>Syntactic Parse
                        <TH>Matcher remarks
                        <TH>LS Remarks
                        <TH>LS NN
                        <TH>LS Co-Ref
                        <TH>CPL triples
                        <TH>Answer Detail
                        <TH>Timings
                        <TH>Problem-Solving Tree
                        <TH>Remarks
                        </TH>
                    </TD>")
    )) 

(defun ps-generate-footer-output-for-regression-table(stream)
  (progn
    (format stream "finished at ~A<BR><BR>" (get-time-of-day))
    (format stream "</HTML>")))

(defun ps-iterate-question-list-output-to-stream(stream header question-list)
  (progn
    (ps-generate-header-output-for-regression-table stream header)
    (ps-attempt-question-list stream question-list)
    (ps-generate-footer-output-for-regression-table stream)))

(defun ps-print-triples(stream cpl-output)
  (let* ((triple-list       (remove-irrelevant-km-triples (first cpl-output)))
	 (compute-question  (second cpl-output))
	 (yes-no-questions  (third  cpl-output)))
    (format stream "<B>Scenario triples</B>")
    (format stream "<PRE>")
    (dolist (triple triple-list)
      (let ((head (triple-head triple))
	    (relation (triple-relation triple))
	    (tail (triple-tail triple)))
	(format stream "~a ~a ~a~%" head relation tail)))
    (format stream "</PRE>")
    (format stream "<B>Compute-Questions</B><PRE>~a</PRE>" (format nil "~a" compute-question))
    (format stream "<B>YES-NO-QUESTIONS</B><PRE>~a</PRE>"  (format nil "~a" yes-no-questions))
    (format stream "<B>BPS Asserts</B><PRE>~%")
    (dolist (assertion (triples-to-km-assertions triple-list))
      (format stream "~a~%" assertion))
    (format stream "</PRE>")))

(defun ps-generate-row-output-for-regression-table(stream i answer)
  (let* ((question             (nth 0   answer))
	 (cpl-syntactic-parse  (nth 1   answer))
	 (matcher-remarks      (nth 2   answer))
	 (LS-remarks-lst       (nth 3   answer))
	 (triple-list          (nth 4   answer))
	 (answer-detail        (nth 5   answer))
	 (timing-info          (nth 6   answer))
	 (problem-solving-tree (nth 7   answer))
	 (remarks              (nth 8   answer))
	 (answer-brief         "")   ;;Turned off auto scoring.
	 (expectation          ""))  ;;Turned off auto scoring.
    (cond
     ((and 
       *REGRESSION-CHECK-ANSWERS*
       expectation
       (find-if #'(lambda (one-possibility) (compare-two-brief-answers answer-brief one-possibility)) expectation))
      (incf correct-count)
      (format stream "<TR BGCOLOR=\"green\">"))
     ((and
       *REGRESSION-CHECK-ANSWERS*
       expectation
       (not (find-if #'(lambda (one-possibility) (compare-two-brief-answers answer-brief one-possibility)) expectation)))
      (incf incorrect-count)
      (format stream "<TR BGCOLOR=\"red\">"))
     (t (format stream "<TR>")))

    (format stream "<TD>~a</TD>" i)
    (format stream "<TD>~a</TD>" question)
    (format stream "<TD>~a</TD>" cpl-syntactic-parse)
    (format stream "<TD>~a</TD>" matcher-remarks)
    (let ((*ls-remark*       (nth 0 LS-remarks-lst))
	  (*ls-remark-nn*    (nth 1 LS-remarks-lst))
	  (*ls-remark-coref* (nth 2 LS-remarks-lst)))
      (format stream "<TD>~a</TD>" *ls-remark*)
      (format stream "<TD>~a</TD>" *ls-remark-nn*)
      (format stream "<TD>~a</TD>" *ls-remark-coref*))
    (format stream "<TD>")
    (ps-print-triples stream triple-list)
    (format stream "</TD>")
    (format stream "<TD>~a</TD>" answer-detail)
    ;(format stream "<TD>~a</TD>" expectation)
    (format stream "<TD><PRE>")
    (print-timing-info stream timing-info)
    (format stream "</PRE></TD>")
    (format stream "<TD><PRE>~a</PRE></TD>" problem-solving-tree)
    (format stream "<TD><PRE>~a</PRE></TD>" remarks)
    (format stream "</TR>~%")))

#|
(defun ps-generate-all-rows-for-regression-table(stream answer-list)
  (let ((correct-count             0)
	(incorrect-count           0))
    (dotimes (i                    (length answer-list))
      (let* ((answer               (nth i answer-list))
	     (question             (first   answer))
	     (triple-list          (second  answer))
	     (answer-detail        (third   answer))
	     (timing-info          (fourth  answer))
	     (problem-solving-tree (fifth   answer))
	     (remarks              (sixth   answer))
	     (answer-brief         "") ;FIXME, breaks regression test-suite, (extract-brief-answer-from-detail-answer answer-detail))
	     (expectation          (seventh answer)))
	(cond
	 ((and 
	   *REGRESSION-CHECK-ANSWERS*
	   expectation
	   (find-if #'(lambda (one-possibility) (compare-two-brief-answers answer-brief one-possibility)) expectation))
	  (incf correct-count)
	  (format stream "<TR BGCOLOR=\"green\">"))
	 ((and
	   *REGRESSION-CHECK-ANSWERS*
	   expectation
	   (not (find-if #'(lambda (one-possibility) (compare-two-brief-answers answer-brief one-possibility)) expectation)))
	  (incf incorrect-count)
	  (format stream "<TR BGCOLOR=\"red\">"))
	 (t (format stream "<TR>")))
	(format stream "<TD>~a</TD>" i)
	(format stream "<TD>~a</TD>" question)
	(format stream "<TD>")
	(ps-print-triples stream triple-list)
	(format stream "</TD>")
	(format stream "<TD>~a</TD>" answer-detail)
	(format stream "<TD>~a</TD>" expectation)
	(format stream "<TD><PRE>")
	(print-timing-info stream timing-info)
	(format stream "</PRE></TD>")
	(format stream "<TD><PRE>~a</PRE></TD>" problem-solving-tree)
	(format stream "<TD><PRE>~a</PRE></TD>" remarks)
	(format stream "</TR>~%")))
    (format stream "</TABLE>")
    (format stream "~a questions ~a correct ~a wrong ~a unknown~%" (length answer-list) correct-count incorrect-count (- (length answer-list) correct-count incorrect-count))
    ))
|#


(defun ps-iterate-question-list-output-to-filename(filename header question-list)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (ps-iterate-question-list-output-to-stream stream header question-list)))

(defun ps-attempt-question-list (stream question-list)
  (let ((save-kb-location 
	 (format nil "./regression-test-kb-~a~a.km" 
		 (random 100) 
		 (random 100) 
		 (random 100))))
    (format t "Regression Test : Saving KM state into ~a~%" save-kb-location)
    (save-kb save-kb-location)
    (dotimes (i                    (length question-list))
      (let ((question (nth i question-list))
	    expected)
	(format t "Regression Test : Restoring KM state from ~a~%" save-kb-location)
	(load-kb save-kb-location)
	(cond
	 ((stringp question) 
	  (let ((attempted-solution (append (ps-attempt-question question) 
					    (list nil))));;Empty expected answer
	    (ps-generate-row-output-for-regression-table stream i attempted-solution)
	    attempted-solution))
	 (t  ;;For automated scoring, i.e.(setf expected (cdr question))
	  (let* ((question-str (car question))
		 (expected     (cdr question))
		 (attempted-solution (append (ps-attempt-question question-str) (list expected))))
	    (ps-generate-row-output-for-regression-table stream i attempted-solution)
	    attempted-solution)))))))

(defun print-timing-info(stream timing-info)
  (let* ((interpret-time   (first timing-info))
	 (bps-time         (second timing-info))
	 (explanation-time (third timing-info))
	 (total-time       (+ interpret-time bps-time explanation-time)))
    (format stream "CPL  : ~a s~%" interpret-time)
    (format stream "BPS  : ~a s~%" bps-time)
    (format stream "Expl : ~a s~%" explanation-time)
    (format stream "Total: ~a s"   total-time)))

(defun generate-instance-of-triples-for-instance-list (instance-list)
  (let (result)
    (dolist (instance instance-list)
      (dolist (concept (get-concept-for-kb-instance instance))
	(setf result (cons (list instance '|instance-of| concept) result))))
    result))
