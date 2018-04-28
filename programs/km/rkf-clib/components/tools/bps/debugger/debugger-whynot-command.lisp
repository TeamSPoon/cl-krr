;;
;; $Id: debugger-whynot-command.lisp,v 1.14 2008/01/03 19:58:43 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun whynot-command (operands)
  (let* ((operand-num (length operands))
	 (stream      (make-string-output-stream)))
    (cond ((= operand-num 1)
	   (let* ((vp-inst      *CURRENT-VP-INST*)
		  (concept-name (first operands)))
	     (cond ((and (kb-viewpointp vp-inst)
			 (known-frame concept-name))
		    (handle-whynot stream vp-inst concept-name))
		   ((not (known-frame concept-name))
		    (format stream "~a~%~a~%" 
			    (format nil "Error : ~a is not a valid concept." concept-name)
			    "Usage : whynot <conceptname>")))))
	   ((= operand-num 2)
	   (let* ((vp-inst      (first  operands))
		  (concept-name (Second operands)))
	     (cond ((and (kb-viewpointp vp-inst)
			 (known-frame concept-name))
		    (handle-whynot stream vp-inst concept-name))
		   ((not (kb-viewpointp vp-inst))
		    (format stream "~a~%~a~%" 
			      (format nil "Error : ~a is not a valid Viewpoint." vp-inst)
			      "Usage : whynot <vp-inst> <conceptname>"))
		   ((not (known-frame concept-name))
		    (format stream "~a~%~a~%" 
			      (format nil "Error : ~a is not a valid concept." concept-name)
			      "Usage : whynot <vp-inst> <conceptname>")))))
	  (t (format stream "~a~%~a~%"
		     "Error : Unknown WHYNOT operands"
		     "Usage : whynot <vp-inst> <conceptname>")))
    (get-output-stream-string stream)))

(defun handle-whynot(stream vp-inst concept-name)
  (let* ((vp-context (get-simplified-context-for-viewpoint vp-inst)))
    (if (not (is-match-for-viewpointp concept-name (find-matches-for-viewpoint vp-inst)))
	(parse-whynot-output stream
	 (compute-whynot-match-reasons vp-context concept-name))
        (progn 
	  #|
	  (if (member concept-name (get-concept-for-kb-instance (get-viewpoint-target-for-viewpoint-list (get-children vp-inst))))
	  (format stream "~a is a valid match candidate for ~a.~%" concept-name vp-inst)
	  (format stream "~a is a valid match candidate for ~a, however it was not considered due to redundant mappings~%" concept-name vp-inst))))))
	  |#
	  (format stream "~a is a valid candidate for continuing problem-solving.~%" concept-name)))))

(defun parse-whynot-output(stream output)
  (progn
    (print-whynot-output-for-error-code-0 stream (sieve-whynot-output-by-error-code output 0))
    (format stream "~%")
    (print-whynot-output-for-error-code-1 stream (sieve-whynot-output-by-error-code output 1))
    (format stream "~%")
    (print-whynot-output-for-error-code-2 stream (sieve-whynot-output-by-error-code output 2))
    (format stream "~%")
))

(defun whynot-reasons(stream vp-inst concept-name)
  (let* ((parent-vp-context (get-simplified-context-for-viewpoint (first (get-viewpoint-parent vp-inst)))))
    (parse-whynot-output stream	(compute-whynot-match-reasons parent-vp-context concept-name))))

(defun print-whynot-output-for-error-code-0(stream output)
  (if (not (null output))
  (progn
    (format stream "The following concept triples have no correspondences in the Viewpoint context.~%");
    (dolist (item output)
      (let* ((error-code (car (last item)))
	     (concept-triple (replace-kb-instance-with-kb-concept (make-triple-forward-pointing  (nth 0 item))))
	     (context-triple (replace-kb-instance-with-kb-concept (make-triple-forward-pointing  (nth 1 item))))
	     (concept-triple-head       (triple-head     concept-triple))
	     (concept-triple-relation   (triple-relation concept-triple))
	     (concept-triple-tail       (triple-tail     concept-triple)))
	     (format stream "   KB Concept triple : (<~a> ~a <~a>)~%" concept-triple-head concept-triple-relation concept-triple-tail))))))

(defun print-whynot-output-for-error-code-1(stream output)
  (if (not (null output))
      (progn
	(format stream "The following context triples have mis-aligned slot filler types.~%")
	(dolist (item (get-unique-scenario-triples output))
	  (let* ((target-list (sieve-list-having-val-for-idx item output 1))
		 (context-triple            (replace-kb-instance-with-kb-concept item))
		 (context-triple-head       (triple-head     context-triple))
		 (context-triple-relation   (triple-relation context-triple))
		 (context-triple-tail       (triple-tail     context-triple)))
	    (format stream "   Context triple : ~a~%" context-triple)
	    (dolist (target target-list)
	      (let* ((error-code     (car (last target)))
		     (concept-triple (replace-kb-instance-with-kb-concept (make-triple-forward-pointing  (nth 0 target))))
		     (concept-triple-head       (triple-head concept-triple))
		     (concept-triple-relation   (triple-relation concept-triple))
		     (concept-triple-tail       (triple-tail concept-triple)))
		(format stream "     Did you mean : (~a ~a ~a)~%" context-triple-head concept-triple-relation concept-triple-tail)))
	    (format stream "~%"))))))
    
(defun print-whynot-output-for-error-code-2(stream output)
  (if (not (null output))
      (progn
	(format stream "The following context triples have mis-aligned slot relations.~%")
	(dolist (item (get-unique-scenario-triples output))
	  (let* ((target-list (sieve-list-having-val-for-idx item output 1))
		 (context-triple (replace-kb-instance-with-kb-concept item))
		 (context-triple-head       (triple-head     context-triple))
		 (context-triple-relation   (triple-relation context-triple))
		 (context-triple-tail       (triple-tail     context-triple)))
	    (format stream "   Context triple : ~a~%" context-triple)
	    (dolist (target target-list)
	      (let* ((error-code (car (last target)))
		     (concept-triple (replace-kb-instance-with-kb-concept (make-triple-forward-pointing  (nth 0 target))))
		     (concept-triple-head       (triple-head concept-triple))
		     (concept-triple-relation   (triple-relation concept-triple))
		     (concept-triple-tail       (triple-tail concept-triple)))
		(format stream "     Did you mean : (~a ~a ~a)~%" context-triple-head concept-triple-relation concept-triple-tail)))
	    (format stream "~%"))))))

(defun sieve-whynot-output-by-error-code(output code)
  (remove code output :test #'(lambda(x y) (let* ((error-code (first (last y)))) (not (eq x error-code))))))

(defun replace-kb-instance-with-kb-concept (triple)
  (let* ((head            (triple-head     triple))
	 (relation        (triple-relation triple))
	 (tail            (triple-tail     triple))
	 (concept-head    (car (get-concept-for-kb-instance head)))
	 (concept-tail 	  (if (= (length (get-concept-for-kb-instance tail)) 1)
			      (car (get-concept-for-kb-instance tail))
			      (get-concept-for-kb-instance tail))))
    (list concept-head
	  relation
	  concept-tail)))