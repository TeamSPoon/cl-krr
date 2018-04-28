;;
;; $Id: debugger-see-command.lisp,v 1.17 2008/02/02 19:21:32 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun see-viewpoint-descendants (operands)
  (let* ((vp-target (first operands)))
    (if (viewpoint-existp vp-target)
	(tree-printer t (get-descendants vp-target) nil)
        (invalid-vp-inst-err-msg vp-target))))

(defun see-viewpoint-ancestors (operands)
  (let* ((vp-target (first operands)))
    (if (viewpoint-existp vp-target)
	(tree-printer t (get-ancestors vp-target) nil)
        (invalid-vp-inst-err-msg vp-target))))

(defun see-param-openlist()	
  (progn 
    (format t "Data Structure *OPENLIST* contains~%")
    (format t "~a~%" (extract-configs-from-openlist *OPENLIST*))
    t))

(defun see-param-closedlist()
  (progn (format t "Data Structure *CLOSEDLIST* contains")
	 (print *CLOSEDLIST*)
	 (format t "~%")
	 t))

(defun see-param-queue_fn()
  (format t "*QUEUE_FN* has value ~a~%" *queue_fn*)
  t)

(defun see-param-heuristic()
  (format t "*HEURISTIC* has value ~a~%" *heuristic*)
  t)

(defun see-param-pruning-condition()
  (format t "*PRUNING-CONDITION* has value ~a~%" *pruning-condition*) 
  t)

(defun see-param-clock()
  (format t "*CLOCK* has value ~a~%" *clock*)
  t)

(defun see-param-CURRENT-NODE()
  (format t "*CURRENT-NODE* has value ~a~%" *CURRENT-NODE*)
  t)

(defun see-param-CURRENT-VP-INST()
  (format t "*CURRENT-VP-INST* has value ~a~%" *CURRENT-VP-INST*)
  t)

(defun see-param-CURRENT-STEP()
  (format t "*CURRENT-STEP* has value ~a~%" *CURRENT-STEP*)
  t)

(defun see-user-viewpoint-query()
  (format t "~a~%" (attach-query (cons '<viewpoint> 
				       *viewpoint-query-prefixes*)))
  t)

(defun see-user-viewpoint-result-query()
  (format t "~a~%" (attach-query (cons '<viewpoint-result> 
				       *viewpoint-result-query-prefixes*)))
  t)

(defun see-user(operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|viewpoint-query|        param))
	   (see-user-viewpoint-query))
	  ((or (equal '|viewpoint-result-query| param)) 	   
	   (see-user-viewpoint-result-query))
	  (t "Error: Unknown SEE ADHOC operands"))))

(defun see-viewpoint(operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|scenario|        param))
	   (see-viewpoint-scenario (cdr operands)))
	  ((or (equal '|model|           param))
	   (see-viewpoint-model    (cdr operands)))
	  ((or (equal '|context|         param))
	   (see-viewpoint-context  (cdr operands)))
	  (t "Error: Unknown SEE VIEWPOINT operands"))))

(defun see-viewpoint-scenario(operands)
  (let* ((param (first  operands)))
    (format t "~a~%" (triples2km (get-viewpoint-scenario param)))))

(defun see-viewpoint-model(operands)
  (let* ((param (first  operands)))
    (get-viewpoint-model param)))

(defun see-viewpoint-context(operands)
  (let* ((param (first  operands)))
    (append (get-viewpoint-scenario param)
	    (get-viewpoint-mode     param))))

(defun see-param-all-variables ()
  (progn 
    (see-param-openlist)
    (see-param-closedlist)
    (see-param-queue_fn)
    (see-param-heuristic)
    (see-param-pruning-condition)
    (see-param-clock)
    (see-param-current-node)
    (see-param-current-vp-inst)
    (see-param-current-step)
    t))

(defun see-km-instance(km-inst)
  (ps-km-query `(|showme| ,km-inst)))

(defun see-command(operands)
  (let* ((param (first  operands)))
    (cond ((or (equal '|openlist|              param))  (see-param-openlist))
	  ((or (equal '|closedlist|            param))  (see-param-closedlist))
	  ((or (equal '|queue_fn|              param))  (see-param-queue_fn))
	  ((or (equal '|heuristic|             param))  (see-param-heuristic))
	  ((or (equal '|pruning-condition|     param))  (see-param-pruning-condition))
	  ((or (equal '|clock|                 param))  (see-param-clock))
	  ((or (equal '|current-node|          param))  (see-param-current-node))
	  ((or (equal '|current-vp-inst|       param))  (see-param-current-vp-inst))
	  ((or (equal '|current-step|          param))  (see-param-current-step))
	  ((or (equal '|ancestor|              param))  (see-viewpoint-ancestors   (cdr operands)))
	  ((or (equal '|descendant|            param))  (see-viewpoint-descendants (cdr operands)))
	  ((or (equal '|user|                  param))  (see-user                  (cdr operands)))
	  ((or (equal '|viewpoint|             param))  (see-viewpoint             (cdr operands)))
	  ((or (equal '|mapping|               param))  (see-mapping               (cdr operands)))
	  ((or (equal '|context|               param))  (see-context               (cdr operands)))
	  ((or (equal '|equation|              param))  (see-equation              (cdr operands)))
	  ((or (equal '|lineage|               param))  (see-lineage               (cdr operands)))
	  ((or (equal '|all|                   param))  (see-param-all-variables))
	  (t                                            (see-km-instance param)))))

(defun see-lineage(operands)
  (let ((rootname nil)
	(vp-tree  (get-ancestors (first (last *CLOSEDLIST*)))))
    (if (clone-kb-instancep (first operands))
	(progn 
	  (setf rootname (ps-unclone-naively (first operands)))
	  (format t "The root name for ~a is ~a.~%" (first operands) rootname))
        (progn
	  (setf rootname (first operands))))
    (format t "The the lineage for ~a is,~%~%" rootname)
    (tree-printer t 
		  vp-tree 
		  (mapcar #'(lambda (vp-inst)
			      (list vp-inst
				    (viewpoint-contains-rootname rootname
								 vp-inst)))
			  *CLOSEDLIST*))))
    
(defun see-mapping(operands)
  (let* ((vp-inst    (first operands))
	 (redundancy (redundant-viewpoint-mappingp vp-inst)))
    (if (kb-viewpointp vp-inst)
	(progn 
	  (dolist (vp-mapping (get-ancestry-mapping-for-viewpoint vp-inst))
	    (let ((vp-ancestor         (first  vp-mapping))
		  (vp-ancestor-mapping (second vp-mapping)))
	      (format t "~a mappings,~%" vp-ancestor)
	      (if (null vp-ancestor-mapping)
		  (format t "   None applicable~%")
		(dolist (mapping vp-ancestor-mapping)
		  (format t "   ~a~%" mapping)))))
	  (if redundancy
	      (format t "~%~a contains redundant mappings, thus redundant.~%" vp-inst)))
        (format t "~%~a is not a valid Viewpoint instance.~%" vp-inst))))

(defun see-context(operands)
  (let* ((vp-inst    (first operands)))
    (cond ((null vp-inst)          (write-context-to-file t *current-vp-inst*))
	  ((kb-viewpointp vp-inst) (write-context-to-file t vp-inst))
	  (t (format t "~%~a is not a valid Viewpoint instance.~%" vp-inst)))))

(defun see-equation(operands)
  (let* ((vp-inst    (first operands)))
    (cond ((null vp-inst)          (write-equation-to-file t *current-vp-inst*))
	  ((kb-viewpointp vp-inst) (write-equation-to-file t vp-inst))
	  (t (format t "~%~a is not a valid Viewpoint instance.~%" vp-inst)))))
#|
(defun triples2frame (triples)
  (setf triples
	(mapcar #'(lambda (triple)
		    (let ((x (triple-head     triple))
			  (y (triple-relation triple))
			  (z (triple-tail     triple)))
		      (if (scalar-const? x)
			  (setf x `(:|pair| ,x |Thing|)))
		      (if (scalar-const? z)
			  (setf z `(:|pair| ,z |Thing|)))
		      (list x y z)))
		triples))
  (cond
   ;; if is a query
   #|((ls-find-all-triples-by-pattern '(?x |query-varp| |t|) triples)
    (ls-extract-question triples))|#
   ;; it is an assertion
   (t
    (triples2km-assertion nil triples)))
  )

;; input: a root instance and a set of triples
;; output: a KM expression and the triples not covered by the root instance
;; example: (nil ((_A r1 _B) (_B r2 _C))) => (_A has (r1 ((_B has (r2 (_C)))))) nil
;; example: (_A ((_A r1 _B) (_C r2 _D))) => (_A has (r1 (_B))) (_C r2 _D)
(defun triples2km-assertion (root triples)
  (let ((nodes nil)
	(km-expr nil)
	(retval nil)
	(dependent-triples nil)
	(dependent-nn nil)
	)
    (cond 
     ;; no root node is specified, then
     ;; find all the top level nodes
     ((null root) 
      (setf nodes (remove-duplicates (extract-root-nodes triples)))
      (setf retval '(:|set|))
      (dolist (node nodes)
	      (multiple-value-setq (km-expr triples)(triples2km-assertion node triples))
	      (setf retval (set-append-element retval km-expr))
	      )
      (if (= (length retval) 2)
	  (setf retval (second retval)))
      )
     ;; if root not used as a head in triples
     ((not (find root (mapcar #'first triples) :test #'equal))
      (setf retval root))
     ;; else
     (t
      (setf dependent-triples (remove-if-not #'(lambda (triple) (and (equal root (first triple))
								     (not (equal '|unknown-relation| (second triple))))) triples))
      (setf dependent-nn (remove-if-not #'(lambda (triple) (and (equal root (first triple))
								(equal '|unknown-relation| (second triple)))) triples))
      (setf triples (set-difference triples (append dependent-triples dependent-nn) :test #'equal))
      (setf retval `(,root |has| ))
      (dolist (triple dependent-triples)
	      (multiple-value-setq (km-expr triples)
				   (triples2km-assertion (third triple) 
					       triples))
	      ;;(format t "triples = ~S~%" triple)
	      (if (and (equal (second triple) '|instance-of|)
		       (consp (third triple)))
		  (setf retval (append-element retval `(,(second triple) ,km-expr)))
		(setf retval (append-element retval `(,(second triple) (,km-expr)))))
	      )
      (when dependent-nn
	(setf retval (list ':|set| retval))
	(dolist (nn dependent-nn)
		(multiple-value-setq (km-expr triples)
				     (triples2km-assertion (third nn)
						 triples))
		(if (atom km-expr)
		    (setf retval (append-element retval `(:|nn| ,root ,(third nn))))
		  (setf retval (append-element
				(append-element retval `(:|nn| ,root ,(third nn)))
				km-expr)))))))
    (values retval triples)
    ))
|#