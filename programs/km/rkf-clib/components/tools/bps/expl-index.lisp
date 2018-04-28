(in-package :km)

;;; AURA expl indexing ;;;

(defparameter *EXPLANATION-LINK-INDEX* nil)

(defun index-kb-frame-slot (frame slot)
  (if frame
      (let* ((frame-evald  (car (km0 `,frame)))
             (slot-evald   (car (km0 `,slot)))
             (index-rec    (list "slot-q" (list frame-evald slot-evald)))
             (index-string (find-index-rec index-rec))
            )
            (if index-string
                index-string
                (let ((new-index (string (gensym "bps"))))
                     (setf *EXPLANATION-LINK-INDEX*
                           (append *EXPLANATION-LINK-INDEX*
                                   (list (list new-index index-rec))
                           )
                     )
                     new-index
                )
            )
      )
  )
)


;; here is the justifications-only version of index-kb-triple
;; it does not call get-explanations (which may return raw KM)
(defun index-kb-triple (triple)
  (if triple
      (let* ((index-string   (string (gensym "bps")))
             (rule-strings   (get-kb-rule-for-triple triple))
             (rule-ok        (not (equal (get-kb-rule-for-triple triple) "")))
             (justif-strings (get-justification :triple triple))
             (justif-ok      (not (and (listp justif-strings)
                                       (equal (length justif-strings) 2)
                                       (equal (first justif-strings) "<explanation-structure>")
                                       (equal (second justif-strings) "</explanation-structure>")
                             ))
             )
             (s (make-string-output-stream))
            )
;        (if (or rule-ok justif-ok)
            (if justif-ok
                (progn
                  (if justif-ok
                      (progn
                        (format s "<h3>Justification</h3>~%")
                        (mapcar #'(lambda (str) (format s "~a~%" str)) justif-strings)
                      )
                  )
; don't show raw km; eventually maybe we write a pprinter?
;              (if rule-strings
;                  (format s "<h3>Raw Knowledge Base Rule</h3>~%~a~%" rule-strings)
;              )
                  (setf *EXPLANATION-LINK-INDEX* 
                        (append *EXPLANATION-LINK-INDEX*
                                (list (list index-string (get-output-stream-string s)))))
                  index-string
                )
                NIL
            )
;        )
      )
  )
)


#|
;; here is the justifications-and-explanations version of index-kb-triple
;; it calls get-explanations (which may return raw KM)
(defun index-kb-triple (triple)
  (if triple
      (let* ((index-string   (string (gensym "bps")))
             (rule-strings   (get-kb-rule-for-triple triple))
             (rule-ok        (not (equal (get-kb-rule-for-triple triple) "")))
             (justif-strings (get-justification :triple triple))
             (justif-ok      (not (and (listp justif-strings)
                                       (equal (length justif-strings) 2)
                                       (equal (first justif-strings) "<explanation-structure>")
                                       (equal (second justif-strings) "</explanation-structure>")
                             ))
             )
             (s (make-string-output-stream))
            )
            (if (or rule-ok justif-ok)
                (progn
                  (if justif-ok
                      (progn
                        (format s "<h3>Justification</h3>~%")
                        (mapcar #'(lambda (str) (format s "~a~%" str)) justif-strings)
                      )
                  )
                  (if rule-strings
                      (format s "<h3>Raw Knowledge Base Rule</h3>~%~a~%" rule-strings)
                  )
                  (setf *EXPLANATION-LINK-INDEX* 
                        (append *EXPLANATION-LINK-INDEX*
                                (list (list index-string (get-output-stream-string s)))))
                  index-string
                )
                NIL
            )
      )
  )
)
|#

(defun index-kb-instance (inst)
  (if inst
      (let* ((inst-evald   (car (km0 `,inst)))
             (index-rec    (list "inst-q" inst-evald))
             (index-string (find-index-rec index-rec))
            )
            (if index-string   ;; already exists
                index-string
                (let ((new-index (string (gensym "bps"))))
                     (setf *EXPLANATION-LINK-INDEX*
                           (append *EXPLANATION-LINK-INDEX*
                                   (list (list new-index index-rec))
                           )
                     )
                     new-index
                )
            )
      )
  )
)

(defun find-index-rec (index-rec)
  (car (find-if #'(lambda (rec) (equal index-rec (second rec)))
                *EXPLANATION-LINK-INDEX*)
  )
)

(defun bps-linkify-domspec-instance (km-instance km-string-expr)
  (let ((inst-evald   (car (km0 `,km-instance)))
        (inst-class   (car (km0 `(|the| |classes| |of| ,km-instance))))
        (string-evald (car (km0 `,km-string-expr)))
       )
       (with-output-to-string (s)
         (if (concept-is-domspec inst-class)
             (let ((inst-index (index-kb-instance inst-evald)))
                  (format s "<A HREF=\"javascript:void(0)\" onClick=\"javascript:linkCommand('~a')\">~a</A>" inst-index string-evald)
             )
             (format s "~a" string-evald)
         )
       )
  )
)

(defun bps-get-link-target-text (index-string)
  (let ((index-rec (second (car (member-if #'(lambda (x) (equal (car x) index-string))
                                           *EXPLANATION-LINK-INDEX*)))
        )
       )
       (cond ((listp index-rec) 
                 (cond ((equal (car index-rec) "text-q")
                        (ps-attempt-question (cadr index-rec)))
                       ((equal (car index-rec) "slot-q")
                        (ps-slot-lookup-for-instance (first (cadr index-rec))
                                                     (second (cadr index-rec))))
                       ((equal (car index-rec) "inst-q")
                        (cond ((class-order-instp (cadr index-rec))
                               (generate-property-table (cadr index-rec)))
                              (t
                               (describe-and-cache (car (km0 `(|the| |classes| |of|,(cadr index-rec))))
                                                   index-string))
                        ))
                 )
             )
             ((stringp index-rec) index-rec)
             ((viewpoint-instp index-rec) (km0 `(|the| |viewpoint-answer-page| |of| ,index-rec)))
       )
  )
)

(defun describe-and-cache (class index-string)
  (let ((description-page (multiple-value-bind
                            (answer answer-page timing error)
                            (ps-describe class)
                            answer-page
                          )
       ))
       (if description-page
           (setf *EXPLANATION-LINK-INDEX* 
                 (append (remove-if #'(lambda (erec) (equal (first erec) index-string)) *EXPLANATION-LINK-INDEX*)
                         (list (list index-string description-page)))
           )
       )
       description-page
  )
)

(defun concept-is-domspec (conc)
;  (and (not (member conc *clib-list*))
       (or (member conc *bio-halo-pump-prime-list*)
           (member conc *chem-halo-pump-prime-list*)
           (member conc *phys-halo-pump-prime-list*)
           (member conc *science-shared-pump-prime-list*)
           (member conc *all-user-defined-concepts*)
       )
;  )
)

(defun get-kb-rule (index-string)
  (let ((kb-rule (third (car (member-if #'(lambda (x) (equal (second x) index-string)) *KB-TRIPLES-INDEX*)))))
       (if kb-rule
           kb-rule
           ""
       )
  )
)

(defun get-kb-rule-for-triple (triple)
 (with-restoring-situations-reasoning ()
  (let* ((debug nil)
	 (push-sit (curr-situation))
         (inst     (second triple))
         (slot     (third  triple))
         (val      (fourth triple))
         (trip-sit (car (ps-km-query `(|oneof| (|the| |instances| |of| |Situation|) |where|
                                            (|in-situation| |It| ((|the| ,SLOT |of| ,INST) |includes| ,VAL))) 
				     debug)))
        )
;(format t "trip-sit is now: ~a~%======~%" trip-sit)
        (in-situation trip-sit)
        (let ((explanation (get-explanations0 inst slot val)))
;(format t "1 push-sit is now: ~a~%======~%" push-sit)
             (in-situation push-sit)
             (with-output-to-string (s) 
               (if explanation
                   (format s "<PRE>~a</PRE>" (multiple-value-bind (expl justif rule path body)
                                               (get-comments (car (fourth explanation)))
                                               rule)
                   )
                   (let* ((inv-slot     (invert-slot slot))
                          (inv-trip-sit (car (ps-km-query `(|oneof| (|the| |instances| |of| |Situation|) |where|
                                                                (|in-situation| |It| ((|the| ,INV-SLOT |of| ,VAL) |includes| ,INST))) 
							  debug)))
                         )
;(format t "inv-trip-sit is now: ~a~%======~%" inv-trip-sit)
                         (in-situation inv-trip-sit)
                         (let ((inv-expl (get-explanations0 val inv-slot inst)))
;(format t "2 push-sit is now: ~a~%======~%" push-sit)
                              (in-situation push-sit)
                              (if inv-expl
                                  (format s "<PRE>~a</PRE>" (multiple-value-bind (i-expl i-justif i-rule i-path i-body)
                                                              (get-comments (car (fourth inv-expl)))
                                                              i-rule)
                                  )
;;                                  (format s "source unknown (possibly inferred)")
                              )
                         )
                   )
               )
             )
        )
  )
)
)

(defun get-instance-constraints (instance slot)
  (let ((constr-str (make-phrase 
                      (andify 
                        (mapcar #'list2string 
                            (remove-if-not #'(lambda (c) (or (equal (first c) '|at-least|)
                                                             (equal (first c) '|exactly|)
                                                             (equal (first c) '|at-most|))
                                             )
                                             (collect-constraints-on-instance instance slot))
                        )
                      )
                    )
       ))
       (if (not (equal constr-str ""))
           constr-str
           NIL
       )
  )
)

(defun generate-property-table (class-order-inst)
  (let* ((class-seq   (km0 `(|the| |element| |of| ,class-order-inst)))
         (property    (car  (km0 `(|the| |property-slot| |of| ,class-order-inst))))
         (table-class (get-common-superclass class-seq))
        )
        (with-output-to-string (s) 
          (format s "<TABLE BORDER=1><TR><TD><B><I>Kinds of ~a ordered by increasing ~a</I></B></TD></TR>~%"
                    table-class property)
          (dolist (cl (cdar class-seq))
            (format s "<TR><TD>~a</TD></TR>~%" cl)
          )
          (format s "</TABLE>~%")
        )
  )
)

(defun list2string (in-list)
  (let ((s (make-string-output-stream)))
       (mapcar #'(lambda (elem) (format s "~a " elem)) in-list)
       (string-right-trim " " (get-output-stream-string s))
  )
)

(defun viewpoint-instp (inst)
  (car (km0 `(,inst |isa| |Viewpoint|)))
)

(defun class-order-instp (inst)
  (car (km0 `(,inst |isa| |Property-Class-Order|)))
)

(defun get-common-superclass (class-seq)
  (if class-seq
      (let* ((classes (cdar class-seq))
             (super-candidates (km0 `(|the| |superclasses| |of| ,(car classes))))
            )
            (one-subsumes-all super-candidates classes)
      )
  )
)

(defun one-subsumes-all (subsumer-candidates classes)
  (if subsumer-candidates
      (let ((found-one (find-if #'(lambda (sc) (subsumes-allp sc classes)) subsumer-candidates)))
           (if found-one
               found-one
               (one-subsumes-all (km0 `(|the| |superclasses| |of|
                                        ,(cons ':|set| subsumer-candidates)))
                                 classes)
           )
      )
  )
)

(defun subsumes-allp (class class-list)
  (every #'(lambda (c) (km-subsumes class c)) class-list)
)

(defun km-subsumes (c1 c2)
  (car (km0 `(,c1 |subsumes| ,c2)))
)


;; JC: attempts slot-lookup for specific instance and slot
(defun ps-slot-lookup-for-instance (instance slot)
  (let* ((debug nil)
	 (concept  (km-slotvals instance '|instance-of|))
         (filler   (km-slotvals instance slot))
         (scenario (deaggregate-triple-list `((,instance |instance-of| ,concept)
                                              (,instance ,slot ,filler))))
         (sv-viewpoint (ps-instantiate-concept '|Slot-Value-Viewpoint|))
        )
        (ps-km-query `(,sv-viewpoint |has| 
                        (|viewpoint-scenario| ,(affix-triple-prefix scenario))
                        (|viewpoint-score| (1))
                        (|viewpoint-query| ((:|seq| (:|pair| |*slot-value| (:|triple| ,instance ,slot *)))))
                        (|viewpoint-answerable-query| ((:|seq| (:|pair| |*slot-value| (:|triple| ,instance ,slot *)))))
                        (|viewpoint-query-matched| (T))) 
		     debug)
        (if sv-viewpoint
            (car (km0 `(|the| |viewpoint-answer-page| |of| ,sv-viewpoint)))
        )
  )
)

#|
;;deprecated original version? This one cranks up BPS, which may be undesirable when it was buggy.
;;attempts slot-lookup for specific instance and slot
(defun ps-slot-lookup-for-instance(instance slot)
  (let* ((concept  (km-slotvals instance '|instance-of|))
	 (filler   (km-slotvals instance slot))
	 (scenario (deaggregate-triple-list `((,instance |instance-of| ,concept)
					      (,instance ,slot ,filler)))))
    (basic-problem-solver scenario filler nil)))
|#




(defun get-trigger-documentation (instance)
  (let* ((debug nil)
	 (inst-prototype (car (ps-km-query `(|the| |first| |of| 
                                                   (|the| |prototypes| |of| (|the| |classes| |of| ,INSTANCE))))))
         (trigger-nodes  (cons :|set| (ps-km-query `(|the| |Trigger-Node| |big-nodes| |of| ,INST-PROTOTYPE))))
        )
    (ps-km-query `(|the| |node-documentation| |of| ,TRIGGER-NODES))
  )
)




;; get the prototype of the viewpoint root instance
;; get the target frame instance's analog in the root's prototype
;; get the target value instance's analog in the root's prototype
;; get the edge-documentation on the target's analog in the root's prototype
;; filter edge-documentation to the desired slot
;; convert instances in the edge-documentation's :seq to their text-phrase
;; make-phrase the whole shebang
(defun get-edge-documentation (viewpoint-instance root-prototype target-frame-inst target-val-inst slot)
  (let* ((target-analog-frame-inst (car (ps-km-query `(|oneof| (|the| |cloned-from| |of| ,TARGET-FRAME-INST)
                                                       |where| ((|the| |prototype-participant-of| |of| |It|)
                                                                |includes| ,ROOT-PROTOTYPE)
                                                      )
         )))
         (target-analog-val-inst (car (ps-km-query `(|oneof| (|the| |cloned-from| |of| ,TARGET-VAL-INST)
                                                     |where| ((|the| |prototype-participant-of| |of| |It|)
                                                              |includes| ,ROOT-PROTOTYPE)
                                                    )
         )))
         (edge-doc-list (ps-km-query `(|allof| (|the| |edge-documentation| |of| ,TARGET-ANALOG-FRAME-INST)
                                       |where| (((|the1| |of| |It|) = ,SLOT)
                                                |and|
                                                ((|the2| |of| |It|) = ,TARGET-ANALOG-VAL-INST)
                                               )
                                      )
         ))
         (edge-doc-list-filtered (remove-if-not #'(lambda (doc-seq) (equal (second doc-seq) slot))
                                                edge-doc-list
                                 )
         )
         (edge-doc-string-list '())
        )
    (dolist (edge-doc-seq edge-doc-list-filtered)
      (setq edge-doc-string-list
            (append edge-doc-string-list 
              (list (make-phrase (ps-km-query `(|forall-seq| (|the3| |of| ,EDGE-DOC-SEQ) 
                                                  (|if| (|It| |isa| |String|)
                                                   |then| |It|
                                                   |else| (|the| |text-phrase| |of| |It|)
                                                  )
                                               )
              )))
            )
      )
    )
    edge-doc-string-list
  )
)

(defun follow-vp-path (root-inst slot-list)
  (if (null slot-list)
      NIL
      (ps-km-query (build-km-path-query root-inst (reverse slot-list)))
  )
)

(defun build-km-path-query (root-inst slot-list)
  (if (null slot-list)
      root-inst
      (append `(|the| ,(car slot-list) |of|) (list (build-km-path-query root-inst (cdr slot-list))))
  )
)


(defun lispify-vp-scenario (vp-inst)
  (let ((vp-scenario-trip-list (ps-km-query `(|allof| (|the| |viewpoint-scenario| |of| ,VP-INST)
                                              |where| (|not| ((|the2| |of| |It|) = |instance-of|))))
       ))
     (expand-triple-list-with-inverses (strip-all-colon-triple vp-scenario-trip-list))
  )
)

(defun collapse-path (vp-inst start-inst end-inst)
  (cdr (reverse (remove-duplicates (flatten (ps-get-path start-inst end-inst (lispify-vp-scenario vp-inst))))))
)

(defun gen-def-ref (vp-inst root-inst target-inst)
  (let ((term-list (collapse-path vp-inst root-inst target-inst)))
    (with-output-to-string (s)
      (if (null term-list)
          (setq s (car (ps-km-query `(make-phrase (|the| |text-def-head| |of| ,TARGET-INST)))))
          (progn
            (format s "the")
            (dolist (term term-list)
              (if (slotp term)
                  (format s " ~a of the" term)
                  (format s " ~a" (car (ps-km-query `(|the| |name| |of| ,TERM))))
              )
            )
          )
      )
    )
  )
)

(defun get-vp-path (vp-inst root-inst target-inst)
  (search-vp-scenario (lispify-vp-scenario vp-inst) root-inst target-inst)
)

(defun search-vp-scenario (vp-trip-list root-inst target-inst)
  (mapcar #'second (get-directed-path-in-triples root-inst target-inst vp-trip-list)) 
)

(defun expand-triple-list-with-inverses (triple-list)
  (let ((expanded-list '()))
    (dolist (triple triple-list)
      (setq expanded-list (append expanded-list (list triple (invert-triple triple))))
    )
    (remove-duplicates expanded-list)
  )
)

(defun strip-all-colon-triple (triple-list)
  (if (null triple-list)
      triple-list
      (cons (strip-colon-triple (car triple-list)) (strip-all-colon-triple (cdr triple-list)))
  )
)

(defun strip-colon-triple (triple)
  (if (equal (car triple) ':|triple|)
      (cdr triple)
      triple
  )
)

(defun get-directed-path-in-triples (source target triple-list)
  (let ((path nil))
    (if (and (source-appears-in-triples source triple-list)
             (target-appears-in-triples target triple-list)
        )
        (let ((direct-link (get-single-linking-triple source target triple-list)))
          (if direct-link
              (setq path (list direct-link))
              (let ((source-triples (get-all-source-in-triples source triple-list)))
                (dolist (source-trip source-triples)
                  (let ((tmp-path (get-directed-path-in-triples 
                                                (third source-trip)
                                                target 
                                                (remove-if #'(lambda (tr) (equal tr source-trip)) triple-list)
                                  )
                       ))
                     (if (and tmp-path (not path))
                         (setq path (cons source-trip tmp-path))
                     )
                  )
                )
              )
          )
        )
    )
    path
  )
)



(defun source-appears-in-triples (source triple-list)
  (find-if #'(lambda (tr) (equal (first tr) source)) triple-list)
)

(defun target-appears-in-triples (target triple-list)
  (find-if #'(lambda (tr) (equal (third tr) target)) triple-list)
)


(defun get-single-linking-triple (head tail triple-list)
  (find-if #'(lambda (tr) (and (equal head (first tr))
                               (equal tail (third tr))
                          )
             ) triple-list
  )
)

(defun get-all-source-in-triples (source triple-list)
  (remove-if-not #'(lambda (tr) (equal source (first tr))) triple-list)
)



