;;
;; $Id: controller-parameters.lisp,v 1.75 2009/08/25 22:00:46 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defparameter *current-step*                        1)
(defparameter *max-step*                            3)
(defparameter *openlist*                            nil)
(defparameter *closedlist*                          nil)
(defparameter *ignoredlist*                         nil)
(defparameter *queue_fn*                            'sorted-queue-insert)
(defparameter *heuristic*                           'neutral-heuristic)
;(defparameter *PRUNING-CONDITION*                   'PRUNING-CONDITION-STUB)
(defparameter *clock*                               0)
(defparameter *current-node*                        nil)
(defparameter *current-vp-inst*                     nil)
(defparameter *cpl-scenario*                        nil)
(defparameter *cpl-compute-question*                nil)
(defparameter *max-clock*                           10)
(defparameter *controller-max-depth*                16)
(defparameter *viewpoint-query-prefixes*            nil)
(defparameter *viewpoint-result-query-prefixes*     nil)
(defparameter *controller-episodic*                 nil)
(defparameter *ignore-redundant-viewpoint*          t)
(defparameter *clone-counter-list*                  nil)
(defparameter *rewrite-counter-list*                nil)
(defparameter *controller-return-all-results*       nil)
(defparameter *viewpoint-question-instance-hash*    (make-hash-table :test #'equal))
(defparameter *controller-show-unification*         nil)
(defparameter *CONTROLLER-SKOLEM-TEXTGEN-LIST*      nil)
(defparameter *CONTROLLER-TOUCHED-LIST*             nil)
(defparameter *CONTROLLER-ANSWER-VIEWPOINT*         nil)
(defparameter *CONTROLLER-PERSISTENT-HASH-MUTABLE*  nil)
;(defparameter *controller-inspection-viewpoint*     nil) ;;no longer necessary
;deprecated (defparameter *CONTROLLER-ENABLE-DEFAULT-ANSWER*    nil) ;;no longer necessary
(defparameter *CONTROLLER-EQ-HISTORY-EXPRESSIONS*   nil)
(defparameter *CONTROLLER-EQ-HISTORY-EXPLANATION*   nil)
(defparameter *CONTROLLER-EQ-HISTORY-VARIABLE-BINDINGS*   nil)

(defparameter *concept-cache*                       (make-hash-table :test #'equal))
(defparameter *user-defined-concept-prefetch-list*  nil)
(clrhash *concept-cache*)
(clrhash *viewpoint-question-instance-hash*)

(defparameter *query-frame-assoc-list*              nil)
(defparameter *query-slot-list*                     nil)

(defparameter *CONTROLLER-INPUT-COMPUTE-QUESTION*   nil)
(defparameter *CONTROLLER-INPUT-YN-QUESTION*        nil)
(defparameter *CONTROLLER-INPUT-SCENARIO*           nil)

(defparameter *CONTROLLER-NON-CPL-TRIPLES*          nil)
(defparameter *CONTROLLER-CPL-TRIPLES*              nil)

(defparameter *CONTROLLER-RULE-ENGINE-TRIPLES*      nil)
(defparameter *CONTROLLER-KB-LOCATION*              "./aura/")
(defparameter *CONTROLLER-ENABLE-RULE-ENGINE*       nil)

(defparameter *CONTROLLER-TRIPLE-INSTANCES*         nil)
(defparameter *CONTROLLER-TRIPLE-LIST*              nil)

(defparameter *CONTROLLER-UNCLONE-MAP*              nil)
(defparameter *CONTROLLER-KM-SITUATION*             '|*Global|)
;;----------------------------------------------------------------------
;; We use this variable to store all user defined concepts. This 
;; variable should only be accessed by two functions -- i.e.
;; add-user-defined-concept and initialize-user-defined-concepts.
;;----------------------------------------------------------------------
(defvar *all-user-defined-concepts* '())

(defparameter *persistent-hash* nil)

(defparameter *controller-show-viewpoint-assertion* t)

(defparameter *controller-open-domain-qa*           nil)
(defparameter *CONTROLLER-DRYRUN*                   nil)
(defparameter *CONTROLLER-CRANKED-UP*               nil)

(defparameter *pruned-vp-reason*                    nil)
(defparameter *REGRESSION-REMARKS*                  nil)

(defparameter *CONCEPTS-TO-IGNORE* '(|SHAKEN-Table| |SHAKEN-Partition| |SHAKEN-Slot-Group| |SHAKEN-Attribute-Group| |SHAKEN-Column-Content-Order-Constant| |Big-Node| |Condition-Node| |Group-Node|
				     |Property-Node| |Equation-Big-Node| |Preparatory-Event| |Trigger-Node| |Salient-Node| |Negation-Node| |SHAKEN-Table-Column| |SHAKEN-Table-Header-Column|))

;(defparameter *CONTROLLER-BACKUP-KB*                nil)
(defparameter *bad-user-defined-concepts*           nil)
(defparameter *controller-allow-alternate-w2c*      nil)

(defparameter *viewpoint-defn-lst*         '())
(defparameter *viewpoint-km-assertion-lst* '())
(defparameter *viewpoint-query-lst*        '())
(defparameter *use-heuristic-rulebase*     nil)
(defparameter *controller-use-reaction-identifier* t)

;;The criterion listed in order. 
;;1st entry has highest-priority.
;;nth entry has lowest-priority.
(defparameter *viewpoint-ranking-criteria-regular*
  '(
    ("#concepts applied"                       safe-<)
    ("concept-root-matched"                    safe->)
    ("%query-triples-matched"                  safe->)
    ("% of nodes matched"                      safe->)
    ("How well nodes matched up"               safe->)
    ("taxonomic distance for concept matched"  safe-<)
    ("assumption triples"                      safe-<)
    ("concept applied"                         string<)
))

;;The criterion listed in order. 
;;1st entry has highest-priority.
;;nth entry has lowest-priority.
(defparameter *viewpoint-ranking-criteria-forward-chaining*
  '(
    ("#concepts applied"                       safe-<)
    ("concept applied"                         string<)
))

(defvar *viewpoint-score-compute-results* nil)  ;; reset inside (reset-controller-parameters)

(defvar *hash-viewpoint-context-data* nil)      ;; htable for viewpoint instances, reset inside (reset-controller-parameters)

;;affects two things
;; a) whether concepts without any semantically-matched triples are selected
;;      affected routine (is-concept-selectable0 ...)
;; b) viewpoint ranking criteria
;;      affected routine (better-viewpoint? ...)
(defvar *bps-available-mode-list* '(regular forward-chaining))

(defvar *bps-mode* 'regular)

(defvar *bps-answer-provenance* ())

(defconstant *fix-cpl-input-procedure-list*
  '(remove-cpl-nl-triples                                ;; do this 1st
    rewrite-cpl-for-find-relationship                    ;; do this 2nd
    ;move-semantic-triples-from-scenario-to-yn-query      ;; do this 3rd
    ;install-ae-triples-for-skolem-instances-in-yn-query  ;; do this 4th
))

(defvar *controller-expand-query* t)