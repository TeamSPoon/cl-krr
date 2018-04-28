;;
;; $Id: system.lisp,v 1.105 2009/06/22 18:41:25 jchaw Exp $
;;
;; Build configuration information for SRI to correctly build Aura.
;;

;;; -*- Mode: LISP; Package: CL-USER -*-

(in-package "CL-USER")

(excl:defsystem clib-tools
 (:default-pathname "aura:clib;contents;components;tools;")
 (:serial clib-bps
          "Conjugate"
          "balance-eq"
          "balancing"
          "builds"
	  "chem-defs"
          "reaction-defs"
          "chemical-api"
	  "new-chem-api"
          "chf-util"
          "conversion"
          "get-ch-ent"
          "get-db"
          "keq-calc"
          "make-string-name"
          "naming"
          "solver"
          "spec-filename"
	  "sub-matrices"
          "unitless-value-utilities"
          "user-defined-arithmetic-operators"
          "user-defined-operators"
          "user-defined-set-aggregation-functions"
          "valuecheck"
	  "valid-equation-expression-predicate"
	  "text-description-routines"
          "novak-UoM-system"
          "novak-equation-solver"
	  "find-example-for-concept"
	  "load-clib-build"
	  "km-patch-clib.lisp"
	  "same-as-operators"
	  ))

(excl:defsystem clib-databases
 (:default-pathname "aura:clib;contents;components;databases;")
 (:serial ("allelements")))

(excl:defsystem clib-wordnet
 (:default-pathname "aura:clib;contents;")
 (:serial ("clib-index-data" (:package "CLIB"))))

(excl:defsystem clib-bps
 (:default-pathname "aura:clib;contents;components;tools;bps;")
 (:serial           
  clib-utility
  "controller-macros"
  "ps-get-path"
  "ps-gather-graph"
  "controller-1st-viewpoint"
  "controller-ancestry"
  "controller-concept-caching"
  "controller-concept-registrar"
  "controller-expand"
  "controller-feedback"
  "controller-km-bridge"
  "controller-aura-bridge"
  "controller-frontend"
  "controller-knowledge-provenance"
  "controller-miscellaneous"
  "controller-multislot-query"
  "controller-mutate-operations"
  "controller-parameters"
  "controller-pruning-conditions"
  "controller-quasi-km-bridge"
  "controller-reify-operations"
  "controller-search"
  "controller-skolem-predicate"
  "controller-stepper"
  "controller-viewpoint-clustering"
  "controller-viewpoint-evolution"
  "controller-viewpoint-querying"
  "controller"
  "expl-index"
  "kb-instance-cloning-routines"
  "km-assertion-ordering-routines"
  "normalize-triple-list"
  "pruning-conditions"
  "query-path-related-routines"
  "question-extraction"
  "triple-list-manipulation-routines"
  "triple-list-ablation-routines"
  "viewpoint-accessor-routines"
  "assumption-extractor"
  "viewpoint-compare"
  "viewpoint-count-question"
  "viewpoint-explanation-question"
  "viewpoint-manipulation-routines"
  "viewpoint-mutator-routines"
  "viewpoint-related-to"
  "viewpoint-what-am-i-question"
  "viewpoint-yn"
  "write-out-sme-kb"
  "subgoal-identification"
  "ps-query-expansion"
  "sme-concept-tester"
  "alternative-w2c-output"
  "viewpoint-generation"
  "check-non-semantic-matching-triples"
  "cpl-output-handling-code"
  "clib-utilities"
  "abstract-kb-web"
  "spreading-activation-code"
  "stitch-triple-list"
  "inspection-scenario-checker"
  "alpha-works"
  "dead-code"
  "provenance-code"
  clib-QA
  clib-eq-solver
  clib-rule-engine
  clib-vp-matcher
  clib-debugger
  clib-loosespeak
  clib-data
  ))

(excl:defsystem clib-data
 (:default-pathname "aura:clib;contents;components;tools;bps;data;")
 (:serial "physics-clib-concept-cache"
	  "chemistry-clib-concept-cache"
	  "biology-clib-concept-cache"))

(excl:defsystem clib-loosespeak
 (:default-pathname "aura:clib;contents;components;tools;bps;loosespeak;")
 (:serial "ls"
	  "loosespeak"))

(excl:defsystem clib-utility
 (:default-pathname "aura:clib;contents;components;tools;bps;utility;")
 (:serial "persistent-hash"
	  "misc"
	  "write-html-table"))

(excl:defsystem clib-rule-engine
 (:default-pathname "aura:clib;contents;components;tools;bps;rule-engine;")
 (:serial "kmhacks"
          "rules"
          "physrules"))

(excl:defsystem clib-QA
 (:default-pathname "aura:clib;contents;components;tools;bps;QA;")
 (:serial "what-is-question-type"
	  "regression-test-routines"
	  "regression-parameters"
	  "mini-tests"
	  "physics-regression-test-questions"
	  "biology-regression-test-questions"
	  "chemistry-regression-test-questions"
	  "halo2-windows-tester"
	  "halo2-tester-report-generator"
	  "halo2-snapshot-diff"))

(excl:defsystem clib-eq-solver
 (:default-pathname "aura:clib;contents;components;tools;bps;eq-solver;")
 (:serial "eq-pretty-print"
	  "eq-solver-explanation"
	  "sri-eq-solver-wrapper-code"
          "eq-solver-interface"	  
	  "inspect-eq-set"
	  "eq-solver-atan-hack"
	  "preferred-uom-code"
	  ))

(excl:defsystem clib-vp-matcher
 (:default-pathname "aura:clib;contents;components;tools;bps;vp-matcher;")
 (:serial "pump-priming-list"
	  "heuristic-transformation-rulebase"
	  "sound-transformation-rulebase"
	  "ps-semantic-matcher"
	  "ps-perform-semantic-match"
	  "sibling-matcher"
          "viewpoint-matcher"
	  "viewpoint-scoring"
          "whynot-util"
	  "reasonable-elaboration-predicates"
	  "concept-selector"
	  "matching-routines"
	  "semantic-matcher-auxiliary-routines"
	  "ps-kb-match"
	  "ps-triple-match"
	  "matching-map"
	  ))


(excl:defsystem clib-debugger
 (:default-pathname "aura:clib;contents;components;tools;bps;debugger;")
 (:serial "debugger-adhoc-mode"
          "debugger-apply-command"
          "debugger-group-by-command"
          "debugger-help-command"
          "debugger-list-command"
          "debugger-parameters"
          "debugger-query-command"
          "debugger-see-command"
          "debugger-set-command"
          "debugger-stepper-commands"
          "debugger-tree-printer"
          "debugger-watch-command"
          "debugger-whynot-command"
	  "debugger-why-command"
          "debugger-find-similar-command"
          "debugger"
	  "write-viewpoint-context"
	  "debugger-problem-solving-report"
	  "debugger-sri-api"
	  ))

;;; system.lisp EOF
