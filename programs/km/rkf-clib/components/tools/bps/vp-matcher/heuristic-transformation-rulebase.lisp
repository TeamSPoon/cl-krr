;;
;; $Id: heuristic-transformation-rulebase.lisp,v 1.1 2008/09/21 16:12:09 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------
;;;; FILE: transformation-rules.lisp
;;;;--------------------------------------------------------------------
;;;;--------------------------------------------------------------------

(defparameter *heuristic-transformations* '(

	;;---------------------------------------------------------------
	;; MISC. This transform addresses the bug reported in JIRA-2075.
	;;---------------------------------------------------------------
	((((|Event| . 1)          |site|           (|Spatial-Entity| . 2))
          ((|Spatial-Entity| . 2) |is-inside|      (|Spatial-Entity| . 3)))
         (((|Event| . 1)          |site|           (|Spatial-Entity| . 3))))

	;;---------------------------------------------------------------
	;; Agentive 
	;;---------------------------------------------------------------
	((((|Event| . 1)  |agent|	(|Entity| . 2)))
	 (((|Event| . 1)  |instrument|	(|Entity| . 2))))
	((((|Event| . 1)  |instrument|	(|Entity| . 2)))
	 (((|Event| . 1)  |agent|	(|Entity| . 2))))

	((((|Event| . 1)  |agent|	(|Entity| . 2)))
	 (((|Event| . 1)  |donor|	(|Entity| . 2))))
	((((|Event| . 1)  |donor|	(|Entity| . 2)))
	 (((|Event| . 1)  |agent|	(|Entity| . 2))))

	((((|Event| . 1)  |agent|	(|Entity| . 2)))
	 (((|Event| . 1)  |is-goal-of|	(|Entity| . 2))))
	((((|Event| . 1)  |is-goal-of|	(|Entity| . 2)))
	 (((|Event| . 1)  |agent|	(|Entity| . 2))))

	((((|Event| . 1)  |instrument|	(|Entity| . 2)))
	 (((|Event| . 1)  |donor|	(|Entity| . 2))))
	((((|Event| . 1)  |donor|	(|Entity| . 2)))
	 (((|Event| . 1)  |instrument|	(|Entity| . 2))))

	((((|Event| . 1)  |instrument|	(|Entity| . 2)))
	 (((|Event| . 1)  |is-goal-of|	(|Entity| . 2))))
	((((|Event| . 1)  |is-goal-of|	(|Entity| . 2)))
	 (((|Event| . 1)  |instrument|	(|Entity| . 2))))

	((((|Event| . 1)  |donor|	(|Entity| . 2)))
	 (((|Event| . 1)  |is-goal-of|	(|Entity| . 2))))
	((((|Event| . 1)  |is-goal-of|	(|Entity| . 2)))
	 (((|Event| . 1)  |donor|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Raw Material
	;;---------------------------------------------------------------
	((((|Event| . 1)  |substrate|	 (|Entity| . 2)))
	 (((|Event| . 1)  |raw-material| (|Entity| . 2))))
	((((|Event| . 1)  |raw-material| (|Entity| . 2)))
	 (((|Event| . 1)  |substrate|	 (|Entity| . 2))))

	((((|Event| . 1)  |substrate|	(|Entity| . 2)))
	 (((|Event| . 1)  |object|	(|Entity| . 2))))
	((((|Event| . 1)  |object|	(|Entity| . 2)))
	 (((|Event| . 1)  |substrate|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Event Location
	;;---------------------------------------------------------------
	((((|Event| . 1)  |site|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |path|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |path|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |site|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |site|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |destination|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |destination|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |site|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |site|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |origin|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |origin|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |site|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |path|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |destination|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |destination|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |path|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |path|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |origin|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |origin|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |path|	(|Spatial-Entity| . 2))))

	;;---------------------------------------------------------------
	;; Starting Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |away-from|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |origin|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |origin|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |away-from|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |away-from|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |base|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |base|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |away-from|	(|Spatial-Entity| . 2))))

	;;---------------------------------------------------------------
	;; End Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |toward|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |destination|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |destination|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |toward|	(|Spatial-Entity| . 2))))

	((((|Event| . 1)  |toward|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |base|	(|Spatial-Entity| . 2))))
	((((|Event| . 1)  |base|	(|Spatial-Entity| . 2)))
	 (((|Event| . 1)  |toward|	(|Spatial-Entity| . 2))))

	;;---------------------------------------------------------------
	;; Beneificiary Reference Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |beneficiary|	(|Entity| . 2)))
	 (((|Event| . 1)  |base|	(|Entity| . 2))))
	((((|Event| . 1)  |base|	(|Entity| . 2)))
	 (((|Event| . 1)  |beneficiary|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Recipient Reference Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |recipient|	(|Entity| . 2)))
	 (((|Event| . 1)  |base|	(|Entity| . 2))))
	((((|Event| . 1)  |base|	(|Entity| . 2)))
	 (((|Event| . 1)  |recipient|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Donor Reference Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |donor|	(|Entity| . 2)))
	 (((|Event| . 1)  |base|	(|Entity| . 2))))
	((((|Event| . 1)  |base|	(|Entity| . 2)))
	 (((|Event| . 1)  |donor|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Object Reference Point
	;;---------------------------------------------------------------
	((((|Event| . 1)  |base|	(|Entity| . 2)))
	 (((|Event| . 1)  |object|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Recipient/Beneficiary
	;;---------------------------------------------------------------
	((((|Event| . 1)  |beneficiary|	(|Entity| . 2)))
	 (((|Event| . 1)  |recipient|	(|Entity| . 2))))
	((((|Event| . 1)  |recipient|	(|Entity| . 2)))
	 (((|Event| . 1)  |beneficiary|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; Space/Object Interchanged.
	;;---------------------------------------------------------------
	((((|Event| . 1)  |donor|	(|Entity| . 2)))
	 (((|Event| . 1)  |origin|	(|Entity| . 2))))
	((((|Event| . 1)  |origin|	(|Entity| . 2)))
	 (((|Event| . 1)  |donor|	(|Entity| . 2))))

	((((|Event| . 1)  |recipient|	(|Entity| . 2)))
	 (((|Event| . 1)  |destination|	(|Entity| . 2))))
	((((|Event| . 1)  |destination|	(|Entity| . 2)))
	 (((|Event| . 1)  |recipient|	(|Entity| . 2))))

	;;---------------------------------------------------------------
	;; first-subevent/subevent
	;;---------------------------------------------------------------
	((((|Event| . 1)  |first-subevent|	(|Event| . 2)))
	 (((|Event| . 1)  |subevent|		(|Event| . 2))))
	((((|Event| . 1)  |subevent|		(|Event| . 2)))
	 (((|Event| . 1)  |first-subevent|	(|Event| . 2))))

	;;---------------------------------------------------------------
	;; Causality
	;;---------------------------------------------------------------
	((((|Event| . 1)  |by-means-of|		(|Event| . 2)))
	 (((|Event| . 1)  |preparatory-event|	(|Event| . 2))))
	((((|Event| . 1)  |preparatory-event|	(|Event| . 2)))
	 (((|Event| . 1)  |by-means-of|		(|Event| . 2))))

	((((|Event| . 1)  |by-means-of|		(|Event| . 2)))
	 (((|Event| . 1)  |caused-by|		(|Event| . 2))))
	((((|Event| . 1)  |caused-by|		(|Event| . 2)))
	 (((|Event| . 1)  |by-means-of|		(|Event| . 2))))

	((((|Event| . 1)  |by-means-of|		(|Event| . 2)))
	 (((|Event| . 1)  |objective-of|	(|Event| . 2))))
	((((|Event| . 1)  |objective-of|	(|Event| . 2)))
	 (((|Event| . 1)  |by-means-of|		(|Event| . 2))))

	((((|Event| . 1)  |preparatory-event|	(|Event| . 2)))
	 (((|Event| . 1)  |caused-by|		(|Event| . 2))))
	((((|Event| . 1)  |caused-by|		(|Event| . 2)))
	 (((|Event| . 1)  |preparatory-event|	(|Event| . 2))))

	((((|Event| . 1)  |preparatory-event|	(|Event| . 2)))
	 (((|Event| . 1)  |objective-of|	(|Event| . 2))))
	((((|Event| . 1)  |objective-of|	(|Event| . 2)))
	 (((|Event| . 1)  |preparatory-event|	(|Event| . 2))))

	((((|Event| . 1)  |caused-by|		(|Event| . 2)))
	 (((|Event| . 1)  |objective-of|	(|Event| . 2))))
	((((|Event| . 1)  |objective-of|	(|Event| . 2)))
	 (((|Event| . 1)  |caused-by|		(|Event| . 2))))

	;;---------------------------------------------------------------
	;; Event Objective
	;;---------------------------------------------------------------
	;; ((((|Event| . 1)  |means-by-which|	 (|Event| . 2)))
	;;  (((|Event| . 1)  |preparatory-event-of| (|Event| . 2))))
	;; ((((|Event| . 1)  |preparatory-event-of| (|Event| . 2)))
	;;  (((|Event| . 1)  |means-by-which|	 (|Event| . 2))))
	;;
	;; ((((|Event| . 1)  |means-by-which|	 (|Event| . 2)))
	;;  (((|Event| . 1)  |objective| 	 (|Event| . 2))))
	;; ((((|Event| . 1)  |objective| 	 (|Event| . 2)))
	;;  (((|Event| . 1)  |means-by-which|	 (|Event| . 2))))
	;; 
	;; ((((|Event| . 1)  |preparatory-event-of| (|Event| . 2)))
	;;  (((|Event| . 1)  |objective| 	    (|Event| . 2))))
	;; ((((|Event| . 1)  |objective| 	    (|Event| . 2)))
	;;  (((|Event| . 1)  |preparatory-event-of| (|Event| . 2))))

	;;---------------------------------------------------------------
	;; Event Order
	;;---------------------------------------------------------------
	((((|Event| . 1)  |preparatory-event| 	(|Event| . 2)))
	 (((|Event| . 1)  |prev-event| 		(|Event| . 2))))
	((((|Event| . 1)  |prev-event| 		(|Event| . 2)))
	 (((|Event| . 1)  |preparatory-event| 	(|Event| . 2))))

	;;---------------------------------------------------------------
	;; Enable/Inhibit Effect
	;;---------------------------------------------------------------
	((((|Event| . 1)  |supports| 	   (|Event| . 2)))
	 (((|Event| . 1)  |enables| 	   (|Event| . 2))))
	((((|Event| . 1)  |enables| 	   (|Event| . 2)))
	 (((|Event| . 1)  |supports| 	   (|Event| . 2))))

	((((|Event| . 1)  |interrupted-by| (|Event| . 2)))
	 (((|Event| . 1)  |inhibited-by|   (|Event| . 2))))
	((((|Event| . 1)  |inhibited-by|   (|Event| . 2)))
	 (((|Event| . 1)  |interrupted-by| (|Event| . 2))))

	;;---------------------------------------------------------------
	;; Weak Parts/Containment
	;;---------------------------------------------------------------
	((((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-part| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |possesses|  (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |possesses|  (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |material| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |possesses| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |content| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |has-region| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |encloses| 	(|Spatial-Entity| . 2))))

	;;---------------------------------------------------------------
	;; Spatial Relationships
	;;---------------------------------------------------------------
	((((|Spatial-Entity| . 1)  |abuts| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-on| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |is-on| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |abuts| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |is-beside| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-along| 	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |is-along| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-beside| 	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |is-facing| 	  (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-opposite|  (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |is-opposite|  (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-facing| 	  (|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |is-facing| 	  	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-oriented-toward| (|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |is-oriented-toward| (|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-facing| 	  	(|Spatial-Entity| . 2))))

	((((|Spatial-Entity| . 1)  |is-over| 	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-above|  	(|Spatial-Entity| . 2))))
	((((|Spatial-Entity| . 1)  |is-above|  	(|Spatial-Entity| . 2)))
	 (((|Spatial-Entity| . 1)  |is-over| 	(|Spatial-Entity| . 2))))

	;;---------------------------------------------------------------
	;; Roles
	;;---------------------------------------------------------------
	((((|Entity| . 1)  |purpose| 	 (|Role| . 2)))
	 (((|Entity| . 1)  |capability|  (|Role| . 2))))
	((((|Entity| . 1)  |capability|  (|Role| . 2)))
	 (((|Entity| . 1)  |purpose| 	 (|Role| . 2))))
))