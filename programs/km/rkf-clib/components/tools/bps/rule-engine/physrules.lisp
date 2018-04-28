(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)
; physrules.lsp     Gordon S. Novak Jr. and Won Ng            ; 27 Mar 06

; Copyright (c) 2006 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; 17 Aug 05; 06 Sep 05; 13 Dec 05

; Rules for analysis and completion of physics problems

; (rldefrules physrules)

(setq physrules '(

; 06 Sep 05
; CPL handles "the height is 20 m" by creating an _X1234 = 20 m
; and making the height "equal" to _X1234.  This rule moves the value.
  (phys51  (and (class ?f 'length-value)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

; It might be better to make one rule for all kinds of values...
  (phys52  (and (class ?f 'velocity-value)   ; currently CPL is using 'speed'
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

  (phys53  (and (class ?f 'speed)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

  (phys54  (and (class ?f 'duration-value)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

  (phys55  (and (class ?f 'mass-value)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

  (phys56  (and (class ?f 'force-value)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))

  (phys57  (and (class ?f 'acceleration-value)
		(not (value ?f ?x))
		(equal ?f ?g)
		(value ?g ?h))
	   (value ?f ?h))


; This rule would not be needed if there is a rule with ground has height = 0
; and the Fall model has an equation: distance of fall = height of origin - height of destination 

  (phys101 (and (class ?f 'fall)       ; if ?f is a Fall
		(origin ?f ?p)         ;    and the origin of the fall is ?p
		(destination ?f ?d)    ;    and the destination of the fall is ?d
		(height ?p ?h)         ;    and the height of ?p is ?h
		(class ?d 'physical-object)   ;    and ?d is the ground
		(input-word ?d "ground" "bottom" "floor" "pavement" "river"))
	   (height ?f ?h) )            ; then the height of the fall is ?h

  (phys102 (and (class ?g 'ground)           ; if ?g is the ground
		(not (height ?g ?e)) )       ;    and no height of ?g is known
	   (progn (create ?h 'height-value)  ; then create a height-value ?h
		  (value ?h '(:|pair| 0 |*meter|)) ;      make the height of ?h be 0
		  (height ?g ?h)) )          ;      make the height of ?g be ?h

  (phys103 (and (class ?c 'physical-object)  ; if ?c is a cliff
		(input-word ?c "cliff" "building" "bridge" "tower")
		(height ?c ?h)           ;    and the height of ?c is ?h
		(has-region ?c ?top)       ;    and ?c has a part ?top
		(input-word ?top "top")  ;    and ?top is a "top" part
		)
	   (height ?top ?h))             ; then make the height of ?top be ?h

; this rule is needed because CPL stores velocity as speed
; surprise: this rule runs on the fall problem too!
  (phys104 (and (class ?m 'move)
		(speed ?m ?s)                ; if speed is known
		(value ?s ?sval)             ;   and a speed value is known
		(or (not (velocity ?m ?v))   ;    but not velocity
		    (not (value ?v ?val))))
	   (progn (create ?newv '|Velocity-Value|)
		  (value ?newv ?sval)
		  (velocity ?m ?newv)))     ; make velocity = speed

  (phys105 (and (class ?m 'move)
		(object ?m ?c)
		(speed ?c ?s)           ; if speed of moving object is known
		(value ?s ?sval)            ;   and a speed value is known
		(not (velocity ?m ?v)))  ;    but not velocity of move
	   (progn (create ?newv '|Velocity-Value|)
		  (value ?newv ?sval)
		  (velocity ?m ?newv)))     ; make velocity = speed

; Rules by Won added 27 Mar 06

  (phys1a (and (class ?p 'two-dimensional-move) ; if p is a two-dimensional-move
	       (not (initial-y-position ?p ?y))     ; and there is no initial-y-position specified
	       (origin ?p ?o)                       ; but there is an origin
	       (or (height ?o ?h)                   ; and the height or the fixed-y-position of the origin is specified
		   (fixed-y-position ?o ?h)))
	  (initial-y-position ?p ?h))               ; set the initial-y-position to ?h
			  
  (phys1b (and (class ?p 'throw) ; if p is a two-dimensional-move
	       (not (initial-y-position ?p ?y))     ; and there is no initial-y-position specified
	       (origin ?p ?o)                       ; but there is an origin
	       (or (height ?o ?h)                   ; and the height or the fixed-y-position of the origin is specified
		   (fixed-y-position ?o ?h)))
	  (initial-y-position ?p ?h))               ; set the initial-y-position to ?h

  (phys1c (and (class ?p 'fall) ; if p is a fall
	       (not (initial-y-position ?p ?y))     ; and there is no initial-y-position specified
	       (origin ?p ?o)                       ; but there is an origin
	       (or (height ?o ?h)                   ; and the height or the fixed-y-position of the origin is specified
		   (fixed-y-position ?o ?h)))
	  (initial-y-position ?p ?h))               ; set the initial-y-position to ?h

  ; horizontal-distance -> x-distance = final-x-position - initial-x-position
  (phys2 (and (class ?p 'two-dimensional-move)
	      (or (not (x-distance ?p ?d))
		  (not (value ?d ?v)))
	      (final-x-position ?p ?xf)      ; what if asking for x-distance and NO final-x-position is mentioned?
	      (initial-x-position ?p ?x0))
	 (progn
	   (create ?x 'length-value)
	   (value ?x '(|?xf| - |?x0|))
	   (x-distance ?p ?x)))
 ;	 (x-distance ?p (- ?xf ?x0)))       ; add equation to eqn set??

  (phys2b (and (class ?p 'throw)
	       (not (x-distance ?p ?d))
	       (final-x-position ?p ?xf)
	       (initial-x-position ?p ?x0))
	  (x-distance ?p (- ?xf ?x0)))       ; add equation to eqn set??

  ; set default x0 = 0 if no x-distance or initial x0 is specified
  (phys3 (and (class ?p 'two-dimensional-move)
	      (not (initial-x-position ?p ?x0))
	      (not (final-x-position ?p ?x))
	      (or (not (x-distance ?p ?d))
		  (not (value ?d ?v))))
	 (progn
	   (create ?x 'length-value)
	   (value ?x '(:|pair| 0 |*meter|))
	   (initial-x-position ?p ?x)))

  (phys3b (and (class ?p 'throw)
	       (not (initial-x-position ?p ?x0))
	       (not (x-distance ?p ?d)))
	  (progn
	    (create ?x 'length-value)
	    (value ?x '(:|pair| 0 |*meter|))
	    (initial-x-position ?p ?x)))

  ; gsn: 2-D move could be on horizontal plane
  ; set default destination to be ground
  (phys4 (and (class ?p 'two-dimensional-move)
	      (not (destination ?p ?d)))
	 (progn
	   (create ?g 'ground)
	   (destination ?p ?g)))

  (phys4b (and (class ?p 'fall)
	       (not (destination ?p ?d)))
	  (progn
	    (create ?g 'ground)
	    (destination ?p ?g)))

  (phys4c (and (class ?p 'throw)
	       (not (destination ?p ?d)))
	  (progn
	    (create ?g 'ground)
	    (destination ?p ?g)))

  (phys102a (and (class ?g 'ground)           ; if ?g is the ground
		 (not (height ?g ?e)) )       ;    and no height of ?g is known
	    (progn 
	      (create ?h 'length-value)  ; then create a length-value ?h
	      (value ?h '(:|pair| 0 |*meter|)) ;      make the height of ?h be 0
	      (height ?g ?h)) )          ;      make the height of ?g be ?h

	;;; Commented out by Bhal
	
  ;(phys5 (and (class ?p 'two-dimensional-move)
	;      (not (final-y-position ?p ?y))
	;      (destination ?p ?d)
	;      (or (height ?d ?h)
;		  (fixed-y-position ?d ?h)))
;	 (final-y-position ?p ?h))

  (phys5b (and (class ?p 'fall)
	       (not (final-y-position ?p ?y))
	       (destination ?p ?d)
	       (or (height ?d ?h)
		   (fixed-y-position ?d ?h)))
	  (final-y-position ?p ?h))

  (phys5c (and (class ?p 'throw)
	       (not (final-y-position ?p ?y))
	       (destination ?p ?d)
	       (or (height ?d ?h)
		   (fixed-y-position ?d ?h)))
	  (final-y-position ?p ?h))


  ; set duration = final-time - initial-time
  ; if no initial-time, should we set t0 = 0?
  (phys6 (and (class ?m 'move)
	      (not (duration ?m ?d))
	      (final-time ?m ?t))
	 (progn
	   (if (not (initial-time ?m ?t0))
	       (progn
		 (create ?t0 'duration-value)
		 (value ?t0 '(:|pair| 0 |*seconds|))
		 (initial-time ?m ?t0)))
	   (duration ?m (- ?t ?t0))))
;		   (if (initial-time ?m ?t0)
;			   (duration ?m (- ?t ?t0))
;			 (duration ?m ?t))))


#| need some rules for question instance
e.g. what is the distance (of a linear-motion) vs.
     what is the x-distance
|#

)) ; myrules
