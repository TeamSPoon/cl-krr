(load "lisp-unit.fas")
(use-package :lisp-unit)

(load "src/hide-seek.lisp")


;;; ===============================================================
;;; HELPERS
;;; ===============================================================

; Run given lambda couple of times (stress test)
(defun stress (times fn)
  (loop for i from 1 to times do (funcall fn)))

; Hack/fix - lisp-unit's fail does not fail the test...
(defun fail_ (msg)
  (assert-true nil msg))


;;; ===============================================================
;;; BASIC LOGIC UNIT TESTS
;;; - when see person, always go to pyky him
;;; - if obstacle, turn left/right (and do not turn back)
;;; - every forward step - check left/right (and turn back if no person)
;;; - sometimes change direction (not return from checking)
;;; ===============================================================

(define-test should-pyky-when-heading-to-person
  (assert-equal 'PYKY
		(decide (make-jinavojt-body) (list 'PERSON))))

(define-test should-go-forward-when-see-person
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL NIL 'PERSON)))
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL 'PERSON)))
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL NIL NIL NIL NIL NIL 'PERSON))))

(define-test should-go-forward-when-see-person-after-checking-turn
  (setq body (make-jinavojt-body))
  (decide body (list NIL NIL 'WALL)) ; should go forward
  (decide body (list NIL 'WALL)) ; should check left/right
  (assert-equal 'FORW (decide body (list NIL NIL NIL 'PERSON))))

(define-test should-turn-when-heading-to-obstacle
  (stress 10 (lambda ()
    (let ((decision (decide (make-jinavojt-body) (list 'BUSH))))
      (assert-true (or (eq decision 'TURNLEFT) (eq decision 'TURNRIGHT))))

    (let ((decision (decide (make-jinavojt-body) (list 'WALL))))
      (assert-true (or (eq decision 'TURNLEFT) (eq decision 'TURNRIGHT))))))
)

(define-test should-not-return-after-turning-from-obstacle
  (stress 10 (lambda ()
    (let ((body (make-jinavojt-body)))
      (case (decide body (list 'BUSH))
        ('TURNLEFT (assert-false (eq (decide body (list 'BUSH)) 'TURNRIGHT)))
        ('TURNRIGHT (assert-false (eq (decide body (list 'BUSH)) 'TURNLEFT)))
        (otherwise (fail_ "Should turn left/right when facing obstacle"))
      ))))
)
; todo: it should turn any direction next step after turning because of obstacle

(define-test should-check-left-or-right-every-forward-step
  (stress 10 (lambda ()
    (setq body (make-jinavojt-body))
    (decide body (list nil nil 'WALL)) ; should go forward
    (let ((decision (decide body (list NIL NIL 'WALL))))
      (assert-true (or (eq decision 'TURNLEFT) (eq decision 'TURNRIGHT))))))
)

(define-test should-return-from-checking-or-stay-sometimes-when-free-space
  (setq left-returns 0
	right-returns 0
	go-forws 0)
  
  (stress 20 (lambda ()
    (setq body (make-jinavojt-body))
    (decide body (list nil nil 'WALL)) ; should go forward
    (let* ((decision1 (decide body (list NIL NIL 'WALL)))
	  (decision2 (decide body (list NIL NIL 'WALL))))
      (cond
	((and (eq decision1 'TURNLEFT) (eq decision2 'TURNRIGHT))
	 (incf left-returns))
	((and (eq decision1 'TURNRIGHT) (eq decision2 'TURNLEFT))
	 (incf right-returns))
	((eq decision2 'FORW)
	 (incf go-forws))
	(T (fail_ "Should return back or go forward when heading to free space")))
      )))
  (assert-true (<= 1 left-returns))
  (assert-true (<= 1 right-returns))
  (assert-true (<= 1 go-forws))
)

(define-test should-return-from-checking-when-obstacle
  (stress 10 (lambda ()
    (setq body (make-jinavojt-body))
    (decide body (list NIL NIL 'WALL)) ; should go forward
    (case (decide body (list NIL NIL 'WALL)) ; should check left/right
      ; should return back, there is an obstacle
      ('TURNLEFT (assert-equal 'TURNRIGHT (decide body (list 'WALL))))
      ('TURNRIGHT (assert-equal 'TURNLEFT (decide body (list 'WALL))))
      (otherwise (fail_ "Should check left or right after each forw step")))))
)

(define-test see-person?
  (assert-true (see-person? (list NIL 'PERSON)))
  (assert-true (see-person? (list NIL NIL NIL 'PERSON)))
  (assert-false (see-person? (list NIL NIL 'WALL)))
  (assert-false (see-person? (list NIL 'BUSH)))
  (assert-false (see-person? (list NIL NIL NIL NIL NIL NIL NIL 'WALL)))
)


;;; ===============================================================
;;; loc-on-left
;;; loc-on-right
;;; ===============================================================

(define-test loc-on-*-should-return-location-object
  (assert-true (xy-p (loc-on-left (@ 3 3) '(0 1))))
  (assert-true (xy-p (loc-on-right (@ 2 3) '(1 0))))
)

(define-test loc-on-*-should-not-mute-original-location
  (let* ((orig-loc (@ 4 5))
         (new-loc (loc-on-left orig-loc '(0 1))))
    (assert-false (eq new-loc orig-loc)))
  (let* ((orig-loc (@ 4 5))
         (new-loc (loc-on-right orig-loc '(1 0))))
    (assert-false (eq new-loc orig-loc)))
)

(define-test loc-on-left
  (assert-equal '(2 4) (loc-on-left (@ 3 4) '(0 1)))
  (assert-equal '(2 4) (loc-on-left (@ 2 3) '(1 0)))
)

(define-test loc-on-right
  (assert-equal '(6 5) (loc-on-right (@ 5 5) '(0 1)))
  (assert-equal '(5 4) (loc-on-right (@ 5 5) '(1 0)))
)


;;; ===============================================================
;;; MAPPING
;;; what-is-on-loc?
;;; learn
;;; ===============================================================

(define-test what-is-on-loc-should-set-the-borders-to-obstacles
  (setq map (jinavojt-body-map (make-jinavojt-body)))
  ; assuming 10x10
  (assert-equal 'SEEN (what-is-on-loc? (@ 0 0) map NIL))
  (assert-equal 'SEEN (what-is-on-loc? (@ 5 0) map NIL))
  (assert-equal 'SEEN (what-is-on-loc? (@ 0 8) map NIL))
  (assert-equal 'SEEN (what-is-on-loc? (@ 9 9) map NIL))
  (assert-equal 'SEEN (what-is-on-loc? (@ 9 3) map NIL))
)

(define-test learn-should-learn-wall
  (setq body (make-jinavojt-body :loc (@ 1 1) :heading '(1 0)))
  (learn body (list NIL NIL 'WALL))
  (assert-equal 'SEEN (aref (jinavojt-body-map body) 4 1)))

(define-test learn-should-learn-person
  (setq body (make-jinavojt-body :loc (@ 1 2) :heading '(0 1)))
  (learn body (list NIL NIL NIL 'PERSON))
  (assert-equal 'SEEN (aref (jinavojt-body-map body) 1 6)))

(define-test learn-should-learn-bush
  (setq body (make-jinavojt-body :loc (@ 8 3) :heading '(0 -1)))
  (learn body (list NIL 'BUSH))
  (setf (jinavojt-body-loc body) (@ 7 1))
  (assert-equal 'HOPE (what-is-on-left? body))
  (setf (jinavojt-body-loc body) (@ 9 1))
  (assert-equal 'HOPE (what-is-on-right? body))
  (setf (jinavojt-body-loc body) (@ 8 2))
  (setf (jinavojt-body-heading body) '(1 0))
  (assert-equal 'SEEN (what-is-on-right? body))
)

(define-test remove-heading-from-list
  (unordered-equal (list '(0 -1) '(1 0))
		   (remove-heading-from-list (list '(0 1) '(0 -1) '(1 0)) '(0 1)))
  (unordered-equal (list '(0 1) '(0 -1))
		   (remove-heading-from-list (list '(1 0) '(0 1) '(0 -1)) '(1 0)))
)

(define-test remove-heading-from-list-should-init-if-null
  (unordered-equal (list '(0 1) '(1 0) '(-1 0))
		   (remove-heading-from-list NIL '(0 -1)))
)

(define-test hope-on-*-should-check-first-known-in-direction
  (setq body (make-jinavojt-body)
	map (jinavojt-body-map body))
  (setf (aref map 1 1) 'HOPE)
  (setf (object-loc body) (@ 5 1))
  (setf (object-heading body) (@ 0 1))

  (assert-true (hope-on-left? body))
  (assert-false (hope-on-right? body))
)

;;; ===============================================================
;;; HIGHER LEVEL TESTS
;;; using learned (mapped knowledge) - SEEN, NIL, HOPE
;;; ===============================================================

(defun fake-step (env action)
  (setq agent (first (environment-agents env)))
  (setf (agent-percept agent) (get-percept env agent))
  (setf (agent-action agent) action)
  (learn (first (agent-percept agent)) (rest (agent-percept agent)))
  (update-fn env)
  (setf (agent-score agent) (performance-measure env agent))
)

(defun fake-decide (env)
  (setq agent (first (environment-agents env))
        percept (get-percept env agent)
	body (first percept)
	perc (rest percept))
  (learn body perc)
  (decide body perc)
)

(defun create-fake-env (id)
  (case id
    ; env for testing facing obstacle + HOPE
    (1 (setq env (make-hs-world :max-steps 10
				:start (@ 3 1)
				:bspec '((at edge WALL)
					 (at (2 3) BUSH)
					 (at (4 3) BUSH)
					 (at (3 2) BUSH)))))
    ; env for testing facing obstacle + SEEN
    (2 (setq env (make-hs-world :max-steps 10
				:start (@ 2 2)
				:bspec '((at edge WALL)
					 (at (1 2) BUSH)
					 (at (3 2) BUSH)
					 (at (2 3) BUSH)))))
    ; env for testing facing free + HOPE
    (3 (setq env (make-hs-world :max-steps 10
				:start (@ 4 1)
				:bspec '((at edge WALL)
					 (at (4 2) BUSH)))))
    ; env for testing facing free + SEEN
    (4 (setq env (make-hs-world :max-steps 10
				:start (@ 3 2)
				:bspec '((at edge WALL)
					 (at (3 3) BUSH)))))
    ; env for testing facing free + SEEN on both sides
    (5 (setq env (make-hs-world :max-steps 10
				:start (@ 3 2)
				:bspec '((at edge WALL)
					 (at (2 2) BUSH)
					 (at (4 2) BUSH)))))
    ; env for testing HOPE on next known on left/right
    (6 (setq env (make-hs-world :max-steps 10
				:start (@ 4 1)
				:bspec '((at edge WALL)
					 (at (4 2) BUSH)))))

  )
  (initialize env)
  (setq agent (first (environment-agents env)))
  (setf (agent-body-heading (agent-body agent)) (@ 0 1))
  (values env)
)

(define-test should-always-turn-left-when-facing-obstacle-and-hope-is-on-left
  (stress 10 (lambda ()
    (setq env (create-fake-env 1))
    (fake-step env 'LEFT)
    (fake-step env 'FORW)
    (assert-equal 'TURNRIGHT (fake-decide env))))
  ;(display-environment env)
)

(define-test should-always-turn-right-when-facing-obstacle-and-hope-is-on-right
  (stress 10 (lambda ()
    (setq env (create-fake-env 1))
    (fake-step env 'RIGHT)
    (fake-step env 'FORW)
    (assert-equal 'TURNLEFT (fake-decide env)))))

(define-test should-always-turn-left-when-facing-obstacle-and-seen-is-on-right
  (stress 10 (lambda ()
    (setq env (create-fake-env 2))
    (fake-step env 'TURNLEFT)    
    (assert-equal 'TURNLEFT (fake-decide env)))))

(define-test should-always-turn-right-when-facing-obstacle-and-seen-is-on-left
  (stress 10 (lambda ()
    (setq env (create-fake-env 2))
    (fake-step env 'TURNRIGHT)
    (assert-equal 'TURNRIGHT (fake-decide env)))))

(define-test should-always-turn-right-when-facing-free-and-hope-is-on-right
  (stress 10 (lambda ()
    (setq env (create-fake-env 3))
    (fake-step env 'LEFT)
    (fake-step env 'FORW)
    (assert-equal 'TURNRIGHT (fake-decide env)))))

(define-test should-always-turn-left-when-facing-free-and-hope-is-on-left
  (stress 10 (lambda ()
    (setq env (create-fake-env 3))
    (fake-step env 'RIGHT)
    (fake-step env 'FORW)
    (assert-equal 'TURNLEFT (fake-decide env)))))

(define-test should-always-check-right-when-clean-and-seen-is-on-left
  (stress 10 (lambda ()
    (setq env (create-fake-env 4))
    (fake-step env 'TURNLEFT)
    (setf (jinavojt-body-in-action (agent-body (first (environment-agents env))))
	  'CLEAN)
    (assert-equal 'TURNLEFT (fake-decide env)))))

(define-test should-always-check-left-when-clean-and-seen-is-on-right
  (stress 10 (lambda ()
    (setq env (create-fake-env 4))
    (fake-step env 'TURNRIGHT)
    (setf (jinavojt-body-in-action (agent-body (first (environment-agents env))))
	  'CLEAN)
    (assert-equal 'TURNRIGHT (fake-decide env)))))

(define-test should-never-check-left-or-right-when-both-seen
  (stress 10 (lambda ()
    (setq env (create-fake-env 5))
    (fake-step env 'TURNRIGHT)
    (fake-step env 'TURNRIGHT)
    (fake-step env 'TURNRIGHT)
    (fake-step env 'TURNRIGHT)
    (setf (jinavojt-body-in-action (agent-body (first (environment-agents env))))
	  'CLEAN)
    (assert-equal 'FORW (fake-decide env)))))

(define-test should-always-check-when-hope-on-further-right
  ; even if hope is not on next, could be further
  (stress 10 (lambda ()
    (setq env (create-fake-env 6))
    (fake-step env 'LEFT)
    (fake-step env 'LEFT)
    (fake-step env 'LEFT)
    (fake-step env 'FORW)
    (assert-equal 'TURNRIGHT (fake-decide env)))))

(define-test should-always-check-when-hope-on-further-left
  ; even if hope is not on next, could be further
  (stress 10 (lambda ()
    (setq env (create-fake-env 6))
    (fake-step env 'RIGHT)
    (fake-step env 'RIGHT)
    (fake-step env 'RIGHT)
    (fake-step env 'FORW)
    (assert-equal 'TURNLEFT (fake-decide env)))))
