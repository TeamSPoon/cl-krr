(load "lisp-unit.fas")
(use-package :lisp-unit)

(load "src/load-aima.lisp")
(load "src/blind-seek.lisp")

(define-test hide-seek-basic
  (assert-equal 1 1 )
)
