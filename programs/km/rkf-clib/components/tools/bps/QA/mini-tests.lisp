;;
;; $Id: mini-tests.lisp,v 1.2 2006/06/21 18:54:11 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun chem-mini-test-suite()
  (progn
    (format t "Running mini testsuite for chemistry~%")
    (ps-iterate-question-list-output-to-filename "./mini-chem-results.html" "Selected Chemistry Questions." *MINI-CHEM-QUESTIONS*)))

(defun phys-mini-test-suite()
  (progn
    (format t "Running mini testsuite for physics~%")
    (ps-iterate-question-list-output-to-filename "./mini-phys-results.html" "Selected Physics Questions." *MINI-PHYS-QUESTIONS*)))

(defun bio-mini-test-suite()
  (progn
    (format t "Running mini testsuite for biology~%")
    (ps-iterate-question-list-output-to-filename "./mini-bio-results.html" "Selected Biology Questions." *MINI-BIO-QUESTIONS*)))

(setf *mini-bio-questions*
'(
  "What are the parts of a cell?"                                      ;; <-- Multi-Slot
  "What are the similarities between an animal cell and a plant cell?" ;; <-- Sim
  "What are the differences between an animal cell and a plant cell?"  ;; <-- Diff
  "What is a chromatin?"                                               ;; <-- What is a
  "Is it true that a chloroplast is part of a plant cell?"             ;; <-- Y/N
))

(setf *mini-phys-questions* 
'(
  "There is a move. 
   The distance of the move is 100 m. 
   The duration of the move is 5 s. 
   What is the average velocity of the move?"
  
  "There is a move. 
   The duration of the move is 6 s. 
   The initial velocity of the move is 12 m/s. 
   The final velocity of the move is 25 m/s. What is the distance of the move?"

  "A plane is moving. 
   The initial velocity of the moving is 0 m/s. 
   The final velocity of the moving is 30 m/s. 
   The acceleration of the moving is constant. 
   The acceleration of the moving equals 3.0 m/s^2. 
   What is the distance of the moving?"
))

(setf *mini-chem-questions* '(

"A mixture is the result of a mix. 
 The volume of an h2o substance is 2 liters. 
 The volume of an nh3 substance is 3 liters. 
 The h2o substance is the first reactant of the mix. 
 The nh3 substance is the second reactant of the mix. 
 What is the volume of the mixture?"

"A H2 Substance and A O2 Substance react. 
 The result of the reaction is a H2O Substance. 
 What is the chemical equation of the reaction?"

"A hydrogen and an oxygen react. 
 What is the result of the reaction?"

"A Na Substance and a H2O Substance react. 
 The first result of the reaction is a NaOH Substance. 
 The second result of the reaction is a H2 Substance. 
 What is the chemical equation of the reaction?"

"A Fe and a O2 react. 
 What is the result of the reaction?"

"What is the difference between a strong electrolyte and a weak electrolyte?"

"What is the example of an acid?"

"What is a monoprotic acid?"
))

