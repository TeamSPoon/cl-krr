(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)
; kmhacks.lsp                  Gordon S. Novak Jr.            ; 20 May 05

; Copyright (c) 2005 Gordon S. Novak Jr. and The University of Texas at Austin.
; All rights reserved.

; a few hacks to work with KM

; 11 May 05; 17 May 05

(defmacro kmpropname           (x) `(get ,x 'kmpropname))
(defmacro kmclassname          (x) `(get ,x 'kmclassname))

; translate a plain Lisp symbol to KM property (lower-case)
(defun kmifyprop (name)
  (let (new)
    (or (kmpropname name)
	(setf (kmpropname name)
	      (intern (string-downcase (symbol-name name))))) ))

; translate a plain Lisp symbol to KM class (capitalized lower-case)
(defun kmifyclass (name)
  (let (new)
    (or (and (classp name) name)
	(kmclassname name)
	(setf (kmclassname name)
	      (intern (string-capitalize (string-downcase (symbol-name name))))))
    ))

; get in km
(defun kmget (inst prop)
  (km (list '|the| (kmifyprop prop) '|of| inst)))

; make something quoted
(defun kwote (x) (if (constantp x) x (list 'quote x) ) )
