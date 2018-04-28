(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)

(unless (fboundp 'with-standard-aura-parameters)
  (defmacro with-standard-aura-parameters (() &body body)
    `(progn
       (format t "Using stubby (with-standard-aura-parameters) macro. SRI's version located in the-back-end/tools/km-parameter.lisp is not used here.~%")
       ,@body)))