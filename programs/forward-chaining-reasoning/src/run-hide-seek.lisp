(load "src/hide-seek.lisp")

; runs environment given times and report results
(defun run-it (times max-steps)
  (setq results NIL)
  (loop for i from 1 to times do
    (setq results (cons (hs-world-persons-remaining (run-environment (make-hs-world :max-steps max-steps))) results)))

  (print results)
  (print (apply #'+ results))
)

(run-it 50 100)
(quit)
