;;
;;    $Id: load-clib-build.lisp,v 1.4 2008/02/25 16:07:08 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

;; =============================================================================
;; loads:
;; - km
;; - the specified clib build from clib-path
;; returns a list of km components loaded
(defun load-clib-build (km-path clib-path build &optional (test-comp nil))
  (let ((comp-path (concatenate 'string (string-right-trim "/" clib-path) "/")))
    (load km-path)
    (load (concatenate 'string comp-path "tools/builds.lisp"))
    (mappend #'(lambda (dir)
		 (load-dir dir test-comp)) 
	     (append (list (concatenate 'string comp-path "tools/")
			   (concatenate 'string comp-path "databases/"))
		     (mapcar #'(lambda (rel-path)
				 (concatenate 'string comp-path 
					      (string-left-trim "/" 
								(string-left-trim "components" (namestring rel-path)))))
			     (cdr (assoc build *clib-builds* :test #'string=)))))))

;; ================================================================================

;; ========================================================
;; loads all files (lisp, km, axiom) in comp-path
;; returns a list of all km files loaded 
(defun load-dir (comp-path &optional (test-comp nil))
  (setq *recursive-classification* t)
  (setq *indirect-classification*  t)
  (let ((all-components))
    (dolist (component (trav-dir-rec comp-path))
      (cond ((or (equal (pathname-type component) "km")
		 (equal (pathname-type component) "axiom"))
	     (if (or (not test-comp)
		     (and test-comp 
			  (funcall test-comp component)))
		 (progn 
		   (load-kb component)
		   (push (pathname-name component) all-components))))
	    ((or (equal (pathname-type component) "lisp")
		 (equal (pathname-type component) "lsp"))
	     (load component))))
    all-components))
;; ========================================================




;; ================================================================================
;; traverses recursively root-dir-path and lists all files
(defun trav-dir-rec (root-dir-path)
  (let ((dir-list (directory root-dir-path)) sub-dir-path result)
    (dolist (element dir-list result)
      (if (directory (setf sub-dir-path (concatenate 'string root-dir-path 
						     (pathname-name element) "/")))
          (setf result (append (trav-dir-rec sub-dir-path) result))
          (setf result (cons element result))))))
;; =================================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'mappend)
    (defun mappend (fn &rest lsts)
      (apply #'append (apply #'mapcar fn lsts)))))
