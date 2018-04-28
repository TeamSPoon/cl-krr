To load the CLib follow these steps:

1) Cut the lisp code at end of this file and paste it in a new 
   file. We'll call this file load-lib.lisp
2) Start Lisp.
3) Load up KM.
4) Load the file load-lib.lisp
5) Issue the following command from the LISP prompt

        (load-lib "<path>/components/")

   where <path> is the path to the components directory.
5) Start up KM and you're ready to go.

-----------------------------------------------------------------

(defun load-lib (&optional (comp-path "./components/"))
  (let (result)
    (dolist (component (traverse-directory comp-path) result)
      (cond 
	((equal (pathname-type component) "km") 
         (load-kb component)
         (setf result (cons (pathname-name component) result)))
	((equal (pathname-type component) "lisp")
	 (load component))))))

(defun traverse-directory (root-dir-path)
  (let ((dir-list (directory root-dir-path)) sub-dir-path result)
    (dolist (element dir-list result)
      (if (directory (setf sub-dir-path (concatenate 'string root-dir-path (pathname-name element) "/")))
          (setf result (append (traverse-directory sub-dir-path) result))
          (setf result (cons element result))))))

