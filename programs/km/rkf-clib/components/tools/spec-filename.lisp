;;
;;    $Id: spec-filename.lisp,v 1.26 2005/11/14 19:14:52 tecuci Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun km-filepath (comp-name &key (build "all") (type "km") (path-to-build-root #p"") path-key (hash-name nil))
  (let* ((relative-path (if path-key
			    (funcall path-key comp-name build path-to-build-root)
			  (find-km-path comp-name build path-to-build-root)))
	 (km-filepath (when relative-path
			(make-pathname :name comp-name :type type
				       :defaults (merge-pathnames relative-path path-to-build-root)))))
    (if hash-name (setf (gethash comp-name hash-name) km-filepath))
    km-filepath))

;; returns the path to the spec file correspoding to filepath
(defun spec-filepath (comp-name &key (build "all") (type "kml") (path-to-build-root #p"") path-key (hash-name nil))
  (let* ((path (if path-key
		   (funcall path-key comp-name build path-to-build-root)
		 (find-km-path comp-name build path-to-build-root)))
	 ;;(comp-symbol (intern (string-left-trim "*" comp-name) :km))
	 (comp-symbol (intern comp-name :km))
	 (spec-filepath (when path
			  (merge-pathnames
			   (make-pathname :name (concatenate 'string (spec-file comp-symbol) ".spec") :type type
					  :directory '(:relative "specs"))
			   (merge-pathnames path path-to-build-root)))))
    (if hash-name (setf (gethash comp-name hash-name) spec-filepath))
    spec-filepath))

(defun find-km-path (comp build path-to-build-root)
  (if (comp-is-instance comp)
      (find-km-path (string (first (km0 `(|the| |instance-of| |of| ,(INTERN COMP :KM))))) 
		    build path-to-build-root)
    (let ((candidates (cdr (assoc build *clib-builds* :test #'string=))))
      (dolist (candidate candidates)
	(let ((comp-in-candidate
	       (merge-pathnames (merge-pathnames (make-pathname :name comp :type "km") candidate)
				path-to-build-root)))
	  (if (string= comp '|Thing|)
	      (return-from find-km-path candidate)
	    (when (probe-file comp-in-candidate)
	      (return-from find-km-path candidate))))))))

;; returns the name of the documentation file that corresponds to comp
;; e.g. spec-filename('|age|) -> "Age-Value.spec.kml"

(defun spec-filename (comp &optional (type "kml"))
  (make-pathname :name (concatenate 'string (spec-file comp) ".spec")
                 :type type))

;; returns the name of the file for the spec of comp should point to
;; the returned value has to be concatenated with ".spec.kml"
(defun spec-file (comp)
  (cond 

   ;; this is to override other options
   ((first (km0 `(|the| |doc-file| |of| ,comp)))
    (first (km0 `(|the| |doc-file| |of| ,comp))))

   ;; property instance
   ((or (km0 `(((|the| |instance-of| |of| ,comp) |includes| |Property|)))
	(km0 `((|the| |instance-of| |of| ,comp) |includes| |EntityProperty|))
	(km0 `((|the| |instance-of| |of| ,comp) |includes| |EventProperty|))
	(km0 `((|the| |instance-of| |of| ,comp) |includes| |PropertyProperty|)))
    (string comp))
   
   ;; for Prop-Values with only one range
   ((and (km0 `(((|the| |superclasses| |of| ,comp) |includes| |Property-Value|)))
	 (km0 `((|the| |number| |of| (|the| |range-of| |of| ,comp)) = 1)))
    (string (car (km0 `(|the| |range-of| |of| ,comp)))))
   
   ;; constant class
   ((km0 `((|the| |superclasses| |of| ,comp) |includes| |Constant|))
    (let ((scalar-const-class-of (km0 `(|the| |scalar-constant-class-of| |of| ,comp)))
	  (categorical-const-class-of (km0 `(|the| |categorical-constant-class-of| |of| ,comp))))
      (cond (scalar-const-class-of (string (car scalar-const-class-of)))
	    (categorical-const-class-of (string (car categorical-const-class-of))))))
   
   ;; constant instance
   ((first (km0 `(,comp |isa| |Constant|)))
    (let ((inst-of (km0 `(|the| |instance-of| |of| ,comp))))
      (spec-file (if (listp inst-of) (car inst-of) inst-of))))
   
   ;; scale class
   ((km0 `((|the| |superclasses| |of| ,comp) |includes| |Scale|))
    (string (first (km0 `(|the| |scale-class-of| |of| ,comp)))))
   
   ;; scale instance
   ((first (km0 `(,comp |isa| |Scale|)))
    (string (first (km0 `(|the| |scale-class-of| |of| (|the| |instance-of| |of| ,comp))))))

   (t (string comp))))


;; detects instances
;; useful as instances don't have their own km files
(defun comp-is-instance (comp)
  (char= #\* (char comp 0)))