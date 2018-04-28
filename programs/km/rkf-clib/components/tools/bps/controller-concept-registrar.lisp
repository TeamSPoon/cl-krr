;;
;; $Id: controller-concept-registrar.lisp,v 1.18 2008/10/25 02:53:36 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun add-user-defined-concept (user-defined-concept)
  (progn
    (if (null (member user-defined-concept *all-user-defined-concepts*))
	(push user-defined-concept *all-user-defined-concepts*))
    (sort-user-defined-concept-list)
    (if *CONTROLLER-PERSISTENT-HASH-MUTABLE*
	(progn 
	  (invalidate-cache-entry user-defined-concept)
	  (clean-taxonomy)
	  (save-user-defined-concept-cache)))))

(defun sort-user-defined-concept-list()
  (if *CONTROLLER-CRANKED-UP*
      (setf *all-user-defined-concepts* 
	    (sort-concepts-by-superclass-count
	     (remove-duplicates 
	      *all-user-defined-concepts*)))))

;;Sorts concept list by their number of superclass count.
(defun sort-concepts-by-superclass-count(concept-list)
  (sort concept-list 'less-superclasses-p))

;;Returns true if concept1 has more superclasses than concept2.
(defun less-superclasses-p(concept1 concept2)
  (< (length (all-superclasses concept1))
     (length (all-superclasses concept2))))

(defun ps-note(concept)
  (progn 
    (add-user-defined-concept concept)
    (format t "Concept list: ~a~%" *all-user-defined-concepts*)))

(defun ps-ignore(concept)
  (progn 
    (setf *all-user-defined-concepts*
	  (remove-if #'(lambda (target) (eql target concept))
		     *ALL-USER-DEFINED-CONCEPTS*))
    (format t "Concept list: ~a~%" *all-user-defined-concepts*)))

;;De-register a concept from BPS usage
(defun remove-user-defined-concept (user-defined-concept)
  (progn 
    (invalidate-cache-entry user-defined-concept)
    (save-user-defined-concept-cache)
    (ps-ignore user-defined-concept)))

;;Returns list of user defined concepts
(defun get-all-user-defined-concepts ()
  *ALL-USER-DEFINED-CONCEPTS*)

;;Returns list of pump-primed concepts
(defun get-ut-pump-priming-concepts ()
  (union *phys-halo-pump-prime-list*
	 (union *chem-halo-pump-prime-list*
		*bio-halo-pump-prime-list*)))

;;Registers all user defined concepts into CPL's lexicon
(defun register-user-defined-concepts-with-CPL()
  (dolist (item *all-user-defined-concepts*)
    (progn
      (format t "Adding ~a into CPL's lexicon~%" item)
      (add-user-concept-to-lexicon item)
      (read-wn20-synsets item)
      )))

;;Cache user concepts and reset to specified list (for debugging)
;;(by always appending to the cache var, the car of the cache var
;;will always contain the first cache, which is the original list)
(defvar *hold-all-user-defined-concepts* '())
(defun force-user-concepts (concept-list)
  (setq *hold-all-user-defined-concepts* 
        (append *hold-all-user-defined-concepts* (list *all-user-defined-concepts*))
  )
  (setq *all-user-defined-concepts* concept-list)
)

(defun reset-user-concepts ()
  (setq *all-user-defined-concepts* (car *hold-all-user-defined-concepts*))
)


