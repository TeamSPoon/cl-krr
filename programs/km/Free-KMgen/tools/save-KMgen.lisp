(in-package :km)

;;;; save-KMgen is a function that saves a KB to a KM file suitable for KMgen import function.
;;;; Like save-kb and others, save-kmgen is a function that functions both
;;;; at the KM and Lisp prompt.
;;;; Load this file after KM.

;;; Name: a symbol denoting a function
(defun add-lisp&KM-function (name)
  (pushnew name *km-lisp-exprs*)
  (pushnew (intern (string-downcase name) *km-package*)
           *downcase-km-lisp-exprs*
           :test #'string=))

(add-lisp&KM-function 'save-kmgen)


;;; adapted from the save-kb KM function
(defun save-kmgen (file)
  (let ((stream (tell file)))
    (write-kmgen stream)
    (close stream)
    (format t "~&~a saved!~%" file) t))	

;;; adapted from the write-kb KM function
(defun write-KMgen (&optional (stream *standard-output*) (objects (get-all-objects))
                              situations0)
  (cond 
   ((and (not (streamp stream))
	 (not (eq stream t)))
    (report-error 'nodebugger-error 
		  "write-KMgen given a non-stream as an argument!
 Use (save-KMgen \"myfile\") to save KB to the file called \"myfile\".~%"))
   (t (let ((situations (or situations0 (all-situations))))
	(multiple-value-bind
            (concepts comment-tags)
            (sort-objects-for-writing objects)
          (mapc
           (lambda (concept)
             (unless (bound concept)
               (cond ((is-an-instance concept) 
                      (cond ((isa concept '#$Situation)
                             (princ ":SITUATION-INSTANCE" stream))
                            ((isa concept '#$Slot)
                             (princ ":SLOT-INSTANCE" stream))
                            ((isa concept '#$Theory)
                             (princ ":THEORY-INSTANCE" stream))
                            (t (princ ":INSTANCE" stream))))
                     ((km0 `#$((the-class Situation) subsumes (the-class ,CONCEPT)))
                      (princ ":SITUATION-CLASS" stream))
                     ;; (the-class X) subsumes (the-class Y) doesn't work with calls
                     ;; of undefined funtion (eg with rkf-clib-one without installing
                     ;; the .lisp files)
                     ((km0 `#$((the-class Slot) subsumes (the-class ,CONCEPT)))
                      (princ ":SLOT-CLASS" stream))
                     ((km0 `#$((the-class Theory) subsumes (the-class ,CONCEPT)))
                      (princ ":THEORY-CLASS" stream))
                     (t (princ ":CLASS" stream)))
               (terpri stream)
               (princ (write-frame concept :situations situations :nulls-okayp t) stream)))
           concepts)
          ;; Useful for importing Comments of Clibone in KMGen. Put all comments in One frame
          (km-format stream ":CLASS
		 ~%(Comments has  ~%(superclasses (Thing )) ~% (members ((")
          (mapc #'(lambda (comment-tag)
                    (km-format stream "~a~%~%"
                               `(#$comment ,comment-tag ,@(get comment-tag 'comment)))
                    (terpri stream))
                comment-tags)
          (km-format stream "))))")
          )))))



