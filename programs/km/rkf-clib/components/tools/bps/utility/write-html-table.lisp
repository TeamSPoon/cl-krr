;;
;; $Id: write-html-table.lisp,v 1.1 2008/04/08 20:20:26 jchaw Exp $
;;

(unless (find-package :km) (make-package :km))
(in-package :km)
(setq *using-km-package* t)

(defun write-table(filename header-lst data)
  (let ((tmp-filename (gen-temp-filename)))
    (with-open-file (stream tmp-filename
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "<HTML><BODY>")
      (format stream "created at ~a, ~a entries.<BR><BR>" (get-time-of-day) (length data))
      ;(format stream "~a<BR><BR>" (generate-index-for-q-number-list q-number-list))
      (format stream "<TABLE BORDER=\"1\">")
      (format stream "<TR><TH width=\"50%\">")
      (dolist (header header-lst)
	(format stream "~a<TH width=\"50%\">" header))
      (dolist (entry data)
	(format t ".")
	(write-table-entry stream entry))
      (format stream "</TABLE>")
      (format stream "</body>")
      (format stream "</HTML>")
      (format t "~%"))
    (tester-rename-file tmp-filename filename)))

(defun write-table-entry(stream entry)
  (progn
    (format stream "<TR>~%")
    (dolist (col entry)
      (format stream "<TD><pre>~a</pre></TD>" col))
    (format stream "</TR>")))
