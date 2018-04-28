;;
;; $Id: gen-tarball.lisp,v 1.2 2008/02/22 20:20:12 tecuci Exp $
;;

;; functions to generate tabralls for various clib builds
;; replaces /projects/rkf/util/gen-tarball.sh
;; uses build definitions from components/tools/builds.lisp

(unless (find-package :km) (make-package :km))(in-package :km)(setq *using-km-package* t)

;; assumes components have been checked-out to clib-path
(defun gen-tarball (&optional (path-to-km "/projects/rkf/util/km") (clib-path "/tmp/tarball/") (build "halo2"))
  (format t "~%~% Generating tarball for build: ~a" build)
  (format t "~% Started at: ~a" (print-local-time))

  (format t "~% Generating index...")
  (load "/projects/rkf/util/build-clib-index.lisp")
  (build-index-standalone path-to-km clib-path build)

  (if (or (string= build "halo2")
	  (string= build "chem-halo"))
      (progn
	(format t "~% Updating chem defs table...")
	(excl::shell (format nil "/lusr/bin/cvs commit -m \"updated\" ~a" (concatenate 'string clib-path "components/tools/chem-defs.lisp")))))

  (format t "~% Generating downloadable version - separate files...")
  ;; copy Readme.txt
  (excl::shell (format nil "/bin/cp /projects/rkf/util/CLib-Readme.txt ~a" (concatenate 'string clib-path "components/Readme.txt")))
  ;; cd to clib-path
  (excl::shell (format nil "cd ~a" clib-path))
  ;; rename the clib-index-data-all.lisp to clib-index-data.lisp for back-compatibility [dgt 11/08/2005]
  (excl::shell (format nil "/bin/cp /usr/spool/net/www/users/mfkb/RKF/trunktree/clib-index-data-~a.lisp ./clib-index-data.lisp" build)) 
  ;; copy Licence.txt
  (excl::shell (format nil "cp /usr/spool/net/www/users/mfkb/manuals/LICENCE.txt ~aLICENCE.txt" clib-path))
  ;; tar and zip necessary files
  (excl::shell (format nil "/bin/tar cvzf ~a-clib.tar.gz -C ~a ~a clib-index-data.lisp LICENCE.txt" build clib-path (join " " (dirs-for-build build))))
  ;; move it to web directory, change access permissions
  (excl::shell (format nil "/bin/mv ~a-clib.tar.gz /u/www/users/mfkb/RKF/trunktree/~a-clib-untested.tar.gz" build build))
  (excl::shell (format nil "/lusr/gnu/bin/chgrp rkf /u/www/users/mfkb/RKF/trunktree/~a-clib-untested.tar.gz" build))
  (excl::shell (format nil "/lusr/gnu/bin/chmod g+w /u/www/users/mfkb/RKF/trunktree/~a-clib-untested.tar.gz" build))

  ;; generate downloadable version - single file
  (format t "~% Generating downloadable version - single file...")
  
  (dolist (dir (dirs-for-build build))
    (dolist (file (append (directory (concatenate 'string dir "/[a-z]*.km"))
			  (directory (concatenate 'string dir "/[0-9A-Z]*.km"))))
      ;; need to remove dir info??
      (excl::shell (format nil "cat ~a >> ~a~a-clib-one.km" file clib-path build))
      (excl::shell (format nil "echo \" \" >> ~a~a-clib-one.km" clib-path build))))

  (excl::shell (format nil "/bin/tar cvzf ~a-clib-one.tar.gz ~a-clib-one.km clib-index-data.lisp LICENCE.txt ./components/tools ./components/databases/allelements.lisp ./components/specs" build build))

  (format t "~% Cleaning up...")
  ;; clean-up
  (excl::shell (format nil "rm ~a~a-clib-one.km" clib-path build))
  (excl::shell "rm clib-index-data.lisp")

  ;; move it to web directory, change access permissions
  (excl::shell (format nil "/bin/mv ~a-clib-one.tar.gz /u/www/users/mfkb/RKF/trunktree/~a-clib-one-untested.tar.gz" build build))
  (excl::shell (format nil "/lusr/gnu/bin/chgrp rkf /u/www/users/mfkb/RKF/trunktree/~a-clib-one-untested.tar.gz" build))
  (excl::shell (format nil "/lusr/gnu/bin/chmod g+w /u/www/users/mfkb/RKF/trunktree/~a-clib-one-untested.tar.gz" build))

  ;; done!
  (format t "~% Done!")
)

(defun print-local-time ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (format nil "~a:~a:~a ~a ~a ~a ~a" hour minute second date month year zone))) 

(defun dirs-for-build (build)
  (append (list "components/tools/"
		"components/databases/")
	  (mapcar #'(lambda (rel-path)
		      (string-left-trim "/" (namestring rel-path)))
		  (cdr (assoc build *clib-builds* :test #'string=)))))

