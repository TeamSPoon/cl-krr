(in-package :common-lisp-user)

(asdf:defsystem #:fricas_snark
  :serial t
  :description "FriCAS Automated Theorem Prover - based on SNARK"
  :version "1.0.0"
  :author "Kurt Pagani, <nilqed@gmail.com>"
  :license "MPL 1.1, see file LICENSE"
  :depends-on (#:snark)
  :pathname "src/"
  :components ((:file "snark")))
  
