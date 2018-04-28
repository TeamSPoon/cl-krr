.. _introduction:

--------------
1 Introduction
--------------
The package *fricas_snark* is an interface to SNARK, SRI's New Automated 
Reasoning Kit, a theorem prover intended for 
applications in artificial intelligence and software engineering. SNARK is 
geared toward dealing with large sets of assertions; it can be specialized 
with strategic controls that tune its performance; and it has facilities 
for integrating special-purpose reasoning procedures with general-purpose 
inference. 

The goal is to have a powerful automated theorem prover (ATP in the sequel)
combined with the algebraic power of FriCAS. Since SNARK is being developed 
in Common Lisp (on which FriCAS is mainly built on), it is not difficult to 
interface the prover on the Lisp level (see ``src/snark.lisp``). A new domain
called ``Propositions`` (abbrev. ``PROP``) builds the link between FriCAS'
algebraic language (SPAD) and SNARK's own logic language in CL which is close
to the [KIF]_ standard (actually KIF is an option).

The end user will be able to use most of SNARK's features on the SPAD level,
however, it is always possible, of course, to access the CL level by using
FriCAS' Lisp interface - ``)lisp`` or ``)fin``. So this documentation will
focus on the ``Proposition`` domain, whereas for the Common Lisp part (CL in
the following) we refer to the SNARK documentation [tutorial]_, [paper]_, 
[home]_, [author]_. For the understanding of the ATP it is certainly necessary 
to be acquainted with the SNARK tutorial.



.. [tutorial] http://www.ai.sri.com/snark/tutorial/tutorial.html
.. [paper] http://www.sri.com/work/publications/guide-snark
.. [home] http://www.ai.sri.com/~stickel/snark.html
.. [author] https://en.wikipedia.org/wiki/Mark_E._Stickel
.. [KIF] http://logic.stanford.edu/kif/dpans.html

