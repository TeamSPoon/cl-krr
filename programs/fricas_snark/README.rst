============================
FriCAS package: fricas_snark        
============================
 
Automated Theorem Prover for FriCAS (based on SNARK_).

.. _SNARK: https://github.com/nilqed/SNARK

*Dedicated to the memory of Mark E. Stickel (1947-2013)*

-------------
Documentation
-------------
The folder ``sphinx`` contains the sphinx sources of the manual while the 
compiled HTML files are in ``docs``. There is an online_ version served by 
GitHub pages (``master branch /docs folder``).

.. _online:  http://nilqed.github.io/fricas_snark/


Documentation and detailed information about **SNARK**:

:SNARK tutorial: http://www.ai.sri.com/snark/tutorial/tutorial.html
:SNARK paper: http://www.sri.com/work/publications/guide-snark
:SNARK home: http://www.ai.sri.com/~stickel/snark.html
:SNARK author: https://en.wikipedia.org/wiki/Mark_E._Stickel

SNARK_ will now be automatically installed from the quicklisp repository.


-----------
Quick start
-----------
We assume Quicklisp_ is installed.

1. Add the following code to your ``~/.fricas.input``:

::

   )set mess type off
   quickLoad(p) ==
     systemCommand("lisp (load _"~/quicklisp/setup_")")
     systemCommand(concat ["lisp (ql::quickload _"",string p,"_")"])
   )set mess type on  

whereby assuming your Quicklisp home is ``~/quicklisp``. Otherwise adjust 
the path in the function defined above.

2. Clone https://github.com/nilqed/fricas_snark.git  to ``~/quicklisp/local-projects``,
   that is::
   
      cd ~/quicklisp/local-projects
      git clone https://github.com/nilqed/fricas_snark.git 
   
3. Start *FriCAS* and issue:::
   
    quickLoad fricas_snark

    
**NOTE**
 
  If you start fricas without the ``-nosman`` option then you have to use ::
    
    )frame next 
    
  in order to see the function ``quickLoad`` (This is because ``.fricas.input``
  is read into frame ``initial`` - use )frame names to see a list of all 
  frames. 
  If you do not like to use a startup file then you can use the lisp commands 
  of course:

::

    )lisp (load "~/quicklisp/setup")
    )lisp (ql:quickload :dform)



.. _QuickLisp: https://www.quicklisp.org/beta/


The function ``quickLoad`` will compile everything once (src) and when called 
next time the binaries are loaded (lib). If you want to recompile the sources 
then either delete everything in ``lib`` or use::
    
    )lisp (compile-prop)
    

Testing
-------
There is a test file ``test_prop.input`` in folder ``test`` which can be run
either by the function::
    
    testPROP()$Lisp 
    
when installed using Quicklisp, or manually by::
    
    )read test_prop 
    
as usual. Note that a ``)clear all`` will be issued!

In the folder ``input`` are some other examples and test files.


Sphinx
------
The folder ``sphinx`` contains the sources of the documentation. To rebuild
the *HTML* files or even building other formats (e.g. *LaTeX*) you will need
a Sphinx_ installation::
    
    make html
    
To change the *style* you have to edit ``config.py``.    
    
.. _Sphinx: http://www.sphinx-doc.org/en/stable/


Features
--------
Principal inference rules are resolution and paramodulation. Some distinctive 
features of SNARK are its support for special unification  algorithms, sorts, 
answer construction for program synthesis, procedural attachment, and 
extensibility by Lisp/SPAD code. SNARK has been used as the reasoning component
of SRI's High Performance Knowledge Base (HPKB) system, which deduces answers 
to questions based on large repositories of information, and as the deductive 
core of NASA's Amphion system, which composes software from components to meet 
users' specifications, e.g., to perform computations in planetary astronomy. 
SNARK has also been connected to Kestrel's SPECWARE environment for software 
development.


The language
------------

The language::

    variables, terms ......  Expression(T), where T has Comparable
    
    /\      ...............  logical conjunction (and)
    \/      ...............  logical disjunction (or)
    >>      ...............  implication (implies, =>)
    <<      ...............  implied by (implied-by, <=)
    ~       ...............  logical negation (not)
    all     ...............  forall quantifier
    ex      ...............  exists quantifier
    
    =       ...............  equality predicate (eq)
    <       ...............  less than predicate (lt)
    >       ...............  greater than predicate (gt)
    <=      ...............  less than or equal predicate (leq)
    >=      ...............  greater than or equal predicate (geq) 


    true(), false(): constant propositions
    
    pred    ...............  builds predicates of any order
                             pred('P,[x,y,z]) -> P(x,y,z)

                             

                             
Example (input/ex2.input)
-------------------------

An example from group theory ::

    )clear all    
    
    X ==> EXPR INT  -- Terms                                                                    
    P ==> PROP INT  -- Propositions
                                                                         
    --------
    -- Group
    --------
    
    * : BOP:=operator 'op  -- group multiplication  
    / : BOP:=operator 'inv -- inverse element
    
    e:X -- unit element
    
                                                                    
    ---------
    -- Axioms
    ---------   
    leftId := all(x,(e*x=x)$P)   -- left unit element     
    leftInv := all(x,(/x*x=e)$P) -- left inverse                                                      
    assoc := all([x,y,z],(x*(y*z)=(x*y)*z)$P)  -- associativity of (*)
       
                                                           
    -------------
    -- Hypotheses
    ------------- 
    leftCancel := all([x,y,z], (x*y=x*z)$P >> (y=z)$P)  
    rightId := all(x,(x*e=x)$P)   
    rightInv := all(x,(x*/x=e)$P)  
    rightInvUnique := all([x,y],(x*y=e)$P >> (y=/x)$P) 
    invInvolution := all(x, (/(/x)=x)$P)   
    invProd := all([x,y],( /(x*y)=(/y)*(/x))$P)
    
        
    -------------
    -- Init/prove
    -------------   
    prove(leftCancel,[leftId,leftInv,assoc])                                                                  
    prove(rightId,[leftId,leftInv,assoc])   
    prove(rightInv,[leftId,leftInv,assoc])   
    prove(invInvolution,[leftId,leftInv,assoc])  
    prove(rightInvUnique,[leftId,leftInv,assoc])   
    prove(invProd,[leftId,leftInv,assoc])
    
    --> PROOF-FOUND
    
    printRows()
    
    (Row 1
       (= (op e ?X) ?X)
       SNARK:ASSUMPTION)
    (Row 2
       (= (op (inv ?X) ?X) e)
       SNARK:ASSUMPTION)
    (Row 3
       (= (op ?X (op ?Y ?Z)) (op (op ?X ?Y) ?Z))
       SNARK:ASSUMPTION)
    (Row 6
       (= (op (inv ?X) (op ?X ?Y)) ?Y)
       (SNARK:REWRITE (SNARK:PARAMODULATE 3 2) 1))
    ...
    


)show PROP
----------

:: 

    (24) -> )show P
    Proposition(Integer) is a domain constructor.
    Abbreviation for Proposition is PROP
    This constructor is exposed in this frame.
    ------------------------------- Operations --------------------------------
    
    ?/\? : (%,%) -> %                     ?<<? : (%,%) -> %
    ?>>? : (%,%) -> %                     ?\/? : (%,%) -> %
    ?^? : (%,%) -> %                      assert : % -> SExpression
    assume : % -> SExpression             coerce : % -> OutputForm
    convert : % -> InputForm              false : () -> %
    getOption : String -> SExpression     initialize : () -> SExpression
    printAgenda : () -> SExpression       printOptions : () -> SExpression
    printRows : () -> SExpression         printSummary : () -> SExpression
    printTPTP : () -> SExpression         prove : % -> SExpression
    reset : () -> SExpression             runTimeLimit? : () -> SExpression
    true : () -> %                        useParaModulation? : () -> Boolean
    useResolution : Boolean -> Boolean    useResolution? : () -> Boolean
    ~? : % -> %
    ?<? : (Expression(Integer),Expression(Integer)) -> %
    ?<=? : (Expression(Integer),Expression(Integer)) -> %
    ?=? : (Expression(Integer),Expression(Integer)) -> %
    ?>? : (Expression(Integer),Expression(Integer)) -> %
    ?>=? : (Expression(Integer),Expression(Integer)) -> %
    all : (Expression(Integer),%) -> %
    all : (List(Expression(Integer)),%) -> %
    ex : (Expression(Integer),%) -> %
    ex : (List(Expression(Integer)),%) -> %
    getCurrentOptions : () -> Table(String,String)
    getDefaultOptions : () -> Table(String,String)
    ppOptions : Table(String,String) -> Void
    pred : (Symbol,List(Expression(Integer))) -> %
    printRow : PositiveInteger -> SExpression
    prove : (%,List(%)) -> SExpression
    prove? : (%,List(%),%) -> SExpression
    reset : Table(String,String) -> SExpression
    runTimeLimit : PositiveInteger -> PositiveInteger
    setOption : (String,String) -> SExpression
    useHyperResolution : Boolean -> Boolean
    useHyperResolution? : () -> Boolean
    useParaModulation : Boolean -> Boolean
    


Tested OS/Lisp
--------------
::
    
    )lisp (lisp-implementation-version)
    Value = "2.48 (2009-07-28) (built on win32)"
    )lisp (lisp-implementation-type)
    Value = "CLISP"
    )sys uname -a
    CYGWIN_NT-6.1-WOW64 ajax 1.7.32 i686 Cygwin
    
    )lisp (lisp-implementation-version)
    Value = "1.2.16" 
    )lisp (lisp-implementation-type)
    Value = "SBCL"
    )sys uname -a
    Linux helix 3.13.0-49-generic #83-Ubuntu SMP 
                                 