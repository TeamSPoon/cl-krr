.. _propositions:

--------------
2 Propositions
--------------
The notion of ``Proposition``, as it is used here, is a synonym for the
*well formed formulas* (WFF) of the logic language implemented. The FriCAS
domain ``Proposition(R)`` (PROP in the sequel) defines the type of logical
formulas over terms of type ``Expression R``. The terms of the language are 
therefore of type X:::
    
    R: Join(Ring, Comparable) 
    X ==> Expression R
    P ==> Proposition R   

Taking for example ``R:=Integer``, the following expression are terms:::
    
    2::X
    a::X
    sin(b)::X
    f:X->X, then f(c) is a term if c is a term
    m:=operator 'mother --> m(Helen) is a term  
    f:=operator 'father --> f(Jack)  is a term
    ...
    
Simple propositions are built by *predicates*::
    
    (a=2)$P  -- equality between terms always gives a proposition
             -- whether true or false.
             
    (x>y)$P  -- besides equality we also have <,>,>=,<= built in,
            
The built-in predicates ``=,<,>,<=,>=`` and the functions ``true()`` and
``false()`` are the only ones which have not to defined explicitly. 

Any predicate of any order can (and must) be defined by the function ``pred``::
    
    pred : (Symbol,List X) -> %
    
For example we can define predicates ``parent, grandparent`` as follows::
    
    parent:(X,X)->P
    grandparent:(X,X)->P

    parent(x,y)==pred('parent,[x,y])
    grandparent(x,y)==pred('grandparent,[x,y])

So that ``parent(father(x),x)`` for instance is a proposition if ``x`` is a
term (a name like Helen or Jack in this case).

The logical connectives are then used to build the common logical formulas:::
    
    all(x,parent(father(x),x))  
    all(x,parent(mother(x),x))
    all([x,y,z],(parent(x,y)/\parent(y,z))>>grandparent(x,z))
    
The propositions above would be written in mathematical language as

.. math::

    \forall x:\mathtt{parent(father(x),x)}  \\
    \forall x:\mathtt{parent(mother(x),x))}  \\
    \forall x,y,z: \ (\mathtt{parent(x,y)}\wedge \ \mathtt{parent(y,z)}) 
          \implies \mathtt{grandparent(x,z)}
    
The building of terms and propositions therefore is rather straightforward and
thanks to FriCAS' type inference, we always can be sure that the result will be
well defined.


    
