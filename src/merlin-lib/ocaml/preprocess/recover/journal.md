OLD: everything below is outdated

Problematic cases when detecting cycle only on item at pos = 1 
==============================================================

Here, recovery should favor second reduction, first will obviously cycle:

    mod_longident   ::= mod_longident . DOT UIDENT
                        5.            4.    3.    
    label_longident ::= mod_longident . DOT LIDENT
                        5.            4.    3.    

However there is only one transition (on DOT), so the decision really matters
at next state:

    mod_longident   ::= mod_longident DOT . UIDENT
                        5.            4.  3.      
    label_longident ::= mod_longident DOT . LIDENT
                       5.            4.  3.      

Option 1:
---------

Recovery doesn't send a single symbol but a full sentence.  So from the second
state, we might wrongly choose the first reduction, but then we will be back to
first state where we will reduce everything in a state.

Simpler algorithm, less good recovery.

Option 1':
----------

(doesn't always work, currently implemented)
Analysis on the grammar reveals that second reduction always end quicker
(first-item cost approximation in current gen_recover.ml).
Thus it is ok to always favor the second.

It might fail because it is sometime context dependent which of the two
reductions is the good one. (One can come up with a grammar where sometime the
first reduction ends as quick, defeating the heuristic).

Option 2:
---------

Decide non locally. In second state, compute nth predecessors, and decide
according to nth predecessors.

Long term, 2 is needed. We should work on the full LR(1) automaton where not all transision exists, analysis needed for 2 is an intermediate step.

Statically detecting this problem:
----------------------------------

Since we can enumerate transition, we can statically ensure by interpreting the
automaton that all states can be reduced in a finite number of step.
