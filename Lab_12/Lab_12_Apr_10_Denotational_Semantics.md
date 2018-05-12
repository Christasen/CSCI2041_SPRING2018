# Lab 12: Denotational Semantics

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, April 13 at 5:00pm. You should be able to complete
this work during lab. 


## Lab goals

This lab will explore the interpreter that we wrote in lecture and
extend it a bit.  This is based on "denotational semantics". This is a
mechanism for defining the semantics of imperative programs in which
the meaning of a program is seen as a state transformation function.

You can read more about denotational semantics in the paper 
["The Denotational Semantics of Programming Languages"](https://github.umn.edu/umn-csci-2041-S18/public-class-repo/blob/master/Resources/Denotational_Semantics_Tennent.pdf) by
R. D. Tennent.  This paper is also in the Resources directory in the
public class repository.


## Getting started.

Copy the file ``interpreter.ml`` from either the
``Sample Programs/Sec_01_1:25pm`` or
``Sample Programs/Sec_10_3:35pm`` directory of
the public class repository into a
new directory named ``Lab_12``.  Name the copy of this file
``interpreter.ml``.


## Review

Open the file and familiarize your self with the data types and the
functions ``eval`` and ``exec``.

Run ``exec program_while []``.  And enter ``10``.  How many times does
``sum`` appear in the final state?

Create a let-binding in your file of the from
```
let num_sum = ...
```
where ``...`` is replaced by the number of times that ``sum`` appears
in the final state.


## Add a conditional statement -- if-then-else

Add a new constructor of the following form to ``stmt``:
```
  | IfThenElse of expr * stmt * stmt
```
Then write to clause for this new constructor in the ``match``
expression in ``exec``.

Next, add a modulus operator constructor to ``expr``:
```
  | Mod of expr * expr
```
Then write the clause for this new constructor in the ``match``
expression in ``eval``.

Hint: in OCaml, ``mod`` is the infix operator for modulus.  Try it out
in ``utop``.


## Another program

Construct a new program of type ``stmt`` named
``program_while_ifthenelse``.  It should correspond to the following
comment already in ``interpreter.ml``.  Note how ``program_seq`` and
``program_while`` both have similar comments for them.  You need to
construct a let-binding for ``program_while_ifthenelse`` that
corresponds to the program in the comment below:
```
(* read x;
   i = 0;
   sum_evens = 0;
   sum_odds = 0;
   while (i < x) {
     write i;
     if i mod 2 = 0 then
        sum_evens = sum_evens + i;
     else
        sum_odds = sum_odds + i;
     i = i + 1
   }
   write sum_evens;
   write sum_odds
 *)
```

## Test your extended ``exec``

Run ``exec program_while_ifthenelse []`` and enter ``8`` when prompted to enter a
number.

It should print out the integers from 0 to 7 and the print 12 and then
16.

Run ``exec program_while_ifthenelse []`` and enter ``15`` when prompted. Then 
create the following let-bindings in your file:
```
let val_sum_evens =
let val_sum_odds =
let num_sum_evens =
let num_sum_odds =
```
+ give ``val_sum_evens`` the value of ``sum_evens`` in the final state
+ give ``val_sum_odds`` the value of ``sum_odds`` in the final state
+ give ``num_sum_evens`` the number of times ``sum_evens`` appears in
  the final state
+ give ``num_sum_odds`` the number of times ``sum_odds`` appears in
  the final state


## Create a testable version of ``program_while_ifthenelse``
Define ``program_while_ifthenelse_test`` in your file to be the same as
``program_while_ifthenelse``, but replace
```
ReadNum "x"
```
 with 
```
Assign ("x", Val (Int 12))
```


The following should evaluate to ``Int 30``
```
lookup "sum_evens" (exec program_while_ifthenelse_test [])
```


## Add a skip statement

Add the following constructor to ``stmt``:
```
  | Skip
```
This is a "skip" statement that does nothing.  It is like ``pass`` in
Python or a "noop" in assembly language.

Complete the implementation of ``exec`` to handle this new
construct.  

Also, re-implement the ``IfThen`` construct based on the
observation that executing "if ...cond... then ...stmt..." is the same
as executing "if ...cond... then ...stmt... else skip".

Next, define ``program_ifthen`` to correspond to to the following comment:
```
(* y = 0;
   if x mod 2 = 0 then y = y + 2;
   if x mod 3 = 0 then y = y + 3;
   if x mod 4 = 0 then y = y + 4;
  *)
```
Now try ``exec program_ifthen [ ("x",Int 4) ]``.  

For example ``lookup "y" (exec program_ifthen [ ("x",Int 4) ])`` should
evaluate to ``Int 6``.

## Add a for loop

Next, add the following ``stmt`` constructor
```
  | For of string * expr * expr * stmt
```
that represents a for-loop with a name of the integer counter (the ``string``)
and the lower and upper bounds (the two ``expr`` components) and the
body of the loop (the ``stmt``).

Complete the implementation of ``exec`` to handle this new
construct.  

Define ``program_for`` to be
```
For ("i", Val (Int 1), Val (Int 5), WriteNum (Var "i"))
```
When run, this will print out the values 1, 2, 3, 4, and 5.  You can
see the bounds are inclusive.

Add the following declaration to your file:
```
let program_sum_10 =
(* sum = 0
   for i = 1 to 10
     sum = sum + i
   write sum
 *)
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Val (Int 10),
            Assign("sum", Add (Var "sum", Var "i"))),
       WriteNum (Var "sum")
      ) )
```
Run it to check that ``55`` is displayed.

Consider this alternate version.  It should print out ``55`` two
times.  Why is that?  Ensure that your implementation of the for-loop
gives this same behavior.
```
let program_sum_10_n =
(* n = 10
   sum = 0
   for i = 1 to n
     sum = sum + i
     n = sum
   write sum
   write n  
 *)
  Seq (Assign ("n", Val (Int 10)),
  Seq (Assign ("sum", Val (Int 0)),
  Seq (For ("i", Val (Int 1), Var "n",
            Seq (Assign("sum", Add (Var "sum", Var "i")),
                 Assign("n", Var "sum")
                )
           ),
  Seq(WriteNum (Var "sum"),
      WriteNum (Var "n")
     ) ) ) )
```


## Repeat-until

If time allows, attempt this optional task.  Add a repeat-until loop.
It consists of a ``stmt`` and ``expr``.  It executes the loop body
first, then does the check.  It keeps looping until the ``expr``
evaluates to true.

You might use this as the constructor in ``stmt``:
```
  | Repeat of stmt * expr
```


## Push your work.

Now be sure to commit and push your work.  Check the feedback file
that should be generated each time you push this work.


