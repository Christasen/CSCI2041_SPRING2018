# Lab 13: Purely Functional Data Structures

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, April 20 at 5:00 PM. You should be able to complete the
discussion portion of this lab work during your lab period. 


## Introduction

The work we'll do for this lab is based on material we've seen in
Chris Okasaki's book "Purely Functional Data Structures" - referred to
as PFDS below.  Some material also comes from the S8.1 slides we've
been using in class.

Our goal is to implement binomial heaps following the examples in
PFDS, but in OCaml instead of Standard ML.  This work will set us up
for the last homework assignment, Homework 8.


## Getting Started

Create a ``Lab_13`` directory in your GitHub repository and in that
create a file named ``binomialheap.ml``.


## Ordered values

In binomial heaps, and other data structures, we need to compare
values in various ways.  This idea is captures in PFDS at the top of
Figure 2.9.  This is similar to the ``Comparable`` signature we
developed in the interval examples from our discussion on modules.

Write a signature named ``OrderedSig`` that has a type ``t`` and the
three functions in Figure 2.9.

The functions in Fig 2.9 are not curried - that is, they take an
ordered pair as input. We would like ours to be curried so that the
type is ``t -> t -> bool`` instead.


## Integers as ordered values

Implement a module named ``Int`` that conforms to this ``OrderedSig``
signature.  You may need to use some of the constructs we learned in
versions 6 and 7 of the interval examples for this to work with the
functor we will define below.


##  A signature for heaps

Figure 3.1 of PFDS provides a samples signature for heaps.  To make
testing and experimenting with modules that satisfy this signature we
will extend it and give it the name ``BinomialHeapSig`` instead of
``Heap``.

We will not make the type of the heap abstract and will will also
provide a type for the binomial trees in binomial heaps.

We will also not include a structure ``Elem`` as done in Figure 3.1.
Instead we will simply have an abstract type ``elem`` in the
signature.

Thus, the beginning of your ``BinomialHeapSig`` signature should be
the following:
```
module type BinomialHeapSig = sig
  type elem
  type tree = Node of int * elem * tree list
  type t = tree list 
```

After this, provide the types for the functions Figure 3.1.  Again,
our functions should be curried.  So the types of ``insert`` and
``merge`` should not take a pair but should instead take their
arguments one at a time.  This is the same thing we did with the
functions in ``OrderedSig``.


## A functor for binomial heaps

Figure 3.4 in PFDS provides a functor for binomial heaps.  We will
write a similar functor, but it will match our modified signatures,
``BinomialHeapSig``, from above.

Our functor, named ``BinomialHeap`` should take a module matching
signature ``OrderedSig`` as it argument.

It will set ``elem`` to the type defined in the argument module and
implement the ``t`` and ``tree`` types as in the ``BinomialHeapSig``
as above.

Again, we may need to use what we learned in versions 6 or 7 of the
interval examples from our discussion of modules.

The remainder of the body of the functor should implement those
functions in Figure 3.4.

Again, these are all in Standard ML and thus need to be translated
into OCaml.


## Putting the pieces together.

Construct a module ``BHI`` - for Binomial Heap of Integers - as
follows:
```
module BHI = BinomialHeap(Int)
```

Next, add the following to your file:
```
let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4

let m1 = BHI.findMin h5

let h6 = BHI.deleteMin h5

let m2 = BHI.findMin h6
```

These replay the example we did in lecture of adding the values 20,
30, 10, and 40 in succession, starting with the empty heap.

Your solution should see ``m1`` evaluate to 10 and ``m2`` evaluate to
20.



## What to turn in.

Commit and push your ``binomialheap.ml`` file and check for the
results from the feedback tests in your GitHut repo.
