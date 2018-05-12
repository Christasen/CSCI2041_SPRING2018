# Lab 10: OCaml Modules, implementing intervals


*CSci 2041: Advanced Programming Principles, Spring 2018*

In today's lab you have some practice problems to work on.  These are
designed to introduce you to the OCaml module system and get used to
writing modules and signatures.

**Due:** Friday, March 30 at 5:00pm.  You should be able to complete
lab work during the lab.   But occasionally some work may not get
completed, thus this due date. 


## Introduction

In this lab you will be creating more instances of the intervals
seen in class. You will create new modules for intervals with
endpoints that are floating point numbers and for endpoints
that are rationals.

## Getting Started

To get you started there are two files in the ``Labs/Files`` directory
in the public class repository.

1. The file ``intervals.ml`` contains the necessary signature and functor
definitions for creating our interval instances.

2. The file ``intInterval.ml`` contains an example interval with integer
endpoints.

3. The file ``useInterval.ml`` contains some sample usage of all the sorts
of intervals you will create.

Copy these three files into a directory named ``Lab_10`` in your personal
GitHub course repository.

## Float Intervals

Create a file named ``floatInterval.ml`` in your ``Lab_10`` directory.

In this file, define a module for intervals with endpoints that are
floating point numbers (``float``) named ``Float_interval``.


## Rational Intervals

Create a file named ``rationalInterval.ml`` in your ``Lab_10`` directory.

In this file, define a module for intervals whith endpoints that are
rationals (represented as pairs of type ``int * int``) named
``Rational_interval``.

The ``to_string`` function should print rationals that are simplified.
Eg. If the pair is ``(4,6)``, it should print as "2/3". You may find your
function ``frac_simplify`` from Homework 1 useful here.


## Putting it All Together

Once you have implemented both the floating point intervals and the
rational intervals, use the ``#mod_use`` directive to load
``intervals.ml`` first, followed by all three interval implementations.
Then load the ``useInterval.ml`` file, which provides some sample usage
of the interval modules you have implemented. (Use it to check out how
your code works.)


## What to Submit

You should submit all files in your ``Lab_10`` directory, including
the three files copied from the ``Labs/Files/`` directory. This means
there should be a total of 5 files in your submission:

1. ``intervals.ml``

2. ``intInterval.ml``

3. ``useInterval.ml``

4. ``floatInterval.ml``

5. ``rationalInterval.ml``
