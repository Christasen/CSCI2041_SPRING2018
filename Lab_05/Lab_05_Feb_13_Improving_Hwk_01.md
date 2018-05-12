# Lab 5: Improving your Homework 1 solutions

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, February 16 at 5:00 PM. You should be able to complete the
discussion portion of this lab work during your lab period. Take notes by
writing comments in your code (including decisions about what to change), then
make the actual changes after fully discussing everything. That is, you may need
additional time for making sure that all your functions meet the new
requirements for writing "good" code. Complete your work for this lab (so that
you are satisfied with your auto-generated feedback file) by the due date.

## Introduction

The goal for this lab is improving your OCaml function structure and style. You
will work with 1 or 2 of your classmates, and you will view their solutions for
Homework 1---and allow them to view yours.

Specifically, you will be looking at your function ``split`` from Part
2, ``answers`` from Part 3, and ``format`` from Part 4.

This sort of sharing is allowed only for lab exercises. (Discussing lab
exercises with classmates is always encouraged.)


### Form groups and prepare the files for lab 5

Find 1 or 2 people to work with as a group.  This is not optional.
You must work with others on this. 

Feel free to move around in lab to sit next to the people you want to
work with, but everyone should have their own computer. Introduce
yourselves.

Create a new directory in your repository named ``Lab_05`` and copy
your ``higher.ml``, ``puzzle.ml``, and ``formatter.ml`` files from
``Hwk_01`` into this new directory.

For each of the three functions of interest, (``split``, ``answers``,
 and ``format``) put a copy of your current version in a comment so
 that you can easily compare your original and new versions of these
 functions.

``(* ML comments look like this... *)``

Also, at the top of your new file, add a comment with your name and
the names of everyone in your group.


## Fixing the easy stuff.

The first step is to fix the more easily identifiable problems with
your code.  Feel free to discuss your solutions with your lab-mates.
Remember, limit your work to function ``split`` from Part 2,
``answers`` from Part 3, and ``format`` from Part 4.

- remove unnecessary ``;;``
- raise exceptions for input that is not allowed
- nicely indent your code so that the structure of the indentation
  matches the structure of the code.
  - make sure that there are no lines longer than 80 characters.


## Meaningful names.

Meaningful names for values and functions can significantly improve
the readability of your code.  

The names in let-expressions should be indicative of the value bound
to that name.

Using "generic" names like ``x`` and ``xs`` for the head and tail of a
list makes sense when the types of these are not concrete types, but
instead polymorphic types with a type variable such as ``'a``, ``'b``,
or ``'a list``.

You may consider the (updated) examples of ``group_by_3.ml`` in the
sample program directories for each lecture section in the public
class repository.

Identify the opportunities to use more descriptive names in your 3
functions and change them to use these better names.


## Type annotations
Type annotations are not required in a language like OCaml that uses
type inference.  However, writing type annotations, especially for the
functions that are declared at the "top level" of your file (not
inside a let-expression, can really help uses of that code understand
what your code does.

This also applies to some names declared inside a let-expression.  For
example, specifying the types of local names, for example the initial
accumulator used in a fold, can help readers understand your code.

Furthermore, using type synonyms like we discussed in lecture on
Monday, can also aid in the readability.   For example, there may be a
few different ``char list``, ``char list list`` and ``char list list
list`` types in your ``format`` implementation.  Using type names to
indicate when a ``char list`` is a "word" or a "line" can also help.

Note that using type synonyms so can make it reasonable to use shorter
names for the values, since information about them is conveyed by the
type annotations and not only by the name of the values.

Add type annotations to at least 2 of the 3 functions in a way to
improve your code.  Also use type synonyms to make your code easier to
understand. 

## Use your own split function

If you used the function ``String.split_on_char`` from OCaml's
String library rather than the ``split`` function in your 
implementation of ``answers`` and ``format``, use your own ``split``
function instead for ``answers`` and ``format`` in this lab.

## Improved code structure

Spend a few minutes discussing your solution with your lab-mates.
What other features (besides those discussed explicitly above) of
their solution do you appreciate and feel would help to make your
solution easier to read and understand?

Pick 2 of the 3 functions of interest (``split``, ``answers``, and
``format``) and adopt some aspect of their solution, if you you feel
it can help your solution.


## Assessment

Your work on this lab is assessed by the final quality of your code
and the absence of more of the above mentioned opportunities for
further improvement in 2 of the 3 functions of interest.

As always, **push your work to GitHub**.
