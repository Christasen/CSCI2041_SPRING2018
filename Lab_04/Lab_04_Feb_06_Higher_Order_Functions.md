# Lab 4: Higher Order Functions

*CSci 2041: Advanced Programming Principles, Spring 2018*

In lab today, you'll be working on some of the sample functions we
hinted at in lecture on Monday.

Writing these functions will give you more intuition about how to use
the folding functions we discussed in class.  You are **strongly**
encouraged to write these before attempting ``split`` in Homework 1.
Understanding the patterns here will help you in solving the problems
on the homework.  In fact, the TAs may want to see your solution to
these before entertaining questions on Homework 1.

You are encouraged to implement these functions by working with others
in the lab, or by yourself.

You should not **write** recursive functions for these, but you should
**use** recursive functions.  Specifically, you should implement the
following functions using ``List.fold_left`` or ``List.fold_right``.

It is quite helpful to have the types of ``List.fold_left`` and
``List.fold_right`` in mind.  Understanding these types helps you to
understand what the function used by these needs to do.

Here are those types:

- ``List.fold_left: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a``
- ``List.fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b``


### length

``length: 'a list -> int``

This function takes a list and returns its length.

### andf

``andf: bool list -> bool``

This function takes a list of Boolean values and determines if they
are all true.  If so, the function returns ``true``, and ``false``
otherwise.  If there are no elements in the list then return ``true``

Note that ``and`` is a keyword in OCaml, so we use ``andf`` here.

### orf

``orf: bool list -> bool``

Similar to the function above, but a disjunction (or) instead of a
conjunction (and).  If there are no elements in the list then return
``false``. 

We use ``orf`` to match ``andf`` above.

### is-element

``is_elem: 'a -> 'a list -> bool``

This function takes an element and a list and checks if that element
is in the list.

### list reverse

``rev: 'a list -> 'a list``

Reverse a list using ``List.fold_left``.  Think of starting with an
empty list and then putting new elements on that list as we march down
the input list.


### ASCII sum

``ascii_sum: char list -> int``

This function takes a list of characters, determines their ASCII
character code, using ``Char.code`` and adds up the resulting values.

Write this without use ``List.map`` to convert the list of characters
into a list of integers.  Instead, do this conversion from characters
to integers inside the function that you pass to fold.

Pay close attention to the types and notice that the types we
instantiate for ``'a`` and ``'b`` are not the same in this case.

For example ``ascii_sum ['a'; 'b']`` should evaluate to ``195``.


### lebowski

``lebowski: char list -> char list``

Replace all ``'.'`` characters in a list of characters with
a bigger list ``[','; ' '; 'd'; 'u'; 'd'; 'e'; '.']``
 
 
