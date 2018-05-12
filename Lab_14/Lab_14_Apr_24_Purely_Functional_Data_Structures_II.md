# Lab 14: Purely Functional Data Structures II

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, April 27 at 5:00 PM.

**Changelog:**
+ Tuesday, April 24: (Sean) Clarified what to push and how to remove ``*.byte``
  files.
+ Tuesday, April 24: (Sean) Initial version.

> Time for a booster! 💊💪

## Introduction 💁

For today's lab, you will complete Exercise 3.5 from Okasaki's book:

> **Exercise 3.5** Define findMin directly rather than via a call to
> ``removeMinTree``.

Your will define this new function as ``findMinDirect`` in your
``Hwk_08/binomialheap.ml`` file.

## Getting started 🐔

Perform the "getting started" step from Hwk_08. (If you have already done this
for Hwk_08, you are all set.)

## Be productive 🤳

Open your ``Hwk_08/binomialheap.ml`` file. In your ``BinomialHeapSig``
signature, add your new function:

```
findMinDirect: t -> elem
```

The function has the same type as ``findMin``. That is, it takes a binomial
heap as input and outputs an element.

## Be direct 🎯

Now, implement ``findMinDirect`` in your ``BinomialHeap`` functor, defined as
follows: Given a binomial heap, output the minimum value element in the heap.

Your function must not call ``removeMinTree`` or ``findMin``.

## Be confident 💩

Test your work by using the "putting it all together" step from Lab_13, and
adding the following line at the bottom of your file:

```
let m3 = BHI.findMinDirect h6
```

Notice that ``m3`` should evaluate the same as ``m2`` (namely, 20).

## Compiling your code 🎪

Add this line at the top of your ``Hwk_08/binomialheap.ml`` file:

```
open Ordered
```

Then, run the following commands in order:

```
ocamlbuild ordered.byte

ocamlbuild binomialheap.byte
```

You should see no errors.

*Note: If you want to test your work again in the OCaml toplevel, you must
comment or remove the ``open Ordered`` line. But add it back in before pushing
your work to your repository.*

## Turning in your work 🎰

Push your updated ``Hwk_08/binomialheap.ml`` file to your repository before the
due date.

Do not push ``ordered.byte`` or ``binomialheap.byte``. These are binary
executable files, and our main goal is to ensure that your code compiles
successfully. (You can even try running the command ./binomialheap.byte which
will appear to do nothing.)

If you accidentally committed and pushed these files, please do the following
from your ``Hwk_08`` directory in your repository:

```
git rm ordered.byte

git rm binomialheap.byte
```

Commit this change and pull and push as usual, before the due date.

Thank you and many apologies for any confusion. 🙇
