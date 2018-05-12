# Homework 8: Purely Functional Data Structures

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Sunday, April 29 at 11:59pm. Extensions until Tuesday, May 1 at 11:59
PM (but not after) will be granted.

**Changelog:**
+ Wednesday, April 25 (Eric).  Please note that your functions
  ``isBinomialTree``, ``isBinomialHeap``, and ``isRedBlackTree`` must
  check for all properties / invariants of these data structures that
  are not captured by the OCaml type system.  We can see that the
  OCaml type system will ensure that all elements of, for example, a
  binomial tree have the same type.  But the type system cannot check
  that trees have appropriate ranks.  Some additional tests have been
  added, but the purpose of this part of the assignment is for you to
  read the material and understand what these functions need to check
  for.  At this point in the semester, implementing these checks
  should not be that difficult.  It is the understanding these invariants and
  properties that is the main point of this assignment.  

+ Tuesday, April 24: (Sean) Moved ``open Ordered`` instructions to new "Testing
  your work" section, which now includes build instructions. Updated "Turning
  in your work" section.
+ Tuesday, April 24: (Sean) Add ``open Ordered`` statement.
+ Friday, April 20: (Sean) Initial version. 🌴

## Introduction
> Get ready to implement the coolest self-balancing binary search tree that you
> never reach in 4041. 🔴⚫

In this assignment you shall implement red-black trees in a manner similar to
how you implemented binomial heaps in lab.

Additionally, you will write functions that verify if the invariants
(requirements) for these tree-based structures hold for particular heaps or
trees.


## Getting started

Create a directory named ``Hwk_08`` in your GitHub repository and copy the
``binomialheap.ml`` file from your ``Lab_13`` directory into this one.

Move the ``OrderedSig`` signature and the ``Int`` module out of your
``binomialheap.ml`` file into a new file named ``ordered.ml``. This is because
both red-black trees and binomial heaps can use these.


## Implement red-black trees

Create a file named ``redblacktree.ml`` in your ``Hwk_08`` directory to hold
your red-black trees implementation.

Next, we need a signature and a functor.


### A signature for red-black trees

As with binomial heaps, we will create a signature for red-black trees which
exposes the tree type so we can see what its values from outside of the module.  

To do this, create a signature named ``RedBlackSetSig`` with the following
tree type declarations:

```
type elem
type color = R | B
type t = E | T of color * t * elem * t
```

Add additional declarations for functions that are analogous to the functions
in the ``Set`` signature (Figure 2.7) in Okasaki's book. The type names will be
different and the functions should be curried instead of taking pairs as
inputs. (This is exactly like what you did in lab.) The function names and
values should otherwise be as they appear in the figure.


### A functor for red-black trees

Implement a functor named ``RedBlackTree`` which takes an argument module
sealed by the signature ``OrderedSig``. The resulting module must be sealed by
an extension to the ``RedBlackSetSig``.  Again, this is exactly like what you
did in lab for the ``BinomialHeap`` functor.

Implement the values in the ``RedBlackSetSig``.  Consider the slides and
Figure 3.6 in Okasaki's book for guidance.


### A red-black tree for integers

Add the following definition to your ``redblacktree.ml`` file. As you can see,
it creates a module for red-black trees for integers:

```
module RBTI = RedBlackTree (Int)
```

Test your code by writing some sample red-black trees using the values and
functions in this ``RBTI`` module.


## Verify binomial heaps

Recall the following parts of the ``BinomialHeapSig`` signature:

```
type elem
type tree = Node of int * elem * tree list
type t = tree list 
```

The ``tree`` type holds binomial trees.  While OCaml's type system is
expressive, it cannot specify invariants to ensure ``tree`` is invariably (pun
intended) a binomial tree. The type system also cannot ensure that binomial
heaps, represented by type ``t`` (a ``tree list``), maintain the properties
we require of them.

In other words, OCaml's type system cannot guarantee these requirements:

+ A ``tree`` value must have the correct rank sorted in each ``Node``,
+ The ``tree list`` must contain the correct number of trees,
+ The trees in the ``tree list`` must be binomial trees with a certain rank.

So, now you will write some code to verify these requirements. But first you
must specify the requirements in detail.


### Specify binomial heap invariants

Add a comment to your ``binomialheap.ml`` file clearly specifying the
invariants (requirements) a binomial tree must satisfy and the invariants that
a binomial heap must satisfy. You should specify them in your own words and in
full detail. The details are in Okasaki's text and in the lecture slides.

Include all relevant invariants that are required, except for invariants
enforced by the type system. For example, the type system will ensure all
elements in a binomial tree have the same type. Thus, this invariant should
not be part of your answer.

Next, you will implement a checker for the invariants in your comment. In a
sense, your comment determines how easy or difficult your implementation will
be. In some cases, we saw alternative definitions of some of the invariants.
You may want to choose the specification that you find easier to implement.


### Implement a checker for binomial heap invariants

In your ``BinomialHeapSig`` signature, add these two functions:

```
isBinomialTree: tree -> bool
isBinomialHeap: t -> bool
```

The first function checks if a value of type ``tree`` satisfies the
requirements to be a binomial tree. The second checks if a value of type ``t``
(``tree list``) satisfies the requirements to be a binomial heap.

Now, implement these functions in your ``BinomialHeap`` functor. Your
functions must faithfully implement the invariants as you described them
in your comment for the step above.

*Please be kind and put your comment near your implementation.* 💁


## Verify red-black trees

Now you will repeat the above verification task for red-black trees. Recall the
following type definition in your ``RedBlackSetSig`` signature:

```
type t = E | T of color * t * elem * t
```

Again, the type ``t`` by itself does not capture the important invariants of a
red-black tree. (These invariants involved statements about consecutive red
nodes, and the number of black nodes in the branches of the tree.)


### Specify red-black invariants

Add a comment to your ``redblacktree.ml`` file that specifies accurately and
completely the invariants that a red-black tree must satisfy.

Again, the type system will enforce a few "easy" invariants, mainly that all
values in the tree have the same type. Do not specify these invariants in
your comment.


### Implement a checker for red-black invariants

In your ``RedBlackSetSig`` signature, add this function type declaration:

```
isRedBlackTree: t -> bool
```

This function is analogous to the one for binomial trees.

Implement the function in your ``RedBlackTree`` functor. Again, your function
must faithfully implement the invariants as you described them in your comment
from the step above. 


## A tip for debugging

Consider adding helper functions to your signature for debugging purposes. This
way you can test your invariant-checking functions from outside the module.

You must remove these from your signature before you push your work once and
for all.


## Extra credit

In verifying the invariants of red-black trees, can you verify the property
that all paths from the root to a leaf node have the same number of black nodes
without explicitly generating all paths?

Write a comment explaining your solution in your file for full credit for
tackling this problem.


## Testing your work

Add the following line to the tops of both ``binomialheap.ml`` and
``redblacktree.ml``:

```
open Ordered
```

Then, run the following commands in order:

```
ocamlbuild ordered.byte

ocamlbuild binomialheap.byte

ocamlbuild redblacktree.byte
```

You should see no errors.

## Turning in your work

Push these files to your repository before the due date:

1. ``Hwk_08/ordered.ml``
2. ``Hwk_08/binomialheap.ml``
3. ``Hwk_08/redblacktree.ml``

Do not push ``ordered.byte``, ``binomialheap.byte``, or ``redblacktree.byte``.
These are binary executable files, and our main goal is to ensure that your
code compiles successfully. (You can even try running the command
``./binomialheap.byte`` which will appear to do nothing.)

If you accidentally committed and pushed these files, please do the following
from your ``Hwk_08`` directory in your repository:

```
git rm ordered.byte

git rm binomialheap.byte
```

Commit this change and pull and push as usual, before the due date.

As in past homework assignments, part of your score will be based on your code
quality. (See "Writing transparent code" in Homework 4.)

For this last assignment, code quality will count for a significant portion of
your total score.
