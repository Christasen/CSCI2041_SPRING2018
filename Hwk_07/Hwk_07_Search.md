# Homework 7: Search

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Thursday, April 19 at 11:59 PM.

**Changelog:** 
+ (Duanyang) April 8: Add sample output for coloring problem.

## Introduction

You will solve two searching problems in this assignment.  The first
is graph coloring, the second is the so-called "water jug" problem.


## Part 1: Graph Coloring

Write your solution to this problem in a file named
``graph_coloring.ml`` in a directory named ``Hwk_07``.

### Getting Started

Graph coloring is a famous problem in mathematics and computer
science.  To familiarize yourself with this problem, read the
beginning of the Wikipedia entry on this topic, found 
here: https://en.wikipedia.org/wiki/Graph_coloring.  You can stop
reading when you get to the part labeled Chromatic Polynomial.

In our approach to solving this problem we will represent nodes and
colors as integers, and edges are a pair of nodes.  Since it is easy
to confuse the integers representing colors and nodes we will wrap
these in an OCaml type with a simple one letter constructor to
distinguish them.

Below are the type declarations that we will use:
```
type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list
```

Nodes are ``int`` values under the ``N`` constructor.  These are
distinguished from colors which are under the ``C`` constructor.  You
might wonder if it is worth the trouble to do this.  In my original
solution to this problem I just used ``int`` for nodes and colors and
my solution was returning wrong results.  After changing the types of
``node`` and ``color`` to those above OCaml immediately identified the
place where I was mistakenly using a node value as a
color. Additionally, they make graph literals and colorings easier to
read.  Thus, these simple constructors are well worth the small added
effort.

As an example, below is a declaration of simple 4-node graph and its
3-coloring:
```
let g1 = ( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4) ] )

let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]
```
This is the second example graph from the Wikipedia that has 12
different 3-colorings.

Below you are asked to write two functions that each check **if there is
a coloring of a given graph using 3 colors**.  These functions are not
looking for the lowest number of colors that provide a valid coloring,
they are only checking if there is a solution that uses 3 colors.


### Graph coloring using options

Write a function named ``color_option`` with the type ``graph ->
coloring option``.  It should use the ``option`` type and its values
as the mechanism for controlling the search of the search space.

If you test your function with ``g1`` above your function should
return a valid coloring.  Here is a sample output:
```
Some [(N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2)]
```
Note that your solution may be different
from the sample solution ``g1_coloring`` given above since there are
many possible ways to assign colors to this graph.

The feedback tests, when complete, will only tests that your coloring
is a valid and complete one.  They will not test for a specific
solution since there are many.


### Graph coloring using exceptions

Now, write a function named ``color_exception`` with type ``graph ->
unit``.  This should raise an exception named ``FoundColoring``
defined as follows:
```
exception FoundColoring of coloring
```

This solution will use exceptions to control the search process.
Here is a sample output:
```
Exception: FoundColoring [(N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2)].
```
Similarly, the feedback tests will check for valid and complete
colorings, not for a specific solution.


### Explaining your solution

Write a comment near the top of your file that answers the following
questions.  The answers will very likely be the same for both of your
solutions.  But you can answer the questions by referring to only one
of your solutions if you like.  Be clear which solution you are
discussing in your answers.

1. What is the search space that your solution explores?  Specifically,
   explain what decisions are made at each step in exploring this
   space.  In the case of the subset-sum problem, we made a decision
   about including, or not including, an element from the original set in
   the partial subset we were constructing.  Explain the decisions being
   made in your solution, with references to those parts of your
   solution.

2. In exploring the potential coloring of a graph, your solution must
   not continue searching along that path if two adjacent nodes are
   colored with the same color.  Explain how your solution avoids this
   potential inefficiency.

   Note that we did not have this concern in the subset-sum problem.
   In choosing certain elements of the set, we could not tell if this
   was a dead end because the last element in the set could result in
   getting a sum of 0.  In this problem, that is not the case.  If we
   color adjacent nodes with the same color early in the process then
   there is no point in continuing.


## Part 2: The Water Jug Problem

Write your solution to this problem in a file named
``water_jug.ml`` in a directory named ``Hwk_07``.

### Getting Started

The water jug problem is a simple one.  We start with 2 jugs, one is a
4 gallon jug, the other is a 3 gallon jug.  Initially both jugs are
empty.  The goal is to get 2 gallons into the 4 gallon jug.

There are 8 permitted operations and a solution to this problem is a
list of operations that take us from the initial state in which both
jugs are empty to the goal state in which the 4 gallon jug has 2
gallons in it and the 3 gallon jug is empty.

Valid operations are enumerated by the following type:
```
type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug
```
A solution to the problem has type ``operation list``.

In this problems, some operations have pre-conditions.  That is, even
though they may lead to a valid state, they are not allowed.  

For example, one cannot fill up the 4 gallon jug if it is already
full.  We also cannot fill the 4 gallon jug using the contents of the
3 gallon jug if the total number of gallons in both is not at least 4
gallons.

Performing either of these actions may lead to a "valid state", but
they are still not allowed.  This is a situation we did not encounter
in the problems solved in class and thus you need to consider this
here.

A description and the pre-condition for each operation are listed below:

- ``Fill4GallonJugFromTap``.  This operation fills the 4 gallon jug from
  the tap. It is only allowed when the 4 gallon holds less than 4
  gallons. 

- ``Fill3GallonJugFromTap``. This operation fills the 3 gallon jug from
  the tap. It is only allowed when the 3 gallon holds less than 3
  gallons. 

- ``Empty4GallonJugOnGround``. This operations empties the 4 gallon jug
  onto the ground.  It is only allowed when the jug is not empty.

- ``Empty3GallonJugOnGround``. This operations empties the 3 gallon jug
  onto the ground.  It is only allowed when the jug is not empty.

- ``Fill4GallonJugFrom3GallonJug``. This operation pours some of the
  contents of the 3 gallon jug into the 4 gallon jug, filling the 4
  gallon jug.  Some water may remain in the 3 gallon jug.  This is
  only allowed with the 3 gallon jug is not empty and the 4 gallon
  jug would be full after the operation.

- ``Fill3GallonJugFrom4GallonJug``. This operation pours some of the
  contents of the 4 gallon jug into the 3 gallon jug, filling the 3
  gallon jug.  Some water may remain in the 4 gallon jug.  This is
  only allowed with the 4 gallon jug is not empty and the 3 gallon
  jug would be full after the operation.

- ``Empty4GallonJugInto3GallonJug``.  This operation pours ALL the contents
  of the 4 gallon jug into the 3 gallon jug.  After this operation the 4
  gallon jug will be empty and the 3 gallon jug will not be empty (but
  it is possible that the 3 gallon jug is not full.  This is only
  allowed if 4 gallon jug is not empty and no water is wasted (meaning
  that there is not more than 3 gallons between the two jugs before
  the operation).

- ``Empty3GallonJugInto4GallonJug``. This operation pours ALL the contents
  of the 3 gallon jug into the 4 gallon jug.  After this operation the 3
  gallon jug will be empty and the 4 gallon jug will not be empty (but
  it is possible that the 4 gallon jug is not full.  This is only
  allowed if 3 gallon jug is not empty and no water is wasted (meaning
  that there is not more than 4 gallons between the two jugs before
  the operation).

Note that some operations may result in the same state.  But we still
consider them to be different operations.


## Finding a solution

Write a function named ``play`` with the type ``unit -> (operation *
string) list option``.  No real input is needed since we know the
initial state is when both jugs are empty.

Your function will compute the list of operations needed to get to the
solution.  With each operation we associate a string that describes the
situation after the operation is complete. 

Below is an interaction in ``utop`` of a solution.  Note that there
are different possible solutions to this problem - this is just one of
them.
```
utop # play () ;;
- : (operation * string) list option =
Some
 [(Fill4GallonJugFromTap,
   "The 4 gallon jug  contains 4 gallons, The 3 gallon jug  is empty.");
  (Fill3GallonJugFromTap,
   "The 4 gallon jug  contains 4 gallons, The 3 gallon jug  contains 3 gallons.");
  (Empty4GallonJugOnGround,
   "The 4 gallon jug  is empty, The 3 gallon jug  contains 3 gallons.");
  (Empty3GallonJugInto4GallonJug,
   "The 4 gallon jug  contains 3 gallons, The 3 gallon jug  is empty.");
  (Fill3GallonJugFromTap,
   "The 4 gallon jug  contains 3 gallons, The 3 gallon jug  contains 3 gallons.");
  (Fill4GallonJugFrom3GallonJug,
   "The 4 gallon jug  contains 4 gallons, The 3 gallon jug  contains 2 gallons.");
  (Empty4GallonJugOnGround,
   "The 4 gallon jug  is empty, The 3 gallon jug  contains 2 gallons.");
  (Empty3GallonJugInto4GallonJug,
   "The 4 gallon jug  contains 2 gallons, The 3 gallon jug  is empty.")]
─( 11:52:30 )─< command 45 >──────────────────────────────────────{ counter: 0 }─
utop # 
```

The ``string`` description can be generated by using the following
function which you are free to copy into your file.
```
let describe (four:int) (three:int) : string = 
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."
```
It takes the number of gallons from the 4 gallon and and 3 gallon jug
as input and generates the string.

Feedback tests, when complete, will check for any valid sequence of
moves.  They will not explain what is incorrect with your set of moves
since that is something you should figure out on your own.

### Explaining your solution

Write a comment near the top of your ``water_jug.ml`` file explaining
how you represent the state of the problem.  That is, what type do
you use to represent the amount of water in the jugs?

You should define a type named ``state`` in your solution and use it
for this purpose.  Place this definition near the comment with the
answer to this question.


## Turning in your work

Push these two files, ``graph_coloring.ml`` and ``water_jug.ml`` to
the ``Hwk_07`` directory of your repository by the due date.

As in past homework assignments, part of your score will be based on
your code quality. (See "Writing transparent code" in Homework 4.) 
