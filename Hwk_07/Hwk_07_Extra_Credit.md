# Homework 7 Continuation EC: Search with continuations and streams

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Thursday, April 26 at 11:59 PM.

This assignment was prepared, and all questions about its contents may be answered, by Charles Harper.

## Introduction

Having solved the graph coloring problem we would like to be able to go
further, to be able to query one answer after another until there are no more.
We would not, however, like to have all the answers computed at once
should we not want all of them. In homework 6 we had you create a stream
module that would be perfect for this if we modify it slightly.

## On-demand Answers

The previous design of streams was great, but it always calculated
the first value in the stream. We would like to redesign our stream to
allow us to realize a ``Cons`` only when it is relevant to our needs.
Additionally, streams may end so we would like for our implementation to handle
this situation as well by adding a ``Nil`` constructor. To this end we will have
you create a file ``streamModulesRedux.ml`` within the ``Hwk_07``directory
into which you will place a new signature:
```
module type StreamSig_v2 = sig
  type 'a lazee
  type 'a t = 'a node_t lazee
  and 'a node_t =
    | Cons of 'a * 'a t
    | Nil
  exception Empty_stream
  
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 
  
  val head: 'a t -> 'a    (* raise Empty_stream if Nil *)
  val tail: 'a t -> 'a t  (* raise Empty_stream if Nil *)
  val take: int -> 'a t -> 'a list
  val to_list : 'a t -> 'a list
end
```
and a functor ``Stream_v2`` that implements this signature using a module implementing ``LazeeSig``.
Note that you will have to copy the ``lazeeModules.ml`` file here and open it as was done in homework 6.

## K-Colorings

Having designed searches to find 3-colorings of graphs we wish now to find all the valid colorings
of a graph using k colors, its k-colorings.
Please create a file
``continuation_graph_coloring.ml`` in the folder ``Hwk_07``. Place at the top
of this file the lines
```
open Graph_coloring
open LazeeModules
open StreamModulesRedux
module LS = Stream_v2(Lazee_v1)
```
and implement within it a search to find the k-colorings of a graph. We will do this by
adding a parameter to the created function which accepts a list of colors (careful
not to use the same color twice!):
```
val k_color_continuation : color list -> graph -> coloring LS.t
```
This search should return a stream of valid colorings
for the graph until no more remain. It must do this without producing duplicate answers,
stitching together independently constructed streams, or executing calculations related
to finding any colorings we have not yet demanded.

## Programming With Continuations

In order to accomplish this you will need to use a helper function that utilizes
a continuation parameter, a lambda that represents the continuation of a calculation.
The simplest use of continuations can already be seen within our lazy
definitions. A Thunk, as used to define the ``'a lazee`` type,
wraps around a continuation which takes only a unit argument and returns the
evaluation of whichever expression is encapsulated within it.

In the case of recursively defined streams this pattern is used to delay (encapsulate
within a continuation) the construction of the next node in the stream until it is
demanded (the continuation is evaluated).

With a careful design of continuations - what expressions they evaluate, what parameters
they take, and how the parameters get used - we can use them to control the flow
of execution through our algorithms. That is what we will do here.

## Turning in your work

Push the files, ``lazeeModules.ml``, ``streamModulesRedux.ml``, and ``continuation_graph_coloring.ml`` to
the ``Hwk_07`` directory of your repository by the due date.

As in past homework assignments, part of your score will be based on
your code quality. (See "Writing transparent code" in Homework 4.) 
