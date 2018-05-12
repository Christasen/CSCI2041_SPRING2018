# Homework 5: Lazy Evaluation Extra Credit

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:**  5pm Thursday, April 12

Assignment prepared by Nathan Ringo.

## The Samefringe Problem

In this problem, we'll explore a situation in which the lazy solution
provides an asymptotic performance improvement over the eager one.  In
the samefringe problem, we compare two binary trees to see if they
have the same leaves, when read left-to-right.  For example, these
trees all have the same fringe:

```
                     .        .
     .              / \      / \
    / \            .   4    1   .
   /   \          / \          / \
  .     .        .   3        2   .
 / \   / \      / \              / \
1   2 3   4    1   2            3   4
```

This can be done in *O(n)* time and *O(log n)* space.  However, the
simple solution takes up *O(n)* space.  While this problem is
deceptively difficult to solve efficiently, there are a few different
"tricks" that make solving it much easier:

 - [coroutines](https://en.wikipedia.org/wiki/Coroutine)
 - [generators](https://www.learnpython.org/en/Generators)
 - laziness
 - [zippers](http://learnyouahaskell.com/zippers)

Fitting with the rest of this homework, we'll use laziness to solve this.

## The Tree Type

Include the following in your file named `samefringe.ml`.
This file must be placed in a directory named `Hwk_05` in your GitHub repository.

```ocaml
type 'a bintree
  = Lf of 'a
  | Nd of 'a bintree * 'a bintree

let t1 = Nd(Nd(Lf(1), Lf(2)), Nd(Lf(3), Lf(4)))
and t2 = Nd(Lf(1), Nd(Lf(2), Nd(Lf(3), Lf(4))))
and t3 = Nd(Nd(Nd(Lf(1), Lf(2)), Lf(3)), Lf(4))
and t4 = Nd(Lf(4), Nd(Lf(3), Nd(Lf(2), Lf(1))))
and t5 = Nd(Nd(Nd(Lf(4), Lf(3)), Lf(2)), Lf(1))

(* Trees t1, t2, and t3 have the same fringe. *)
(* Trees t4 and t5 have the same fringe. *)
```

## The Naive Solution

The simplest solution, which does **not** get the optimal *O(log n)* space bound, is as follows:

```ocaml
let rec append_strict l1 l2 =
  match l1 with
  | [] -> l2
  | h::t -> h :: append_strict t l2

let rec equal_list_strict l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (x::xs, y::ys) -> x = y && equal_list_strict xs ys
  | _ -> false

let rec flatten_strict = function
  | Lf(x) -> [x]
  | Nd(l, r) -> append_strict (flatten_strict l) (flatten_strict r)

let eqleaves_strict t1 t2 =
  equal_list_strict (flatten_strict t1) (flatten_strict t2)
```

## The Lazy Solution

Making the list and comparison lazy has asymptotically better space usage.

We can define a lazy list type, a lazy version of `flatten`, as well as a function that compares two lazy lists for equality.

```ocaml
type 'a lazee = 'a hidden ref
and 'a hidden
	= Value of 'a
	| Thunk of (unit -> 'a);;

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


type 'a lazy_list
  = Cons of 'a * 'a lazy_list lazee
  | Nil

let rec append_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : 'a lazy_list =
  failwith "TODO"

let rec equal_list_lazy (l1: 'a lazy_list) (l2: 'a lazy_list) : bool =
  failwith "TODO"

let rec flatten_lazy (t: 'a bintree) : 'a lazy_list =
  failwith "TODO"

let eqleaves_lazy (t1: 'a bintree) (t2: 'a bintree) : bool =
  failwith "TODO"
```

Fill out the bodies of these functions.
If you see a common pattern in the code, feel free to abstract it out into a helper function.

## The Writeup

Write a few sentences in a comment in the bottom of the file about why `eqleaves_lazy` uses less space than `eqleaves_strict`.

## Turning in your work

Be sure that the file `samefringe.ml` is in the `Hwk_05` directory in your GitHub repository and pushed by the due date of 5pm on Thursday April 12.

As in Homework 4, part of your score will be based on the quality of your code.
See the section on writing transparent code from Homework 4.
