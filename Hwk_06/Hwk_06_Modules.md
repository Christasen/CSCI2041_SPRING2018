# Homework 6: Modules

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Thursday, April 12 at 11:59 PM.

**Change log:**
+ Monday, April 2: (Mitchell), typo fix ("us" changed to "use") in 
A stream functor description.

## Introduction

In this assignment you will package up parts of your solution to
Homework 5 using OCaml modules to understand how write and use both
signatures and modules.  These provide an expressive way to specify
components and combine them in a principled way.

In this work you will create 4 OCaml files - named
``lazeeModules.ml``, ``streamModules.ml``, ``hwk5Modules.ml``, and
``hwk6.ml`` These should be put in a directory named ``Hwk_06``.

You should read the entire specification before you start.  You may
end up developing these three files concurrently, instead of writing
them completely in order.

## Lazy modules

In class, and in homework 5, we saw types and functions that could be
used to implement a form of lazy evaluation.  You can find these in
the ``lazy.ml`` in your lecture section's files under
``SamplePrograms`` in the public course repository.

In ``lazeeModules.ml`` define a signature for a lazy type using the
following code:
```
module type LazeeSig = sig
  type 'a t
  val delay: (unit -> 'a) -> 'a t
  val demand: 'a t -> 'a 
end
```
We see that ``t`` is an opaque type constructor accompanied by two
functions for creating and using values of this type.

Your first task is to define a module ``Lazee_v1`` that implements the
signature ``LazeeSig`` following the definition of lazy values from ``lazy.ml``.  
This can be done by filling in the following
incomplete module.  Note that your module may define values that are
not in the ``LazeeSig`` signature and thus not used outside of the
``Lazee_v1`` module.
```
module Lazee_v1 : LazeeSig = struct

end
```

We can also write a module satisfying this signature that does not use
references and save the value of executing a ``Thunk`` in a ``Value``
constructor so that these functions are only run once.  This is done
in the following implementation which you should copy into the
``lazeeModules.ml`` file.
```
module Lazee_v2 : LazeeSig = struct
  type 'a t = unit -> 'a
  let delay (unit_to_x: unit -> 'a) : 'a t = unit_to_x
  let demand (l: 'a t) : 'a = l ()
end
```


## A stream functor

Streams, as defined in the ``lazy.ml`` file mentioned above, made use
of the ``lazee`` type and its functions defined in that same file.
Here we want to make this dependency explicit by defining a stream
functor that takes a lazy module as it argument.

To do this, you are asked to complete the following stream signature
and functor in the file named ``streamModules.ml``.  Note that both of 
these are incomplete and you need to add elements to both of them. 
```
module type StreamSig = sig
  type 'a lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a 

  type 'a t = Cons of 'a * 'a t lazee

  (* Add more elements here. *)
end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  let delay = L.delay
  let demand = L.demand

  (* add more elements here *)
end
```

Notice that a stream module created by the ``Streams`` functor will
need to expose some aspects of the lazy module they are parameterized
by.  This is because some of our stream functions, such as ``filter``
needed the ``delay`` function to construct the stream.

The ``Stream`` functor should provide implementations of the
functions ``head``, ``tail``, ``take``, ``filter``, ``map``, and
``zip``.  We have seen implementations of these before so it is just a
matter of packaging them up in a module.

Since we have two modules that implement a lazy type, we can construct
two different stream implementations using our stream functor.  This
can be done with the following to module specifications.

```
module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)
```

## Homework 5 - using modules

For this part of the assignment you are asked to write a functor named
``Hwk5`` that takes a module implementing the ``StreamSig``
signature.  This functor will be sealed by the ``Hwk5Sig`` signature.
This signature, and the incomplete functor are shown below:
```
module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
   (* add elements here to complete the functor *)
end
```

Place these definitions into the file named ``hwk5Modules.ml``.
Many of the functions above should be familiar to you.  You
implemented them in Homework 5.  Here, just package those solutions in
the functor, making any required changes in doing so.  It may not be
sufficient to simply copy-and-paste those old solutions into the
functor.

## Testing your code.

Create a file named ``hwk6.ml`` and add the following OCaml code.
Compiling this file, using ``ocamlbuild`` will let you test your
code.  When running it, you should see the final message "Success, all
tests passed." appear.  You can also test your code in ``utop`` by
appropriate use of ``#mod_use`` and ``#use``.

```
open LazeeModules
open StreamModules
open Hwk5Modules

module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)

module Hwk5_Lazy = Hwk5( Stream_Lazy )
module Hwk5_Slow = Hwk5( Stream_Slow )

module Hwk6_Test (Hwk5: Hwk5Sig) = struct
  let () =
    assert (Hwk5.take 4 (Hwk5.nats) = [1; 2; 3; 4]);
    assert (Hwk5.take 10 (Hwk5.nats) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
    assert (Hwk5.take 4 (Hwk5.cubes_from 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 4 (Hwk5.cubes_from_zip 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 4 (Hwk5.cubes_from_map 1) = [1; 8; 27; 64]);
    assert (Hwk5.take 2 (Hwk5.drop 3 (Hwk5.cubes_from 3 ) ) = [ 216; 343 ] );
    assert (Hwk5.head (Hwk5.drop_until (fun v -> v > 35) Hwk5.nats) = 36 ) ;
    assert (Hwk5.sum_positive_prefix (
                Hwk5.zip ( - ) (Hwk5.from 1000) (Hwk5.cubes_from 1)) = 7020);
    assert (Hwk5.take 10 Hwk5.primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29])
end

module Test_Lazy = Hwk6_Test(Hwk5_Lazy)
module Test_Slow = Hwk6_Test(Hwk5_Slow)

let () =
  print_endline ("Success, all tests passed.")
```


## Turning in your work.

Turn in these four files by 11:59pm on Thursday, April 12.
