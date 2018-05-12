-------------------------------------------------------------------------------
# Homework 5: Lazy Evaluation

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:**  Thursday, April 5, at 5:00 PM.

**Changelog:**
+ Monday, March 26: Initial version.

## Introduction

This homework will highlight lazy evaluation. Note: Part 3 is the most
substantial part of the homework.

To start, create a directory in your personal repository named ``Hwk_05``.
You must place all files for this homework in this directory.

## Part 1: Evaluating expressions by hand

As demonstrated in class, and in the document "Expression Evaluation
Examples" (``expr_eval.pdf`` in ``Resources`` on GitHub), we have
evaluated expressions by hand, step by step, to understand the
different ways in which call by value semantics, call by name
semantics, and lazy evaluation work.

### Question 1

Consider the following definitions:

```
sum [] = 0
sum x::xs -> x + sum xs

take 0 lst = [ ]
take n [ ] = [ ]
take n (x::xs) = x::take (n-1) xs

evens_from 0 v = [ ]
evens_from n v = v+v :: evens_from (n-1) (v+1)
```

Now, create a file in your homework directory called ``question_1.txt`` with
the following:

- Your name
- Your Internet ID

You will now evaluate the following expression by hand:

```
sum (take 3 (evens_from 5 1))
``` 

Similarly to the examples in ``expr_eval.pdf``, you must evaluate the
expression three times using three different semantics:

1. Call by value
2. Call by name
3. Lazy evaluation (also known as "call by need")

Label each of your evaluations clearly by evaluation strategy employed, perhaps
as in this example:
```
CALL BY VALUE


CALL BY NAME


LAZY EVALUATION
```

Please write plain text. We also will not accept handwritten work.

### Question 2

Recall these definitions for ``foldl`` and ``foldr``, and the functions for
folding ``and`` over a boolean list. (Note: We removed the underscores from the
names as they appeared on the S4.1 slides.)

```
foldr f [] v = v
foldr f (x::xs) v = f x (foldr f xs v)

foldl f v [] = v
foldl f v (x::xs) = foldl f (f v x) xs

and b1 b2 = if b1 then b2 else false

andl l = foldl and true l
andr l = foldr and l true
```

Now, create a file in your homework directory called ``question_2.txt`` with
the following:

- Your name
- Your Internet ID

#### Let's evaluate by hand

You will now evaluate these two expressions:

```
andl (true::false::true::true::[])
```
and
```
andr (true::false::true::true::[])
```

You must evaluate the expressions using only two different semantics:

1. Call by value
2. Call by name

Label each of your evaluations clearly by evaluation strategy employed, perhaps
as in this example:

```
andr - CALL BY VALUE


andr - CALL BY NAME


andl - CALL BY VALUE


andl - CALL BY NAME
```

A couple of notes 🎵:

+ Write lists in their basic form using the cons (``::``) and nil (``[]``)
  operators instead of using semicolons to separate list values between square
  brackets.

+ We don't consider lazy evaluation for this question. Why not? (Try it and
  compare with your other results.)

#### Which is better?

After you have completed all four evaluations (call by value and call by name
for both `andr` and `andl`), state which evaluation is most efficient and
briefly explain why.

#### Reminder

As before, please write plain text. We also will not accept handwritten work.


## Part 2: Efficiently computing the conjunction of a list of Boolean values

Create an OCaml file in your homework directory named ``lazy_and.ml``.

Write a function named ``ands`` with type ``bool list -> bool`` which computes
the same result as the  ``andl`` and ``andr`` functions described in the
problem above, except with the following quality:

> Your function should avoid examining the entire list unless absolutely
necessary, hence the "lazy." (Hint: We saw this behavior in one of the by-hand
evaluations above.) Stated explicitly: If the function encounters a ``false``
value then it can terminate and return ``false``.


## Part 3: Implementing Streams in OCaml

As demonstrated in class (``lazy.ml`` in ``Sample Programs`` on GitHub) we
developed a type constructor ``stream`` which can create "lazy" streams and use
lazy evaluation techniques within OCaml (which itself is a strict/eager
language).

Start this part by creating a file named ``streams.ml`` in your homework
directory and copying the following code (including comments) into it:

```
(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee

(* The code below is from YOUR NAME HERE *)
```

Change ``YOUR NAME HERE`` to your name.

***Proper attribution:** Always clearly mark all parts of your files that you
did not write, as in the code above, and attribute them to their author (your
instructor, for example). Then indicate where your work starts in the file by
adding your name and a comment to this effect, such as in the code above.*

You will now define a collection of stream values, and functions over streams.


### 3.1 ``cubes_from``

Define a function ``cubes_from`` with type ``int -> int stream`` that creates
a stream of the cubes of numbers starting with the cube of the input value.
For example, ``cubes_from 5`` should return a stream that contains the values
125, 216, 343, ...

Demonstrate to yourself that it works by using ``take`` to generate a finite
number of cubes. 

### 3.2 ``cubes_from_zip``

Copy the ``zip`` function from the ``lazy.ml`` file from class to your current
file (with proper attribution), then define a function ``cubes_from_zip`` with
the same type and behavior as ``cubes_from`` above, but which uses the ``zip``
function (maybe more than once) to construct the stream of cubes.

You may copy and use more functions besides ``zip`` from the ``lazy.ml`` file
from class, but you must use proper attribution.


### 3.3 ``cubes_from_map``

Now copy the ``map`` function from the ``lazy.ml`` file from class to your
current file (with proper attribution), and define a function named
``cubes_from_map`` with the same type and behavior as ``cubes_from`` above, but
which uses ``map``.


### 3.4 ``drop``

Define a function named ``drop`` with type ``int -> 'a stream -> 'a stream``.
This function should discard zero or more elements from the stream, and the
first argument indicates how many to discard ("drop").

*(Notice the type difference between ``drop``, and ``take`` (from ``lazy.ml``).
This is because ``take`` removes finite elements from a stream, so it stores
them as a list.)*


### 3.5 ``drop_until``

Write a function named ``drop_until`` with type
``('a -> bool) -> 'a stream -> 'a stream``. This function should discard as
many consecutive initial elements as possible from the stream, and output the
remaining stream. The first argument (a function) indicates whether to keep an
element (``true`` means keep, ``false`` means drop).

Stated explicitly, this function drops elements from the input stream until it
finds one for which the function returns ``true``. It then outputs the
remaining part of the stream (including the "``true``" element it found).

Example (using ``head`` and ``nats`` from ``lazy.ml``):

```
head (drop_until (fun v -> v > 35) nats)
```

The above expression should give ``36``.


### 3.6 ``foldr``, ``and_fold``, and ``sum_positive_prefix``

Recall ``foldr`` from Part 1 above. For this question, you will define a new
``foldr`` function which uses streams instead of lists. Here are some hints:

+ Because streams have no "empty stream" or "nil" constructor, there is no
  "base" value passed to your new ``foldr`` -- it takes only two arguments.
+ Your new ``foldr``'s type must include the ``lazee`` type somewhere, so it
  can give a value without examining the entire stream.

For full points, you must do the following:

1. Write the ``foldr`` function with type annotations, including both input
   types and the output type.
2. Write an OCaml comment above your ``foldr`` function explaining ``foldr``'s
   type and briefly how your function works.


#### ``and_fold``

Next, define a function named ``and_fold`` with type ``bool stream -> bool``.
It attempts to determine if all elements in the stream are ``true``. If it
encounters a ``false`` element, it should return ``false``.

This function should simply pass some kind of "and" function, and the stream,
to your new ``foldr``.


#### ``sum_positive_prefix``

Also, define a function named ``sum_positive_prefix``.  It should use your
``foldr`` to add up all the positive integers in an ``int stream`` which appear
**before** any non-positive number.


#### A concrete example

To help us think about the above functions, consider the following stream
examples:

```
let ns : int stream = zip ( - ) (from 1000) (cubes_from 1)
```

Notice that ``ns`` is an ``int stream`` computed by using subtraction to zip
together two streams; The first stream is all positive numbers starting from
1000, and the second stream is cubes starting from 1. In particular, this
stream has some positive numbers at the beginning, but then the numbers in the
stream become negative because second stream gets larger faster than the first
stream.

For example, evaluating ``take 15 ns`` yields this:

```
[999; 993; 975; 939; 879; 789; 663; 495; 279; 9; -321; -717; -1185; -1731; -2361]
```

With ``ns`` in mind, consider these two streams:

```
let are_positive ns = map (fun n -> n > 0) ns

let ns_positive : bool stream = are_positive ns
```
Here, ``ns_positive`` is a ``bool stream`` whose initial values are ``true``,
followed by an unbounded number of ``false`` elements.

Thus
```
and_fold ns_positive
```
will evaluate to ``false``, but
```
and_fold (are_positive (from 1))
```
will not terminate normally.

Similarly, ``sum_positive_prefix ns`` will evaluate to ``7020``, but
``sum_positive_prefix (from 1)`` will not terminate normally.

### 3.7 The Sieve of Eratosthenes, "``sieve``"

For this final question, we ask you to write a function which computes the
[Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).
Here is some background:

#### The Sieve

We can compute prime numbers by starting with the sequence (a stream) of
natural numbers beginning at 2 and using the following process:

1. Consider the first number in the sequence as a prime number.
2. Discard all multiples of the above number in the remaining sequence.
3. For resulting remaining sequence, go to step (1).

To see this process in action, consider this prefix of natural numbers starting
at 2:
```
2 3 4 5 6 7 8 9 10 11 ...
```

Starting at step (1), we consider 2 as prime for step (2) we discard all
multiples of 2 in the remaining sequence. Notice that because 3 is not a
multiple of 2, we won't discard it:

```
3 5 7 9 11 ...
```

We then essentially "cons" 2 onto the resulting sequence beginning at 3, which
gives us this:
```
Cons (2,  3 5 7 9 11 ... )
```

For step (3), we have the resulting sequence beginning at 3 as our new sequence
and go back to step (1) to repeat the process and get this:

```
Cons (2,  Cons (3,  5 7 11 ... ))
```

You can see that we will repeat again, with 5 as the new prime and the
remaining sequence beginning at 7. This process repeats for each prime number
"discovered."

#### The ``sieve`` function

Implement the above process as a function named ``sieve``, which has type
``int stream -> int stream``. Here are some hints:

+ With your ``sieve`` and the ``from`` function from class, you should be able
  to define the stream of prime numbers as ``let primes = sieve (from 2)``.
  Thus, ``take 10 primes`` should evaluate to
  ``[2; 3; 5; 7; 11; 13; 17; 19; 23; 29]``.
+ You might consider a helper ``sift`` function which implements the process we
  described above. For example, it could have type
  ``int -> int stream -> int stream`` and would output a stream which does not
  contain any multiples of its input ``int`` argument. For example, using the
  ``from`` function from ``lazy.ml``, you could have ``sift 2 (from 3)`` output
  the stream ``3 5 7 9 11 ...`` as we saw above.


## Turning in your work

Push these files to your ``Hwk_05`` GitHub directory by the due date:
- ``question_1.txt``
- ``question_2.txt``
- ``lazy_and.ml``
- ``streams.ml``

Remember: If you use any other functions from ``lazy.ml`` that we developed in
class, then clearly denote them with proper attribution in your solution.

As in Homework 4, part of your score will be based on your code quality. (See
"Writing transparent code" in Homework 4.)

-------------------------------------------------------------------------------
