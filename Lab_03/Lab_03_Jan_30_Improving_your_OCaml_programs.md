# Lab 3: Improving your OCaml programs

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, February 2 at 5:00 PM. You should be able to complete the
discussion portion of this lab work during your lab period. Take notes by
writing comments in your code (including decisions about what to change), then
make the actual changes after fully discussing everything. That is, you may need
additional time for making sure that all your functions meet the new
requirements for writing "good" code. Complete your work for this lab (so that
you are satisfied with your auto-generated feedback file) by the due date.

## Introduction

The goal for this lab is improving your OCaml function structure and style. You
will work with 1 or 2 of your classmates, and you will view their solutions for
Lab 2---and allow them to view yours.

This sort of sharing is allowed only for lab exercises. (Discussing lab
exercises with classmates is always encouraged.)

## Form groups.

Find 1 or 2 people to work with as a group.  Feel free to move around in lab to
sit next to the people you want to work with, but everyone should have their
own computer. Introduce yourselves.

Log into the computer at your desk, then log into GitHub and open your
``lab_02.ml`` file so your solution is visible.

## Prepare the files for lab 3

Create a new directory in your repository named ``Lab_03`` and copy your
``lab_02.ml`` file from ``Lab_02`` into this new directory.

At the top of your new file, add a comment with your name and the names of
everyone in your group.

## Remove unnecessary ;;

As a first step, make a comment "TODO" to go through your new ``lab_02.ml`` and remove
any ``;;`` (if present). These are necessary only in the OCaml interpreter, for
indicating end-of-input. (They are not needed in an OCaml source file.)

For the rest of the lab, you'll discuss and modify each of the functions in the
file.

## Discuss the circle_circum_v1 function (readability).

In this part, make sure your ``circle_circum_v1`` function definition is
visible. Discuss:

* How many lines is it?
* Could someone with a modest math and programming background understand it at
a glance?
* Does it use an intuitive formal argument name (if any)? Like ``radius``,
``rad``, ``r``, etc...

Single-expression functions (like this) are best to have on one line, like so:

```
let circle_circum_v1 r = 2.0 *. 3.1415926 *. r
```

Variable names in OCaml code are typically very short, which visually
distinguishes them from more significant identifiers like function names.

Discuss the above topics with your group and decide which changes you will
make to your ``circle_circum_v1`` function. Add a comment describing any
changes and how they will improve your code. (Don't implement the changes now.)

Note: If you are doing anything unintuitive that might need explanation, such
as multiplying two times pi ahead of time, you should include a comment:

```
(* Two times pi times the radius. *)
let circle_circum_v1 r = 6.2831852 *. r
```

## Discuss the circle_circum_v2 function (indentation).

Now we'll consider ``circle_circum_v2``. Discuss with your group:

* How many lines does it use?
* Are any lines indented?
* If there is indentation, are they tabs? Spaces? How many?
* Is it easy to see where the nested ``let`` is and where its corresponding ``in`` is located?

Typical OCaml functions are more complex than ``circle_circum_v1`` so we split
them onto multiple lines and use indentation for readability. Indentation is
usually 2-4 spaces, and commonly we have related constructs on the same column,
like ``let``/``in``, as shown here:

```
let circle_circum_v2 r =
  let pi = 3.1415926
  in 
  2.0 *. pi *. r
```

The most important point is to write readable code and to be consistent in how
you write it.

As before, discuss the above topics with your group members and add a comment
about how you want to change your ``circle_circum_v2`` function where applicable.
Again, you will make these changes later, potentially on your own, so be sure to
leave yourself instructive comments.

## Discuss the product function (warnings).

Load your new ``lab_02.ml`` into the OCaml interpreter (``ocaml`` or ``utop``).
There should be no errors. (Work together to fix any if necessary). However,
you may see warnings like this:

```
File "lab_02.ml", line 27, characters 2-57:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]
```

This message indicates that your function doesn't have a clause for the empty
list (``[]``). The line number and character range may be different, but,
discuss with your group:

* Did you get a warning like the above, relating to your ``product`` function?
* Why do you suppose you got (or didn't get) a warning?

In the case of an empty list, we can and should define the output for this
function. As some students noticed, there is such a thing as a "product of no
factors" as described here:

https://en.wikipedia.org/wiki/Empty_product

Ideally, the ``product`` function should have exactly two clauses. Discuss this with your group, 
discuss your function, and add a comment describing any changes necessary for eliminating warnings
for your ``product``.

## Discuss the sum_sqrdiffs function (list construction and exceptions).

Now consider the ``sum_sqrdiffs`` function. Take turns discussing how your
function works.

Then discuss the characteristics of your different implementations that you
think represent well-written functions. Also discuss the characteristics of
your functions that you think that you can improve.

If necessary, work together to ensure everyone's function works sufficiently;
Cite your group member (in a comment) if you borrow a large portion from
them. *Always give credit where credit is due.*

Take note of the following:

* Does your ``sum_sqrdiffs`` function use the ``raise`` construct?
* Did you use the append operator (``@``) in your recursive call?

We'll consider each of these below.

### Raising exceptions.

Your code might currently generate a warning like this:

```
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(_::[]|[])
```

This indicates your function has no clauses for the case of a list with one
element, and the case of an empty list. The function is not completely defined
unless we cover this case. But recall from Lab 2 that we indicated you may
assume ``sum_sqrdiffs`` will be called with lists containing at least two
elements.

By this requirement, if someone calls ``sum_sqrdiffs`` with a list with fewer
than two elements, this should be an error in the call, not in your function
itself. In other words, you are justified in raising an exception in
``sum_sqrdiffs`` if the input has fewer than two elements. Add a rule like
the following to your clauses:

```
  | _ -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
```

Discuss with your group where you will place this rule (if not already
present). Write yourself a comment with instructions for any necessary changes.

### Avoiding awkward list construction.

If your recursive call uses the append operator (``@``), take note: When
constructing a new list, use the append operator (``@``) only in the case where
the first (left) argument to ``@`` is a list for which you do not know the size.  

Do not write, for example, ``sum_sqrdiffs ([x] @ rest)``.

Instead use the cons constructor (``::``) and write ``sum_sqrdiffs (x::rest)``.

Leave a comment for yourself to change this construction if you used it.

## Discuss the distance and triangle_perimeter functions.

Finally, take turns discussing your ``distance`` and ``triangle_perimeter``
functions. Discuss the strengths of each implementations, and how you might
improve the style or readability of your own implementation.

Consider these points:

* Does your ``triangle_perimeter`` use ``distance`` as a helper function?
* Could you simplify your formal arguments at all? (Especially the inputs to
``triangle_perimeter``.)
* If there was an exponentiation operator, would that make ``distance`` more
readable?

Notice that ``triangle_perimeter`` simply takes three points and sums the
distance between each of them. We have the ``distance`` function, so using it
would be prudent. Furthermore, ``triangle_perimeter`` doesn't need to know
anything about ``distance`` except that it takes two arguments. So, we can
write something like the following and let the type-checker handle the rest:

```
let triangle_perimeter v1 v2 v3 =
  (distance v1 v2) +. (distance v2 v3) +. (distance v3 v1)
```

As we are implementing mathematical concepts in code, it makes sense to use
common mathematical conventions: ``(x1,y1)`` and ``(x2,y2)`` as inputs to
``distance``, because these look like Cartesian coordinates.

With a quick Web search, you can ascertain there is an exponentiation operator
which you might like to use for squaring quantities. Compare the following.
Which do you prefer?

```
(* Using ** operator. *)
sqrt ((x2 -. x1)**2.0 +. (y2 -. y1)**2.0)

(* Using *. operator. *)
sqrt ((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1))
```

After discussing with your group members, write a comment in your code for how
to make any desired changes and briefly comment on how they will improve your code.

## Implement your improvements.

Now that you have discussed all the functions, you may begin implementing the changes
that you commented in your code. If you run out of time in lab, you may still continue
to work on your improvements elsewhere.

*This concludes lab 3.*

## What to turn in.

You need to turn in your new ``lab_02.ml`` file in the new ``Lab_03``
directory via GitHub. The file should satisfy the following:

* Generates no warnings (or errors, of course).
* Includes a comment with your name and those of your group members.
* Each function should have a *brief* comment explaining your changes and how
they improve your code's readability, structure, and/or style.
* You must cite any code or ideas you received from a group member.
