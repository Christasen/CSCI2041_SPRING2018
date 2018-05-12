
# Additional optional problems for Homework 4

You should only attempt the problems if you have completed the
required work for Homework 4.

Please complete this work by the end of March or talk with Eric about
why you would like additional time.


# Part 1: Lists and tuples

Consider adding lists and tuples to the final part of homework 4.  You
will need to extend ``expr`` and ``value`` types to represent these
kinds of values.

You will then need to extend ``eval`` accordingly.

Please put your solution to this problem in a file called
``expr_data.ml`` in your ``Hwk_04`` directory to distinguish it from
your other work.

Part of any extra credit score will be based on the quality of the
documentation accompanying your solution.

To receive any sort of extra credit for this you must write a detailed
comment in the file explaining how to run your solution.

You must also provide several sample expressions to comprehensively
test your solution.  Document these with comments explaining what
parts of the language are being tested.

Push your solution to GitHub and email Eric and the TAs (at
``csci2041@umn.edu``) to let them know you have completed this work.

# Part 2: Type checking

There is a third version of the full language (with lambda expressions,
recursive let-expressions, etc.) with type annotations for binding
constructs (let- and lambda-expressions).  This is defined in the file
``expr_functions_typed.ml`` in the ``Homework/Files`` directory of the
public class repository.  Copy that file to your ``Hwk_04`` directory.

The main challenge of this problem is to complete ``eval`` and
``check`` in the same way that was done in the ``expr_let_typing.ml`` 
file.

   
That is, you will want to complete the following functions:
```
let rec eval (e:expr) (env: environment) : value result =
  match e with 
  | Val v -> OK v


let rec check (e:expr) (ctxt:context) : typ result =
  match e with 
  | Val (Int i) -> OK IntType
```

Part of any extra credit score will be based on the quality of the
documentation accompanying your solution.

Specifically, there are many programming decisions that you will have
to make in completing this work.  Describe the decisions you needed to
make and explain why you made such decisions.

As before, to receive any sort of extra credit for this you must write
a detailed comment in the file explaining how to run your solution.
You must also provide several sample expressions to comprehensively
test your solution.  Document these with comments explaining what
parts of the language are being tested.

Push your solution to GitHub and email Eric and the TAs (at
``csci2041@umn.edu``) to let them know you have completed this work.

