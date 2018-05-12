# Homework 4: Programs as Data

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Sunday, March 25, at ~~5:00~~ 11:59 PM.

This homework will focus expression evaluation and other functions
over expressions represented as inductive data types in OCaml.  The
goal of this assignment is to give you the experience of working with
symbolic data at an abstract level.  It will also give you some
insight into how compilers and interpreters work.  While the languages
we are implementing are quite simple, the principles being used are
also used in production implementation of mainstream programming
languages.


**Change log:**
+ Monday, March 5: (Sean), corrected example in Part 2, type inference,
to be "2 * (3 + (4 < 5))" (instead of "2 * (3 + (4 < 5)0").

+ Tuesday, March 6: (Yuze Jiang, via Eric), fixed error in example evaluation
  for the `unparse` function.
  
+ Wednesday, March 7: (Eric), fixed some awkward wording description of type 
  inference for the ``Eq`` expression form.
  
+ Thursday, March 8: (Duanyang), fixed typo in the type of ``get_type`` , changed 
  from ``value -> type`` to ``value -> typ``.
  
+ Friday, March 9: (Duanyang), In Part 1 Step 6, recursive functions, changed 
  ``sumTo`` to match definition given in ``expr_functions.ml``.

+ Monday, March 12: (Duanyang), added clarification to ``unparse``

# Writing transparent code

We have discussed the importance of writing code in a manner that
makes it easier for others to understand it.  We've even had a few
labs devoted to this topic in which you improved your own code.  For
this assignment, part of your score will be based on how well you
write code that others can follow.

You should consider writing type annotations where they are useful to
others, consider using let-expressions to give intermediate values
names that help reader understand their intended use, not write lines
longer than 80 characters, along with using other means that we've
discussed for making your code easier to read.

You do not need to over-think this process, but take some care to make
your programs readable to others.


# Languages and files

For this assignment we will consider 3 different language - that is, 3
different versions of the ``expr`` type.

1. The first is the "full" language with functions and recursive
   let-expressions as defined in ``expr_functions.ml`` in the
   ``Homework/Files/expr`` directory in the public class repository.
   
2. The second is a subset of this that does not include functions and
   is defined in the ``expr_let_typing.ml`` file in the same directory.
   
3. The third is a version of the "full" language with type annotations
   on identifier declarations.  This will be provided soon (by Mar 6
   at the latest).
   
Copy each of these files into a directory named ``Hwk_04`` in
your personal GitHub course repository.

Each of these files has a number of ``assert`` expressions that you
can un-comment when you are ready to test your code.  Additional
asserts may be provided to you later.  Also, the feedback scripts will
be up in a few days to also help clarify some questions you may have.
But keep in mind that neither the asserts not the feedback tests are
intended to be exhaustive.  You do need to read and understand the
assignment specifications without relying completely on these.


# Part 1: Problems over the first language in ``expr_functions.ml``

The problems below should be solved in the ``expr_functions.ml``
file. 

## Serialize expressions

This problem asks you to write a function named ``serialize`` that
converts an expression to a ``string`` that is a valid OCaml
expression of type ``expr``.

For example,
```
serialize (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))
```
should evaluate to the string
```
"Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))"
```

Constructs such as ``Lambda`` and ``Let`` that have ``string``
components will need to "escape" the double quote characters that
need to be in the resulting string.

For example,
```
serialize (Lambda ("n", Add (Id "n", Val (Int 1))))
```
should evaluate to the string
```
"Lambda (\"n\", Add (Id \"n\", Val (Int 1)))"
```

To see a version of this string the way that OCaml prints ``expr``
values, that is, one that is output with the quotes not escaped, use
``print_endline``.  For example, type the following into utop or ocaml:
```
print_endline (serialize (Lambda ("n", Add (Id "n", Val (Int 1)))));;
```
You should see
```
Lambda ("n", Add (Id "n", Val (Int 1)))
- : unit = ()
```

You ~~cannot~~ can copy the ``Lambda...`` line above and paste it into utop.
This will be an easy way to check that your ``serialize`` function is
generating valid OCaml code.


Your ``serialize`` function should compute EXACTLY the string that
OCaml displays in the utop environment when it displays the value of
an ``expr``.  The only distinction can be the newlines and indentation
that OCaml will add.  So white-space will not matter in checking this.

Note that your function should only serialize values constructed by
``Int`` or ``Bool``.  Raise a ``Failure`` exception stating
``"Will only serialize integer and Boolean values"``.


## Unparse expressions

This problem asks you to write a function named ``unparse`` that
converts an ``expr`` value into a string that is what we write
in OCaml for similar computations.

For example,
```
unparse (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))
```
should evaluate to
```
"(1 + (2 * 3))"
```
Notice that ``unparse`` generates more parentheses than are needed.
This simplifies the implementation of ``unparse``.

Your function should generate output that you can copy and paste into
the OCaml interpreter (in utop) to **compute the value** of the expression.

By copying the output ``(1 + (2 * 3))`` from above, OCaml will display
``7``.

~~You need to handle quotes in a manner similar to in ``serialize``.~~
Constructs such as ``Lambda`` and ``Let`` that have ``string``
components will need to generate the actual name without double quotes
in the resulting string.

For example,
```
unparse (Lambda ("n", Add(Id "n", Val (Int 1))))
```
should evaluate to
```
"(fun n -> (n + 1))"
```
To test your function, you can copy and paste your output into utop
and let OCaml evaluate the expressions. For example, you can paste
the output ``(fun n -> (n + 1))`` in utop and you should get a function of type
``int -> int``.

For another example,
```
unparse (App (Lambda ("n", Add (Id "n", Val (Int 1))), Val (Int 4)))
```
should evaluate to
```
"((fun n -> (n + 1)) 4)"
```
If you paste ``((fun n -> (n + 1)) 4)`` into utop, you
should get the value ``5``.

Note that your function should only ~~serialize~~ unparse values constructed by
``Int`` or ``Bool``.  Raise a ``Failure`` exception stating
``"Will only unparse integer and Boolean values"``.


## Free variables

Complete the implementation ``freevars`` developed in class and copied
into the ``expr_functions.ml``file.

Our ``expr`` language defines the notion of free variables the same
way that OCaml does.  If you are unclear what should be computed by
``freevars`` for a particular expressions you can use your ``unparse``
function and just type it in to utop.  Note that in utop only the
first free variable (called "Unbound value" by OCaml) is displayed.
But using this you can discover the semantics of ``freevars`` if it is
not already clear.



## Expression evaluation 

This problem asks you to complete the ``eval`` function to evaluate
all forms of expression.

In ``expr_functions.ml`` you will find the definitions of the 
``expr``, ``value``, and ``environment`` types.  You will also find an
implementation of ``lookup``.  There is also an ``evaluate`` function
that simply calls ``eval`` for an expression starting with the empty
environment.

Some parts of ``eval`` are provided, but many of the interesting cases
need to be implemented by you.


### Step 1 - arithmetic expressions

To get stared, first ensure that ``evaluate`` will work for the
arithmetic operations and integer constants.  Much of the work for this
has been done in some of the in-class example already.

An example evaluation:

+ ``evaluate (Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Int 7``


### Step 2 - logical and relational expressions

Logical and relational operations are also straightforward:

Some sample evaluations:

+ ``evaluate (Eq (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Bool false``

+ ``evaluate (Lt (Val (Int 1), Mul (Val (Int 2), Val (Int 3))))``
  evaluates to ``Bool true``

### Step 3 - conditional expressions
Conditional expressions should also pose not significant challenge.  
For example 

```
evaluate 
 (If (Lt (Val (Int 1), Mul (Val (Int 2), Val (Int 3))),
     Val (Int 4),
     Val (Int 5)))
```
evaluates to ``Int 4  ``


### Step 4 - let expressions

We have implemented non-recursive let-expressions in a forms in
class. Adapting that work to this setting should be straightforward.


### Step 5 - non-recursive functions

We have spent some time in class discussing closures as the way to
represent the value of a lambda expression.  The slides have several
examples of this, a few of which are reproduced here.  There is
additional information in the lecture slides.

The values ``inc`` and ``add`` are defined as follows:
```
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                  Lambda ("y", Add (Id "x", Id "y"))
                 )
```

Some sample evaluations:

+ ``evaluate inc``
  evaluates to ``Closure ("n", Add (Id "n", Val (Int 1)), [])``

+ ``evaluate add``
  evaluates to ``Closure ("x", Lambda ("y", Add (Id "x", Id "y")), [])``

+ ``evaluate (App (add, Val (Int 1)))``
  evaluates to ``Closure ("y", Add (Id "x", Id "y"), [("x", Int 1)])``

+ ``evaluate  (App ( (App (add, Val (Int 1))), Val (Int 2)))``
  evaluates to ``Int 3``


### Step 6 - recursive functions

Consider the ``sumTo`` function we discussed in class.  In OCaml,
we'd write this function as follows:
```
let rec sumTo = fun n ->
      if n = 0 then 0 else n + sumTo (n-1)
in sumTo
```
To represent this function in our mini-OCaml language defined by the
``expr`` type, we'd represent the function as follows:
```
let sumToN : expr =
    LetRec ("sumToN", 
            Lambda ("n", 
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n", 
                             App (Id "sumToN", 
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )
```
Here we've given the name ``sumToN`` to the ``expr`` value that
represent our ``sumTo`` function.

Thus,
+ ``evaluate (App (sumToN, Val (Int 10)))``
  evaluates to ``Int 55``



# Part 2: Problems on the second language in ``expr_let_typing.ml``

The problems below should be solved in the ``expr_let_typing.ml``
file. 

## Serialize and unparse expressions

Copy the ``serialize`` and ``unparse`` functions you completed above
into the file for this work and remove the clauses for language
constructs (such as lambda expressions) that are not part of the
``expr_let`` language.


## Evaluation

Modify ``eval`` to return a ``value result`` instead of a ``value``.

The ``value`` type is as before, but we introduce a new ``result``
type that indicated a successful computation or that some errors have
occurred.  It is defined as follows:
```
type 'a result = OK of 'a
               | Err of error list
```

There are 3 types of errors, as defined in the ``error`` type.  Your
``eval`` function will return a value constructed by ``OK`` if there
are no errors detected during evaluation.

But if an error is detected, then that error must be propagated out as
the result.  Even though the ``Err`` constructor builds values from an
``error list``, when you use ``Err`` in ``eval`` you should only ever
give it a list with exactly one ``error``.  In a later part of this
assignment you will find an opportunity to report multiple errors.

The ``error`` and ``typ`` types are defined as follows:
```
type typ = 
  | IntType
  | BoolType

type error =
  | UnboundName of string
  | IncorrectType of expr * typ * (typ list)
  | DivisionByZero of expr
```

Our simple language only has values of that are either of type
``IntType`` or ``BoolType``.  When errors are reported they are one of
the three possible errors indicated by the three constructors:

1. When an identifier is not found in the environment, a
   ``UnboundName`` error is created.

2. When there is a type error at evaluation time, an ``IncorrectType``
   error is created.  A error ``IncorrectType (e, actual_type,
   expected_types)`` can be read as "Expression ``e`` has type
   ``actual_type`` but was expected to have one of the types in the
   list ``expected_types``."

3. The ``DivisionByZero`` error is created when the right
   sub-expression of a ``Div`` expression evaluates to zero.  The
   entire ``Div`` expression is placed into this error, not just the
   right sub-expression.
 

Below are some guidelines for implementing this new version of
``eval``.


### Values

Expressions constructed by ``Val`` will also return an ``OK`` result.

### Operators

An expression like ``Add`` will first evaluate it left
sub-expression.  If this produces and error, then that error is to be
reported.  If, on the other hand, it produces a value, then ``eval``
should evaluate the right sub-expression.  If the right sub-expression
returns a value, then a value should be computed and returned.  If the
right sub-expression produces an error then that error is to be
returned.

One exception: if the right sub-expression of ``Div`` produces a value
of ``Int 0`` then you should report a ``DivisionByZero`` error.  The
argument to this error construct should be the entire ``Div``
expression, not just the part that evaluated to zero.

Another concern: the ``Eq`` expression can operate on two integer
values or on two Boolean values to produce a value.  If the first is
an integer and the second is not, then the error message should
indicate that the second expression was expected to have a type of
``IntType``.  Similarly, if the first is a Boolean type then the
second is expected to have type ``BoolType``.

You should also write a function named ``get_type`` of type ``value ->
typ`` useful in generating error messages and in later parts of this
assignment. 


### Let expressions and identifiers

Like binary operators, we only report errors on evaluated parts of the
expression.  If the binding expression contains produces an error
instead of a value, then we cannot extend the environment with a
name/value binding to evaluate the body.  Thus, in this case we just
report the error in the binding expression and do not evaluate the
body.  If the binding expression does produce a value, then the result
of evaluating the let expression is simply the result of evaluating
the body in the extended environment.




## Type inference

For this part of the problem you will implement a function named
``check`` that checks an expression for unbound names and incorrect
types **without** evaluating the expression.

The ``check`` function has the type
```
expr -> context -> typ result
```
where
```
type context = (string * typ) list
```

The aim of this function is to return the type (that is, ``typ``) of
an expression or as many errors as feasible if some type or name
errors indicate that the expression does not have a type.

The principle here is that an expression can only be ascribed a type
if all of its sub-expressions can be given a type.  Thus an expression
such as "2 * (3 + (4 < 5))" does not "have a type" because the
addition sub-expression is erroneous.

For this function, the ``Err`` constructor may be used with multiple
``error`` values in the list.

Some guidelines for completing this function follow:

### Values
Expressions such as ``Val (Int 4)`` or ``Val (Bool true)`` are correct
in isolation and have the type ``IntType`` or ``BoolType``
respectively.

### Operators

But an expression such as 
```
Add (Val (Int 4), Val (Bool true))
```
is erroneous and we cannot compute a type for it.

Here we have a sub-expression (``Val (Bool true)``) that has a type,
but that type does not meet the typing requirements of the construct
(``Add``) that is a part of. 

We identify expressions that have a type (that is have no type errors)
but fail to meet the typing requirements of the construct they are in,
as being the expression that is erroneous.


The type inference process for ``Add``, ``Sub``, ``Mul``, and ``Div``
are all exactly the same.  Your code should be written so that no
duplicate code is used in these cases.   You should not check for
division by zero errors in this function.  Since only the trivial
cases could be detected we will not attempt to find any of them.
Thus, an expression such as 
```
Div (Val (Int 5), Val (Int 0))
```
will yield the type ``IntType``. That is,
```
check (Div (Val (Int 5), Val (Int 0))) []
```
should produce the result ``OK IntType``.


The equality construct ``Eq`` works on integers and Boolean values,
some additional care must be taken here.  If the first argument has
type ``IntType`` then the second one must as well.  If the first
argument does not have a type (that is, there is some error), 
then we can still check that the second one has
either an ``IntType`` or a ``BoolType``.  Here, these are the only two
options.  When we add function types in a later problem, this will
become a concern.

Let-expressions should first check the binding expression for errors.
If there are none, then the context can be extended and the body can
be checked in that context.  If the binding expression doesn't have a
type (and thus has errors) we will just report those error and not
check the body of the let.



## Relating ``eval`` and ``check``

In writing ``eval`` and ``check`` you should keep in mind that they
are related.  It should be the case that if the ``check`` function
does not find any type errors (unbound name or incorrect types) then
``eval`` should compute a value that has the same type as the original
expression. 

```
P(e) = forall v in value, t in typ .
       check e [] = OK t => 
       ( (eval e [] = OK v  && get_type v = t) ||
         (eval e [] = Err [ DivisionByZero e' ], for some expression e')
       )
```

Statements such as this are common in verification of programming
language.  Together, these are called "progress" and "preservation"
statements.  These state that if type checking succeeds, then
evaluation will result in a value (progress) and the resulting value
has the same types as the original expression (preservation).

We have the caveat, that type checking will not detect division by
zero.  So it is possible for a specific type of error to occur.  But
we are guaranteeing that the other two types or error will not occur
during evaluation.

While you are not (yet) asked to prove this property, your functions
should satisfy it.


# Part 3: Type checking the full language

~~This specification will appear soon - by March 6 at the latest.  When
it is ready and announcement on Canvas will be sent.~~
This part is now extra credit.
You can find it [here](Hwk_04_Programs_as_Data_Extra_Credit.md).


