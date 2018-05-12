# A simple expression language

The OCaml files in this directory define an series of increasingly
complex (but still quite simple) langauge and the functions evaluate
expressions in these language and perform a various static analyses
over them.

There are 4 languages, each building on the previous one:
- simple arithmetic in ``expr_arithmetic.ml``
- arithmetic with Boolean valued comparison operators in ``expr_int_bool.ml``
- adding let-expresssions and identifiers, in ``expr_let.ml`` and ``expr_let_typing.ml``
- adding lambda-expressions, function application, and recursive
  let-expressions, in ``expr_functions.ml``


In most of these files, typing is done dynamically - as the expression
evaluates.

The exception is ``expr_let_typing.ml`` where static type inference is
performed. 



Note that these 4 files are similar to the ones we developed in class.
Those were ``arithmetic.ml``, ``int_bool_expr.ml``, and
``expr_let.ml`` (which was renamed to ``let_expr.ml``).  These are
left as they were from class, but the 4 above have been cleaned up and
commented.  Sample ``assert`` expressions have been added for simple
testing.

