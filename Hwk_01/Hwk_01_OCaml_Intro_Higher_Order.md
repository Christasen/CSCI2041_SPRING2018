# Homework 1: Recursive and higher order functions in OCaml

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Monday, February 12, at 11:59pm

All Feedback tests for this homework are active.

## Introduction

Below, you are asked to write a number of OCaml functions.  

Part 1 asks you to write some simple functions, for example, to
determine if an integer input is even or compute the greatest common
divisor of two positive integers.

Part 2 begins an exploration of higher order functions and
re-implements common list functions using folds. You **cannot** write
any recursive functions for this part.

Part 3 is based on a word puzzle and computes all possible answers to
the puzzle given a list of words.  You must use higher order functions
from the ``List`` module and you **cannot** write any recursive functions
for this part.

Part 4 also requires the use of higher order functions to write a
paragraph formatting function. You **cannot** write any recursive functions
for this part.

Recall that while some labs may be done collaboratively, **this work
must be done on your own.**


## Part 1.

Create a directory in your personal repository named ``Hwk_01``.  All
files mentioned below need to be placed into this directory.

Place your solution to these problems in a file named ``intro.ml``.

### An even function

Write an OCaml function named ``even`` with the type ``int -> bool``
that returns ``true`` if its input is even, ``false`` otherwise.

Recall that we used the OCaml infix operator ``mod`` in class.  You
may find it useful here.

Some example evaluations:
+ ``even 4`` evaluates to ``true``
+ ``even 5`` evaluates to ``false``


### Euclid's GCD algorithm

In class we wrote a greatest common divisor function that computed the
GCD of two positive integers by counting down by 1 from an initial
value that was greater than or equal to the GCD until we reached a
common divisor.

You are now asked to write another GCD function that is both simpler
to write and faster.

This one is based on the following observations:
+ gcd(a,b) = a, if a = b
+ gcd(a,b) = gcd(a, b-a), if a<b
+ gcd(a,b) = gcd(a-b,b) if a>b

This function should be named ``euclid`` and have the type ``int ->
int -> int``.

To get full credit on this problem, your solution must be based on the
observations listed above.

Some example evaluations:
+ ``euclid 6 9`` evaluates to ``3``
+ ``euclid 5 9`` evaluates to ``1``


### Simplifying fractions

We can use OCaml's tuples to represent fractions as a pair of
integers.  For example, the value ``(1,2)`` of type ``int * int``
represents the value one-half; ``(5,8)`` represents the value
five-eighths.

Write a fraction function that simplifies fractions.  It should
be called ``frac_simplify`` with type ``(int * int) -> (int * int)``.

Consider the following sample evaluations:
+ ``frac_simplify (8,16)`` evaluates to ``(1,2)``
+ ``frac_simplify (4,9)`` evaluates to ``(4,9)``
+ ``frac_simplify (3,9)`` evaluates to ``(1,3)``

As before, you may assume that the denominator is never 0.

You may want to use your ``euclid`` function in writing ``frac_simplify``.


### Max in a list of integers

Write a function called ``max`` of type ``int list -> int`` that
returns the maximal element of a non-empty list of integers.  These
may be positive, negative, or 0.

If the input list is empty raise an exception using the expression
```
raise (Failure "Input list must not be empty")
```


### Taking elements from a list

Write a function named ``take`` with the type ``int -> 'a list -> 'a
list``.  The first argument determines how many of the elements of the
second argument to return.

For example
+ ``take 3 [1;2;3;4;5]`` evaluates to ``[1;2;3]``.
+ ``take 0 ['a'; 'b'; 'c']`` evaluates to ``[]``.
+ ``take (-2) ['a'; 'b'; 'c']`` evaluates to ``[]``.


## Part 2.

Place your solution to these problems in a file named ``higher.ml``.
This file should not contain definitions of any recursive functions. Thus
the keyword ``rec`` may not appear in this file.

### All evens

Write a function named ``all_evens`` that returns all of the even
numbers in a list of integers.  Use ``List.filter`` in your solution.

Your function ``all_evens`` must have the type ``int list -> int
list``.

Do not use your ``even`` function from Part 1.  Instead write a lambda
expression as input to ``List.filter``.


### Increment all

Write a function named ``increment_all`` that increments all elements
of an integer list by one.  

Your function ``increment_all`` must have the same type as
``all_evens``.


### Max of integers as a fold

Write another version of the ``max`` function from Part 1 named
``max_fold`` that uses higher order functions to compute the maximum.
It should have the same behavior as ``max`` but cannot be implemented
directly as a recursive function.


### Sum and prod in one pass

Write a function named ``sum_prod`` that folds up a list on integers
into a pair containing the sum and products.  The type of this
function will be ``int list -> int * int``.

For example,
+ ``sum_prod [1;2;3]`` evaluates to ``(6,6)``.
+ ``sum_prod [1; 2; 3; 4]`` evaluates to ``(10, 24)``.
+ ``sum_prod []`` evaluates to ``(0,1)``.
 


### Splitting up a list.

The most challenging problem in Part 2 is this one.  You are asked to
write a function named ``split`` with the following type:
```
('a -> bool) -> 'a list -> 'a list list
```

This function chops up a list into sub-lists using the function
argument to determine which elements to drop from the result and
indicate a point of chopping up the list.

For example:
+ ``split (fun x -> x mod 3 = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]``

  evaluates to ``[[1; 2]; [4; 5]; [7; 8]; [10]]``.

The integers that are multiples of 3 indicate where to chop up the
list.  These values also do not appear in the output.

Consider this example:
+ ``split (fun x -> x mod 3 = 0) [1; 2; 3; 3; 4; 5; 6; 6; 6; 7; 8; 9 ;10]``

  evaluates to ``[[1; 2]; []; [4; 5]; []; []; [7; 8]; [10]]``.

Here, we see that an empty list is returned after ``[1; 2]``.  This
is because there is an empty list between the two ``3``s.  Similarly,
we see two empty lists after ``[4; 5]`` because there are three
multiples of 3 in a row.

For the base case, 
+ ``split (fun x -> x mod 3 = 0) []`` 

  evaluates to ``[ [] ]``.  

Although this may seem odd, this simplifies the implementation of
``split`` in that we may use an accumulator in a fold that indicates
the "current group" of elements we are collecting is the empty list.

You might design your function thinking only of the first two examples
and your solution will likely naturally satisfy this requirement for
the empty list.

You might be sure to understand how the function ``group_by_3`` works.


## Part 3. Playing the puzzle

We will have discussed this problem in lecture and you can find the
inspiration for it here:

https://www.npr.org/2018/01/21/579110492/sunday-puzzle-it-takes-two

You will write two functions for this part:
- ``answers : string -> string list``
- ``pretty_answers : string list -> (string * string) list``

The primary work will be done by ``answers``.  It takes as input the
name of a file containing a list of words are returns all 6 letter
words whose inner 4 letters form a word also in the file.

For example, if the file contains "planet" and "lane", then
``answers`` will return the string ``"planet"`` in its output.

### Getting started.

Create a file named ``puzzle.ml`` that will contain your two
functions.

To it, add the following function for reading files:
```
let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic
```

This is the only function in this part that may contain the ``rec`` keyword. 
This function will return a list of characters. Since our output
requires a list of strings, then you may want to add the following
function to your file as well:
```
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)
```

Also, your ``split`` function from Part 2 will be useful.  Copy
that function into ``puzzle.ml`` as well.


Finally, add the following two lines
```
let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"
```
These are fine name paths to two sample dictionaries from inside your
``Hwk_01`` directory. This arrangement assumes that your individual
repository and the public class repository are both cloned into the
same directory, as instructed in Lab 1.

This will allow you to test your work with the simple expressions
``answers d1`` or ``answers d2``.


### The ``answers`` function

The ``answers`` function will need to read in the contents of the
given file.  You may then want to use ``split`` to group that original
``char list`` into a list of words, that is a ``char list list``.

From here, consider extracting the words of length 4 and 6.

It should then be a matter extracting the appropriate words from the
list of 6 letter words. Clarification: Say you have a 6 letter word 
named w6, you want to check if the substring of the inner four letters
(``String.sub w6 1 4``) contains a word from the list of 4 letter words. 
Do this for each 6 letter word, extracting those which contain a 4 letter word.

There are several ways one can go about solving this problem.  It is
probably wise to consider these steps in some details before you start
programming. 

That is, ask yourself how the input is transformed and what the OCaml
type of those intermediate values will be.


### The ``pretty_answers`` function.

This function is rather simple.  It just takes the result from
``answers`` and converts it into a list of string pairs.  The first is
the 4 letter word, the second is the 6 letter word that contains the 4
letter word.

For example, here is the result of using both function in utop:
```
utop # pretty_answers (answers d1) ;;
- : (string * string) list =
[("lane", "planet"); ("moot", "smooth"); ("hang", "change");
 ("went", "twenty"); ("nigh", "knight"); ("tree", "street");
 ("refi", "prefix"); ("brad", "abrade"); ("awes", "rawest");
 ("onto", "wonton"); ("craw", "scrawl"); ("isle", "misled");
 ("rest", "presto"); ("plan", "upland"); ("afar", "safari")]
```



### Note the file ``words-google-10000.txt`` comes from this repository:

https://github.com/first20hours/google-10000-english

## Part 4. Paragraph formatter

Here, you will write a function named ``format`` with the type
```
string -> int -> string
```
that takes a string and re-formats it as a paragraph with the width of
the integer argument.

This will be put in a file named ``formatter.ml``.


### Getting started

Many of the strings that we will test this function with contain tab
(``\t``) and newline (``\n`` or ``\r``) characters.  So that the
escaped characters are not displayed but instead used to format the
output, we will use the ``print_endline`` function that is part of the
OCaml standard library.

Add the following to your ``formatter.ml`` file:
```
let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "
```

Load this file into utop and we can see how ``print_endline`` works.
```
utop # p1 ;;
- : string = "Hello world!\n\n How are you today? \t\t I hope all is well. "

utop # print_endline p1 ;;
Hello world!

 How are you today? 		 I hope all is well. 
- : unit = ()
```

In this work you may find the functions ``implode`` and ``split`` useful. Also
we will need to test your code using the ``read_file`` function. So copy these 
into ``formatter.ml``. 

You might also add this function as it may be useful for converting
the input to ``format`` into a ``char list``:
```
let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0
```
Only the ``read_file`` and ``explode`` function
in this part may contain the ``rec`` keyword.

### Writing ``format``

Your ``format`` function will output a string in which each line
(terminated by a ``\n`` character) is not longer than the input
integer.

Here are some sample interactions to illustrate this:
```
utop # format p1 12 ;;
- : string = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."

utop # print_endline (format p1 12) ;;
Hello world!
How are you
today? I
hope all is
well.
- : unit = ()

utop # format p1 11 ;;
- : string = "Hello\nworld! How\nare you\ntoday? I\nhope all is\nwell."

utop # print_endline (format p1 11) ;;
Hello
world! How
are you
today? I
hope all is
well.
- : unit = ()

utop # format p1 20 ;;
- : string = "Hello world! How are\nyou today? I hope\nall is well."

utop # print_endline (format p1 20) ;;
Hello world! How are
you today? I hope
all is well.
- : unit = ()

utop # format p1 4 ;;
- : string = "Hello\nworld!\nHow\nare\nyou\ntoday?\nI\nhope\nall\nis\nwell."

utop # print_endline (format p1 4) ;;
Hello
world!
How
are
you
today?
I
hope
all
is
well.
- : unit = ()
```

Notice that the length of ``Hello world!`` is 12 and thus fits on a
single line when 12 is the paragraph width, but it gets split when the
paragraph width is set to 11. When the width is set to 4, which
is less than the length of ``Hello``, we don't split the single word
but still keep it in one line.

Your function will need to remove extra white space when splitting the
input up into a list of words.  The extra spaces, tabs, and new lines
are all initially removed.

In your output you should also ensure that there is no extra white
space. Thus, there should be no space characters at the beginning or 
end of a line. The function ``String.trim`` from OCaml's String
library, which removes the leading and trailing whitespace in a given
string, might be useful in meeting this requirement.


To test you solution, you might add the following to your file:
```
let p1f = "Hello world!\nHow are you\ntoday? I\nhope all is\nwell."
```
and then check that ``format p1 12 = p1f`` evaluates to ``true``.


This function transforms the input from an unformatted ``string`` to a
formatted ``string``. In doing so, you may need to represent the text
in a number of different ways.  Perhaps as a ``char list`` or a ``char
list list`` after the words have been identified.  Then perhaps the
formatting process will output a list of lines (for some
representation of line) that need to be converted into a ``string`` to
finally be output.

Note that you may want to consider strings like "world!" or "today?"
as a single word since that punctuation should not stripped off and
should not appear, for example, on the following line.  We saw this in
the ``p1`` example where the width was 11.

### Some advice

As with the word puzzle problem, there are several ways in which you
can go about solving this problem. You should
probably consider the intermediate steps in processing the text and
the types of those intermediate values in some detail before you start
programming. 

Then, work to implement each step in order to make progress towards
the solution.
