# Homework 2: Inductive Data Types

*CSci 2041: Advanced Programming Principles, Spring 2018*

**Due:** Friday, March 2, at 5:00 PM.

**Changelog:**
+ Monday, February 19: (Sean) **`btf_insert_by`** now emphasizes the
word _comment_, and explicitly states that you need not write
a `btf_insert_by` function; Added "Introduction" heading to introduction.

## Introduction

This homework will focus on tree-based inductive data types and on
functions for working with them.

Create a directory in your personal repository named ``Hwk_02``.  

Place your solution to the problems below in a file named ``trees.ml``.

## Part A: 

For this part of the assignment, use the following type for ``tree``:
```
type 'a tree = Leaf of 'a
             | Fork of 'a * 'a tree * 'a tree
```

Some examples using this: 
```
let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))
```

Now you will implement the following functions.

#### 1. ``t_size``

Given an ``'a tree``, write a function ``t_size`` to find the size of the
tree. Here, size is the number of constructors (either ``Leaf`` or
``Fork``) used in the tree.  This function will be of this type:

``t_size : 'a tree -> int``

Example: ``t_size t3`` gives ``3``. 

#### 2. ``t_sum``

Given an ``int tree``, write a function ``t_sum`` to find the sum of the
values in the tree. The function will have this type:

``t_sum : int tree -> int``.

Example: ``t_sum t4`` gives ``28``.

#### 3. ``t_charcount``

Write a function ``t_charcount`` that takes a ``string tree`` as input and
counts the total number of characters that the values contain. This is the
function's type:

``t_charcount : string tree -> int``

Example: ``t_charcount t3`` gives ``11``. 

#### 4. ``t_concat``

Now, write a function ``t_concat`` that will concatenate together the
values of a string tree. The function's type is here:

``t_concat : string tree -> string``

Example: ``t_concat t3`` gives ``"HelloWorld!"``. 

## Part B: 

In this part, you will introduce ``option``s into the above tree
type and handle those cases. 

An example: 
```
let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))
```
This is a tree of type ``string option tree``. More examples:
```
let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))
```
(Can you say what type of tree ``t7`` is?)

Write 4 new functions, similar to the ones above, but that work on
trees with ``option`` values in them. For example, ``t_opt_size``
should count the number of values in the tree that are under the ``Some``
constructor.  The types are as follows:

1) ``t_opt_size : 'a option tree -> int``

Example: ``t_opt_size t7`` gives ``3``.

2) ``t_opt_sum : int option tree -> int``

Example: ``t_opt_sum t7`` gives ``6``.

3) ``t_opt_charcount : string option tree -> int``

Example: ``t_opt_charcount t8`` gives ``4``.

4) ``t_opt_concat : string option tree -> string``

Example: ``t_opt_concat t8`` gives ``"abcd"``.

## Part C: 

Consider the function ``tfold`` with type ``('a -> 'b) -> ('a -> 'b -> 'b
-> 'b) -> 'a tree -> 'b`` below.  It is similar to one introduced in class.  
```
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b = 
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)
```

The next set of questions will require you to write all the functions
in Part A and Part B using ``tfold`` and without writing a recursive
function. 

Their names and types are as follows:  
```
1) tf_size : 'a tree -> int

2) tf_sum : int tree -> int

3) tf_charcount : string tree -> int

4) tf_concat : string tree -> string

5) tf_opt_size : 'a option tree -> int

6) tf_opt_sum : int option tree -> int

7) tf_opt_charcount : string option tree -> int

8) tf_opt_concat : string option tree -> string
```

## Part D: 

This is the final part of this homework.  Instead of using the above ``tree``,
will we now use a tree of this type:  
```
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree
```
This is a sorted tree, and will enable us to create empty trees as
well as trees with size two. An example:

```
let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))
```

Using this btree type, perform the following: 

#### ``bt_insert_by``

Write a function ``bt_insert_by`` that will take a comparator (details
follow), an element and a btree as the input, and insert this element into the
btree using the comparator to find the correct position. Because the tree
is sorted, the insertion criteria are as follows:  

1. The values in the left subtree are always less than or equal to the
   value of the Node.  
2. The values in the right subtree are always greater that the value of
   the Node.

The comparator function has the type ``'a -> 'a -> int``. If the
first argument is larger than the second then the comparator evaluates
to ``1``. If they are equal then it evaluates to ``0``. Otherwise, it
evaluates to ``-1``.

Now, here is ``bt_insert_by``'s type:

``bt_insert_by : ('a -> 'a -> int) -> 'a -> 'a btree -> 'a btree``

Example: Using the built-in comparator function ``compare``,
``bt_insert_by compare 6 t6`` evaluates to this:

```
int btree = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Node (Empty, 6, Empty)))
```

You may use the built-in comparator function ``compare``, but ensure
that you understand how it actually works, lest it inserts the element
in the incorrect location.

#### ``bt_elem_by``

Now write a function ``bt_elem_by`` which takes as inputs a function,
an element, and a btree, and evaluates to true if the element exists
in the tree, and false otherwise. The type is here:

``bt_elem_by : ('a -> 'b -> bool) -> 'b -> 'a btree -> bool``

Example: ``bt_elem_by (=) 4 t6`` gives ``true``.

#### ``bt_to_list``

Create a function ``bt_to_list`` that will take as input a btree and
create a list of all the values in the btree. The type is as follows:

``bt_to_list : 'a btree -> 'a list``.  

Example: ``bt_to_list t6`` gives ``int list = [3; 4; 5]``. 

#### ``btfold``

Like ``tfold``, now create a function ``btfold`` of type ``'b ->
('b -> 'a -> 'b -> 'b) -> 'a btree -> 'b``.  

#### ``btf_elem_by``

Write a function named ``btf_elem_by`` so that it has the same behavior as
``bt_elem_by`` that you wrote above, but this one must not be recursive
and must instead use ``btfold``.

#### ``btf_to_list``

Similarly, write a new function named ``btf_to_list`` that has the
same behavior as your ``bt_to_list`` function that you wrote above.
But now use ``btfold`` and no recursion.

#### ``btf_insert_by``

Finally, write a _comment_ on why using ``btfold`` for creating a
function like ``bt_insert_by`` might be difficult. (You need not
write the ``btf_insert_by`` function.) You may join a Canvas
discussion on this topic
[here](https://canvas.umn.edu/courses/39626/discussion_topics/79650).
