# Why writing this
I see many students seem to not having a great time understanding fold_left, fold right and the difference of "::" and "@". So I thought it might be helpful to post it here to help you understand it. I personally love to use graphs to understand how things work so I hope the below graphs might help.

## "::" and '@' operators

![](https://upload.wikimedia.org/wikipedia/commons/3/3e/Right-fold-transformation.png)

From the list on the left, you can see how a list in Ocaml is built. It's actually a **tree** structure.

### how "cons" work?

--> []
--> 5 :: []
--> 4 :: (5 :: []), or **4 :: [5]**
--> 3 :: (4 :: (5 :: [])), or **3 :: [4 ; 5]**
...

You are always "consing" an **element** to the **left** side of the **list**, or `elem :: list`

That's how it works

When you cons 3 to [4;5], this is what you are doing:
![](https://github.com/floydchenchen/pictures/blob/master/WechatIMG106.jpeg?raw=true)

That's why you can only "cons" an **element** to the **left side** of the **list**. Basically you are building a new root whose left child is 3, and right child is [4;5].

### How "@" works?
Rather than building a new tree (how "cons" work), you are connecting two trees, by changing the right child "[]" to another tree
![](https://github.com/floydchenchen/pictures/blob/master/WechatIMG107.jpeg?raw=true)

## fold_left and fold_right


### fold_right
![](https://upload.wikimedia.org/wikipedia/commons/3/3e/Right-fold-transformation.png)

```
let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (accum: 'b) : 'b = 
  match lst with
  | [] -> base
  | x::xs -> f x (foldr f xs base)
```


In this case, `z` in the graph is the initial value for `accum`


#### fold_left
![](https://upload.wikimedia.org/wikipedia/commons/5/5a/Left-fold-transformation.png)

```
let rec foldl (f: 'b -> 'a -> 'b) (accum: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> accum
  | x::xs -> foldl f (f accum x) xs
```

In this case, `z` in the graph is the initial value for `accum`

##### important things about the types

the function `f` takes in two arguments:

* an accumulator `accum`, which is also **the return type** (`'b`)!
* an element(`'a`) from the list(`'a list`)

**Then the value that the function (`f accum x`) returns becomes the next value for `accum`!**

In the graph above, `z` is the initial value for `accum`.

##### Sample graph that uses fold_left to sum up every element in a list

fold_left:

```
let sum lst =
  List.fold_left (fun accum x -> accum + x) 0 lst 
```

![](https://github.com/floydchenchen/pictures/blob/master/WechatIMG108.jpeg?raw=true)


Since we are "folding" the tree forward from **the first element** from the **left child** of the tree, this method is called `fold_left`

fold_right:

```
let sum lst =
  List.fold_right (fun x accum -> accum + x) lst 0
```
![](https://github.com/floydchenchen/pictures/blob/master/WechatIMG110.jpeg?raw=true)

Since we are "folding" the tree backward from **the last element** from the **right child** of the tree, this method is called `fold_right`


#### In a nutshell
* `accum`:
    * `accum` stores the current result at element `x`; 
    * `accum` also stores the result of `f accum x` or `f x accum` in the next element of `x`
    * after going over the entire list, the final `accum` value is returned.
* `fold_left` "folds" from the left child from the first element; `fold_right` "folds" from the right child from the last element;
* If you are trying to understand the code for `fold_left` and `fold_right`, look at the graph above them.
* What do `fold_left` and `fold_right` do? Two methods to apply a function `f` to every element in the list and store the current result in `accum`, the accumulator, and return the final `acum` which stores the accumulated result after going over the entire list.
* "::": (element of the list) :: list; only cons element(s) to the left of the list because of the binary tree structure of the list.
* "[]": list @ list; combine the two binary tree representation of lists.

