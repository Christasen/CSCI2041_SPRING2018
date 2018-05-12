(* Some functions used in S4.2 Improving Performance 

Some of this material comes from section 9.4 of Chris Reade's book
``Elements of Functional Programming''.  This is in the `Resources`
directory of the public class repository.

Some were written by Charlie Harper.

 *)

(* What is problematic with these functions? 

   These are not tail recursive. We have to return and do more work. 
 *)

let rec length lst = match lst with
  | [] -> 0 
  | _::xs -> 1 + length xs

let rec sum lst = match lst with
  | [] -> 0
  | x::xs -> x + sum xs

(* 
  sum 1::2::3::[]
= 1 + sum 2::3::[]
= 1 + (2 + sum 3::[]))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
 *)

let rec listof n =
  match n with
  | 0 -> []
  | _ -> n :: listof (n-1)

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | x::xs -> x :: (append xs l2)

let rec rev lst =
  match lst with
  | [] -> []
  | x::xs -> append (rev xs) [x]


(* Problem: how can we improve ``sum``, for example? *)

(* What does an imperative version look like? 
   sum = 0
   while ( lst matches x::xs ) do
     sum = sum + x  
     lst = xs  

   What is a trace of the values of sum in this code?

lst = 1::2::3::[]
sum = 0
x = 1, xs 2::3::[]
sum = 0 + 1
lst = 2::3::[]
x = 2, xs = 3::[]
sum = (0 + 1) + 2
...
sum = ((0 + 1) + 2) + 3

 *)



let sum_a lst = 
  let rec accsum acc lst = match lst with
    | [] -> acc
    | x::xs -> accsum (acc + x) xs
  in
  accsum 0 lst

(* 
What ist the evaluation trace of sum_a [1; 2; 3]?

1::2::3::[]

accsum 0 1::2::3::[]
accsum (0 + 1) 2::3::[]
accsum ((0 + 1) + 2)  3::[]
accsum (((0 + 1) + 2) + 3) []
(((0 + 1) + 2) + 3)

See Slide #6
This relies on the associativity of +.
 *)


(* So, can we do this using a fold? *)
let rec fold_left f v l =
  match l with
  | [] -> v
  | x::xs ->  fold_left f (f v x) xs

let sum_f lst = fold_left (+) 0 lst

(* What is the trace of this?   *)


(* Use an accumulating parameter to convert reverse from
   quadradic to linear time. *)
let rev_a lst = 
  let rec rev revlst lst = match lst with
    | [ ] -> revlst
    | x::xs -> rev (x::revlst) xs
  in rev [] lst


(* Does the above function remind us of imperative programming? 

   What is the trace of it?
 *)




(* Exercise #1. Tail recursive version of length. *)

let length_a lst = 
  let rec accum len lst = match lst with
    | [] -> len
    | _::xs -> accum (len + 1) xs
  in
  accum 0 lst


(* Exercise #2. Length using folds. *)



(* Fibonacci numbers *)
let rec fib n = match n with
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)
(* Exercise #3: What is the tail recursive version of the
   fib function that uses accumulators to avoid all the 
   recomputation?

   What are the Fibonacci numbers?
   nth: 0   1   2   3   4   5   6   7   8   9

        0,  1,  1,  2,  3,  5,  8, 13, 21, 34
                        ^   ^   ^ 
                        |   |   |
                        |   |___+
                        |_______|      *)
let fib' n =
  let rec fiba a b num =
    (* a = (n - num)-th fib
       b = ((n - num) + 1)-th fib
     *)
    if num = 0
    then a
    else fiba b (b + a) (num - 1)
  in fiba 0 1 n

(* See another formulation in Sec_10_3-35/tail.ml *)
