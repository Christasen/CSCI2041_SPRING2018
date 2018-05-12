(* Continuation passing style is another mechanism for improving

   Many of these are from Charlie Harper.
*)

type 'a tree = Empty
             | Fork of 'a tree * 'a * 'a tree

let t = 
  Fork (
      Fork (
          Fork ( Empty, 1, Empty ),
          2,
          Fork ( Empty, 3, Empty )
        ),
      4,
      Fork (
          Fork ( Empty, 5, Empty ),
          6,
          Fork ( Empty, 7, Empty )
        )
    )
      
let rec flatten t = match t with
  | Empty -> []
  | Fork (t1, v, t2) -> flatten t1 @ [v] @ flatten t2


(* How can we improve the performance of this function? *)

(* Here the extra parameter is not accumulating the result in the same
   way as before. That is, we don't perform an operation on it
   directly.

   It is a continuation for the result.

   We keep adding to it.
 *)

let flatten_c t = 
  let rec flatten_with t c = match t with
    | Empty -> c
    | Fork (t1, v, t2) -> flatten_with t1 (v :: flatten_with t2 c)
  in
  flatten_with t []


(* When we speak of "continuation passing style" we typically
   mean passing a continuation that is a function.
 *)

let id x = x

let rec tail_fact_rec n k = match n with
  | 0 -> k 1
  | _ -> tail_fact_rec (n-1) (fun r -> k (r*n)) 
(* What is the trace of tail_fact on 4?
tf 4 id
tf 3 c1
 where c1 = (fun r -> id (r * 4))
tf 2 c2
 where c2 = (fun r -> c1 (r * 3))
tf 1 c3
 where c3 = (fun r -> c2 (r * 2))
tf 0 c4
 where c4 = (fun r -> c3 (r * 1))
c4 1
(fun r -> c3 (r * 1)) 1
c3 1
(fun r -> c2 (r * 2)) 1
c2 2
(fun r -> c1 (r * 3)) 2
c1 6
(fun r -> id (r * 4)) 6
id 24
24
 *)

let tail_fact n = tail_fact_rec n id

let print_fact n = 
  let k x = print_endline ("Factorial of " ^ string_of_int n ^ " is " ^
                             string_of_int x ^ ".")
  in tail_fact_rec n k

(* Recall, *)
let rec fact n = match n with
  | 0 -> 1
  | _ -> n * fact (n-1)


(* This one is not quite so basic, but that's what you
   get when linearizing traversals of bifurcating paths. *)

exception InvalidArgument

let tail_fib n =
  let rec tail_fib_rec n k =
    match n with
    | 0 -> k 0
    | 1 -> k 1
    | n -> tail_fib_rec 
           (n - 1) 
           (fun rn1 ->
             tail_fib_rec 
               (n - 2)
               (fun rn2 ->
                 (k (rn1 + rn2))) )
  in
  if n >= 0
  then tail_fib_rec n id
  else raise InvalidArgument

(*
tfr 3 id
tfr 2 c1
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
tfr 1 c2
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
 where c2 = (\rn1 -> tfr 0 (\rn2 -> c1 (rn1 + rn2)))
c2 1
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
 where c2 = (\rn1 -> tfr 0 (\rn2 -> c1 (rn1 + rn2)))
(\rn1 -> tfr 0 (\rn2 -> c1 (rn1 + rn2))) 1
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
tfr 0 (\rn2 -> c1 (1 + rn2))
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
tfr 0 c3
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
 where c3 = (\rn2 -> c1 (1 + rn2))
c3 0
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
 where c3 = (\rn2 -> c1 (1 + rn2))
(\rn2 -> c1 (1 + rn2)) 0
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
c1 (1 + 0)
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
c1 1
 where c1 = (\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2)))
(\rn1 -> tfr 1 (\rn2 -> id (rn1 + rn2))) 1
tfr 1 (\rn2 -> id (1 + rn2))
(\rn2 -> id (1 + rn2)) 1
id (1 + 1)
2
 *)


let sum_range f low high step =
  let rec sum_range_rec x k =
    if x <= high
    then sum_range_rec (x + step) (fun r -> k (r + f x))
    else k 0 
  in
  if low > high
  then raise InvalidArgument
  else sum_range_rec low id


(* Really useful for these, since the continuations allow the lists
   and values to be constructed in the correct patterns, especially
   for lists being recursively constructed non-reversed without
   using list concatenation. *)

let rec map f lst =
  match lst with
  | [] -> []
  | h::t -> f h :: map f t

(* Exercise: write a tail recursive version of map. *)

let map_c f lst =
  let rec mc f lst k =
    match lst with
    | [] -> k []
    | x::xs -> mc f xs (fun r -> k (f x :: r))
  in
  mc f lst id

let tail_fold_right f lst v =
  let rec tail_fr_rec l k =
    match l with
    | an::[] -> k (f an v)
    | a::t -> tail_fr_rec t (fun r -> k (f a r))
    | _ -> v in (* never goes down this branch *)
  match lst with
  | [] -> v
  | _ -> tail_fr_rec lst id

let tail_filter f lst =
  let rec tail_filter_rec l k =
    match l with
    | [] -> k []
    | h::t when f h -> tail_filter_rec t (fun r -> k (h::r))
    | _::t -> tail_filter_rec t k in
  tail_filter_rec lst id

(* A rewrite of tail_filter showing how some of the business logic of
   the problem can be moved into and out of the continuation. *)
let tail_filter2 f lst =
  let rec tail_filter_rec l k =
    match l with
    | [] -> k []
    | h::t -> 
       tail_filter_rec 
         t 
         (fun r -> if f h then k (h::r) else k r)
  in
  tail_filter_rec lst id



