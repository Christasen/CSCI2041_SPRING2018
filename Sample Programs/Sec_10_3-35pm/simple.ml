(* A collection of simple functions for used in introductory
   lectures on OCaml. 
   by Eric Van Wyk
 *)

let inc_v1 = fun x -> x + 1

let inc_v2 x = x + 1 

let square x = x * x

let cube x = x * x * x

let add x y = x + y

let inc_v3 = add 1

let add3 x y z = x + y + z

let greater x y = if x > y then x else y

let circle_area radius =
  let pi = 3.1415 in
     pi *. pi *. radius

let rec power : int -> (float -> float)
  = fun x ->
      ( fun y ->
          if x = 0 then 1.0 else y *. power (x-1) y )

let rec power_v2 exp base = if exp = 0 then 1.0
                            else base *. power_v2 (exp-1) base

let cube = power 3

let foo = cube 2.4
let bar = (power 3) 2.4

let gcd m n = 
  let smaller = if m < n then m else n
  in 
  let rec help guess = 
    if m mod guess = 0 && n mod guess = 0
    then guess
    else help (guess - 1)
  in
  help smaller



let rec all xs =
  match xs with
  | [] -> true
  | x::rest -> if x then all rest else false


let rec even2ways xs =
  match xs with
  | [] -> true
  | p::[] -> false
  | x1::(x2::rest) -> x1 mod 2 = 0 &&
                      x2 mod 2 = 0 && 
                      even2ways rest

(* an example of pattern matching on int values *)
let rec even x =
  match x with
  | 0 -> true
  | 1 -> false
  | _ -> even (x-2)


let rec sum xs =
  match xs with
  | [] -> 0 
  | x::rest -> x + sum rest

let rec string_concat sep ss = 
  match ss with
  | [] -> ""
  | s::[] -> s
  | hd::tl -> hd ^ sep ^ string_concat sep tl

(* Jan 24.  Note that:
   sum - process 1 at a time, look at 1 at a time
   evens2ways - process 2 at a time, look at 2 at a time
   sum_sqrdiffs - process 1 at a time, look at 2 at a time
 *)


let is_empty xs =
  match xs with
  | [] -> true
  | _  -> false

let is_empty' xs = xs = [] 

let not_empty xs =
  match xs with
  | [] -> false
  | _  -> true

(* let not_empty' xs = xs = _ :: _  *)
let not_empty'' xs = not (xs =[] )


let rec sum xs =
  match xs with
  | [] -> 0 
  | x::rest -> x + sum rest

let rec length xs =
  match xs with
  | [] -> 0 
  | _::rest -> 1 + length rest


let head xs = 
  match xs with
  | x::rest -> x
  | [] -> raise (Failure "oh nuts!")

let rec sum_v2 = function
  | [] -> 0 
  | x::rest -> x + sum_v2 rest

let rec sum xs =
  match xs with
  | [] -> 0 
  | x::rest -> x + sum rest

let first t =
  match t with 
  | (f,_,_) -> f

let first' (f,_,_) = f

let first'' = function
  | (f,_,_) -> f


(* Jan 29 - tuples, pulling the pieces together *)

let m = [ ("dog", 1); ("chicken",2); ("dog",3);
          ("cat",5)]

let rec lookup_all s m = 
  match m with
  | [] -> []
  | (x,y)::rest -> if x = s 
                   then y :: lookup_all s rest
                   else lookup_all s rest
let rec lookup_all' s m = 
  match m with
  | [] -> []
  | (x,y)::rest when  x = s ->
                y :: lookup_all' s rest
  | _ ::rest-> lookup_all' s rest

let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | x -> fib (x-1) + fib (x-2)


