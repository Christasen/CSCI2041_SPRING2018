let rec power : int -> (float -> float)
          = fun exponent base ->
              if exponent < 0
              then power (0-exponent) (1.0 /. base)
              else 
              if exponent = 0
              then 1.0  
              else base *. power (exponent-1) base

let cube = power 3 

let gcd m n =
  let smaller = if m < n then m else n
  in
  let rec f guess =
      if m mod guess = 0 && n mod guess = 0 then guess
      else f (guess - 1)

  in
  f smaller


let rec sum xs =
  match xs with
  | [] -> 0 
  | x::rest -> x + sum rest

let rec all xs =
  match xs with
  | [] -> true
  | x::rest -> if x then all rest else false

let rec even2ways xs =
  match xs with
  | [] -> true 
  | x::[] -> false
  | x1::(x2::xs) -> if x1 mod 2 = 0 && x2 mod 2 = 0 
                        && even2ways xs then true else false

let rec is_even n = 
  match n with 
  | 0 -> true
  | 1 -> false
  | n' -> is_even (n-2)

let rec even2ways_better xs =
  let even x = x mod 2 = 0
  in match xs with
     | [] -> true 
     | x::[] -> false
     | x1::(x2::rest) ->
        x1 mod 2 = 0 && x2 mod 2 = 0 && even2ways rest 

(* Jan 24.  Note that:
   sum - process 1 at a time, look at 1 at a time
   evens2ways - process 2 at a time, look at 2 at a time
   sum-sqr-diffs - process 1 at a time, look at 2 at a time
 *)

let rec string_concat sep ss =
  match ss with
  | [] -> ""
  | s::[] -> s
  | s1::s2::[] -> s1 ^ sep ^ s2
  | s::tail -> s ^ sep ^ string_concat sep tail


let is_empty xs =
  match xs with
  | [] -> true
  | _ -> false


let rec length xs = 
match xs  with
| [] -> 0 
| _::rest -> 1 + length rest


let head xs = 
  match xs with
  | x::rest -> x

let first triple = 
  match triple with
  | (x,y,z) -> x 

let first' (x,y,z) = x

let first'' = function
  | (x,_,_) -> x 


(* Jan 29 - tuples, pulling the pieces together *)

let m = [ ("dog", 1); ("chicken",2); ("dog",3); ("cat",5)]

let rec lookup_all s lst =
  match lst with
  | [] -> []
  | (name,value) :: rest -> 
     let more = lookup_all s rest
     in
     if s = name 
     then value :: more
     else more

let rec lookup_all' s lst =
  match lst with
  | [] -> []
  | (name,value) :: rest when s = name -> value :: lookup_all' s rest
  | _ :: rest -> lookup_all' s rest

let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | x -> fib (x-1) + fib (x-2)
