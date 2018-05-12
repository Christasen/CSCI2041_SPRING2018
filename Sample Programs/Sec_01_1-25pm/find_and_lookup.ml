let m = [ ("dog", 1); ("chicken",2); ("dog",3); ("cat",5)]

let rec lookup_all s m =
  match m with
  | [] -> []
  | (name,value)::ms -> 
     let rest = lookup_all s ms
     in if s = name  then value :: rest else rest

(* find all by : (’a -> ’a ->bool) -> ’a ->
                        ’a list -> ’a list
 *)

let rec find_all_by eq elem lst =
  match lst with
  | [] -> []
  | x::xs when eq elem x -> x :: find_all_by eq elem xs
  | _::xs -> find_all_by eq elem xs

let rec find_all_with f l =
  match l with
  | [] -> []
  | x::xs ->
     let rest = find_all_with f xs
     in if f x then x::rest else rest

(* Implementing find_all_by using find_all_with, and
   implementing find_all_with using find_all_by.

   We can do both - despite what your slides say.

   Here are a few implementations. 
 *)

let find_all_by' eq elem lst = find_all_with (eq elem) lst

let find_all_with' f lst = 
  let g x y = f y
  in 
  find_all_by g "0" lst

let find_all_with'' f lst = 
  let apply f a = f a 
  in 
  find_all_by apply f lst



let rec take_while lst f =
  match lst with
  | [] -> []
  | x::xs -> if f x
            then x :: take_while xs f
            else []

let rec drop_while lst f =
  match lst with
  | [] -> []
  | x::xs -> if f x
             then drop_while xs f
             else lst

let flip (f: 'a -> 'b -> 'c) (a: 'a) (b: 'b) = f b a 

(*compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) *)
let compose f g x = f (g x)
let compose' = 
  fun f -> fun g -> fun x -> f (g x)
