let m = [ ("dog", 1); ("chicken",2); ("dog",3); 
          ("cat",5)]

let rec lookup_all s m =
  match m with
  | [] -> []
  | (name,value)::ms -> 
     let rest = lookup_all s ms
     in if s = name  then value :: rest else rest

(* find all by : (’a -> ’a ->bool) -> ’a ->
                        ’a list -> ’a list
 *)


let rec find_all_by cmp elem lst =
  match lst with
  | [] -> []
  | x::xs -> if cmp elem x 
             then x :: find_all_by cmp elem xs
             else find_all_by cmp elem xs

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

let find_all_by' (cmp : 'a -> 'b -> bool) elem lst =
 find_all_with (cmp elem) lst


let find_all_with' f lst = 
  let g a b = f b
  in 
  find_all_by g "0" lst

let find_all_with'' f lst = 
  let apply f a = f a 
  in 
  find_all_by apply f lst


let rec take_while lst pred =
  match lst with
  | [] -> []
  | x::xs -> if pred x 
             then x :: take_while xs pred
             else []

let rec drop_while lst pred =
  match lst with
  | [] -> []
  | x::xs -> if pred x
             then drop_while xs pred
             else lst 

let compose f g a = f (g a)
