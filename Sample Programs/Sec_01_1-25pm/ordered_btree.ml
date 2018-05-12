(* Ordered b trees 
 *)

type 'a btree = Empty
              | Node of 'a * 'a btree * 'a btree

let rec insert t i = match t with
  | Empty -> Node (i, Empty, Empty)
  | Node (v, t1, t2) 
      -> if v <= i
         then Node (v, t1, insert t2 i)
	 else Node (v, insert t1 i, t2)

let rec to_list t = match t with
  | Empty -> [ ]
  | Node(v, t1, t2) -> to_list t1 @ [v] @ to_list t2
 
let to_tree l = 
  let rec from_list l t =
    match l with
    | [ ] -> t
    | v::vs -> from_list vs (insert t v)
  in from_list l Empty 

