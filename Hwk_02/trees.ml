(* CSCI 2041 Homework2 *)
(* GUANGYU YAN *)

type 'a tree = Leaf of 'a | Fork of 'a * 'a tree * 'a tree

(*Part A*)
let t1 = Leaf 5
let t2 = Fork (3, Leaf 3, Fork (2, t1, t1))
let t3 = Fork ("Hello", Leaf "World", Leaf "!")
let t4 = Fork (7, Fork (5, Leaf 1, Leaf 2), Fork (6, Leaf 3, Leaf 4))

let rec t_size (t: 'a tree): int =
  match t with
  |Leaf e -> 1
  |Fork(e, l, r) -> 1 + t_size l + t_size r

let rec t_sum (t: int tree): int =
  match t with
  |Leaf e -> e
  |Fork(e, l, r) -> e + t_sum l + t_sum r


let rec t_charcount (t: string tree): int =
  match t with
  |Leaf e -> String.length e
  |Fork(e, l, r) -> String.length e + t_charcount l + t_charcount r

let rec t_concat (t: string tree): string =
  match t with
  |Leaf e -> e
  |Fork(e, l, r) -> e ^ t_concat l ^ t_concat r

let t5 : string option tree =
  Fork (Some "a",
        Leaf (Some "b"),
        Fork (Some "c",
              Leaf None,
              Leaf (Some "d")))

(*Part B*)
let t7 = Fork (Some 1, Leaf (Some 2), Fork (Some 3, Leaf None, Leaf None))
let t8 = Fork (Some "a", Leaf (Some "b"), Fork (Some "c", Leaf None, Leaf (Some "d")))

let rec t_opt_size (t: 'a option tree): int =
  match t with
  |Leaf Some e -> 1
  |Leaf None -> 0
  |Fork (None, l, r) -> 0 + t_opt_size l + t_opt_size r
  |Fork (Some e, l, r) -> 1 + t_opt_size l + t_opt_size r

let rec t_opt_sum (t: int option tree): int =
  match t with
  |Leaf Some e -> e
  |Leaf None -> 0
  |Fork(None, l, r) -> 0 + t_opt_sum l + t_opt_sum r
  |Fork(Some e, l, r) -> e + t_opt_sum l + t_opt_sum r


let rec t_opt_charcount (t: string option tree): int =
  match t with
  |Leaf Some e -> String.length e
  |Leaf None -> 0
  |Fork(None, l, r) -> 0 + t_opt_charcount l + t_opt_charcount r
  |Fork(Some e, l, r) -> String.length e + t_opt_charcount l + t_opt_charcount r


let rec t_opt_concat (t: string option tree): string =
  match t with
  |Leaf Some e -> e
  |Leaf None -> ""
  |Fork(None, l, r) -> "" ^ t_opt_concat l ^ t_opt_concat r
  |Fork(Some e, l, r) -> e ^ t_opt_concat l ^ t_opt_concat r

(*Part C*)
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b =
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_size (t: 'a tree): int =
  let f = fun a b c -> 1 + b + c
  in
  tfold (fun x -> 1) f t

let tf_sum (t: int tree): int =
  let f = fun a b c -> a + b + c
  in
  tfold (fun x -> x) f t

let tf_charcount (t: string tree): int =
  let f = fun a b c -> String.length a + b + c
  in
  tfold (fun x -> String.length x) f t

let tf_concat (t: string tree): string =
  let f = fun a b c -> a ^ b ^ c
  in
  tfold (fun x -> x) f t

let tf_opt_size (t: 'a option tree): int =
  let l (t1: 'a option): int =
  match t1 with
  |Some e -> 1
  |None -> 0
  in
  let f = fun a b c -> l a + b + c
  in
  tfold l f t

let tf_opt_sum (t: 'a option tree): int =
  let l (t1: 'a option): int =
  match t1 with
  |Some e -> e
  |None -> 0
  in
  let f = fun a b c -> l a + b + c
  in
  tfold l f t

let tf_opt_charcount (t: string option tree): int =
  let l (t1: string option): int =
  match t1 with
  |Some e -> String.length e
  |None -> 0
  in
  let f = fun a b c -> l a + b + c
  in
  tfold l f t

let tf_opt_concat (t: string option tree): string =
  let l (t1: string option): string =
  match t1 with
  |Some e -> e
  |None -> ""
  in
  let f = fun a b c -> l a ^ b ^ c
  in
  tfold l f t

(*Part D*)
type 'a btree = Empty
              | Node of 'a btree * 'a * 'a btree

let t6 = Node (Node (Empty, 3, Empty), 4, Node (Empty, 5, Empty))

let rec bt_insert_by (f:'a -> 'a -> int) (elem: 'a) (t: 'a btree): 'a btree =
match t with
|Empty -> Node(Empty, elem, Empty)
|Node(l, v, r)-> if f v elem < 1 then Node(l, v, bt_insert_by f elem r)
else Node(bt_insert_by f elem l, v, r)


let rec bt_elem_by (f: 'a -> 'b -> bool) (elem: 'b )(t: 'a btree): bool =
match t with
|Empty -> false
|Node(lt, v, rt) -> (bt_elem_by f elem lt) || f v elem || (bt_elem_by f elem rt)


let rec bt_to_list (t: 'a btree): 'a list =
match t with
|Empty -> []
|Node(l, v, r) -> bt_to_list l @ [v] @ bt_to_list r


let rec btfold (l: 'b) (f: 'b -> 'a -> 'b -> 'b)  (t: 'a btree) : 'b =
         match t with
         | Empty -> l
         | Node (t1, v, t2) -> f (btfold l f t1) v (btfold l f t2)

let btf_elem_by (f: 'a -> 'b -> bool) (elem: 'a) (t: 'a btree): bool =
btfold false (fun x y z -> x || f elem y || z) t

let btf_to_list (t: 'a btree): 'a list = btfold [] (fun x y z -> x @ [y] @ z) t




(*btf_insert_by*)
(*The reason why using fold in this case may cause complexity
is because when using btfold, you would always get a 'b value, but
in this case, we want to get a a' btree and if we using the fold,
the function f would be complex, it is better we just use rec method*)
