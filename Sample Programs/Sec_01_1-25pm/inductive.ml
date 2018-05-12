type color = Red | Green | Blue 

let isRed c = 
  match c with
  | Red -> true
  | Green -> false
  | Blue -> false

type weekday = Mon | Tue | Wed | Thr | Fri | Sat | Sun

let isWorkDay d =
  match d with
  | Sat | Sun -> false
  | _ -> true

type intorstr = Int of int | Str of string

type coord = float * float
type circ_desc = coord * float
type tri_desc = coord * coord * coord
type sqr_desc = coord * coord * coord * coord

type shape = 
   Circle of circ_desc 
 | Triangle of tri_desc 
 | Square of sqr_desc

let area (s: shape) : float =
  match s with
  | Circle ( _, r ) -> 3.1415 *. r *. r
  | _ -> 45.0



type 'a maybe = Nothing | Just of 'a

let mysqrt x = 
  if x < 0.0 then Nothing
  else Just (sqrt x )


let listHd : 'a list -> 'a option = function
  | [] -> None
  | hd::_ -> Some hd


type 'a myList = Nil | Cons of 'a * 'a myList

let empytlist = Nil
let alist = Cons ( 3, Cons ( 2, Cons ( 1, Nil ) ) ) 

let rec sumMyList (lst: int myList) : int   =
  match lst with
  | Nil -> 0
  | Cons(x, xs) -> x + sumMyList xs

type 'a btree = Empty
              | Node of 'a * 'a btree * 'a btree

let t7 = Node ( 7, Empty, Empty )
let t13 = Node ( 13, Empty, Empty )
let t10 = Node ( 10, t7, t13 )

let t = Node (10, Node (7, Empty, Empty), Node (13, Empty, Empty))

let rec sumTree t = 
  match t with
  | Empty -> 0 
  | Node (x, l, r) -> x + sumTree l + sumTree r

let rec isElem (t: 'a btree) (elem: 'a) : bool =
  match t with
  | Empty -> false
  | Node (e, tl, tr) -> elem = e || isElem tl elem || isElem tr elem

let rec inc100 (t: int btree) : int btree =
  match t with
  | Empty -> Empty
  | Node (e, tl ,tr) -> Node (e + 100, inc100 tl, inc100 tr) 

let rec treeMap (f: 'a -> 'b) (t: 'a btree) : 'b btree =
  match t with
  | Empty -> Empty
  | Node (e, tl, tr) -> Node (f e, treeMap f tl, treeMap f tr)

(* fold functions for tree? 
   left - "accumulate" 
   right - "reduce"
 *)

let rec treeReduce (f: 'a -> 'b -> 'b -> 'b) (base: 'b) (t: 'a btree) : 'b =
  match t with
  | Empty -> base
  | Node (e, tl, tr) -> f e (treeReduce f base tl)
                            (treeReduce f base tr)

let add3 x y z = x + y + z

let tstr = Node ("a", Node ("Hello", Empty, Empty),
                 Node("Why?", Empty, Empty) )

let concat3ints x s1 s2 = string_of_int x ^ s1 ^ s2
