type color = Red | Green | Blue

type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun 

type boolean = True | False

let isRed c =
  match c with
  | Red -> true
  | Green -> false
  | Blue -> false

let isWorkday d =
  match d with
  | Sat | Sun -> false
  | _ -> true

type intorstr = Int of int | Str of string

type coord = float * float
type circ_desc = coord * float
type tri_desc = coord * coord * coord
type sqr_desc = coord * coord * coord * coord

type shape =
  | Circ of circ_desc
  | Tri of tri_desc
  | Sqr of sqr_desc

let isRect = function
  | Sqr _ -> true
  | _ -> false

let area s =
  match s with
  | Circ ( (_,_), r ) -> 3.1415 *. r *. r
  | Tri _ -> 45.0

type 'a maybe = Nothing | Just of 'a 

let divide n d =
  if d = 0
  then Nothing
  else Just (n / d)

let listHd lst = 
  match lst with
  | [] -> None
  | x::_ -> Some x 

type 'a myList = Nil | Cons of 'a * 'a myList 
type intList = IntNil | IntCons of int * intList

type ('a, 'b) dictionary = ('a * 'b) list

let l1 = Nil
let l2 = Cons (1, Cons (2, Cons (3, Nil)))

let rec sumList lst =
  match lst with
  | Nil -> 0
  | Cons(h,t) -> h + sumList t

type 'a btree = Empty 
              | Node of 'a * 'a btree * 'a btree

let treeAmin = Node (1, Node(2, Empty, Empty), Node(3, Empty, Empty))

let rec sumTree t =
  match t with
  | Empty -> 0
  | Node (a, left, right) -> a + sumTree left + sumTree right

let tstr = Node ("a", Node ("Hello", Empty, Empty),
                 Node("Why?", Empty, Empty) )
let rec concatTree (t: string btree) : string =
  match t with
  | Empty -> ""
  | Node (a, left, right) -> concatTree left ^ a ^ concatTree right

let rec treeMap (f: 'a -> 'b) (t: 'a btree) : 'b btree =
  match t with
  | Empty -> Empty
  | Node(a, left, right) -> 
     Node (f a, treeMap f left, treeMap f right)

(* folding up tree 
   - left - "accumulate"
   - right - "reduce" - 
      - replacing data constructors with functions
         and then reducing the resulting expression

 *)

(* Node: 'a -> 'a btree -> 'a btree -> 'a btree *)
let rec treeFold (f : 'a -> 'b -> 'b -> 'b) (base: 'b)
                 (t :'a btree) : 'b = 
  match t with
  | Empty -> base
  | Node (a, left, right) -> f a
                               (treeFold f base left)
                               (treeFold f base right)

let add3 (x: int) (y :int) (z: int) : int = x + y + z

let a_sum = treeFold add3 0 treeAmin                 

let rec treeFoldLeft (f : 'a -> 'b -> 'b) (accum: 'b)
                     (t :'a btree) : 'b = 
  match t with 
  | Empty -> accum 
  | Node (a, left, right) ->
     let step1 = f a accum
     in
     let step2 = treeFoldLeft f step1 left
     in
     treeFoldLeft f step2 right

let sumLeft (t: int btree) : int = 
  let f elem accum = elem + accum
  in
  let accum = 0
  in
  treeFoldLeft f accum t

let toPreOrderList (t: 'a btree) : 'a list = 
  let f elem accum = elem :: accum
  in
  let accum = []
  in
  List.rev (treeFoldLeft f accum t)
