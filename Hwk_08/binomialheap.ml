


open Ordered
module type BinomialHeapSig = sig
    type elem
    type tree = Node of int * elem * tree list
    type t = tree list
    val empty: t
    val isEmpty : t -> bool
    val insert : elem -> t -> t
    val merge : t -> t -> t
    val findMin : t -> elem
    val deleteMin : t -> t
    val isBinomialTree: tree -> bool
    val isBinomialHeap: t -> bool
    val findMinDirect: t -> elem
    val heapRank : t -> int -> bool
    val rankvalid : t -> int -> bool
  end

module BinomialHeap (Od: OrderedSig):

    (BinomialHeapSig with type elem = Od.t) = struct
    type elem = Od.t
    type tree = Node of int * elem * tree list
    type t = tree list
    let empty = []
    let isEmpty t : bool = t = empty

    let rank (Node (r, x, c)) = r
    let root (Node (r, x, c)) = x

    let link t1 t2 = match t1,t2 with
	     |Node (r, x1 , c1), Node (_, x2 , c2 )->
	      if (Od.leq x1 x2) then Node (r+1, x1 , t2 :: c1)
	      else Node (r+1, x2 , t1 :: c2 )

    let rec insTree t ts =
	   match t,ts with
    	|t, [] -> [t]
	    |t, t':: ts' -> if rank t >= rank t' then (insTree (link t t') ts')
        else t:: ts

    let insert elemt t  = insTree (Node (0, elemt, [])) t
    let rec merge ts1 ts2  =
	     match ts1,ts2 with
       	 |[], t2 -> t2
	       |t1, [] -> t1
	       |t1::ts', t2::ts'' ->
          if rank t2 < rank t1 then t2 :: (merge ts1 ts'')
		       else if rank t1 < rank t2 then t1 :: (merge ts' ts2)
		           else insTree (link t1 t2 )  (merge ts' ts'')

    let rec removeMinTree = function
	    |[]-> raise (Failure ("Empty"))
	    |[t] -> (t, [])
	    |t::ts-> let (t',ts') = removeMinTree ts
		     in if Od.leq (root t) (root t') then (t, ts) else (t',t::ts')

    let deleteMin t = let (Node (_, x, ts1), ts2) = removeMinTree t
				   in (merge (List.rev ts1) ts2)

    let findMin t : elem
        = let (ts, _) = removeMinTree t in root ts



  (*
  For binomial trees:
  First, if we see rhe rank of the binomial tree is 0 then this tree must
  be a single node. Second, a rank r binomial tree is composed of two trees
  with a rank r-1 and this will made by procucing one tree the leftmost child
  of the other trees. Third, A rank r bionomial tree means that this tree has
  r children and we can denote its children frin t(1) to t(r). Last but not least,
  the rank of the child t(i) is r- i.




    *)

    let rec rankvalid (l: tree list) (r: int) =
      match l with
      | [] -> true
      | Node(r', e, lt) :: tl -> if r' != (r-1) then false
                                  else rankvalid tl r'


    let isBinomialTree (t: tree): bool =
      let checkr0 t = match t with
      | Node(r, e, l) -> if r > 0 then true
                         else match l with
                         | [] -> true
                         | _ -> false

      in let checkComposedT t = match t with
      | Node(r, e, l) -> if r > 0
                        then match l with
                          | Node (r', e', l') :: tl ->
                            if r' != (r - 1) then false
                            else true
                          | [] -> true

                        else true
      in let rec checkComposedTree t =
        let rec validtree tl =
          match tl with
          | [] -> true
          | x :: xs -> checkComposedT x && validtree xs
         in match t with
         |  Node(r, e, l) -> (validtree l) &&
                            (rankvalid l r)
      in (checkr0 t) && (checkComposedT t)
        && (checkComposedTree t)



      (*




        Basically, binomial heaps are composed of many binomial trees
        So for binomal heaps:
        First,the rank of the trees should be in a ascending order.
        Second, every trees in the binomal heap will have their own unique rank.
        In other words, no two trees will have the same rank.
        Like I said before, another important thing
        is that bionomial heaps would only contains binomial trees instead of other
        types of trees
*)

     let rec heapRank (b: t) (r: int): bool =
        match b with
        | [] -> true
        | hd::tl -> if (rank hd) > r then heapRank tl (rank hd)
                                      else false


     let isBinomialHeap (b: t) : bool =
       let rec checkHeap (b: t) =
               match b with
               | [] -> true
               | hd :: tl -> isBinomialTree hd && checkHeap tl
      in match b with
        | [] -> true
        | hd :: tl -> heapRank tl (rank hd)&& checkHeap b


    let rec findMinDirect t : elem =
      match t with
      | [] -> raise (Failure "Empty")
      | [x] -> root x
      | hd::tl -> let acc = findMinDirect tl
                  in
                  if acc < (root hd) then findMinDirect tl
                  else root hd


  end

module BHI = BinomialHeap(Int)

let h1 = BHI.empty
let h2 = BHI.insert 20 h1
let h3 = BHI.insert 30 h2
let h4 = BHI.insert 10 h3
let h5 = BHI.insert 40 h4
let m1 = BHI.findMin h5
let h6 = BHI.deleteMin h5
let m2 = BHI.findMin h6
let m3 = BHI.findMinDirect h6
