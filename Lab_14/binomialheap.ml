open Ordered

module type BinomialHeapSig = sig
    type elem
    type tree = Node of int * elem * tree list
    type t = tree list
    val empty :t
    val isEmpty : t -> bool
    val insert : elem -> t -> t
    val merge : t -> t -> t
    val findMin : t -> elem
    val deleteMin : t -> t
    val findMinDirect: t -> elem
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


    let rec findMinDirect t : elem =



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
