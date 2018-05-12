open Ordered



(*
All black nodes are the children of a red nodes and all paths from
the root to the leaf pass by the same number of black nodes.



  *)
(*signature*)
module type RedBlackSetSig = sig
  type elem
  type color = R | B
  type t = E | T of color * t * elem * t
  val empty : t
  val insert : elem -> t -> t
  val member : elem -> t -> bool
  val balance: t -> t
  val isRedBlackTree: t -> bool
  val validNode: t -> bool
  val validBTS: t -> bool
end


module RedBlackTree (Od: OrderedSig):
                   (RedBlackSetSig with type elem = Od.t) = struct
  type elem = Od.t
  type color = R | B
  type t = E | T of color * t * elem * t
  let empty = E
  let eq = Od.eq
  let leq = Od.leq
  let lt = Od.lt


  let rec balance (tr: t) : t =
    match tr with
    | T(c, lt, z, rt) ->
      if c = B then
      match lt, rt with
        | T(R, T(R,a,x,b), y, c), _ -> T(R, T(B,a,x,b), y,T(B,c,z,rt))
        | T(R,a,x,T(R,b,y,c)), _ -> T(R,T(B,lt,x,b),y, T(B,c,z,rt))
        | _, T(R,T(R,b,x,c),y,d) -> T(R,T(B,lt,z,b),x,T(B,c,y,d))
        | _, T(R,b,x,T(R,c,y,d)) -> T(R,T(B,lt,z,b),x,T(B,c,y,d))
        | _, _ -> T(c,lt,z,rt)

      else T(c,lt,z,rt)

    let lt t1 t2 : bool = t1 < t2

  let insert (x: elem) (tr: t) : t =
    let rec helper tree = match tree with
                   | E -> T(R, E, x, E)
                   | T(c, a, y, b) -> if (x < y)
                                      then balance (T(c,(helper a),y,b))
                                      else if (y < x)
                                      then balance (T(c,a,y,(helper b)))
                                      else tree
    in let T(_, a, y, b) = helper tr
    in T(B,a,y,b)


  let rec member (x: elem) (tr: t) : bool =
    match tr with
    | E -> false
    | T(_, ltr, el, rtr) -> if (x < el) then member x ltr
                           else if (el < x) then member x rtr
                                else true

(*
First, the red black tree is actually a binary search tree.
Secondly, the children of a red rodes would never be red
it will be all black instead.
Third, every path from the root to the end of the leaf(Empty) case
should have the same number which is the number of the black nodes
that it pass by.

*)

(*
Extra credits:
I did not generate all the path from the root.
What I did here was count the number of the black nodes
when we move the path by using a helper function. And this helper
function will start from the leaf and it will reach the root. There
is an trick here, I used a tuple to store two values as a accumulator,
the first one is the number of the black node that the path
pass by and the second one is a boolean value that justify whether the
subtree follows all the Invariants. IN this process, if the black node's number
of the left child is equal to the black node's number of right child. We can
increament the left child's black nodes number by 1 and set the value to
true. If we find a red node then we just return the accumulator. If
it does not satisfy the number's Invariants then we return (-1, false).

In my isRedBlackTree function,
 I have first check function called valid node. This function is try
 to satisfy the invariants that all of the children of a red nodes will
 be black nodes.
 The second check function called validBTS would satisfy that
 every path from the root to the end of the leaf(Empty) case
 should have the same number which is the number of the black nodes
 that it pass by.
 Also i explained the helper function in the above contents.
 By USING these three helpers,
 I can check whether it is a red black tree.

*)
   let rec validNode (tr: t) : bool =
      match tr with
       | E -> true
       | T(c, lt, z, rt) -> if (c != B ) then
                            match lt, rt with
                              | E,  E -> true
                              | E, T(c', lt', z', rt') ->
                                if (c' != B) then false
                                else validNode rt
                              | T(c', lt', z', rt'), E ->
                                    if c' != B then false
                                    else validNode lt
                              | T(c', lt', z', rt'), T(c'', lt'', z'', rt'') ->
                                if (c'' != B) || (c' != B) then false
                                else validNode lt && validNode rt

                            else validNode lt && validNode rt



    let rec validBTS (tr: t): bool = match tr with
    | E -> true
    | T (_,lt,e,rt) -> (match lt,rt with
                |E, E -> true
                |E, T (_,_,e1,_) -> if (e < e1) then validBTS rt
                                   else false
                |T (_,_,e1,_),E -> if (e1 < e) then validBTS lt
                                   else false
                |T (_,_,e1,_),T (_,_,e2,_) ->
                                      if ((e1 < e) && (e < e2))
                                      then validBTS lt && validBTS rt
                                      else false)

    let takel (a, b) = a
    let taker (a, b) = b



    let isRedBlackTree (tr: t): bool =
      let count tr =
        let rec helper tr
        = match tr with
        | E ->  (0, true)
        | T(c, lt, el, rt) -> if c = B
                            then if takel (helper lt) = takel (helper rt)
                            && taker (helper lt) && taker (helper rt)
                            then (1 + (takel (helper lt)), true)
                            else (-1, false)

                            else if takel (helper lt) = takel (helper rt)
                            && taker (helper lt) && taker (helper rt)
                            then ((takel (helper lt)), true)
                            else (-1, false)
          in  taker (helper tr)

      in
          validNode tr && count tr && validBTS tr




end



module RBTI = RedBlackTree(Int)
