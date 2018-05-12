module type StreamSig_v2 = sig
  type 'a lazee
  type 'a t = 'a node_t lazee
  and 'a node_t =
    | Cons of 'a * 'a t
    | Nil
  exception Empty_stream

  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a

  val head: 'a t -> 'a    (* raise Empty_stream if Nil *)
  val tail: 'a t -> 'a t  (* raise Empty_stream if Nil *)
  val take: int -> 'a t -> 'a list
  val to_list : 'a t -> 'a list
end

  type 'a t = Cons of 'a * 'a t lazee

module Stream_v2 (L: LazeeSig) : StreamSig_V2 = struct
  type 'a lazee = 'a L.t
  type 'a t = 'a node_t lazee
  and 'a node_t =
    | Cons of 'a * 'a t
    | Nil
  exception Empty_stream
  let delay = L.delay
  let demand = L.demand


  let head (s:'a t): 'a = match s with
   | Cons (v, _) -> v
   | Nil -> Empty_stream

  let tail (s: 'a t) : 'a t = match s with
   | Cons (_, tl) -> demand tl
   | Nil -> Empty_stream

  let rec take (n:int) (s : 'a t) : 'a list =
   match n, s with
   | 0, _ -> []
   | _, Cons (v, tl) -> v :: take (n-1) (demand tl)
   |_,  Nil -> Empty_stream

   let rec to_list (s: 'a t): 'a list =
   match s with
   | Nil -> Empty_stream
   | Cons(v, tl) -> v:: to_list (demand tl)

   end
