open LazeeModules

module type StreamSig = sig
  type 'a lazee
  
  type 'a t = Cons of 'a * 'a t lazee
  val delay: (unit -> 'a) -> 'a lazee
  val demand: 'a lazee -> 'a
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val take: int -> 'a t -> 'a list
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t-> 'b t
  val zip: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t


  (* Add more elements here. *)
end

module Stream (L: LazeeSig) : StreamSig = struct
  type 'a lazee = 'a L.t
  type 'a t = Cons of 'a * 'a t lazee

  let delay = L.delay
  let demand = L.demand
  
  let head (s:'a t): 'a = match s with
   | Cons (v, _) -> v

  let tail (s: 'a t) : 'a t = match s with
   | Cons (_, tl) -> demand tl

  let rec take (n:int) (s : 'a t) : 'a list =
   match n, s with
   | 0, _ -> []
   | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

  let rec map (f: 'a -> 'b) (s: 'a t) : 'b t =
  match s with
  | Cons (hd, tl) ->
     Cons (f hd, delay (fun () -> map f (demand tl)))

  let rec filter (p: 'a -> bool) (s: 'a t) : 'a t =
   match s with
   | Cons (hd, tl) ->
      let rest = delay (fun () -> filter p (demand tl)) in
      if p hd
      then Cons (hd, rest)
      else demand rest
      
let rec zip (f: 'a -> 'b -> 'c) (s1: 'a t) (s2: 'b t) : 'c t =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1) (demand tl2)))

  (* add more elements here *)
end
