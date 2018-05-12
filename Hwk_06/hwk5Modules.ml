open StreamModules

module type Hwk5Sig = sig
  type 'a stream
  val take: int -> 'a stream -> 'a list
  val head: 'a stream -> 'a
  val zip: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

  val from: int -> int stream
  val nats: int stream
  val cubes_from: int -> int stream
  val cubes_from_zip: int -> int stream
  val cubes_from_map: int -> int stream
  val drop: int -> 'a stream -> 'a stream
  val drop_until: ('a -> bool) -> 'a stream -> 'a stream
  val sum_positive_prefix: int stream -> int
  val primes: int stream
end

module Hwk5(S: StreamSig) : Hwk5Sig = struct
  type 'a stream = 'a S.t
  let delay = S.delay
  let demand = S.demand
  let take = S.take
  let head = S.head
  let zip = S.zip
  let filter = S.filter
  let map = S.map
  let tail = S.tail


  let rec from n = S.Cons (n, S.delay (fun () -> from (n + 1)))

  let nats = from 1

  let rec cubes_from (n: int): int stream =
  let c = n * n * n
    in S.Cons ( c,
      delay ( fun () -> cubes_from (n + 1) ) (*fun () is a function unit)*)
      )

  let rec cubes_from_zip (n: int): int stream =
    let f x y = x * y
    in
    let stream1 = zip f (from n) (from n)
    in
    let stream2 = from n
    in zip f stream1 stream2

  let rec cubes_from_map (n: int): int stream =
    let f x = x * x * x
    in
    let stream1 = from (n)
    in map f stream1


  let rec drop (n: int) (stream1: 'a stream) : 'a stream =
    if n <> 0 then drop (n-1) (tail stream1)
    else stream1

  let rec drop_until (f: 'a -> bool) (s: 'a stream): 'a stream =
    if (f (head s) ) then s
    else drop_until f (tail s)

  let rec foldr (f: 'a -> 'b S.lazee -> 'b) (s: 'a stream) : 'b =
  match s with
  |S.Cons(hd, tl) -> f hd (
      delay ( fun () -> foldr f (demand tl)) (*fun () is a function unit)*)
    )

  let rec and_fold (s: bool stream): bool =
    let helper x xs = x && (demand xs)
    in
    foldr helper s

  let rec sum_positive_prefix (s: int stream) : int =
    let sum = 0
    in
    let helper x xs = if (x < 0) then sum
                      else sum + x + (demand xs)
    in
    foldr helper s

  let sift (n: int) (s: int stream) =
    let helper1 f x = not (f x)
    in
    let helper2 a b = b mod a = 0
    in
    filter (helper1 (helper2 n)) s

  let rec sieve (s: int stream): int stream =
    match s with
    | S.Cons(hd, tl) -> S.Cons(hd, delay (fun ()-> sieve (sift hd (demand tl) )))
  let primes = sieve (from 2)
   (* add elements here to complete the functor *)
end
