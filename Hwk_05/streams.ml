(* The code below is from Professor Eric Van Wyk. *)

(* Types and functions for lazy values *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a
               | Thunk of (unit -> 'a)
type 'a stream = Cons of 'a * 'a stream lazee

let delay (unit_to_x: unit -> 'a) : 'a lazee = ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let rec demand (l: 'a lazee) : 'a =
  force l;
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")


let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (hd1, tl1), Cons (hd2, tl2) ->
     Cons (f hd1 hd2, delay (fun () -> zip f (demand tl1)
                                           (demand tl2) ) )


let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
 match s with
 | Cons (hd, tl) ->
    Cons (f hd, delay (fun () -> map f (demand tl)))

let rec from n = Cons (n, delay (fun () -> from (n + 1)))

let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n:int) (s : 'a stream) : ('a list) =
  match n, s with
  | 0, _ -> []
  | _, Cons (v, tl) -> v :: take (n-1) (demand tl)

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = delay (fun () -> filter p (demand tl) )
     in if p hd
        then Cons (hd, rest)
        else demand rest
(* Streams, using lazy values *)


(* The code below is from Guangyyu Yan *)
let rec cubes_from (n: int): int stream =
  let c = n * n * n
    in Cons ( c,
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

let rec foldr (f: 'a -> 'b lazee -> 'b) (s: 'a stream) : 'b =
  match s with
  |Cons(hd, tl) -> f hd (
      delay ( fun () -> foldr f (demand tl)) (*fun () is a function unit)*)
    )
(* In foldr function, basically what we wanna do is that we have a 'a stream,
and we somehow want to convert this stream to another type 'b, so we will have
a function that could process every single element in the input stream. In my
version, the first argument is a function f that can convert 'a type to 'b typ
with an accumulator 'b lazee. The second element is the stream. Then we start
the matching, if we have a Cons(hd, tl) type, we start to process out stream by
applying f to the first argument. After this step, a 'b type will be generated
and the rest of the input stream will be applied when we want to process the
rest( demand (tl) ). And this is the function we are looking for
*)

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
  | Cons(hd, tl) -> Cons(hd, delay (fun ()-> sieve (sift hd (demand tl) )))
let primes = sieve (from 2)
