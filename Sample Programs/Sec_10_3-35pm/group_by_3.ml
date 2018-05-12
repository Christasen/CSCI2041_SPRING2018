(* Sample programs from February 9

   Eric Van Wyk
 *)

let even x = x mod 2 = 0

let ns = [1;2;3;4;5;6;7;8;9;10]

let partition_l (predicate: 'a -> bool) (lst: 'a list) : 'a list * 'a list =
  let accum: 'a list * 'a list = ( [], [] )
  in
  let f (successes,failures) x = 
    if predicate x
    then (x::successes, failures)
    else (successes, x::failures)
  in
  let (successes_backwards, failures_backwards) =
    List.fold_left f accum lst 
  in
  (List.rev successes_backwards, List.rev failures_backwards)

let partition_r (predicate: 'a -> bool) (lst: 'a list) : 'a list * 'a list =
  let accum: 'a list * 'a list = ( [], [] )
  in
  let f x (successes,failures) = 
    if predicate x
    then (x::successes, failures)
    else (successes, x::failures)
  in
  List.fold_right f lst accum


let rec append xs ys = 
  match xs with
  | [] -> ys
  | z::zs -> z :: (append zs ys)


let group_by_3 (lst: 'a list) : 'a list list =
  let accum: ('a list list * 'a list * int) = ( [], [], 0)
  in
  let f (all_groups, current_group, size) x =
    if size = 3
    then ( (List.rev current_group) :: all_groups, [x], 1 )
    else ( all_groups, x::current_group, size+1)
  in
  let (groups, last, _) = List.fold_left f accum lst
  in List.rev ( List.rev last :: groups)


