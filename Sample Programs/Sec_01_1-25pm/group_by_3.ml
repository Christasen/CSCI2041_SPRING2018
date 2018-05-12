(* Sample programs from February 9

   Eric Van Wyk
 *)

let even x = x mod 2 = 0

let ns = [1;2;3;4;5;6;7;8;9;10]

let partition (predicate: 'a -> bool) (lst: 'a list) : 'a list * 'a list =
  let accum: 'a list * 'a list = ( [], [] )
  in
  let f (trues, falses) x = 
    if predicate x 
    then (x::trues, falses) 
    else (trues, x::falses)
  in 
  let (trues_backwards, falses_backwards) = List.fold_left f accum lst
  in (List.rev trues_backwards, List.rev falses_backwards)

(* Consider partition_l and partition_r in Sec 10's sample programs. *)


let rec append xs ys =
  match xs with
  | [] -> ys
  | z::zs -> z :: (append zs ys)


let group_by_3 (lst: 'a list) : 'a list list =
  let accum: ('a list list * 'a list * int) = ( [], [], 0)
  in
  let f (sublists, current_list, size) x =
    if size = 3 
    then ( List.rev current_list :: sublists , [x], 1 )
    else ( sublists, x :: current_list, size + 1 )
  in
  let (lsts, curr, size) = List.fold_left f accum lst 
  in
  List.rev (List.rev curr :: lsts)
