
(* sum sqr diffs *)

let ns = [5;4;2]

(* Recall the code *)
let rec sum_sqrdiffs lst = 
  match lst with
  | x1::x2::[] -> (x1 - x2) * (x1 - x2)
  | x1::x2::xs -> (x1 - x2) * (x1 - x2) + sum_sqrdiffs (x2::xs)
  | _ -> raise (Failure "list must contain at least 2 elements")


let sum_sqr_diffs lst =
  match lst with
  | [] | [_] -> raise (Failure "incorrect values to \"sum_sqrdiffs\"")
  | x1::x2::xs ->
     let accum = (x1, 0)
     in
     let f (x1, sum) x2 =
       (x2, sum + (x1 - x2) * (x1 - x2))
     in
     List.fold_left f accum lst

let quizAB f1 f2 f3 lst =
  match lst with
  | [] | [_] -> raise (Failure "incorrect values to \"sum_sqrdiffs\"")
  | x1::x2::xs ->
     let accum = (x2, f2 (f3 x1 x2))
     in
     let f (x1, sum) x2 =
       (x2, f1 sum (f2 (f3 x1 x2)))
     in
     let (a, result) =  List.fold_left f accum xs
     in (a,result)

let sum_squr_diffs' lst = quizAB (+)  (fun x -> x * x) ( - ) lst

(* Different versions of the quiz questions can be programmed by
   changng the arguments to quizAB above.  This is seen in the
   definition of the quiz A and B problems below. *)

let quizA lst = quizAB ( +. )  sqrt ( +. ) lst

let quizB lst = quizAB ( *. )  (fun x -> 1.0 /. x) ( -. ) lst
