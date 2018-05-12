(* arithmetic *)

type expr = Plus of expr * expr 
          | Mult of expr * expr
          | Sub of expr * expr
          | Div of expr * expr
          | Neg of expr
          | Int of int

let rec eval (e: expr) : int =
  match e with
  | Int x -> x
  | Neg e' -> - (eval e')
  | Plus (l, r) -> eval l + eval r
  | Mult (l, r) -> eval l * eval r
  | Sub (l, r) -> eval l - eval r
  | Div (l, r) -> eval l / eval r

(* 1 + 2 * 3 *)
let e1 = Plus (Int 1, Mult (Int 2, Int 3))

(* 2 * (5 - 3) / 2 *)
let e2 = Div ( Mult (Int 2, Sub (Int 5, Int 3)), Int 2 )
