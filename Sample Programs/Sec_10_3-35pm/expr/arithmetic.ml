type expr = Int of int
          | Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr

(* 1 + 2 * 3 *)
let e1 : expr = Add (Int 1, Mul (Int 2, Int 3) )

let rec eval (e: expr) : int = 
  match e with
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

