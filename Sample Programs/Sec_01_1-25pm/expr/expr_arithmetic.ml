(* A definition of arithmetic expressions and their evaluation. 

   The `expr` type defines 5 variants of expressions and `eval`
   evaluates these expressions to the integer value.

   Eric Van Wyk
 *)

type expr 
  = Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

let rec eval (e:expr) : int =
  match e with
  | Int x -> x
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

(* Some sample expressions and their values *)
let e1 = Add (Int 1, Mul (Int 2, Int 3))
let v1 = eval e1

let e2 = Sub (Int 10, Div (e1, Int 2))
let v2 = eval e2

(* Assert expressions to test our functions.  If all asserts are
   satisfied then no exception will be raised and the success message
   will be printed. *)
let () =
  assert (eval e1 = 7);
  assert (eval e2 = 7);
  print_endline ("Success! All tests passed.")
