(* This file was originally named "let_expr.ml" but was renamed
   to fit the file naming scheme of Hwk 04.
 *)

type expr = Plus of expr * expr 
          | Mult of expr * expr
          | Sub of expr * expr
          | Div of expr * expr
          | Neg of expr
          | Int of int
          | Let of string * expr * expr
          | Id of string

let rec lookup (x: string) (env : (string * int) list) : int =
  match env with
  | [] -> raise (Failure ("unbound name \"" ^ x ^ "\"!"))
  | (x',v):: rest -> if x = x' then v else lookup x rest

let rec eval (e: expr) (env: (string * int) list) : int =
  match e with
  | Int n -> n
  | Neg e' -> - (eval e' env)
  | Plus (l, r) -> eval l env + eval r env
  | Mult (l, r) -> eval l env * eval r env
  | Sub (l, r) -> eval l env - eval r env
  | Div (l, r) -> eval l env / eval r env
  | Id x -> lookup x env 
  | Let (x, vexpr, body) -> 
     let new_env = (x, eval vexpr env) :: env
     in eval body new_env

(* 3 + let x = 4 + 5 in x * 5 *)
let e3 = Plus (Int 3, Let ("x", Plus (Int 4, Int 5), Mult (Id "x", Int 5)))

(* let x = 3 in let y = x in let x = 4 in x + y *)
