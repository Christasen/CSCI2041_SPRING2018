(* This file was originally named "let_expr.ml" but was renamed
   to fit the file naming scheme of Hwk 04.
 *)

type expr = Int of int
          | Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr
          | Let of string * expr * expr
          | Id  of string

(* 1 + 2 * 3 *)
let e1 : expr = Add (Int 1, Mul (Int 2, Int 3) )
let rec lookup (n: string) (env: (string * int) list) : int = 
  match env with
  | [] -> raise (Failure ("unbound name \"" ^ n ^ "\""))
  | (n',v)::rest -> if n = n' then v else lookup n rest

let rec eval (e: expr) (env: (string * int) list) : int = 
  match e with
  | Int i -> i
  | Add (e1, e2) -> eval e1 env + eval e2 env
  | Sub (e1, e2) -> eval e1 env - eval e2 env
  | Mul (e1, e2) -> eval e1 env * eval e2 env
  | Div (e1, e2) -> eval e1 env / eval e2 env
  | Let (n, bexpr, body) ->
     let new_env = (n, eval bexpr env) :: env
     in eval body new_env
  | Id n -> lookup n env

let e2 = Id "x"
let e3 = Let ("x", Add (Int 2, Int 3), Mul (Id "x", Int 2) )
let e4 = Let ("x", Int 5,
              Add ( Let ("x", Int 100, Add (Id "x", Int 1)),
                    Mul (Id "x", Int 7)
                  )
             )
                        
