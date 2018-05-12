type expr 
  = Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  
  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | If of expr * expr * expr

  | Let of string * expr * expr
  | Id of string

  | Value of value

and value 
  = Int of int
  | Bool of bool


type environment = (string * value) list

let rec eval (e:expr) (env:environment) : value =
  match e with 
  | Value v -> v
  | Add (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "Incompatible types on Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "Incompatible types on Sub")
     )

  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool v1, Bool v2 -> Bool (v1 && v2)
       | _ -> raise (Failure "Incompatible types on And")
     )

let e1 = Add (Value (Int 1), Sub (Value (Int 10), Value (Int 3)))



(* freevars (Add(Id "x", Value (Int 3)))  ---->>  ["x"]
   freevars (Let ("x", Id "y", (Add(Id "x", Value (Int 3)))))  ---->>  ["y"] *)

let rec freevars (e:expr)  : string list =  
  match e with
  | Value v -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Id x -> [x]
  | Let (x, bexpr, body) -> 
     freevars bexpr @ List.filter (fun y -> y <> x) (freevars body)
