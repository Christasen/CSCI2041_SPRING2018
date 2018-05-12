(* A definition of arithmetic, relational, logical, and let
   expressions and their evaluation.

   This extends the language in ``expr_int_bool.ml``.

   The ``expr`` type now contains varians for let-expressions and
   identifiers.

   Evaluation now requires contextual information in the form of an
   environment.  The ``environment`` type binds names (as string) to
   values and stores them in a list.  Thus ``eval`` take this as an
   additional argument.  The helper function ``evaluate`` starts
   evaluation with the empty environment.

   The "scoping rules" of our language are such that an identifier is
   bound (that is, refers to) the closest lexically-enclosing
   let-expression defining that name.  This rule is realized in the
   functions below by searching for the first occurence of a name in
   the environment list when the value of a name is looked up by the
   ``lookup`` function.

   Eric Van Wyk 
 *)

type value 
  = Int of int
  | Bool of bool

type expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr 

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id of string

type environment = (string * value) list


let rec lookup (n:string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Identifier " ^ n ^ " not in scope"))
  | (n',v) :: rest when n = n' -> v
  | _ :: rest -> lookup n rest


let rec eval (e:expr) (env: environment) : value =
  match e with 
  | Val v -> v

  | Add (e1, e2) -> 
    ( match eval e1 env, eval e2 env with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _, _ -> raise (Failure "incompatible values on Add")
    )
  | Sub (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int i1, Int i2 -> Int (i1 - i2)
       | _, _ -> raise (Failure "incompatible values on Sub")
     )
  | Mul (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int i1, Int i2 -> Int (i1 * i2)
       | _, _ -> raise (Failure "incompatible values on Mul")
     )
  | Div (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int i1, Int i2 -> Int (i1 / i2)
       | _, _ -> raise (Failure "incompatible values on Div")
     )

  | Lt (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int i1, Int i2 -> Bool (i1 < i2)
       | _, _ -> raise (Failure "incompatible values on Lt")
     )
  | And (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "incompatible values on And")
     )
  | Eq (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> raise (Failure "incompatible values on Eq")
     )
  | Not e1 ->
     ( match eval e1 env with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "incompatible value on Not")
     )

  | Let (n, bexpr, body) -> 
     let bexpr_v = eval bexpr env in 
     eval body ((n,bexpr_v)::env) 

  | Id n -> lookup n env


(* A helper function to start evaluation with the empty environment. *)
let evaluate e = eval e []


(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* ``let y = 5 in let x = y + 5 in x + y'' *)
let e6 = Let ("y", 
              Val (Int 5), 
              Let ("x", 
                   Add (Id "y", Val (Int 5)), 
                   Add (Id "x", Id "y")
                  )
             )

(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e7 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

(* Assert expressions to test the evaluate function. *)
let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true);
  print_endline ("Success! All tests passed.")


