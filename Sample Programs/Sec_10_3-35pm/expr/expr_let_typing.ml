(* A definition of arithmetic, relational, logical, and let
   expressions and their evaluation.  This language was originally
   defined in ``expr_let.ml``.

   Here we change the type of ``eval`` and introduce static type
   checking of expressions.

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

type typ = 
  | IntType
  | BoolType

type error =
  (* An unbound name error *)
  | UnboundName of string

  (* An incorrect type error.  The expr has a type (the second
     component) but one of the types in the ``typ list`` was
     expected. *)
  | IncorrectType of expr * typ * (typ list)

  | DivisionByZero of expr


type 'a result = OK of 'a
               | Err of error list

let rec lookup (n:string) (env: (string * 'a) list) : 'a result =
  match env with
  | [] -> Err ( [ UnboundName n ] )
  | (n',v) :: rest when n = n' -> OK v
  | _ :: rest -> lookup n rest


let rec eval (e:expr) (env: environment) : value result =
  match e with 
  | Val v -> OK v



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
  assert (evaluate e1 = OK (Int 7));
  assert (evaluate e2 = OK (Int 7));
  assert (evaluate e3 = OK (Bool true));
  assert (evaluate e4 = OK (Bool false));
  assert (evaluate e5 = OK (Bool true));
  assert (evaluate e6 = OK (Int 15));
  assert (evaluate e7 = OK (Bool true))


let er1 = Add (Val (Int 1), Mul (Val (Bool true), Val (Int 3)))
let er2 = Eq (Val (Bool true), Val (Int 3))
let er3 = Eq (e1, e4)

let er4 = Let ("y", 
               Val (Int 5), 
               And (Val (Bool true), Id "y")
              )

let er5 = And (Val (Bool true), Id "y")

let er6 = Let ("y", 
               Val (Int 0), 
               Div (Val (Int 5), Id "y")
              )

let er7 = Let ("x", 
              Add (Val (Int 5), Val (Bool true)),
              Add (Id "x", Val (Int 5))
              )

let has_eval_errors (e:expr) : bool =
  match evaluate e with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (has_eval_errors er1);
  assert (has_eval_errors er2);
  assert (has_eval_errors er3);
  assert (has_eval_errors er4);
  assert (has_eval_errors er5);
  assert (has_eval_errors er6);
  assert (has_eval_errors er7)


(* To check the type correctness of expressions by infering their
   type, we use the following data types. *)

type context = (string * typ) list

let rec check (e:expr) (ctxt:context) : typ result =
  match e with 
  | Val (Int _) -> OK IntType
  | Val (Bool _) -> OK BoolType



let e8 = Div (Val (Int 5), Val (Int 0))

let has_type_errors (e:expr) : bool =
  match check e [] with
  | OK _ -> false
  | Err _ -> true

let () =
  assert (not (has_type_errors e8))

let () =
  assert (has_type_errors er1);
  assert (has_type_errors er2);
  assert (has_type_errors er3);
  assert (has_type_errors er4);
  assert (has_type_errors er5);
  (* er6 has not type error *)
  assert (has_type_errors er7)



let () =
  print_endline ("Success! All tests passed.")


