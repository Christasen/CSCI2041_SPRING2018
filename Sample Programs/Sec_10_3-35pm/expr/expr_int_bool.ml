(* A definition of arithmetic, relational, and logical expressions 
   and their evaluation.  

   This extends the language in ``expr_arithmetic.ml``.

   Here, values may be either integers or Booleans.  The a new
   ``value`` type is created with a variant for each possible value.

   The ``expr`` type defines a variants for values and the new operators
   that generate Boolean values.

   With the types ``expr`` and ``value`` we see in the code the
   relationship we have discussed before.  That values are one
   particular kind of expression and that expression evaluation
   produces a value.

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

let rec eval (e:expr) : value =
  match e with 
  | Val v -> v

  | Add (e1, e2) -> 
    ( match eval e1, eval e2 with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _, _ -> raise (Failure "incompatible values on Add")
    )
  | Sub (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Int i1, Int i2 -> Int (i1 - i2)
       | _, _ -> raise (Failure "incompatible values on Sub")
     )
  | Mul (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Int i1, Int i2 -> Int (i1 * i2)
       | _, _ -> raise (Failure "incompatible values on Mul")
     )
  | Div (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Int i1, Int i2 -> Int (i1 / i2)
       | _, _ -> raise (Failure "incompatible values on Div")
     )

  | Lt (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Int i1, Int i2 -> Bool (i1 < i2)
       | _, _ -> raise (Failure "incompatible values on Lt")
     )
  | And (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _, _ -> raise (Failure "incompatible values on And")
     )
  | Eq (e1, e2) -> 
     ( match eval e1, eval e2 with
       | Int i1, Int i2 -> Bool (i1 = i2)
       | Bool b1, Bool b2 -> Bool (b1 = b2)
       | _, _ -> raise (Failure "incompatible values on Eq")
     )
  | Not e1 ->
     ( match eval e1 with
       | Bool b -> Bool (not b)
       | _ -> raise (Failure "incompatible value on Not")
     )

(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = eval e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = eval e2

let e3 = Eq (e1, e2)
let e4 = Lt (e1, e2)

let e5 = Not e4

(* Assert expressions to test our functions. *)
let () =
  assert (eval e1 = Int 7);
  assert (eval e2 = Int 7);
  assert (eval e3 = Bool true);
  assert (eval e4 = Bool false);
  assert (eval e5 = Bool true);
  print_endline ("Success! All tests passed.")




