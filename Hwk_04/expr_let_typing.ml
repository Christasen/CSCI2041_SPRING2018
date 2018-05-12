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


type context = (string * typ) list


(* The main challenge of this problem is to complete ``eval`` and
   ``check`` in the same way that was done in the ``expr_let_typing.ml``
   file.
 *)


 let rec serialize (e: expr): string =
   match e with
   | Mul(a, b) -> "Mul(" ^ serialize a ^ ","^ serialize b ^ ")"
   | Div(a, b) -> "Div(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | Sub(a, b) -> "Sub(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | Add(a, b) -> "Add(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | Lt(a, b) -> "Lt(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | Eq(a, b) -> "Eq(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | And(a, b) -> "And(" ^ serialize a ^ "," ^ serialize b ^ ")"
   | Not a -> "Not(" ^ serialize a ^ ")"
   | Id a -> "Id \""^ a ^ "\""
   | Let (a, b, c) -> "Let (\""^a^"\", "^serialize b ^ ", "
      ^  serialize c  ^")"
   | Val (a) -> (match a with
     | Int a -> "  Val(Int " ^ string_of_int a ^ ")"
     | Bool a -> "Val(Bool " ^ string_of_bool a ^ ")"
     )

 let rec unparse (e: expr): string =
   match e with
    | Mul(a, b) -> "(" ^ unparse a ^ "*" ^ unparse b ^ ")"
    | Div(a, b) -> "(" ^ unparse a ^ "/" ^ unparse b ^ ")"
    | Sub(a, b) -> "(" ^ unparse a ^ "-" ^ unparse b ^ ")"
    | Add(a, b) -> "(" ^ unparse a ^ "+" ^ unparse b ^ ")"
    | Lt(a, b) -> "(" ^ unparse a ^ "<" ^ unparse b ^ ")"
    | Eq(a, b) -> "(" ^ unparse a ^ "=" ^ unparse b ^ ")"
    | And(a, b) -> "(" ^ unparse a ^ "&&" ^ unparse b ^ ")"
    | Not a -> "(" ^ "not" ^ unparse a ^ ")"
    | Id a -> a
    | Let(a, b, c) -> "(let" ^ a ^ "=" ^ unparse b ^ "in" ^ unparse c ^ ")"
    | Val (a) -> (match a with
       | Int a -> string_of_int a
       | Bool a -> string_of_bool a
       )

let rec eval (e:expr) (env: environment) : value result =
  match e with
  | Val v -> OK v
  | Add (e1, e2) ->
   ( match eval e1 env, eval e2 env with
     | OK Int t1, OK Int t2 -> OK (Int (t1 + t2))
     | OK Bool a, _ ->
        Err[IncorrectType (Val(Bool a), BoolType, [IntType])]
     | OK Int a, OK Bool b ->
        Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
     | Err r, _ -> Err r
     | OK (Int a), Err b -> Err b

   )
  | Sub (e1, e2) ->
   ( match eval e1 env, eval e2 env with
    | OK Int t1, OK Int t2 -> OK (Int (t1 - t2))
    | OK Bool a, _ ->
       Err[IncorrectType (Val(Bool a), BoolType, [IntType])]
    | OK Int a, OK Bool b ->
       Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
    | Err r, _ -> Err r
    | OK (Int a), Err b -> Err b

  )
  | Mul (e1, e2) ->
   ( match eval e1 env, eval e2 env with
     | OK Int t1, OK Int t2 -> OK (Int (t1 * t2))
     | OK Bool a, _ ->
        Err[IncorrectType (Val(Bool a), BoolType, [IntType])]
     | OK Int a, OK Bool b ->
        Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
     | Err r, _ -> Err r
     | OK (Int a), Err b -> Err b

   )
  | Div (e1, e2) ->
   ( match eval e1 env, eval e2 env with
    | OK Int t1, OK Int 0 -> Err [ DivisionByZero e ]
    | OK Int t1, OK Int t2 -> OK (Int (t1 / t2))
    | Err r, _ -> Err r
    | OK (Int a), Err b -> Err b
    | OK Bool a, _ ->
       Err[IncorrectType (Val(Bool a), BoolType, [IntType])]
    | OK Int a, OK Bool b ->
       Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
  )
  | Lt (e1, e2) ->
   ( match eval e1 env, eval e2 env with
     | OK Int t1, OK Int t2 -> OK (Bool (t1 < t2))
     | OK (Int a), Err b -> Err b
     | OK Bool a, _ ->
        Err[IncorrectType (Val(Bool a), BoolType, [IntType])]
     | OK Int a, OK Bool b ->
        Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
     | Err r, _ -> Err r
  )
  | And (e1, e2) ->
   ( match eval e1 env, eval e2 env with
    | OK Bool t1, OK Bool t2 -> OK (Bool (t1 && t2))
    | OK Bool a, Err b -> Err b
    | OK Int a, _ ->
       Err[IncorrectType (Val(Int a), IntType, [BoolType])]
    | Err r, _ -> Err r
    | OK Bool a, OK Int b ->
       Err[IncorrectType (Val(Int b), IntType, [BoolType])]
  )
  | Eq (e1, e2) ->
   ( match eval e1 env, eval e2 env with
    | OK Int t1, OK Int t2 -> OK (Bool (t1 = t2))
    | OK Bool t1, OK Bool t2 -> OK (Bool (t1 = t2))
    | Err r, _ -> Err r
    | OK Bool a, Err b -> Err b
    | OK (Int a), Err r -> Err r
    | OK Bool a, OK Int b ->
       Err[IncorrectType (Val(Int b), IntType, [BoolType])]
    | OK Int a, OK Bool b ->
       Err[IncorrectType (Val(Bool b), BoolType, [IntType])]
  )

  | Not e1 ->
    ( match eval e1 env with
      | OK Bool a -> OK (Bool (not a))
      | Err r -> Err r
      | OK (Int i) ->
        Err([IncorrectType (Val (Int i),IntType,[BoolType])])

    )
  | Id n -> lookup n env
  | Let (n, dexpr, body) ->
     ( match eval dexpr env with
       | OK Bool v1 -> eval body ( (n,Bool v1)::env )
       | OK Int v1 -> eval body ( (n,Int v1)::env )
       | Err r -> Err r
      )
(* A helper function to start evaluation with the empty environment. *)
let evaluate e = eval e []


let get_value exp=match evaluate exp with
 |OK n->Val n
 |_->exp

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



(* A helper function to start evaluation with the empty environment. *)



let rec check (e:expr) (ctxt:context) : typ result =
  match e with
  | Val (Int _) -> OK IntType
  | Val (Bool _) -> OK BoolType
  | Add (e1, e2) ->
  (match check e1 ctxt, check e2 ctxt with
    |OK IntType, OK IntType -> OK IntType
    |OK BoolType, OK IntType ->
      Err[IncorrectType(get_value e1, BoolType, [IntType])]
      | OK BoolType, OK BoolType ->
        Err([IncorrectType (get_value e1, BoolType,
          [IntType]); IncorrectType (get_value e2, BoolType, [IntType])])
    |OK IntType, OK BoolType ->
      Err[IncorrectType(get_value e2, BoolType, [IntType])]
    |Err a, Err b -> Err(a @ b)
    |Err a, OK IntType -> Err a
    |Err a, OK BoolType ->
      Err (a @ [IncorrectType(get_value e2, BoolType, [IntType])])
    |OK IntType, Err a -> Err a
    |OK BoolType, Err a ->
      Err ([IncorrectType(get_value e1, BoolType, [IntType])]@ a)
  )
  | Sub (e1, e2)->
  (match check e1 ctxt, check e2 ctxt with
    |OK IntType, OK IntType -> OK IntType
    |OK BoolType, OK IntType ->
      Err[IncorrectType(get_value e1, BoolType, [IntType])]
      | OK BoolType, OK BoolType ->
        Err([IncorrectType (get_value e1, BoolType,
          [IntType]); IncorrectType (get_value e2, BoolType, [IntType])])
    |OK IntType, OK BoolType ->
      Err[IncorrectType(get_value e2, BoolType, [IntType])]
    |Err a, Err b -> Err(a @ b)
    |Err a, OK IntType -> Err a
    |Err a, OK BoolType ->
      Err (a @ [IncorrectType(get_value e2, BoolType, [IntType])])
    |OK IntType, Err a -> Err a
    |OK BoolType, Err a ->
      Err ([IncorrectType(get_value e1, BoolType, [IntType])]@ a)
  )

  | Mul (e1, e2)->
  (match check e1 ctxt, check e2 ctxt with
    |OK IntType, OK IntType -> OK IntType
    |OK BoolType, OK IntType ->
      Err[IncorrectType(get_value e1, BoolType, [IntType])]
      | OK BoolType, OK BoolType ->
        Err([IncorrectType (get_value e1, BoolType,
          [IntType]); IncorrectType (get_value e2, BoolType, [IntType])])
    |OK IntType, OK BoolType ->
      Err[IncorrectType(get_value e2, BoolType, [IntType])]
    |Err a, Err b -> Err(a @ b)
    |Err a, OK IntType -> Err a
    |Err a, OK BoolType ->
      Err (a @ [IncorrectType(get_value e2, BoolType, [IntType])])
    |OK IntType, Err a -> Err a
    |OK BoolType, Err a ->
      Err ([IncorrectType(get_value e1, BoolType, [IntType])]@ a)
  )
  | Div(e1, e2) ->
  (match check e1 ctxt, check e2 ctxt with
    |OK IntType, OK IntType -> OK IntType
    |OK BoolType, OK IntType ->
      Err[IncorrectType(get_value e1, BoolType, [IntType])]
      | OK BoolType, OK BoolType ->
        Err([IncorrectType (get_value e1, BoolType,
          [IntType]); IncorrectType (get_value e2, BoolType, [IntType])])
    |OK IntType, OK BoolType ->
      Err[IncorrectType(get_value e2, BoolType, [IntType])]
    |Err a, Err b -> Err(a @ b)
    |Err a, OK IntType -> Err a
    |Err a, OK BoolType ->
      Err (a @ [IncorrectType(get_value e2, BoolType, [IntType])])
    |OK IntType, Err a -> Err a
    |OK BoolType, Err a ->
      Err ([IncorrectType(get_value e1, BoolType, [IntType])]@ a)
  )
  | Lt (e1, e2)  ->
    ( match check e1 ctxt, check e2 ctxt with
      | OK IntType, Err r -> Err r
      | OK IntType, OK IntType ->
        Err([IncorrectType (get_value e1, BoolType,
        [BoolType]); IncorrectType (get_value e2, BoolType, [BoolType])])
      | OK IntType, OK BoolType ->
        Err([IncorrectType (get_value e2,BoolType,[IntType])])
      | OK BoolType, Err r->
        Err ([IncorrectType (get_value e1,BoolType,[IntType])] @ r)
      | OK BoolType, OK IntType->
        Err([IncorrectType (get_value e1, BoolType,[IntType])])
      | OK BoolType, OK BoolType -> OK BoolType
      | Err r1, Err r2 -> Err (r1 @ r2)
      | Err r, OK IntType -> Err r
      | Err r, OK BoolType ->
       Err (r @ [IncorrectType (get_value e2,BoolType,[IntType])])
 )
 | Eq (e1, e2)  ->
   ( match check e1 ctxt, check e2 ctxt with
    | OK IntType, Err r-> Err r
    | OK IntType, OK IntType -> OK BoolType
    | OK BoolType, OK IntType ->
      Err([IncorrectType (get_value e2, IntType,[BoolType])])
    | OK IntType, OK BoolType->
      Err[IncorrectType (get_value e2, BoolType,[IntType])]
    | OK BoolType, Err r-> Err r
    | OK BoolType, OK BoolType -> OK BoolType
    | Err a, Err b -> Err (a @ b)
    | Err r, OK IntType -> Err r
    | Err r, OK BoolType -> Err r
   )

  | And (e1, e2) ->
  (match check e1 ctxt, check e2 ctxt with
    |OK IntType, Err a ->
      Err ([IncorrectType (get_value e1,IntType,[BoolType])] @ a)
    |OK IntType, OK IntType ->
       Err([IncorrectType (get_value e1, IntType,
       [BoolType]); IncorrectType (get_value e2, IntType, [BoolType])])
    |OK IntType, OK BoolType ->
      Err([IncorrectType(get_value e1, BoolType, [IntType])])
    |OK BoolType, OK IntType ->
      Err([IncorrectType(get_value e2, BoolType, [IntType])])
    |OK BoolType, OK BoolType -> OK BoolType
    |OK BoolType, Err a -> Err a
    |Err a, Err b -> Err(a @ b)
    |Err a, OK IntType ->
      Err (a @ [IncorrectType(get_value e2, BoolType, [IntType])])
    |Err a, OK BoolType -> Err a
  )


  | Not e1 ->
    ( match check e1 ctxt  with
      | OK BoolType -> OK BoolType
      | OK IntType -> Err [ IncorrectType (e1, IntType, [BoolType]) ]
      | Err r -> Err r
    )
  | Id b -> lookup b ctxt

  | Let (n, dexpr, body) ->
     ( match check dexpr ctxt with
       | OK t -> check body ((n,t)::ctxt)
       | Err r -> Err r
      )



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

     (* To check the type correctness of expressions by infering their
        type, we use the following data types. *)

     type context = (string * typ) list





     let e8 = Div (Val (Int 5), Val (Int 0))

     let has_type_errors (e:expr) : bool =
       match check e [] with
       | OK _ -> false
       | Err _ -> true


     let () =
       print_endline ("Success! All tests passed.")
