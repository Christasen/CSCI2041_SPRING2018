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
   | App of expr * expr
   | Lambda of string * expr
   | LetRec of string * expr * expr
   | If of expr * expr * expr

and value
 = Int of int
 | Bool of bool
 | Closure of string * expr * environment

and environment = (string * value) list


let rec lookup (n: string) (env: environment) : value =
  match env with
  | [] -> raise (Failure ("Name \"" ^ n ^ "\" not in scope"))
  | (n',v)::_ when n' = n -> v
  | _::rest -> lookup n rest

let rec serialize (e: expr): string =
  match e with
  | Mul(a, b) -> "Mul(" ^ serialize a ^ ","^ serialize b ^ ")"
  | Div(a, b) -> "Div(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Sub(a, b) -> "Sub(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Add(a, b) -> "Add(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Lt(a, b) -> "Lt(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Eq(a, b) -> "Eq(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | And(a, b) -> "And(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | App(a, b) -> "App(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Not a -> "Not(" ^ serialize a ^ ")"
  | Id a -> "Id \""^ a ^ "\""
  | Let (a, b, c) -> "Let (\""^a^"\", "^serialize b ^ ", "
     ^  serialize c  ^")"
  | If (a, b, c) -> "If(" ^ serialize a ^ "," ^ serialize b
     ^ "," ^ serialize c ^ ")"
  | Lambda (a, b) -> "Lambda (\"" ^ a ^ "\", "^ serialize b ^")"
  | LetRec (a, b, c) -> "LetRec (\""^a^ "\", "^serialize b^", "^serialize c^")"
  | Val (a) -> (match a with
    | Int a -> "Val(Int " ^ string_of_int a ^ ")"
    | Bool a -> "Val(Bool " ^ string_of_bool a ^ ")"
    | _ -> raise (Failure "Will only serialize integer and Boolean values")
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
   | App(a, b) -> "(" ^ unparse a ^ " " ^ unparse b ^ ")"
   | Not a -> "(" ^ "not" ^ unparse a ^ ")"
   | Id a -> a
   | Let(a, b, c) -> "(let" ^ a ^ "=" ^ unparse b ^ "in" ^ unparse c ^ ")"
   | If (a, b, c) -> "(If" ^ unparse a ^ "then" ^ unparse b ^
     "else" ^ unparse c ^ ")"
   | Lambda(a, b) -> "(" ^ "fun " ^ a ^ "->" ^ unparse b ^ ")"
   | LetRec (a, b, c) -> "(let rec "^a^" = "^ unparse b ^
     " in "^  unparse c ^")"
   | Val (a) -> (match a with
      | Int a -> string_of_int a
      | Bool a -> string_of_bool a
      | _ -> raise (Failure "Will only unparse integer and Boolean values")
      )

let rec freevars (e: expr) : string list =
  match e with
   | Val _ -> []
   | Add (a, b) -> freevars a @ freevars b
   | Sub (a, b) -> freevars a @ freevars b
   | Mul (a, b) -> freevars a @ freevars b
   | Div (a, b) -> freevars a @ freevars b
   | Lt (a, b) -> freevars a @ freevars b
   | And (a, b) -> freevars a @ freevars b
   | Eq (a, b) -> freevars a @ freevars b
   | App (a, b) -> freevars a @ freevars b
   | Not a -> freevars a
   | If (a, b, c) -> freevars a @ freevars b @ freevars c
   | Let (n, dexpr, body) ->
     freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
   | Id n -> [n]
   | Lambda (n, a) -> List.filter (fun n' -> n <> n') (freevars a)
   | LetRec (n, a, b) -> List.filter (fun n' -> n <> n')
     (freevars a @ freevars b)
   | App (a,b) ->
     (match (freevars a) with
     |x::xs ->xs
     |_->[]
     )

let rec eval (env: environment)(e: expr) : value =
 match e with
  | Val v -> v
  | Add (e1, e2) ->
   ( match eval env e1, eval env e2 with
     | Int t1, Int t2 -> Int (t1 + t2)
     | _, _ -> raise (Failure "incompatible values on Add")
   )
  | Sub (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int t1, Int t2 -> Int (t1 - t2)
      | _, _ -> raise (Failure "incompatible values on Sub")
    )
  | Mul (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int t1, Int t2 -> Int (t1 * t2)
      | _, _ -> raise (Failure "incompatible values on Mul")
    )
  | Div (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int t1, Int t2 -> Int (t1 / t2)
      | _, _ -> raise (Failure "incompatible values on Div")
    )

  | Lt (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int t1, Int t2 -> Bool (t1 < t2)
      | _, _ -> raise (Failure "incompatible values on Lt")
    )
  | And (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Bool a, Bool b -> Bool (a && b)
      | _, _ -> raise (Failure "incompatible values on And")
    )
  | Eq (e1, e2) ->
    ( match eval env e1, eval env e2 with
      | Int t1, Int t2 -> Bool (t1 = t2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | _, _ -> raise (Failure "incompatible values on Eq")
    )
  | Not e1 ->
    ( match eval env e1 with
      | Bool a -> Bool (not a)
      | _ -> raise (Failure "incompatible value on Not")
    )
  | If (e1, e2, e3) ->
    ( match eval env e1 with
      | Bool true -> eval env e2
      | Bool false -> eval env e3
      | _ -> raise (Failure "incompatible values on If")
      )
  | Id n -> lookup n env
  | Let (n, e1, e2) -> eval ((n, eval env e1)::env) e2
  | Lambda(n, e1) -> Closure(n, e1, env)
  | App(e1, e2) ->
    ( match (eval env e1, eval env e2) with
      | (Closure(n, e3, env1), v) ->
        eval ((n, v) :: (env1 @ env)) e3
      | _ -> raise(Failure ("incompatible values on app"))
    )
  | LetRec (n, e1, e2) ->
     match e1 with
     | Lambda (n1, e1) -> eval (
       (n,
       Closure(n1, e1,
         (n, Closure(n1, e1, env)):: env
         )
       )::env
      ) e2
     | _ -> raise (Failure("incompatible values on LetRec") )

let evaluate e = eval [] e
(* Some sample expressions and their values *)
let e1 = Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))
let v1 = evaluate e1

let e2 = Sub (Val (Int 10), Div (e1, Val (Int 2)))
let v2 = evaluate e2

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
(* increment *)
let inc = Lambda ("n", Add(Id "n", Val (Int 1)))

let add = Lambda ("x",
                 Lambda ("y", Add (Id "x", Id "y"))
                )
let inc' = App (add, Val (Int 1))

(* The add2 closure *)
let add2app =
 Let ("add2",
      Let ("two", Val (Int 2), Lambda ("x", Add (Id "x", Id "two"))),
      App (Id "add2", Val (Int 4)))

(* sumToN *)
let sumToN : expr =
    LetRec ("sumToN",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 0),
                        Add (Id "n",
                             App (Id "sumToN",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "sumToN"
           )

(* factorial *)
let fact : expr =
    LetRec ("fact",
            Lambda ("n",
                    If (Eq (Id "n", Val (Int 0)),
                        Val (Int 1),
                        Mul (Id "n",
                             App (Id "fact",
                                  Sub (Id "n", Val (Int 1))
                                 )
                            )
                       )
                   ),
            Id "fact"
           )
