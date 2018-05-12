(* Adding function values, recursive let expressions, lambda
   expressions and function application. 

   This language extends the language in ``expr_let.ml``.

   Eric Van Wyk   
 *)

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


let rec freevars (e: expr) : string list = 
  match e with
  | Val _ -> []
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Let (n, dexpr, body) ->
     freevars dexpr @ (List.filter (fun n' -> n <> n') (freevars body))
  | Id n -> [n]


let rec eval (env: environment) (e: expr) : value = 
  match e with
  | Val v -> v


  | Let (n, dexpr, body) ->
      let v = eval env dexpr in
      eval ( (n,v)::env ) body
  | Id n -> lookup n env


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

let () =
  assert (serialize e1 = "Add (Val (Int 1), Mul (Val (Int 2), Val (Int 3)))");
  assert (serialize e6 = 
            "Let (\"y\", Val (Int 5), Let (\"x\", " ^ 
              "Add (Id \"y\", Val (Int 5)), Add (Id \"x\", Id \"y\")))")


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

let () =
  assert (evaluate e1 = Int 7);
  assert (evaluate e2 = Int 7);
  assert (evaluate e3 = Bool true);
  assert (evaluate e4 = Bool false);
  assert (evaluate e5 = Bool true);
  assert (evaluate e6 = Int 15);
  assert (evaluate e7 = Bool true)


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

let () =
  assert (evaluate (App (inc, Val (Int 4))) = Int 5);
  assert (evaluate (Add (Val (Int 2), Val (Int 3))) = Int 5);
  assert (evaluate (App (inc', Val (Int 4))) = Int 5);
  assert (evaluate add2app = Int 6)


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

(* Assert expressions to test our functions. *)
let () =
  assert (evaluate (App (sumToN, Val (Int 4))) = Int 10);
  assert (evaluate (App (sumToN, Val (Int 10))) = Int 55);
  assert (evaluate (App (sumToN, Val (Int 100))) = Int 5050);
  assert (evaluate (App (fact, Val (Int 0))) = Int 1);
  assert (evaluate (App (fact, Val (Int 1))) = Int 1);
  assert (evaluate (App (fact, Val (Int 2))) = Int 2);
  assert (evaluate (App (fact, Val (Int 4))) = Int 24)



(* If utop gets to this point without raising an ``assert`` exception
   then all tests have passed. *)
let () =
  print_endline ("Success! All tests passed.")
