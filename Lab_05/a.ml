
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



let rec serialize (e: expr): string ==
  match e with
  | Val (n) ->"Val" ^ n
  | Mul(a, b) -> "Mul(" ^ serialize a ^ ","^ serialize b ^ ")"
  | Div(a, b) -> "Div(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Sub(a, b) -> "Sub(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Add(a, b) -> "Add(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Lt(a, b) -> "Lt(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Eq(a, b) -> "Eq(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | And(a, b) -> "And(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | App(a, b) -> "App(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | Lambda(a, b) -> "Lambda(" ^ serialize a ^ "," ^ serialize b ^ ")"
  | _ -> raise Failure exception "Will only serialize integer and Boolean values"
