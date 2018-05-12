
(*Question *)
(*

I have two types variable in my function
Type state (int *int) is to give us an idea that how much
water are there in the two gallons. The first int is the water for
the 4 gallons jug and the second int is the water for the 3 gallons jug.

Type move (state * operation) is to give us an idea that
based on each operation, what is the situation for the water in the two
jugs.





  *)


type operation = Fill4GallonJugFromTap
               | Fill3GallonJugFromTap
               | Empty4GallonJugOnGround
               | Empty3GallonJugOnGround
               | Fill4GallonJugFrom3GallonJug
               | Fill3GallonJugFrom4GallonJug
               | Empty4GallonJugInto3GallonJug
               | Empty3GallonJugInto4GallonJug

let describe (four:int) (three:int) : string =
  let describe' jug amount =
    "The " ^ string_of_int jug ^ " gallon jug " ^
    match amount with
    | 0 -> " is empty"
    | 1 -> " contains 1 gallon"
    | x -> " contains " ^ string_of_int x ^ " gallons"
  in
  describe' 4 four ^ ", " ^ describe' 3 three ^ "."

let show_list show l =
  let rec sl l =
    match l with
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

type state = int * int

type moves = state * operation


let rec is_not_elem (m: moves list) (s,o): bool =
  let f (a,b) = a
  in
  let newmove = List.map f m
  in
  not (List.mem s newmove)

exception FoundPath of moves list

let valid_moves (((a, b), op): moves): bool =
  (a <= 4 && b < 4 && b >=0 && a >= 0)  &&
 match op with
    | Fill3GallonJugFromTap -> b < 3
    | Fill4GallonJugFromTap -> a < 4
    | Empty4GallonJugOnGround -> a > 0
    | Empty3GallonJugOnGround -> b > 0
    | Empty4GallonJugInto3GallonJug -> ((a + b) <= 3) && a > 0
    | Empty3GallonJugInto4GallonJug -> ((a + b) <= 4) && b > 0
    | Fill4GallonJugFrom3GallonJug -> ((a + b) >= 4) && b > 0
    | Fill3GallonJugFrom4GallonJug -> ((a + b) >= 3) && a > 0



let final (s: state): bool = s = (2, 0)

let op_moves (((a,b),op): moves): moves =
   match op with
   | Fill4GallonJugFromTap -> ((4, b), Fill4GallonJugFromTap)
   | Fill3GallonJugFromTap -> ((a, 3), Fill3GallonJugFromTap)
   | Empty4GallonJugOnGround -> ((0, b), Empty4GallonJugOnGround)
   | Empty3GallonJugOnGround -> ((a, 0), Empty3GallonJugOnGround)
   | Empty4GallonJugInto3GallonJug ->
                                  ((0, a+b), Empty4GallonJugInto3GallonJug)
   | Empty3GallonJugInto4GallonJug ->
                                  ((a+b, 0), Empty3GallonJugInto4GallonJug)
   | Fill4GallonJugFrom3GallonJug ->
                                  ((4, a+b-4),Fill4GallonJugFrom3GallonJug)
   | Fill3GallonJugFrom4GallonJug ->
                                   ((a+b-3, 3), Fill3GallonJugFrom4GallonJug)


let possible_outcomeLists ((a,b): state): moves list =
  (((a, b), Fill4GallonJugFromTap)::
  ((a, b), Fill3GallonJugFromTap)
  :: ((a, b), Empty4GallonJugOnGround)
  :: ((a, b), Empty3GallonJugOnGround)
  ::((a, b),Fill4GallonJugFrom3GallonJug)
  ::((a, b), Fill3GallonJugFrom4GallonJug)
  ::((a, b), Empty4GallonJugInto3GallonJug)::
  ((a, b), Empty3GallonJugInto4GallonJug)::[])

let valid_outcomesList ((a,b): state): moves list =
  List.filter valid_moves (possible_outcomeLists (a,b))


let real_moves ((a,b): state): moves list =
  List.map op_moves (valid_outcomesList (a,b))

let rec translate (m: moves list): (operation * string) list =
  match m with
  | []->[]
  | ((a, b), op) :: xs ->  [(op, describe a b)] @ translate xs

let play (): (operation * string) list option =
  let rec go_from state path =
    if final state
    then raise (FoundPath path)
    else
      let generate : moves list =
        List.filter (is_not_elem path) (real_moves state)
      in
      let go_func () m =
      let (a, b) = m in  go_from a (path @ [m])
      in List.fold_left go_func () generate
    in try go_from (0, 0) [] ; None
     with FoundPath path -> Some (translate path)


(*A helper function to test my program*)
 let testplay () =
   let rec go_from state path =
     if final state
     then raise (FoundPath path)
     else
       let generate : moves list =
         List.filter (is_not_elem path) (real_moves state)
       in
       let go_func () m =
       let (a, b) = m in  go_from a (path @ [m])
       in
       List.fold_left go_func () generate
     in try go_from (0, 0) [] ; None
      with FoundPath path -> Some path
