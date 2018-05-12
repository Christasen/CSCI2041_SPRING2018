(* Person, Wolf, Goat, Cabbage 

   Consider the problem of a person needing to move his wolf, goat,
   and cabbage across a river in his canoe, under the following
   restrictions:

   - The canoe holds only the person and one of the wolf, goat, or 
     cabbage. 
   - The goat and cabbage cannot be left unattended or the goat will 
     eat the cabbage. 
   - The wolf and the goat cannot be left unattended or the wolf will
     eat the goat.
   - Only the person can operate the canoe.

   Is there a sequence of moves in which the person can safely transport
   all across the river with nothing being eaten?
*)


let rec is_not_elem set v = not (List.mem v set)

(* Types and functions for the crossing challenge. *)

(* Location: things are on the left (L) or right (R) side of the river. *)
type loc = L | R


(* A state in our search space is a configuration describing on which
   side of the river the person, wolf, goat, and cabbage are. *)
type state = loc * loc * loc * loc

(* A state is safe, or OK, when the goat and cabbage are together only
   when the person is also on the same side of the river and when the
   wolf and the goat are together only when person is on the same side of
   the river. 
 *)
let ok_state ( (p,w,g,c) :state) : bool 
 =  p=g || (g <> c && g <> w)


(* The final state, or gaol state, is when everything is on the right (R)
   side of the river.
 *)
let final s = s = (R,R,R,R)

let other_side = function
  | L -> R
  | R -> L

(* The function moves generates a list of valid moves.  If computes
   all possible moves and then filters out the moves that are not "ok"
   - that is, moves that would result in something being eaten.
 *)
let moves (s:state) : state list = 
 let move_person (p,w,g,c) 
   = [ ( other_side p, w, g, c ) ] 
 in
 let move_wolf (p,w,g,c) 
   = if p = w
     then [ ( other_side p, other_side w, g, c ) ]
     else [ ]
 in
 let move_goat (p,w,g,c)
   = if p = g
     then [ ( other_side p, w, other_side g, c ) ]
     else [ ]
 in
 let move_cabbage (p,w,g,c)
   = if p = c
     then [ ( other_side p, w, g, other_side c ) ]
     else [ ]
 in List.filter ok_state ( move_person s @ move_wolf s @ 
                           move_goat s @ move_cabbage s )


(* A solution using options that returns the first safe sequence of moves. *)
let crossing_v1 () =
  let rec go_from state path =
    if final state
    then Some path
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> None
      | [a] -> (go_from a (path @ [a]) )
      | [a;b] -> 
	 (match go_from a (path @ [a])  with
	  | Some path' -> Some path'
	  | None -> go_from b (path @ [b])
	 )
      | _ -> raise (Failure ("No way to move 3 things!"))
  in go_from (L,L,L,L) [ (L,L,L,L) ] 



(* Here is a solution that raises an exception when we've found a safe
   sequence of moves.  It then stops. 
 *)
exception FoundPath of (loc * loc * loc * loc) list

let crossing_v2 () =
  let rec go_from state path =
    if final state 
    then raise (FoundPath path)
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> ()
      | [a] -> (go_from a (path @ [a]) )
      | [a;b] -> 
         go_from a (path @ [a]) ;
	 go_from b (path @ [b])
      | [a;b;c] -> 
         go_from a (path @ [a]) ;
	 go_from b (path @ [b]) ;
	 go_from c (path @ [c])
      | _ -> raise (Failure ("Being careful, no way to move 4 things!"))

  in try go_from (L,L,L,L) [ (L,L,L,L) ] ; None
     with FoundPath path -> Some path


(* What if we don't know how many moves can be made?  Or if the number
   of moves is large enough that the above pattern becomes even more 
   tedious? *)

let crossing_many_possible_moves () =
  let rec go_from state path =
    if final state 
    then raise (FoundPath path)
    else
      let valid_moves : state list = List.filter (is_not_elem path) (moves state)
      in
      let go_func () m = go_from m (path @ [m])
      in
      List.fold_left go_func () valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ] ; None
     with FoundPath path -> Some path


(* List.fold_left works fine, but there is a more specific function,
   List.iter: ('a -> unit) -> 'a list -> unit
   that more specifically fits this problem.
 *)
let crossing_many_possible_moves' () =
  let rec go_from state path =
    if final state 
    then raise (FoundPath path)
    else
      let valid_moves : state list = List.filter (is_not_elem path) (moves state)
      in
      let go_func m = go_from m (path @ [m])
      in
      List.iter go_func valid_moves

  in try go_from (L,L,L,L) [ (L,L,L,L) ] ; None
     with FoundPath path -> Some path





(* A solution that allows use to keep looking for additional safe
   sequences of moves.  
  *)

exception KeepLooking

(* This is the same process_solution_exn function from search.ml *)
let rec process_solution_exn show s = 
  print_endline ( "Here is a solution:\n" ^ show s) ;
  print_endline ("Do you like it?") ;

  match "Y" = String.sub (String.capitalize_ascii (read_line ())) 0 1 with
  | true  -> print_endline "Thanks for playing..." ; Some s
  | false -> raise KeepLooking


(* Some function for printint a sequence of moves. *)
let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_loc = function
  | L -> "L"
  | R -> "R"

let show_state (p,w,g,c) =
  "(" ^ show_loc p ^ ", " ^ show_loc w ^ ", " ^ 
        show_loc g ^ ", " ^ show_loc c ^ ")"

let show_path = show_list show_state

(* The solution that lets a user selected from all (2) safe paths. *)
let crossing_v3 () =
  let rec go_from state path =
    if final state 
    then process_solution_exn show_path path 
    else
      match List.filter (is_not_elem path) (moves state) with
      | [] -> raise KeepLooking
      | [a] -> go_from a (path @ [a]) 
      | [a;b] -> 
         (try go_from a (path @ [a]) with 
	  | KeepLooking -> go_from b (path @ [b])
	 )
      | _ -> raise (Failure ("No way to move 3 things!"))

  in try go_from (L,L,L,L) [ (L,L,L,L) ] with
     | KeepLooking -> None


