open Graph_coloring
open LazeeModules
open StreamModulesRedux
module LS = Stream_v2(Lazee_v1)
type graph = node list * edge list

type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list
type 'a option = None | Some of 'a


type 'a lazee
type 'a t = 'a node_t lazee
and 'a node_t =
  | Cons of 'a * 'a t
  | Nil


(*my first helper function*)
let rec getColor (c: coloring) (n: node) : color =
  match c with
  | [] -> C 9
  | (n1, c1) :: rest -> if n1 = n then c1
                        else getColor rest n

(*my second helper function*)

let rec validcolor (c: coloring) ((nd, ed): graph): bool =
  match c with
  | [] -> true
  | (n1, c1) :: rest -> let compare (a, b) = (a = n1 || b = n1) in
    let new_node_edge = List.filter compare ed in
    let helper1 (valid: bool) (n1, n2) = (getColor c n1 != getColor c n2)
          && valid in List.fold_left helper1 true new_node_edge



let color_option (glt: graph): coloring option =
  match glt with
  |(nd, eg) ->
  let rec gen_color target rest =
    let find = validcolor target glt in
      if not (find) then None
      else
        if (target <> []) && (rest = [])
        && (List.length target = List.length nd) &&
        let count lst =
          let is_element set (a, b) =  if List.mem b set then set
          else b :: set
          in List.fold_left is_element [] lst in
                (List.length  (count target) = 3)
        then Some (List.rev target)
        else
          match rest with
          | [] -> None
          | hd::tl -> match gen_color ((hd, C 1)::target) tl with
                | Some result -> Some result
                | None -> match gen_color ((hd, C 2)::target) tl with
                    | Some result -> Some result
                    | None ->  match gen_color ((hd, C 3)::target) tl with
                        | Some result -> Some result
                        | None -> None
    in gen_color [] nd
