(*Question 1 *)


(* In my sulution, in order to finish this probelm, I have few helper
functions.
1. The first helper function is called get color and in this function,
I want to check whether for a specific node, there are is a specific node with
a color in a coloring List. So in this function, the search space is the
coloring list and we will compare every node element in the coloring with
a specific node)
ex-> coloring = n1::n2::...::[], target = n;
compare n1 with n the first time, if not equal then compare n2 with n...
loop till it reaches the end of the list or find the solution...

 by the end of this function, we can get a color for the node.

2. The second helper function valid color is to check whether we have a
valid colorfor the next new node in the graph's edge list component.
In order to do that,I found the new node in the coloring list
and I will find the all possible outcomes that satisfy our requirement
in the compare function from the edge list and
this is the search space. Then I will use this outcome list to find
whether it is a valid color by using the fold_left method under our first
helper function.
ex..coloring = [(N1, C1), (N2, C2)......]
g1 = ([N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4)])
g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]

The first thing we do here is to check all possible outcomes for
the node of first element (N11 C1) based on the edge list of g1
and we can found [(N1, N2) (N1, N3)] as the outcome list.
Then we use a helper function and our getColor function to see
whether this is a valid color based on our outcome lists and return a boolean
result.




3. Our main function is mainly based on our two helper function. The search
space is really complex here but basically what I am doing in this function
is try to give every node a color. And there are few requiremnet for given
the node a color, Start from the first node, we will give the first node
a color C1 and append it in a the target list. and then we will try
to give the second node a color if it satisfy the validcolor requirement
and other requirements. If it does not satisfy the requirements then
we will return a none for this round. And then we will go to the next
round...
In here we have a special helper function and this
function is help to guarantee the requirement that
target list contains three different color to represents the nodes. And
the whole process is complex and we will get a output result when we finally
reach to a some type..


  *)

(*Question 2

I have a valid_color function to check it whether this is a valid move
every time when I add a new elememt to my target list. We dont have to
reach to all the possible solutions and go back to elimante few choice
and we could elimante few choices each time by checking the value
of the valid_color function and return a None. Also, there are is a strict
requirement for three different color to represents nodes and there is
a helper function called count to satisfy this requirement. If we don't
have a three different color in the target list, we would never go to the
some result part because everytime we will assign a none type to let the
function goes into next loop round





*)






type node = N of int
type edge = node * node
type graph = node list * edge list

type color = C of int
type coloring = (node * color) list
type 'a option = None | Some of 'a

let g1 = ( [N 1; N 2; N 3; N 4], [ (N 1,N 2); (N 1,N 3); (N 2,N 3); (N 3,N 4)])

let g1_coloring = [ (N 1,C 1); (N 2,C 2); (N 3,C 3); (N 4,C 2) ]


let show_list show l =
  let rec sl l =
    match l with
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"


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





exception FoundColoring of coloring



let color_exception (glt: graph): unit =
  match glt with
  |(nd, eg) ->
  let rec try_color target rest =
  let find = validcolor target glt in
    if not (find) then ()
    else
      if (target <> []) && (rest = [])
      && (List.length target = List.length nd) &&
      let count lst =
        let is_element set (a, b) =  if List.mem b set then set
        else b :: set
        in List.fold_left is_element [] lst in
              (List.length  (count target) = 3)
      then raise (FoundColoring (List.rev target))
      else
          (match rest with
          | [] -> ()
          | hd::tl -> try_color ((hd, C 1)::target) tl ;
                      try_color ((hd, C 2)::target) tl ;
                      try_color ((hd, C 3)::target) tl
          )
  in try_color [] nd;
