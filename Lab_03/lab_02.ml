(*csci2041*)
(*Lab3*)
(*Guangyu Yan*)
(*Sorry I can not find anyone around me so my partners are my two TAs :)))*)

let circle_circum_v1 r = 3.1415926*.r*.2.0
(*I get ride of fun r part to make the code shorter*)

let circle_circum_v2 r = let pi = 3.1415926 in 2.0*.pi*.r
(*same as fun1, I get ride of fun r part*)

let rec product xs =
  match xs with
  | [] -> 1
  | x::rest -> x * product rest
 (*I missed a case that [] should return 1*)
 (*I talked to 2 TAs and she commented I should combine two cases which are |X::[] and |x::rest*)

let rec sum_sqrdiffs xs =
  match xs with
  | x1::(x2::[]) -> (x1 - x2) * (x1 - x2)
  | x1::(x2::rest) -> (x1 - x2) *(x1 - x2) + sum_sqrdiffs (x2::rest)
  | _ -> raise (Failure "sum_sqrdiffs input list needs at least two elements")
(*Initially, I missed the _ case and have warning here. Now I know what is going on*)


let upper_right (x,y) = x > 0.0 && y > 0.0

let distance (a,b) (c,d) = sqrt((a-.c)*.(a-.c) +. (b-.d)*.(b-.d))

let triangle_perimeter p1 p2 p3 = distance p1 p2 +. distance p2 p3 +. distance p1 p3

