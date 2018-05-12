let circle_circum_v1 = fun r ->3.1415*.r*.2.0

let circle_circum_v2 = fun r -> let pi = 3.1415 in 2.0*.pi*.r

let rec product xs =
  match xs with
  | x::[] -> x
  | x::rest -> x * product rest

let rec sum_sqrdiffs xs =
  match xs with
  | x1::(x2::[]) -> (x1 - x2) * (x1 - x2)
  | x1::(x2::rest) -> (x1 - x2) *(x1 - x2) + sum_sqrdiffs (x2::rest)

let upper_right (x,y) = x > 0.0 && y > 0.0


let distance (a,b) (c,d) = sqrt((a-.c)*.(a-.c) +. (b-.d)*.(b-.d))

let triangle_perimeter p1 p2 p3 = distance p1 p2 +. distance p2 p3 +. distance p1 p3
