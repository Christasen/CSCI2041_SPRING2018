(*
SPRING 2018 
CSCI2041 HW01 
GUANGYU YAN
*)

let even x = x mod 2 = 0

let rec euclid x y =
	if x = y then x
	else if x< y then euclid x (y-x)
	else euclid (x-y) y 

let frac_simplify (x,y) =
	let z = euclid x y 
	in if z = 1 then (x,y)
	else (x/z,y/z)
	


let rec max lst =
	let f x y = if x-y > 0 then x else y
	in match lst with	
	|x1::[] -> x1
	|x1::x2::xs -> max (f x1 x2::xs)
	|_ -> raise (Failure "Input list must not the empty")

let rec take x lst =
	if x > 0 then
	match lst with
	|[] -> []
	|x1 :: xs -> x1 :: take (x-1) xs
	else []
	
