(*Lab04*)
(*Guangyu Yan*)
(*Fangfang Qiu*)

let rec length l = 
	match l with
	|[] -> 0
	|x::xs -> 1 + length xs
	
let length l = List.fold_left (fun x y -> 1 + x ) 0 l

let rec andf lst = 
	match lst with
	| [] -> true
	|x::xs -> if x then andf xs else false

let andf l = List.fold_left (fun x y -> y && x) true l

let orf l = List.fold_left (fun x y -> y || x) false l

let is_elem target l = List.fold_left (fun x y -> if y == target then true else false) false l 

let rev lst = List.fold_left(fun x y -> y :: x) [] lst 

let ascii_sum lst = List.fold_left (fun x y  -> Char.code y + x ) 0 lst

let lebowski lst = List.fold_right (fun y x -> if y == '.' then [','; ' '; 'd'; 'u'; 'd'; 'e'; '.']@x else y::x) lst []
