(*Guangyu Yan*)
(*Fangfang Qiu 
gave me some good advice on formatting*)


let all_evens lst1 = List.filter (fun x -> x mod 2=0) lst1


let increment_all lst2 = List.map (fun x -> x+1 ) lst2

let max_fold lst3 =
	match lst3 with
	|x1::[] -> x1
	|x1::xs -> List.fold_left(fun x y -> if x-y> 0 then x else y) x1 xs
	|_ -> raise (Failure "Input list must not be empty")
	 


let sum_prod lst4 = 
	let accum  = (0,1)
	in 
	let f (sum, pro) x = (sum + x, pro * x)
	in 
	List.fold_left f accum lst4
	
	

let split(f:'a -> bool) (lst5:'a list):'a list list = 
	let accum = ([],[])
	in 
	let g(sub, current) x = 	
		if f x = true then (List.rev current :: sub, [])		
		else (sub, x:: current)
	in 	
	let (lsts, curr) = List.fold_left g accum lst5
	in
	List.rev(List.rev curr :: lsts)

(*for the split function, first it is better to explain the function type a little bit and to tell the machine 
what is gonna be the type of your arguements*)

(*Also, just use more space between operators and arguments to make the program looks better*)
(*Keep doing the same let in structure everywhere to make the program looks better*)

(*I talked to my lovely TA and they are really helpful with correcting few coding structure problems*)


