let p1 = "Hello world!\n\n How are you today? \t\t I hope all is well. "


let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0
  
let read_file (file_name: string) : char list =
  let ic = open_in file_name 
  in 
  let rec read_chars ic =
    try 
      let next_char = input_char ic
      in next_char :: read_chars ic
    with 
      _ -> [] 
  in read_chars ic
 
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)


let split f lst5 = 
	let accum = ([],[])
	in 
	let g(sub, current) x = 	
		if f x = true then (List.rev current :: sub, [])		
		else (sub, x:: current)
	in 	
	let (lsts, curr) = List.fold_left g accum lst5
	in
	List.rev(List.rev curr::lsts)

let is_elem target l = List.fold_left (fun x y -> if y = target then true else x||false) false l

let m x = x = '\n' ||  x = '\t' || x = ' ' || x = '\r'

let format string1 n = 
	let list1 = explode string1
	in 
	let list2 = split m list1
	in
	let list3 = List.map implode list2
	in let list4 = List.filter (fun x -> String.length x > 0) list3
	in 
	
	let g(sub,curr, size) x = 
		if size+ String.length x > n then (sub^String.trim curr^"\n", x^" ", String.length x+1)
		else (sub, curr^x^" ", size + String.length x +1)

	in let (s,c,cnt) = List.fold_left g ("","",0) list4
	in  String.trim (s^String.trim c)
	
