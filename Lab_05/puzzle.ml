(*Guangyu Yan*)
(*Fangfang Qiu 
gave me some good advice on formatting*)

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
  
let d1 = "../../public-class-repo/Homework/Files/words-small.txt"
let d2 = "../../public-class-repo/Homework/Files/words-google-10000.txt"

let split(f: 'a -> bool) (lst5: 'a list): 'a list list = 
	let accum = ([],[])
	in 
	let g(sub, current) x = 	
		if f x = true then (List.rev current :: sub, [])		
		else (sub, x:: current)
	in 	
	let (lsts, curr) = List.fold_left g accum lst5
	in
	List.rev(List.rev curr :: lsts)
	
let f x = x = '\n' ||  x = ' '

let is_elem target l = List.fold_left (fun x y -> if y = target then true else x||false) false l


let answers (file: string): string list = 
	let list1 = read_file file
	in
	let list2 = split f list1
	in
	let list3 = List.map implode list2
	in
	let f x y = if String.length y = 6 && (is_elem (String.sub y 1 4) list3) then y :: x else x
	in List.rev (List.fold_left f [] list3)


	
let pretty_answers (lis: string list): (string*string) list = List.rev(List.fold_left(fun x y -> (String.sub y 1 4 , y)::x) [] lis)

(*for the answers function, first it is better to explain the function 
type a little bit and to tell the machine 
what is gonna be the type of your arguements*)
(*In order to make the lines more shorter I can use some helper function 
by using in let statement*)
(*Also, just use more space between operators and arguments to make the program looks better*)
(*Keep doing the same let in structure everywhere to make the program looks better*)
(*I talked to my lovely TA and they are really helpful with correcting few coding structure problems*)
