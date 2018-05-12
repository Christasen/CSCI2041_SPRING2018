
(* Some preliminaries, as seen before. *)
let s = [ 1; 3; -2; 5; -6 ]   (* sample set from the S6 slides *)

let sum lst = List.fold_left (+) 0 lst

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"


 (* --- 
   Exceptions 
   ---
 *)


(* We can also use exceptions in searching.  This goes against the
   general principle of only throwing an exception for truly
   unexpected results, but it does make writing the code a bit more
   convenient, so we will use them in this non-traditional way.

   In our first usage, an exception is thrown when we've found the
   value that we want and this quickly returns us to the top level
   where we can then report success.

   We now execute the two recursive calls to 'try_subset' in sequence,
   not needing to inspect the output of the first one.  If the first
   call finds a solution then it will raise an exception.  So we
   don't care about the value returned by that first call.  If it
   returns it only does so if it didn't find a solution, in which case
   we want to just keep searching.
 *)

exception FoundSubSet of int list 

(* The subsetsum function that raises an exception on finding a
   solution. 
 *)
let subsetsum_exn_on_found (lst: int list) : int list option =
  let rec try_subset partial_subset rest_of_the_set =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest_of_the_set = []
    then raise (FoundSubSet partial_subset)
    else match rest_of_the_set with
         | [] -> () 
         | x::xs -> try_subset (x::partial_subset) xs ;
                    try_subset partial_subset xs

  in try try_subset [] lst; None  with
     | FoundSubSet result -> Some (List.rev result)



(* Another way to use exceptions in searching is to raise an exception
   when we the search process has reached a deadend or the found
   solution is not acceptable.

   In both cases we want to keep looking.  Thus we create a
   "KeepLooking" exception.
 *)
exception KeepLooking


(* In this example, we raise an exception when we reach a deadend in
   the search process.  This exception is caught in one of two places.

   The first is at the point where there are more possibilities to
   explore, and thus another call to try_subset is made.

   The second is at the point where there are no more possibilities
   and thus we catch the exeption and return None.
 *)

let subsetsum_exn_not_found (lst: int list) : int list option =
  let rec try_subset partial_subset rest_of_the_set =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest_of_the_set = []
    then partial_subset 
    else match rest_of_the_set with
         | [] -> raise KeepLooking
         | x::xs -> try try_subset (x::partial_subset) xs with
                    | KeepLooking -> try_subset partial_subset xs
          
  in try Some (List.rev (try_subset [] lst))  with
     | KeepLooking -> None 



(* Begin discussion here on April 4 *)


(* In this example we again raise an exception to indicate that the
   search process should keep looking for more solutions, but now we
   use a version of the procss_solution function from above to have
   some process (the user) that can reject found solutions causing the
   function to keep searching.
 *)

let rec process_solution_exn show s =
  print_endline ("Here is a solution: " ^ show (List.rev s)) ;
  print_endline ("Do you like it ?" ) ;
  match "Y" = String.sub (String.capitalize_ascii (read_line ())) 0 1 with
  | true  -> print_endline "Thanks for playing..." ; s
  | false -> raise KeepLooking


(* This version of subsetsum is similar to subset_sum_option in that
   it uses a version of process_solution to keep looking for more
   solutions.
 *)

let subsetsum_exn (lst: int list) : int list option =
  let rec try_subset partial_subset rest_of_the_set =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest_of_the_set = []
    then process_solution_exn (show_list string_of_int) partial_subset 
    else match rest_of_the_set with
	 | [] -> raise KeepLooking
	 | x::xs -> try try_subset (x::partial_subset) xs with
		    | KeepLooking -> try_subset partial_subset  xs

  in try Some (List.rev (try_subset [] lst)) with
     | KeepLooking -> None






(* We can abstract the subsetsub problem a bit more by parameterizing
   by the function that is called when a candidate solution is found.

   The function passed in is sometimes referred to as a "continuation"
   as it indicates what the function should do, that is, how
   processing should continue, after it has completed its work. 
 *)


let subsetsum_exn_continutation 
      (lst: int list) (success: int list -> int list) 
    : int list option =
  let rec try_subset partial_subset rest_of_the_set =
    if sum partial_subset = 0 && 
       partial_subset <> [] && 
       rest_of_the_set = []
    then success partial_subset
    else match rest_of_the_set with
	 | [] -> raise KeepLooking
	 | x::xs -> try try_subset (x::partial_subset) xs with
		    | KeepLooking -> try_subset partial_subset  xs

  in try Some (List.rev (try_subset [] lst)) with
     | KeepLooking -> None

(* The function below has the same behavior as subsetsum_exn, but we
   pass in process_solution_exn as an argument instead of writing it
   explicitly in the body of the subsetsum function. 
 *)


let subsetsum_exn_v1 lst =
  subsetsum_exn_continutation lst (process_solution_exn (show_list string_of_int))



(* This function has the same behavior as our original subsetsum
   function that accepts the first solution.  Here the continuation
   function just wraps the result in a Some so that it can be
   returned.
 *)
let subsetsum_exn_first lst =
  subsetsum_exn_continutation lst (fun x -> x)

let subsetsum_exn_print_all lst =
  subsetsum_exn_continutation
    lst 
    (fun s -> print_endline ("Here you go: " ^ 
                               (show_list string_of_int (List.rev s))) ;
              raise KeepLooking )

let results = ref [ ]

let subsetsum_exn_save_all lst =
  subsetsum_exn_continutation 
    lst 
    (fun x -> results := x :: !results ; 
	      print_endline (show_list (string_of_int) x) ;
	      raise KeepLooking)

