open Intervals

let rec euclid x y =
	if x = y then x
	else if x< y then euclid x (y-x)
	else euclid (x-y) y


let frac_simplify (x,y) =
	let z = euclid x y
	in if z = 1 then (x,y)
	else (x/z,y/z)

module Rational_comparable : (Comparable with type t = int * int) = struct
  type t = int * int
<<<<<<< HEAD
  let compare = compare
=======
  let compare(n1,d1) (n2,d2) = compare (n1*d2) (n2*d1)
>>>>>>> c6ea1d791678dc28141298e015211e139c4d4b4a
  let to_string (x, y) =
    let (a, b) =
     frac_simplify (x, y)
      in string_of_int a ^ "/" ^ string_of_int b
end

module Rational_interval = Make_interval(Rational_comparable)



(* The following line now works. *)
let i = Rational_interval.create (3, 5)
