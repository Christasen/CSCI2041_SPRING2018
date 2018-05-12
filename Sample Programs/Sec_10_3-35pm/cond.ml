let cond c t e = print_endline "In cond" ; if c then t else e

let rec sumton n 
  = cond (n = 1)
         (1)
         (n + sumton (n-1))

let v = sumton 1
