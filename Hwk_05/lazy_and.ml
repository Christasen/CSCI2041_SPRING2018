let rec ands (lst: bool list): bool =
  match lst with
  | [] -> true
  | hd :: tl -> if hd then hd && (ands tl)
                else false
