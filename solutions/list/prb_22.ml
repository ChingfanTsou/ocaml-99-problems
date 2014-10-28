(* Create a list containing all
   integers within a given range.
 *)

(* If first argument is smaller
   than second, produce a list in
   decreasing order.
 *)

let range l r =
  let d = if (l <= r) then -1 else 1 in
  let rec range_aux r acc =
    if r <> l then
      range_aux (r+d) (r::acc)
    else
      r::acc in
  range_aux r []

assert (range 4 9 = [4;5;6;7;8;9])
assert (range 9 4 = [9;8;7;6;5;4])
assert (range 5 5 = [5])
