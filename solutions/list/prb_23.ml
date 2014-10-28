(* Extract a given number of randomly
   selected elements from a list.
 *)

(* The selected items shall be returned
   in a list. We use the Random module
   but do not initialize it with
   Random.self_init for reproducibility.
 *)

let rand_select l n =
  let rec rs l n_l n =
    if n = 0 then
      n_l
    else
      let rn = Random.int (List.length l) in
      
  in
  rs l [] n
