(* Extract a given number of randomly
   selected elements from a list.
 *)

(* The selected items shall be returned
   in a list. We use the Random module
   but do not initialize it with
   Random.self_init for reproducibility.
 *)

let rand_select l n =
  let rec rs n_l n rl =
    if n = 0 then
      n_l
    else
      let rn = Random.int (List.length rl) in
      rs ((List.nth rl rn)::n_l) (n-1) (Prb_20.remove_at rn rl)
  in
  rs [] n l

TEST "23" =
  let l = ["a";"b";"c";"d";"e";"f";"g";"h"] in
  let rs_l = rand_select l 3 in
  if List.length l <> 3 then
    false
  else
    List.fold_left (fun t x -> (List.mem x l) && t) true rs_l

