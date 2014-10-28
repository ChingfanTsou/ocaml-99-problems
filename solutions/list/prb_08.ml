(* Eliminate consecutive duplicates of list elements. *)
let compress dup_l =
  let rec comp_aux dup_l eli_l =
    match dup_l with
    | [] -> eli_l
    | x :: t ->
       if (eli_l <> []) && x = (List.hd eli_l) then
	 comp_aux t eli_l
       else
	 comp_aux t (x::eli_l) in
  List.rev (comp_aux dup_l [])
