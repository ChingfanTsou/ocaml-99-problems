(* Pack consecutive duplicates of list elements into sublists. *)
let pack l =
  let rec pc_aux d_l pcg p_l =
    match d_l with
    | [] -> p_l
    | x :: t ->
       if pcg = [] || x = (List.hd pcg) then
	 pc_aux t (x::pcg) p_l
       else
	 pc_aux t (x::[]) (pcg::p_l) in
  List.rev (pc_aux l [] [])
