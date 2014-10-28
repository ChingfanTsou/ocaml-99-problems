(* Drop every N'th element from a list. *)

let drop l n =
  let rec dr l n_l num =
    match l with
    | [] -> List.rev n_l
    | x :: t ->
       if num > 1 then
	 dr t (x::n_l) (num-1)
       else
	 dr t n_l n
  in
  dr l [] n
