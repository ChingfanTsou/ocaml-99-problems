(* Duplicate the elements of a list. *)

let duplicate l =
  let rec dup_aux l d_l =
    match l with
    | [] -> d_l
    | x :: t -> dup_aux t (x::x::d_l)
  in
  List.rev (dup_aux l [])
