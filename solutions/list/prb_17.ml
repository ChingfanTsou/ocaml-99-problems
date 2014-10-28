(* Split a list into two parts; the length of the first part is given. *)
(* If the length of the first part is longer than the entire list, *)
(* then the first part is the list and the second part is empty. *)

let split l n =
  let rec sp fst_l l n =
    if n = 0 || l = [] then
      (List.rev fst_l),l
    else
      sp ((List.hd l)::fst_l) (List.tl l) (n-1)
  in sp [] l n
