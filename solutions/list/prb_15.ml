(* Replicate the elements of a list a given number of times. *)

let replicate l n =
  let rec rep_n elt r_l n =
    if n = 0 then r_l
    else rep_n elt (elt::r_l) (n-1)
  in
  let rec rep l r_l =
    match l with
    | [] -> List.rev r_l
    | x :: t -> rep t (rep_n x r_l n)
  in
  rep l []
