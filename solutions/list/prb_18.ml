(* Extract a slice from a list. *)

let slice l bg ed =
  let rec aux_slice i li acc_l =
    if i > ed then
      (List.rev acc_l)
    else if i >= bg then
      aux_slice (i+1) (List.tl li) ((List.hd li)::acc_l)
    else 
      aux_slice (i+1) (List.tl li) acc_l
  in aux_slice 0 l []
