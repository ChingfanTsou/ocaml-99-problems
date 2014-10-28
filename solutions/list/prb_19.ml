(* Rotate a list N places to the left. *)

let rotate l n =
  let rec tie l1 l2 =
    if l1 = [] then
      l2
    else 
      tie (List.tl l1) ((List.hd l1)::l2) in
  let rec rt_aux fst_l l n =
    if n = 0 || l = [] then
      tie (List.rev l) (List.rev fst_l)
    else
      rt_aux ((List.hd l)::fst_l) (List.tl l) (n-1) in
  if n > 0 then
    rt_aux [] l n
  else
    rt_aux [] l ((List.length l) + n)
