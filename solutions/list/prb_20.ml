(* Remove the K'th element from a list. *)
(* The first element of the list is numbered 0, the second 1,... *)
let remove_at n l =
  let rec rm i l a_l =
    match l with
    | [] -> List.rev a_l
    | x :: t ->
       if (i = n) then
	 rm (i+1) t a_l
       else
	 rm (i+1) t (x::a_l) in
  rm 0 l []

assert (remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"])
