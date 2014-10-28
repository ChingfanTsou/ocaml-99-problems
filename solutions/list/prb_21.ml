(* Insert an element at a given position into a list. *)
(* Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.) *)

let insert_at new_ele pos l =
  let rec ins p l n_l =
    match l with
    | [] -> 
       if pos >= p then
	 new_ele::n_l
       else
	 n_l
    | x :: t ->
       if p = pos then
	 ins (p+1) t (x::new_ele::n_l)
       else 
	 ins (p+1) t (x::n_l) in
  List.rev (ins 0 l [])

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"])
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"])
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"])
