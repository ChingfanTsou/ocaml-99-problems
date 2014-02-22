(* Find the k'th element of a list. *)
let rec at n l =
  match l with
  | x :: t ->
     if n = 1 then
       Some x
     else
       at (n-1) t
  | [] -> None
