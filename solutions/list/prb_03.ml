(* Find the k'th element of a list. *)
let rec at n l =
  match l with
  | x :: t ->
     if n = 1 then
       Some x
     else
       at (n-1) t
  | [] -> None

TEST "03_nom" =
  at 3 [ "a" ; "b"; "c"; "d"; "e" ] =
  Some "c"

TEST "03_null" =
  at 3 [ "a" ] =
  None
