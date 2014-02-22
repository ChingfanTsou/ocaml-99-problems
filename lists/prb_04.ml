(* Find the number of elements of a list. *)
let length l =
  let rec len l n =
    match l with
    | [] -> n
    | x :: t -> len t (n+1) in
  len l 0
