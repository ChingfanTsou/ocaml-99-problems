(* Find the number of elements of a list. *)
let length l =
  let rec len l n =
    match l with
    | [] -> n
    | x :: t -> len t (n+1) in
  len l 0

TEST "04_nom" =
  length [ "a" ; "b" ; "c"] =
  3

TEST "04_null" =
  length [] =
  0
