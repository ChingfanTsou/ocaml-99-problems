(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two l =
  match l with
  | [] -> None
  | x :: [] -> None
  | x :: y ::[] -> Some (x, y)
  | x :: t -> last_two t
