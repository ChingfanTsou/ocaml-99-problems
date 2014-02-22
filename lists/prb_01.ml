(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last l =
  match l with
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> last t
