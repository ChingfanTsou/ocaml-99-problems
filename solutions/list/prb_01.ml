(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)
let rec last l =
  match l with
  | [] -> None
  | h :: [] -> Some h
  | h :: t -> last t

TEST "01_nom" =
  Some "d" = last [ "a" ; "b" ; "c" ; "d" ]
               
TEST "01_null" =
  None = last []
