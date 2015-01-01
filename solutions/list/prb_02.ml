(* Find the last but one (last and penultimate) elements of a list. *)
let rec last_two l =
  match l with
  | [] -> None
  | x :: [] -> None
  | x :: y ::[] -> Some (x, y)
  | x :: t -> last_two t

TEST "02_nom" =
  last_two [ "a" ; "b" ; "c" ; "d" ] =
  Some ("c", "d")

TEST "02_one" =
  (last_two [ "a" ]) =
  None

TEST "02_null" =
  (last_two []) =
   None
