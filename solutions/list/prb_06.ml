(* Find out whether a list is a palindrome. *)
let is_palindrome l =
  l = List.rev l

TEST "06_pos" =
  is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true

TEST "06_neg" =
  is_palindrome [ "a" ; "b" ] = false

TEST "06_null" =
  is_palindrome [] = true

