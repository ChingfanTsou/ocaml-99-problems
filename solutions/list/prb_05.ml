(* Reverse a list. *)
let rev l =
  let rec rev_t l l_r =
    match l with
    | [] -> l_r
    | x :: t -> rev_t t (x::l_r) in
  rev_t l []

TEST "05_nom" =
  rev ["a" ; "b" ; "c"] =
  ["c"; "b"; "a"]

TEST "05_null" =
  rev [] =
  []
