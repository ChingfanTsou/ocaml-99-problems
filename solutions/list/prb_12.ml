(* Decode a run-length encoded list. *)

type 'a rle_t =
  | One of 'a
  | Many of int * 'a

let decode l =
  let rec dup n rle acc =
    if n = 0 then acc else dup (n-1) rle (rle::acc) in
  let rec de_aux en_l de_l =
    match en_l with
    | [] -> de_l
    | Many (n,c) :: t -> de_aux t (dup n c de_l)
    | One x :: t -> de_aux t (x::de_l) in
  List.rev (de_aux l [])
