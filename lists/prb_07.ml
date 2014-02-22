(* Flatten a nested list structure. *)

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten n_list =
  let rec fl_aux l f_l =
    match l with
    | [] -> f_l
    | One x :: t -> fl_aux t (x::f_l)
    | Many x :: t -> fl_aux t (fl_aux x f_l) in
  List.rev (fl_aux n_list [])
