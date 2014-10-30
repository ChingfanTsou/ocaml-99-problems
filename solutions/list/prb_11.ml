(** Modified run-length encoding.
    Modify the result of the previous
    problem in such a way that if an
    element has no duplicates it is
    simply copied into the result list.
    Only elements with duplicates
    are transferred as (N E) lists. *)

(** Since OCaml lists are homogeneous,
    one needs to define a type to hold both
    single elements and sub-lists. *)

type 'a rle_t =
  | One of 'a
  | Many of int * 'a

let uw = function
  | Some a -> a
  | None -> raise Not_found
              
let encode l =
  let cur_rle, f_l =
    List.fold_right (fun x par_en_l ->
        let cur_rle, l = par_en_l in
        let n, ele = cur_rle in
        if (Some x) = ele then
          (n+1, ele),l
        else if n = 1 then
          (1, Some x),((One  (uw ele)) :: l)
        else
          (1, Some x),((Many (n, uw ele)) :: l))
      l ((0, None),[]) in
  f_l
